# Project bootstrap / danger-zone reset for collage.
#
# Safe by default when demo = FALSE and force = FALSE.
# Reshape / restore from the app call with reset flags — those wipe data.
#
# Usage (from collage/):
#   source("assets/setup.R")
#   setup_collage(demo = TRUE)
#   restore_collage_defaults(demo = TRUE)   # full factory reset
#   reshape_collage(rows = 10, cols = 10, scheme = "sequential", demo = TRUE)

suppressPackageStartupMessages({
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Please install magick: install.packages(\"magick\")", call. = FALSE)
  }
  library(magick)
})

.setup_root <- function() {
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", ca[startsWith(ca, "--file=")])
  if (length(file_arg) == 1L && nzchar(file_arg)) {
    assets_dir <- dirname(normalizePath(file_arg, winslash = "/", mustWork = FALSE))
    if (basename(assets_dir) == "assets") {
      return(normalizePath(dirname(assets_dir), winslash = "/", mustWork = FALSE))
    }
  }
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile) && nzchar(ofile)) {
    assets_dir <- dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE))
    if (basename(assets_dir) == "assets") {
      return(normalizePath(dirname(assets_dir), winslash = "/", mustWork = FALSE))
    }
  }
  cwd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (dir.exists(file.path(cwd, "R")) && dir.exists(file.path(cwd, "assets"))) {
    return(cwd)
  }
  if (basename(cwd) == "assets" && dir.exists(file.path(dirname(cwd), "R"))) {
    return(dirname(cwd))
  }
  cwd
}

source_project_r <- function(root) {
  for (f in c("paths.R", "slots.R", "crop.R", "images.R", "poster.R")) {
    p <- file.path(root, "R", f)
    if (file.exists(p)) source(p, local = FALSE)
  }
}

#' Generate a solid-color square JPEG with white label text.
make_demo_tile <- function(label, out_path, size = 640L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  rgb <- sample(40:200, 3, replace = TRUE)
  color <- sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
  im <- image_blank(width = size, height = size, color = color)
  font_size <- max(22L, as.integer(size * 0.16))
  im <- image_annotate(
    im,
    label,
    gravity = "center",
    size = font_size,
    color = "white",
    strokecolor = "black",
    weight = 700
  )
  image_write(im, path = out_path, format = "jpeg", quality = 90)
  invisible(out_path)
}

make_blank_tile <- function(out_path, size = 640L) {
  im <- image_blank(width = size, height = size, color = "white")
  image_write(im, path = out_path, format = "jpeg", quality = 90)
  invisible(out_path)
}

clear_dir_images <- function(dir, also_remove_unmatched_demos = NULL) {
  if (!dir.exists(dir)) return(invisible(character(0)))
  files <- list.files(dir, pattern = "\\.(jpg|jpeg|png|webp)$", full.names = TRUE, ignore.case = TRUE)
  removed <- character(0)
  for (f in files) {
    ok <- unlink(f)
    if (ok == 0) removed <- c(removed, basename(f))
  }
  invisible(removed)
}

#' Bootstrap / rewrite collage data.
#'
#' @param reset_data If TRUE, overwrite profile + slots (danger).
#' @param clear_cropped If TRUE, delete files in cropped/
#' @param clear_demos If TRUE, delete existing demos before recreating (when demo=TRUE)
#' @param scheme Slot naming: "yymm", "rowcol", "sequential"
setup_collage <- function(root = NULL,
                          start_yymm = "0801",
                          end_yymm = "2612",
                          demo = FALSE,
                          force = FALSE,
                          seed = 18L,
                          rows = 19L,
                          cols = 12L,
                          dpi = 300,
                          spacing_mm = 1,
                          paper = "A1",
                          orientation = "portrait",
                          scheme = "yymm",
                          reset_data = TRUE,
                          clear_cropped = FALSE,
                          clear_demos = FALSE,
                          images_dir = "images") {
  if (is.null(root)) root <- .setup_root()
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  source_project_r(root)

  layout <- default_layout(root)
  ensure_project_dirs(layout)

  scheme <- tolower(as.character(scheme))
  slot_ids <- make_slot_ids(
    scheme = scheme,
    rows = rows,
    cols = cols,
    start_yymm = start_yymm,
    end_yymm = end_yymm
  )

  if (isTRUE(clear_cropped) && dir.exists(layout$cropped)) {
    clear_dir_images(layout$cropped)
  }

  if (isTRUE(clear_demos) && dir.exists(layout$demos)) {
    clear_dir_images(layout$demos)
  }

  if (isTRUE(reset_data)) {
    profile <- list(
      images_dir = images_dir,
      start_yymm = as.character(start_yymm),
      end_yymm = as.character(end_yymm),
      rows = as.integer(rows),
      cols = as.integer(cols),
      spacing_mm = as.numeric(spacing_mm),
      dpi = as.integer(dpi),
      paper = as.character(paper),
      orientation = as.character(orientation),
      slot_scheme = scheme
    )
    write_profile(profile, layout$profile)
    slots <- empty_slots_df(slot_ids)
    write_slots(slots, layout$slots)
  }

  created <- character(0)
  skipped <- character(0)

  if (isTRUE(demo)) {
    if (!dir.exists(layout$demos)) dir.create(layout$demos, recursive = TRUE)
    blank_path <- file.path(layout$demos, "blank.jpg")
    if (!file.exists(blank_path) || isTRUE(force) || isTRUE(clear_demos)) {
      make_blank_tile(blank_path)
      created <- c(created, "blank.jpg")
    } else {
      skipped <- c(skipped, "blank.jpg")
    }

    for (i in seq_along(slot_ids)) {
      id <- slot_ids[i]
      # Safe filename for demos (slot ids are already safe)
      out <- file.path(layout$demos, paste0(id, ".jpg"))
      if (file.exists(out) && !isTRUE(force) && !isTRUE(clear_demos)) {
        skipped <- c(skipped, paste0(id, ".jpg"))
        next
      }
      make_demo_tile(id, out, seed = as.integer(seed) + i)
      created <- c(created, paste0(id, ".jpg"))
    }

    # Point slots at new demos if we reset data
    if (isTRUE(reset_data)) {
      slots <- empty_slots_df(slot_ids)
      write_slots(slots, layout$slots)
    }
  }

  for (d in c(layout$import, layout$cropped, layout$output)) {
    keep <- file.path(d, ".gitkeep")
    if (!file.exists(keep)) writeLines("", keep)
  }

  message("collage setup complete")
  message("  Root:    ", root)
  message("  Scheme:  ", scheme)
  message("  Grid:    ", rows, " × ", cols, " (", length(slot_ids), " slots)")
  if (identical(scheme, "yymm")) {
    message("  Range:   ", start_yymm, " .. ", end_yymm)
  }
  message("  Profile: ", layout$profile)
  message("  Slots:   ", layout$slots)
  if (isTRUE(demo)) {
    message("  Demos created: ", length(created))
    message("  Demos skipped: ", length(skipped), " (already present)")
  } else {
    message("  Demo tiles: not written (call with demo = TRUE)")
  }
  invisible(list(
    root = root,
    scheme = scheme,
    slot_ids = slot_ids,
    created = created,
    skipped = skipped,
    n_slots = length(slot_ids)
  ))
}

#' Factory reset to PROFILE_DEFAULTS (danger).
restore_collage_defaults <- function(root = NULL, demo = TRUE) {
  d <- PROFILE_DEFAULTS
  setup_collage(
    root = root,
    start_yymm = d$start_yymm,
    end_yymm = d$end_yymm,
    rows = d$rows,
    cols = d$cols,
    dpi = d$dpi,
    spacing_mm = d$spacing_mm,
    paper = d$paper,
    orientation = d$orientation,
    scheme = d$slot_scheme,
    demo = demo,
    force = TRUE,
    reset_data = TRUE,
    clear_cropped = TRUE,
    clear_demos = TRUE,
    images_dir = "images"
  )
}

#' Reshape grid + naming scheme (danger — resets slot data).
reshape_collage <- function(root = NULL,
                            rows,
                            cols,
                            spacing_mm = PROFILE_DEFAULTS$spacing_mm,
                            paper = PROFILE_DEFAULTS$paper,
                            orientation = PROFILE_DEFAULTS$orientation,
                            dpi = PROFILE_DEFAULTS$dpi,
                            scheme = "yymm",
                            start_yymm = PROFILE_DEFAULTS$start_yymm,
                            end_yymm = PROFILE_DEFAULTS$end_yymm,
                            demo = TRUE,
                            clear_cropped = TRUE) {
  setup_collage(
    root = root,
    start_yymm = start_yymm,
    end_yymm = end_yymm,
    rows = rows,
    cols = cols,
    dpi = dpi,
    spacing_mm = spacing_mm,
    paper = paper,
    orientation = orientation,
    scheme = scheme,
    demo = demo,
    force = TRUE,
    reset_data = TRUE,
    clear_cropped = clear_cropped,
    clear_demos = isTRUE(demo),
    images_dir = "images"
  )
}

# If run as a script: Rscript assets/setup.R
if (sys.nframe() == 0L || identical(Sys.getenv("COLLAGE_SETUP_RUN"), "1")) {
  args <- commandArgs(trailingOnly = TRUE)
  do_force <- "--force" %in% args
  setup_collage(demo = TRUE, force = do_force)
}
