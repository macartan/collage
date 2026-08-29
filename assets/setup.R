# One-time project bootstrap for collage.
#
# Safe by default:
#   - Creates folders and data templates
#   - Writes demo images only when demo = TRUE
#   - Never overwrites existing demos unless force = TRUE
#
# Usage (from collage/):
#   source("assets/setup.R")
#   setup_collage(demo = TRUE)           # first time / shared demo pack
#   setup_collage(demo = TRUE, force = TRUE)  # regenerate demos

suppressPackageStartupMessages({
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Please install magick: install.packages(\"magick\")", call. = FALSE)
  }
  library(magick)
})

.setup_root <- function() {
  # Rscript --file=assets/setup.R
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", ca[startsWith(ca, "--file=")])
  if (length(file_arg) == 1L && nzchar(file_arg)) {
    assets_dir <- dirname(normalizePath(file_arg, winslash = "/", mustWork = FALSE))
    if (basename(assets_dir) == "assets") {
      return(normalizePath(dirname(assets_dir), winslash = "/", mustWork = FALSE))
    }
  }
  # source("assets/setup.R") — ofile when available
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
  # Distinct-ish random colors (avoid near-white so label stays readable)
  rgb <- sample(40:200, 3, replace = TRUE)
  color <- sprintf("#%02X%02X%02X", rgb[1], rgb[2], rgb[3])
  im <- image_blank(width = size, height = size, color = color)
  font_size <- max(28L, as.integer(size * 0.18))
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

#' Bootstrap collage.
#'
#' @param root Project root (default: auto-detect)
#' @param start_yymm First slot YYMM
#' @param end_yymm Last slot YYMM
#' @param demo If TRUE, create color demo tiles under images/demos/
#' @param force If TRUE, overwrite existing demo tiles
#' @param seed Base seed for reproducible demo colors
#' @param rows,cols Default poster grid
setup_collage <- function(root = NULL,
                          start_yymm = "0801",
                          end_yymm = "2612",
                          demo = FALSE,
                          force = FALSE,
                          seed = 18L,
                          rows = 19L,
                          cols = 12L,
                          dpi = 300,
                          spacing_mm = 1) {
  if (is.null(root)) root <- .setup_root()
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  source_project_r(root)

  layout <- default_layout(root)
  ensure_project_dirs(layout)

  prefixes <- month_prefixes(start_yymm, end_yymm)

  # Profile: store images_dir as relative "images" for portability
  profile <- list(
    images_dir = "images",
    start_yymm = start_yymm,
    end_yymm = end_yymm,
    rows = as.integer(rows),
    cols = as.integer(cols),
    spacing_mm = as.numeric(spacing_mm),
    dpi = as.integer(dpi),
    paper = "A1",
    orientation = "portrait"
  )
  write_profile(profile, layout$profile)

  # Slots seeded to demos/YYMM.jpg
  slots <- empty_slots_df(prefixes)
  write_slots(slots, layout$slots)

  created <- character(0)
  skipped <- character(0)

  if (isTRUE(demo)) {
    blank_path <- file.path(layout$demos, "blank.jpg")
    if (!file.exists(blank_path) || isTRUE(force)) {
      make_blank_tile(blank_path)
      created <- c(created, "blank.jpg")
    } else {
      skipped <- c(skipped, "blank.jpg")
    }

    for (i in seq_along(prefixes)) {
      pref <- prefixes[i]
      out <- file.path(layout$demos, paste0(pref, ".jpg"))
      if (file.exists(out) && !isTRUE(force)) {
        skipped <- c(skipped, paste0(pref, ".jpg"))
        next
      }
      make_demo_tile(pref, out, seed = as.integer(seed) + i)
      created <- c(created, paste0(pref, ".jpg"))
    }
  }

  # .gitkeep-style placeholders so empty dirs survive sharing without demos
  for (d in c(layout$import, layout$cropped, layout$output)) {
    keep <- file.path(d, ".gitkeep")
    if (!file.exists(keep)) writeLines("", keep)
  }

  message("collage setup complete")
  message("  Root:    ", root)
  message("  Slots:   ", length(prefixes), " (", start_yymm, " .. ", end_yymm, ")")
  message("  Profile: ", layout$profile)
  message("  Slots:   ", layout$slots)
  if (isTRUE(demo)) {
    message("  Demos created: ", length(created))
    message("  Demos skipped: ", length(skipped), " (already present)")
  } else {
    message("  Demo tiles: not written (call with demo = TRUE)")
  }
  invisible(list(root = root, created = created, skipped = skipped, n_slots = length(prefixes)))
}

# If run as a script: Rscript assets/setup.R
if (sys.nframe() == 0L || identical(Sys.getenv("COLLAGE_SETUP_RUN"), "1")) {
  args <- commandArgs(trailingOnly = TRUE)
  do_demo <- TRUE
  do_force <- "--force" %in% args
  setup_collage(demo = do_demo, force = do_force)
}
