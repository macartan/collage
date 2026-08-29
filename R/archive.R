# Project archives under archive/<name>/ (not zipped).
#
#   archive/
#     <project_name>/
#       manifest.csv
#       data/profile.csv
#       data/slots.csv
#       cropped/          # optional square crops
#       output/           # posters for this project
#
# Shared images/ (and demos/) stay at the repo root. Archives do not copy originals.

archives_root <- function(root = project_root()) {
  file.path(canonical_fs_path(root), "archive")
}

sanitize_archive_name <- function(name) {
  name <- trimws(as.character(name))
  if (!nzchar(name)) stop("Archive name is empty.", call. = FALSE)
  name <- gsub("[^A-Za-z0-9._-]+", "_", name)
  name <- gsub("^_+|_+$", "", name)
  if (!nzchar(name)) stop("Archive name invalid after cleaning.", call. = FALSE)
  name
}

archive_dir <- function(name, root = project_root()) {
  file.path(archives_root(root), sanitize_archive_name(name))
}

#' Layout pointing at live workspace or an archive project.
workspace_layout <- function(root = project_root(), archive_name = NULL) {
  root <- canonical_fs_path(root)
  base <- default_layout(root)
  if (is.null(archive_name) || !nzchar(as.character(archive_name))) {
    base$archive_name <- NA_character_
    base$archive_dir <- NA_character_
    base$is_archive <- FALSE
    return(base)
  }
  name <- sanitize_archive_name(archive_name)
  adir <- file.path(archives_root(root), name)
  list(
    root = root,
    images = base$images,
    demos = base$demos,
    import = base$import,
    cropped = file.path(adir, "cropped"),
    data = file.path(adir, "data"),
    output = file.path(adir, "output"),
    profile = file.path(adir, "data", "profile.csv"),
    slots = file.path(adir, "data", "slots.csv"),
    archive_name = name,
    archive_dir = adir,
    is_archive = TRUE
  )
}

ensure_archive_dirs <- function(adir) {
  for (d in c(adir, file.path(adir, "data"), file.path(adir, "cropped"), file.path(adir, "output"))) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(adir)
}

list_archives <- function(root = project_root()) {
  root_a <- archives_root(root)
  if (!dir.exists(root_a)) return(character(0))
  dirs <- list.dirs(root_a, full.names = FALSE, recursive = FALSE)
  dirs <- dirs[nzchar(dirs)]
  keep <- vapply(dirs, function(d) {
    file.exists(file.path(root_a, d, "data", "slots.csv")) &&
      file.exists(file.path(root_a, d, "data", "profile.csv"))
  }, logical(1))
  sort(dirs[keep])
}

read_archive_manifest <- function(archive_path) {
  mp <- file.path(archive_path, "manifest.csv")
  defaults <- list(
    name = basename(archive_path),
    created = NA_character_,
    notes = "",
    includes_cropped = FALSE,
    n_slots = NA_integer_,
    rows = NA_integer_,
    cols = NA_integer_,
    slot_scheme = NA_character_,
    n_cropped_files = NA_integer_,
    has_poster = FALSE
  )
  if (!file.exists(mp)) return(defaults)
  d <- read.csv(mp, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(d) < 1L) return(defaults)
  out <- defaults
  row <- as.list(d[1, , drop = FALSE])
  for (nm in names(row)) out[[nm]] <- row[[nm]]
  out
}

#' Build a short summary row for Setup listing.
archive_summary <- function(name, root = project_root()) {
  adir <- archive_dir(name, root)
  man <- read_archive_manifest(adir)
  slots_path <- file.path(adir, "data", "slots.csv")
  prof_path <- file.path(adir, "data", "profile.csv")
  n_slots <- NA_integer_
  scheme <- NA_character_
  rows <- NA_integer_
  cols <- NA_integer_
  if (file.exists(slots_path)) {
    s <- tryCatch(
      read.csv(slots_path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (!is.null(s)) n_slots <- nrow(s)
  }
  if (file.exists(prof_path)) {
    p <- tryCatch(read_profile(prof_path, root = root), error = function(e) NULL)
    if (!is.null(p)) {
      scheme <- p$slot_scheme
      rows <- p$rows
      cols <- p$cols
    }
  }
  crop_n <- length(list.files(
    file.path(adir, "cropped"),
    pattern = "\\.(jpg|jpeg|png|webp)$",
    ignore.case = TRUE
  ))
  has_poster <- file.exists(file.path(adir, "output", "poster.jpg")) ||
    file.exists(file.path(adir, "output", "poster.pdf"))

  list(
    name = name,
    created = man$created,
    notes = if (is.null(man$notes) || is.na(man$notes)) "" else as.character(man$notes),
    n_slots = if (!is.na(man$n_slots)) as.integer(man$n_slots) else n_slots,
    rows = if (!is.na(man$rows)) as.integer(man$rows) else rows,
    cols = if (!is.na(man$cols)) as.integer(man$cols) else cols,
    slot_scheme = if (!is.na(man$slot_scheme) && nzchar(as.character(man$slot_scheme))) {
      as.character(man$slot_scheme)
    } else {
      as.character(scheme)
    },
    n_cropped = crop_n,
    has_poster = has_poster,
    path = adir
  )
}

format_archive_summary_line <- function(sum) {
  grid <- if (!is.na(sum$rows) && !is.na(sum$cols)) {
    sprintf("%d×%d", sum$rows, sum$cols)
  } else {
    "?"
  }
  slots <- if (!is.na(sum$n_slots)) as.character(sum$n_slots) else "?"
  scheme <- if (!is.null(sum$slot_scheme) && !is.na(sum$slot_scheme) && nzchar(sum$slot_scheme)) {
    sum$slot_scheme
  } else {
    "?"
  }
  paste0(
    sum$name, " — ", grid, " · ", slots, " slots · ", scheme,
    " · crops ", sum$n_cropped,
    if (isTRUE(sum$has_poster)) " · has poster" else "",
    if (nzchar(sum$notes)) paste0(" — ", sum$notes) else ""
  )
}

copy_dir_images <- function(from_dir, to_dir) {
  if (!dir.exists(to_dir)) dir.create(to_dir, recursive = TRUE)
  if (!dir.exists(from_dir)) return(0L)
  files <- list.files(
    from_dir,
    pattern = "\\.(jpg|jpeg|png|webp|pdf)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0) return(0L)
  ok <- file.copy(files, to_dir, overwrite = TRUE, copy.mode = TRUE)
  sum(ok)
}

#' Save live workspace into archive/<name>/.
save_archive <- function(name,
                         root = project_root(),
                         include_cropped = TRUE,
                         include_output = TRUE,
                         notes = "",
                         overwrite = FALSE) {
  layout <- default_layout(root)
  ensure_project_dirs(layout)
  name <- sanitize_archive_name(name)
  dest <- archive_dir(name, root)
  if (dir.exists(dest) && !isTRUE(overwrite)) {
    stop("Archive already exists: ", name, " (choose another name or overwrite).", call. = FALSE)
  }
  if (dir.exists(dest) && isTRUE(overwrite)) {
    unlink(dest, recursive = TRUE)
  }
  ensure_archive_dirs(dest)

  if (!file.exists(layout$profile) || !file.exists(layout$slots)) {
    stop("Nothing to archive: missing data/profile.csv or data/slots.csv.", call. = FALSE)
  }
  file.copy(layout$profile, file.path(dest, "data", "profile.csv"), overwrite = TRUE)
  file.copy(layout$slots, file.path(dest, "data", "slots.csv"), overwrite = TRUE)

  n_crop <- 0L
  if (isTRUE(include_cropped)) {
    n_crop <- copy_dir_images(layout$cropped, file.path(dest, "cropped"))
  }

  n_out <- 0L
  if (isTRUE(include_output)) {
    n_out <- copy_dir_images(layout$output, file.path(dest, "output"))
  }

  slots <- tryCatch(
    read.csv(file.path(dest, "data", "slots.csv"), stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  prof <- tryCatch(read_profile(file.path(dest, "data", "profile.csv"), root = root), error = function(e) NULL)

  man <- data.frame(
    name = name,
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    notes = as.character(notes),
    includes_cropped = isTRUE(include_cropped),
    n_cropped_files = n_crop,
    n_output_files = n_out,
    n_slots = if (is.null(slots)) NA_integer_ else nrow(slots),
    rows = if (is.null(prof)) NA_integer_ else as.integer(prof$rows),
    cols = if (is.null(prof)) NA_integer_ else as.integer(prof$cols),
    slot_scheme = if (is.null(prof)) NA_character_ else as.character(prof$slot_scheme),
    has_poster = file.exists(file.path(dest, "output", "poster.jpg")),
    stringsAsFactors = FALSE
  )
  write.csv(man, file.path(dest, "manifest.csv"), row.names = FALSE)

  invisible(list(path = dest, name = name, n_cropped = n_crop, n_output = n_out))
}

ensure_demos_for_slots <- function(slots_df, layout, seed = 18L) {
  if (!dir.exists(layout$demos)) dir.create(layout$demos, recursive = TRUE)
  blank <- file.path(layout$demos, "blank.jpg")
  if (!file.exists(blank) && exists("make_blank_tile", mode = "function")) {
    make_blank_tile(blank)
  }

  rels <- unique(as.character(slots_df$file_name))
  rels <- rels[!is.na(rels) & nzchar(rels)]
  demo_rels <- rels[grepl("^demos/", gsub("\\\\", "/", rels), ignore.case = TRUE)]
  created <- character(0)
  for (i in seq_along(demo_rels)) {
    rel <- gsub("\\\\", "/", demo_rels[i])
    bn <- basename(rel)
    if (identical(tolower(bn), "blank.jpg")) next
    out <- file.path(layout$demos, bn)
    if (file.exists(out)) next
    id <- tools::file_path_sans_ext(bn)
    if (exists("make_demo_tile", mode = "function")) {
      make_demo_tile(id, out, seed = as.integer(seed) + i)
      created <- c(created, bn)
    }
  }
  invisible(created)
}

#' Copy archive into the live workspace (overwrites live data/cropped, optional output).
restore_archive <- function(name,
                            root = project_root(),
                            restore_cropped = TRUE,
                            restore_output = TRUE,
                            missing_demos = c("regenerate", "blank")) {
  missing_demos <- match.arg(missing_demos)
  layout <- default_layout(root)
  name <- sanitize_archive_name(name)
  src <- archive_dir(name, root)
  if (!dir.exists(src)) stop("Archive not found: ", name, call. = FALSE)
  if (!file.exists(file.path(src, "data", "profile.csv")) ||
      !file.exists(file.path(src, "data", "slots.csv"))) {
    stop("Archive incomplete (need data/profile.csv and data/slots.csv).", call. = FALSE)
  }

  ensure_project_dirs(layout)
  file.copy(file.path(src, "data", "profile.csv"), layout$profile, overwrite = TRUE)
  file.copy(file.path(src, "data", "slots.csv"), layout$slots, overwrite = TRUE)

  n_crop <- 0L
  if (isTRUE(restore_cropped)) {
    old <- list.files(layout$cropped, pattern = "\\.(jpg|jpeg|png|webp)$", full.names = TRUE, ignore.case = TRUE)
    if (length(old)) unlink(old)
    n_crop <- copy_dir_images(file.path(src, "cropped"), layout$cropped)
  }

  n_out <- 0L
  if (isTRUE(restore_output)) {
    n_out <- copy_dir_images(file.path(src, "output"), layout$output)
  }

  slots <- read.csv(layout$slots, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("NA", ""))
  demos_made <- character(0)
  if (identical(missing_demos, "regenerate")) {
    demos_made <- ensure_demos_for_slots(slots, layout)
  } else {
    blank_rel <- "demos/blank.jpg"
    blank_abs <- file.path(layout$demos, "blank.jpg")
    if (!file.exists(blank_abs) && exists("make_blank_tile", mode = "function")) {
      if (!dir.exists(layout$demos)) dir.create(layout$demos, recursive = TRUE)
      make_blank_tile(blank_abs)
    }
    prof <- read_profile(layout$profile, root = root)
    for (i in seq_len(nrow(slots))) {
      rel <- gsub("\\\\", "/", as.character(slots$file_name[i]))
      if (is.na(rel) || !nzchar(rel) || !grepl("^demos/", rel, ignore.case = TRUE)) next
      if (identical(tolower(basename(rel)), "blank.jpg")) next
      abs <- resolve_image_path(prof$images_dir, rel)
      if (is.na(abs) || !file.exists(abs)) {
        slots$file_name[i] <- blank_rel
      }
    }
    write.csv(slots, layout$slots, row.names = FALSE)
  }

  invisible(list(name = name, n_cropped = n_crop, n_output = n_out, demos_made = demos_made))
}

#' Delete an archive folder permanently.
delete_archive <- function(name, root = project_root()) {
  name <- sanitize_archive_name(name)
  src <- archive_dir(name, root)
  if (!dir.exists(src)) stop("Archive not found: ", name, call. = FALSE)
  unlink(src, recursive = TRUE)
  invisible(name)
}
