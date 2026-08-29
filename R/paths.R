# Path helpers and project layout for collage.

canonical_fs_path <- function(p) {
  if (!is.character(p) || length(p) != 1L || !nzchar(p)) {
    return(NA_character_)
  }
  tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) NA_character_)
}

paths_same_directory <- function(path_a, path_b) {
  pa <- canonical_fs_path(path_a)
  pb <- canonical_fs_path(path_b)
  if (is.na(pa) || is.na(pb)) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    return(tolower(pa) == tolower(pb))
  }
  identical(pa, pb)
}

paths_equivalent <- function(a, b) {
  ca <- canonical_fs_path(a)
  cb <- canonical_fs_path(b)
  if (is.na(ca) || is.na(cb)) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    tolower(ca) == tolower(cb)
  } else {
    identical(ca, cb)
  }
}

#' Resolve collage project root (folder that contains app.R).
project_root <- function(start = getwd()) {
  start <- canonical_fs_path(start)
  if (is.na(start)) start <- getwd()
  cur <- start
  for (i in seq_len(8L)) {
    if (file.exists(file.path(cur, "app.R")) && dir.exists(file.path(cur, "R"))) {
      return(canonical_fs_path(cur))
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }
  canonical_fs_path(start)
}

default_layout <- function(root = project_root()) {
  root <- canonical_fs_path(root)
  list(
    root = root,
    images = file.path(root, "images"),
    demos = file.path(root, "images", "demos"),
    import = file.path(root, "images", "import"),
    cropped = file.path(root, "cropped"),
    data = file.path(root, "data"),
    output = file.path(root, "output"),
    archive = file.path(root, "archive"),
    profile = file.path(root, "data", "profile.csv"),
    slots = file.path(root, "data", "slots.csv"),
    archive_name = NA_character_,
    archive_dir = NA_character_,
    is_archive = FALSE
  )
}

ensure_project_dirs <- function(layout = default_layout()) {
  dirs <- c(
    layout$images, layout$demos, layout$import, layout$cropped,
    layout$data, layout$output, layout$archive
  )
  for (d in dirs) {
    if (!is.null(d) && !is.na(d) && nzchar(d) && !dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }
  invisible(layout)
}

#' TRUE if path is a single existing file.
image_path_ok <- function(path) {
  is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path) && file.exists(path)
}

#' Join images_dir with a relative path stored in slots (e.g. demos/0801.jpg or 1205_vacation.jpg).
resolve_image_path <- function(images_dir, rel_path) {
  if (!is.character(rel_path) || length(rel_path) != 1L || is.na(rel_path) || !nzchar(rel_path)) {
    return(NA_character_)
  }
  rel_path <- gsub("\\\\", "/", rel_path)
  # Absolute paths are allowed (external images_dir mapping still uses relative when possible).
  if (grepl("^[A-Za-z]:/", rel_path) || startsWith(rel_path, "/")) {
    return(canonical_fs_path(rel_path))
  }
  canonical_fs_path(file.path(images_dir, rel_path))
}

#' Store path relative to images_dir when possible.
relativize_image_path <- function(images_dir, abs_path) {
  abs_path <- canonical_fs_path(abs_path)
  images_dir <- canonical_fs_path(images_dir)
  if (is.na(abs_path) || is.na(images_dir)) {
    return(basename(abs_path))
  }
  # Normalize for prefix check
  img <- images_dir
  ap <- abs_path
  if (.Platform$OS.type == "windows") {
    img <- tolower(img)
    ap <- tolower(ap)
  }
  prefix <- paste0(img, "/")
  if (startsWith(ap, prefix)) {
    # Preserve original casing from abs_path after the prefix length
    return(substring(abs_path, nchar(images_dir) + 2L))
  }
  basename(abs_path)
}

is_under_demos <- function(images_dir, rel_or_abs) {
  p <- resolve_image_path(images_dir, rel_or_abs)
  demos <- canonical_fs_path(file.path(images_dir, "demos"))
  if (is.na(p) || is.na(demos)) return(FALSE)
  d <- canonical_fs_path(dirname(p))
  paths_equivalent(d, demos)
}
