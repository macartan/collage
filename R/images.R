# Import inbox helpers and safe copy into images/ (never into demos/).

IMAGE_EXTS <- "\\.(jpg|jpeg|png|webp)$"

list_image_files <- function(dir, recursive = FALSE) {
  if (!dir.exists(dir)) return(character(0))
  files <- list.files(
    dir,
    pattern = IMAGE_EXTS,
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = recursive
  )
  files[order(tolower(basename(files)), basename(files))]
}

list_import_files <- function(import_dir) {
  list_image_files(import_dir, recursive = FALSE)
}

#' Copy a source file into images_dir as slot[_label].ext. Never writes under demos/.
#' Returns relative path from images_dir.
copy_into_images <- function(src_path, images_dir, slot, label = "", overwrite = FALSE) {
  if (!file.exists(src_path)) stop("Source file not found: ", src_path, call. = FALSE)
  if (!dir.exists(images_dir)) dir.create(images_dir, recursive = TRUE)
  demos_dir <- canonical_fs_path(file.path(images_dir, "demos"))
  dest_dir <- canonical_fs_path(images_dir)
  if (paths_equivalent(dest_dir, demos_dir)) {
    stop("Refusing to write into images/demos/.", call. = FALSE)
  }
  ext <- tolower(tools::file_ext(src_path))
  if (!nzchar(ext)) ext <- "jpg"
  dest_name <- make_slot_filename(slot, label, ext)
  dest_path <- file.path(images_dir, dest_name)
  dest_canon <- canonical_fs_path(dest_path)
  if (!is.na(demos_dir) && grepl(paste0("^", gsub("([.\\])", "\\\\\\1", demos_dir)), dest_canon, ignore.case = TRUE)) {
    stop("Destination resolves under demos/; aborting.", call. = FALSE)
  }
  if (file.exists(dest_path) && !isTRUE(overwrite)) {
    stop("File already exists: ", dest_name, " (enable overwrite to replace).", call. = FALSE)
  }
  ok <- file.copy(src_path, dest_path, overwrite = isTRUE(overwrite), copy.mode = TRUE)
  if (!ok) stop("Failed to copy into images/: ", dest_name, call. = FALSE)
  dest_name
}

#' Thumbnail as data URI for shiny grids (small, square).
thumb_data_uri <- function(path, size = 120L) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  tryCatch(
    {
      im <- read_source_image(path)
      im <- image_resize(im, sprintf("%dx%d!", size, size))
      tmp <- tempfile(fileext = ".jpg")
      image_write(im, path = tmp, format = "jpeg", quality = 80)
      if (!requireNamespace("base64enc", quietly = TRUE)) {
        stop("Package base64enc is required for thumbnails.", call. = FALSE)
      }
      base64enc::dataURI(file = tmp, mime = "image/jpeg")
    },
    error = function(e) NULL
  )
}
