# Import inbox helpers and safe copy into images/ (never into demos/).

IMAGE_EXTS <- "\\.(jpg|jpeg|png|webp)$"

#' Read width/height/bytes/format from a file (no catalogue needed).
read_image_meta <- function(path) {
  out <- list(
    exists = FALSE,
    path = NA_character_,
    w = NA_real_,
    h = NA_real_,
    bytes = NA_real_,
    format = NA_character_
  )
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(out)
  }
  out$exists <- TRUE
  out$path <- canonical_fs_path(path)
  fi <- suppressWarnings(file.info(path))
  out$bytes <- as.numeric(fi$size)
  ext <- tolower(tools::file_ext(path))
  out$format <- if (nzchar(ext)) toupper(ext) else NA_character_
  dims <- tryCatch(
    {
      info <- image_info(read_source_image(path))
      list(w = as.numeric(info$width[1]), h = as.numeric(info$height[1]))
    },
    error = function(e) list(w = NA_real_, h = NA_real_)
  )
  out$w <- dims$w
  out$h <- dims$h
  out
}

fmt_bytes <- function(n) {
  if (is.null(n) || length(n) != 1L || is.na(n)) return("?")
  if (n < 1024) return(paste0(round(n), " B"))
  if (n < 1024^2) return(paste0(round(n / 1024, 1), " KB"))
  paste0(round(n / 1024^2, 2), " MB")
}

fmt_px <- function(w, h) {
  if (is.na(w) || is.na(h)) return("size unknown")
  paste0(as.integer(w), " × ", as.integer(h), " px")
}

source_kind_label <- function(rel_path) {
  if (is.na(rel_path) || !nzchar(rel_path)) return("none")
  rel <- gsub("\\\\", "/", rel_path)
  if (grepl("/blank\\.jpg$", rel, ignore.case = TRUE) || identical(basename(rel), "blank.jpg")) {
    return("blank placeholder")
  }
  if (grepl("^demos/", rel, ignore.case = TRUE)) {
    return("demo tile")
  }
  "your photo"
}

#' Cropped file path for a slot if it exists on disk.
cropped_file_for_slot <- function(row, cropped_dir) {
  slot <- row$slot[[1L]]
  rel <- row$file_name[[1L]]
  candidates <- c(
    file.path(cropped_dir, paste0(slot, ".jpg")),
    file.path(cropped_dir, paste0(slot, ".jpeg")),
    file.path(cropped_dir, paste0(slot, ".png")),
    if (!is.na(rel) && nzchar(as.character(rel))) file.path(cropped_dir, basename(rel)) else NULL
  )
  for (c in candidates) {
    if (!is.null(c) && file.exists(c)) return(canonical_fs_path(c))
  }
  NA_character_
}

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

#' User images eligible for smart-load matching (images/ root + import/, not demos/).
list_smart_load_candidates <- function(images_dir) {
  root_files <- list_image_files(images_dir, recursive = FALSE)
  # Exclude anything that somehow resolved under demos
  demos <- canonical_fs_path(file.path(images_dir, "demos"))
  root_files <- root_files[vapply(root_files, function(p) {
    !paths_equivalent(canonical_fs_path(dirname(p)), demos)
  }, logical(1))]

  import_dir <- file.path(images_dir, "import")
  import_files <- list_import_files(import_dir)
  c(root_files, import_files)
}

#' TRUE if slot still uses a default placeholder (demo tile or blank).
slot_has_default_fill <- function(file_name) {
  if (is.null(file_name) || length(file_name) != 1L || is.na(file_name) || !nzchar(as.character(file_name))) {
    return(TRUE)
  }
  rel <- gsub("\\\\", "/", as.character(file_name))
  grepl("^demos/", rel, ignore.case = TRUE)
}

#' Match candidate files to a slot id (stem is slot or slot_label).
files_matching_slot <- function(paths, slot) {
  if (length(paths) == 0) return(character(0))
  keep <- vapply(paths, function(p) {
    stem_matches_slot(tools::file_path_sans_ext(basename(p)), slot)
  }, logical(1))
  paths[keep]
}

#' Smart-load: for default-fill slots, assign uniquely named images from images/ (+ import/).
#'
#' - 0 matches → leave slot unchanged
#' - 1 match → assign (copy from import/ into images/ if needed)
#' - 2+ matches → leave unchanged (ambiguous)
#'
#' @return list(slots, n_assigned, n_skipped_none, n_skipped_multi, assigned, ambiguous)
smart_load_slots <- function(slots_df, images_dir) {
  slots_df <- ensure_slot_columns(slots_df)
  candidates <- list_smart_load_candidates(images_dir)
  assigned <- character(0)
  ambiguous <- list()
  n_none <- 0L

  for (i in seq_len(nrow(slots_df))) {
    if (!slot_has_default_fill(slots_df$file_name[i])) next
    slot <- as.character(slots_df$slot[i])
    hits <- files_matching_slot(candidates, slot)
    if (length(hits) == 0L) {
      n_none <- n_none + 1L
      next
    }
    if (length(hits) > 1L) {
      ambiguous[[slot]] <- basename(hits)
      next
    }
    src <- hits[1]
    # Prefer relative path under images/; copy out of import/ first
    import_dir <- canonical_fs_path(file.path(images_dir, "import"))
    if (paths_equivalent(canonical_fs_path(dirname(src)), import_dir)) {
      # Keep original basename if it already matches slot[_label]; else rename to slot.ext
      stem <- tools::file_path_sans_ext(basename(src))
      if (stem_matches_slot(stem, slot)) {
        dest_name <- basename(src)
        dest_path <- file.path(images_dir, dest_name)
        if (!file.exists(dest_path)) {
          file.copy(src, dest_path, overwrite = FALSE, copy.mode = TRUE)
        }
        rel <- dest_name
      } else {
        rel <- copy_into_images(src, images_dir, slot, label = "", overwrite = FALSE)
      }
    } else {
      rel <- relativize_image_path(images_dir, src)
    }
    slots_df <- update_slot_source(
      slots_df, i, rel,
      archive = TRUE,
      reset_crop = TRUE,
      images_dir = images_dir
    )
    assigned <- c(assigned, paste0(slot, " → ", rel))
  }

  list(
    slots = slots_df,
    n_assigned = length(assigned),
    n_skipped_none = n_none,
    n_skipped_multi = length(ambiguous),
    assigned = assigned,
    ambiguous = ambiguous
  )
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
