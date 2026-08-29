# Minimal project archives: data (+ optional crops), not full image copies.
#
# Layout:
#   data/archives/<name>/
#     profile.csv
#     slots.csv
#     manifest.csv          # name, created, notes, includes_cropped
#     cropped/              # optional copies of square crops
#
# Images under images/ are NOT copied. Demo tiles can be regenerated on restore.

archives_root <- function(layout = default_layout()) {
  file.path(layout$data, "archives")
}

sanitize_archive_name <- function(name) {
  name <- trimws(as.character(name))
  if (!nzchar(name)) stop("Archive name is empty.", call. = FALSE)
  name <- gsub("[^A-Za-z0-9._-]+", "_", name)
  name <- gsub("^_+|_+$", "", name)
  if (!nzchar(name)) stop("Archive name invalid after cleaning.", call. = FALSE)
  name
}

list_archives <- function(layout = default_layout()) {
  root <- archives_root(layout)
  if (!dir.exists(root)) return(character(0))
  dirs <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  dirs <- dirs[nzchar(dirs)]
  # Prefer those with slots.csv
  keep <- vapply(dirs, function(d) {
    file.exists(file.path(root, d, "slots.csv")) && file.exists(file.path(root, d, "profile.csv"))
  }, logical(1))
  sort(dirs[keep])
}

read_archive_manifest <- function(archive_dir) {
  mp <- file.path(archive_dir, "manifest.csv")
  if (!file.exists(mp)) {
    return(list(name = basename(archive_dir), created = NA_character_, notes = "", includes_cropped = FALSE))
  }
  d <- read.csv(mp, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(d) < 1L) {
    return(list(name = basename(archive_dir), created = NA_character_, notes = "", includes_cropped = FALSE))
  }
  as.list(d[1, , drop = FALSE])
}

#' Save live data/ (+ optional cropped/) into data/archives/<name>/.
#' Does not copy images/ or demos/.
save_archive <- function(name,
                         root = project_root(),
                         include_cropped = TRUE,
                         notes = "") {
  layout <- default_layout(root)
  ensure_project_dirs(layout)
  name <- sanitize_archive_name(name)
  dest <- file.path(archives_root(layout), name)
  if (dir.exists(dest)) {
    stop("Archive already exists: ", name, " (choose another name).", call. = FALSE)
  }
  dir.create(dest, recursive = TRUE)

  if (!file.exists(layout$profile) || !file.exists(layout$slots)) {
    stop("Nothing to archive: missing data/profile.csv or data/slots.csv.", call. = FALSE)
  }
  file.copy(layout$profile, file.path(dest, "profile.csv"), overwrite = TRUE)
  file.copy(layout$slots, file.path(dest, "slots.csv"), overwrite = TRUE)

  n_crop <- 0L
  if (isTRUE(include_cropped) && dir.exists(layout$cropped)) {
    crop_dest <- file.path(dest, "cropped")
    dir.create(crop_dest, recursive = TRUE, showWarnings = FALSE)
    crops <- list.files(
      layout$cropped,
      pattern = "\\.(jpg|jpeg|png|webp)$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(crops) > 0) {
      ok <- file.copy(crops, crop_dest, overwrite = TRUE, copy.mode = TRUE)
      n_crop <- sum(ok)
    }
  }

  man <- data.frame(
    name = name,
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    notes = as.character(notes),
    includes_cropped = isTRUE(include_cropped),
    n_cropped_files = n_crop,
    stringsAsFactors = FALSE
  )
  write.csv(man, file.path(dest, "manifest.csv"), row.names = FALSE)

  invisible(list(path = dest, name = name, n_cropped = n_crop))
}

#' Ensure demo tiles exist for any demos/* paths referenced in slots.
#' Missing demos are recreated; blank.jpg ensured.
ensure_demos_for_slots <- function(slots_df, layout, seed = 18L) {
  if (!dir.exists(layout$demos)) dir.create(layout$demos, recursive = TRUE)
  blank <- file.path(layout$demos, "blank.jpg")
  if (!file.exists(blank)) {
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
    make_demo_tile(id, out, seed = as.integer(seed) + i)
    created <- c(created, bn)
  }
  invisible(created)
}

#' Restore an archive into live data/ (overwrites current profile + slots).
#'
#' @param missing_demos "regenerate" or "blank"
restore_archive <- function(name,
                            root = project_root(),
                            restore_cropped = TRUE,
                            missing_demos = c("regenerate", "blank")) {
  missing_demos <- match.arg(missing_demos)
  layout <- default_layout(root)
  name <- sanitize_archive_name(name)
  src <- file.path(archives_root(layout), name)
  if (!dir.exists(src)) stop("Archive not found: ", name, call. = FALSE)
  if (!file.exists(file.path(src, "profile.csv")) || !file.exists(file.path(src, "slots.csv"))) {
    stop("Archive incomplete (need profile.csv and slots.csv).", call. = FALSE)
  }

  ensure_project_dirs(layout)
  file.copy(file.path(src, "profile.csv"), layout$profile, overwrite = TRUE)
  file.copy(file.path(src, "slots.csv"), layout$slots, overwrite = TRUE)

  man <- read_archive_manifest(src)
  crop_src <- file.path(src, "cropped")
  n_crop <- 0L
  if (isTRUE(restore_cropped) && dir.exists(crop_src)) {
    if (!dir.exists(layout$cropped)) dir.create(layout$cropped, recursive = TRUE)
    old <- list.files(layout$cropped, pattern = "\\.(jpg|jpeg|png|webp)$", full.names = TRUE, ignore.case = TRUE)
    if (length(old)) unlink(old)
    crops <- list.files(crop_src, pattern = "\\.(jpg|jpeg|png|webp)$", full.names = TRUE, ignore.case = TRUE)
    if (length(crops)) {
      ok <- file.copy(crops, layout$cropped, overwrite = TRUE, copy.mode = TRUE)
      n_crop <- sum(ok)
    }
  }

  slots <- read.csv(layout$slots, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("NA", ""))
  demos_made <- character(0)
  if (identical(missing_demos, "regenerate")) {
    demos_made <- ensure_demos_for_slots(slots, layout)
  } else {
    blank_rel <- "demos/blank.jpg"
    blank_abs <- file.path(layout$demos, "blank.jpg")
    if (!file.exists(blank_abs)) {
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

  invisible(list(
    name = name,
    n_cropped = n_crop,
    demos_made = demos_made,
    manifest = man
  ))
}
