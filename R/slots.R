# Slot calendar, profile, and slots.csv I/O.

SLOT_CROP_COLS <- c(
  "mag_pct", "rotate_cw",
  "shift_x_pct", "shift_y_pct",
  "shift_x_px", "shift_y_px",
  "img_w", "img_h", "S0",
  "crop_side_px", "crop_left", "crop_top",
  "crop_out_px",
  "use_cropped",
  # Source file metadata (original on disk — not the crop)
  "src_w", "src_h", "src_bytes"
)

SLOT_BAK_COLS <- c(
  "file_name_bak",
  paste0(SLOT_CROP_COLS, "_bak")
)

SLOT_ALL_COLS <- c("slot", "file_name", SLOT_CROP_COLS, SLOT_BAK_COLS)

PROFILE_DEFAULTS <- list(
  images_dir = "images",
  start_yymm = "0801",
  end_yymm = "2612",
  rows = 19L,
  cols = 12L,
  spacing_mm = 1,
  dpi = 300,
  paper = "A1",
  orientation = "portrait",
  slot_scheme = "yymm"  # yymm | rowcol | sequential
)

#' Build slot id vector for a grid.
#'
#' @param scheme "yymm" (year-month calendar), "rowcol" (r01c01…), or "sequential" (0001…)
#' @param rows,cols Grid size; final length is rows * cols
#' @param start_yymm,end_yymm Used only for scheme "yymm"
make_slot_ids <- function(scheme = "yymm",
                          rows = 19L,
                          cols = 12L,
                          start_yymm = "0801",
                          end_yymm = "2612") {
  rows <- max(1L, as.integer(rows))
  cols <- max(1L, as.integer(cols))
  n <- rows * cols
  scheme <- tolower(as.character(scheme))

  if (identical(scheme, "yymm")) {
    prefixes <- month_prefixes(start_yymm, end_yymm)
    if (length(prefixes) < n) {
      stop(
        sprintf(
          "YYMM range %s..%s yields %d slots but grid needs %d (rows×cols). Widen the year/month range or shrink the grid.",
          start_yymm, end_yymm, length(prefixes), n
        ),
        call. = FALSE
      )
    }
    if (length(prefixes) > n) {
      warning(
        sprintf(
          "YYMM range has %d months; using the first %d to fill the %d×%d grid.",
          length(prefixes), n, rows, cols
        ),
        call. = FALSE
      )
      prefixes <- prefixes[seq_len(n)]
    }
    return(as.character(prefixes))
  }

  if (identical(scheme, "rowcol")) {
    ids <- character(n)
    k <- 1L
    for (r in seq_len(rows)) {
      for (c in seq_len(cols)) {
        ids[k] <- sprintf("r%02dc%02d", r, c)
        k <- k + 1L
      }
    }
    return(ids)
  }

  if (identical(scheme, "sequential")) {
    width <- max(4L, nchar(as.character(n)))
    return(sprintf(paste0("%0", width, "d"), seq_len(n)))
  }

  stop("Unknown slot_scheme: ", scheme, " (use yymm, rowcol, or sequential).", call. = FALSE)
}

#' YYMM prefixes from start to end inclusive (e.g. 0801 .. 2612).
month_prefixes <- function(start_prefix = "0801", end_prefix = "2612") {
  if (!grepl("^\\d{4}$", start_prefix) || !grepl("^\\d{4}$", end_prefix)) {
    stop("Prefixes must be 4 digits in YYMM format.", call. = FALSE)
  }
  start_yy <- as.integer(substr(start_prefix, 1, 2))
  start_mm <- as.integer(substr(start_prefix, 3, 4))
  end_yy <- as.integer(substr(end_prefix, 1, 2))
  end_mm <- as.integer(substr(end_prefix, 3, 4))
  if (any(is.na(c(start_yy, start_mm, end_yy, end_mm))) ||
      start_mm < 1 || start_mm > 12 || end_mm < 1 || end_mm > 12) {
    stop("Invalid YYMM prefixes: ", start_prefix, " .. ", end_prefix, call. = FALSE)
  }
  years <- seq.int(start_yy, end_yy)
  grid <- expand.grid(mm = 1:12, yy = years)
  grid <- grid[order(grid$yy, grid$mm), , drop = FALSE]
  prefixes <- sprintf("%02d%02d", grid$yy, grid$mm)
  start_idx <- (start_yy * 12) + (start_mm - 1)
  end_idx <- (end_yy * 12) + (end_mm - 1)
  idx <- (grid$yy * 12) + (grid$mm - 1)
  prefixes[idx >= start_idx & idx <= end_idx]
}

#' Parse slot id from a filename stem.
#' Prefer longest known slot match when `known_slots` provided; else YYMM-style 4 digits,
#' else full stem before optional _label.
slot_from_name <- function(name, known_slots = NULL) {
  stem <- tools::file_path_sans_ext(basename(as.character(name)))
  if (!is.null(known_slots) && length(known_slots) > 0) {
    known_slots <- as.character(known_slots)
    # Longest match first so r01c01 beats r01 if both existed
    ord <- order(nchar(known_slots), decreasing = TRUE)
    for (s in known_slots[ord]) {
      if (stem_matches_slot(stem, s)) return(s)
    }
  }
  m <- regexpr("^\\d{4}", stem, perl = TRUE)
  if (m > 0) return(regmatches(stem, m))
  # rowcol / freeform: take part before first _label
  sub("_.*$", "", stem)
}

#' True if stem is slot or slot_label (1205 or 1205_vacation).
stem_matches_slot <- function(stem, slot) {
  stem <- tools::file_path_sans_ext(basename(as.character(stem)))
  slot <- as.character(slot)
  esc <- gsub("([.|()\\[\\]{}+*?^$\\\\])", "\\\\\\1", slot)
  grepl(paste0("^", esc, "(_.*)?$"), stem)
}

sanitize_label <- function(label) {
  if (is.null(label) || length(label) != 1L || is.na(label)) return("")
  label <- trimws(as.character(label))
  label <- gsub("[^A-Za-z0-9_-]+", "_", label)
  label <- gsub("^_+|_+$", "", label)
  label
}

#' Build user image filename: 1205.jpg or 1205_vacation.jpg
make_slot_filename <- function(slot, label = "", ext = "jpg") {
  label <- sanitize_label(label)
  base <- if (nzchar(label)) paste0(slot, "_", label) else slot
  paste0(base, ".", tolower(ext))
}

empty_slots_df <- function(slots) {
  n <- length(slots)
  df <- data.frame(
    slot = slots,
    file_name = paste0("demos/", slots, ".jpg"),
    stringsAsFactors = FALSE
  )
  for (cn in SLOT_CROP_COLS) {
    if (cn == "use_cropped") {
      df[[cn]] <- FALSE
    } else if (cn == "mag_pct") {
      df[[cn]] <- 100
    } else if (cn == "rotate_cw") {
      df[[cn]] <- 0
    } else if (cn == "crop_out_px") {
      df[[cn]] <- 0
    } else if (cn %in% c("shift_x_pct", "shift_y_pct", "shift_x_px", "shift_y_px")) {
      df[[cn]] <- 0
    } else {
      df[[cn]] <- NA_real_
    }
  }
  for (cn in SLOT_BAK_COLS) {
    if (cn == "file_name_bak") {
      df[[cn]] <- NA_character_
    } else if (endsWith(cn, "use_cropped_bak")) {
      df[[cn]] <- NA
    } else {
      df[[cn]] <- NA_real_
    }
  }
  df
}

ensure_slot_columns <- function(df) {
  if (!("slot" %in% names(df))) stop("slots.csv must have a slot column.", call. = FALSE)
  if (!("file_name" %in% names(df))) df$file_name <- NA_character_
  for (cn in SLOT_CROP_COLS) {
    if (!(cn %in% names(df))) {
      if (cn == "use_cropped") {
        df[[cn]] <- FALSE
      } else if (cn == "mag_pct") {
        df[[cn]] <- 100
      } else if (cn == "rotate_cw") {
        df[[cn]] <- 0
      } else if (cn == "crop_out_px") {
        df[[cn]] <- 0
      } else if (cn %in% c("shift_x_pct", "shift_y_pct", "shift_x_px", "shift_y_px")) {
        df[[cn]] <- 0
      } else {
        df[[cn]] <- NA_real_
      }
    }
  }
  for (cn in SLOT_BAK_COLS) {
    if (!(cn %in% names(df))) {
      if (cn == "file_name_bak") {
        df[[cn]] <- NA_character_
      } else {
        df[[cn]] <- NA
      }
    }
  }
  # Coerce use_cropped
  df$use_cropped <- as.logical(df$use_cropped)
  df$use_cropped[is.na(df$use_cropped)] <- FALSE
  df$slot <- as.character(df$slot)
  # Pad pure YYMM / sequential numeric ids; leave rowcol ids alone
  num <- suppressWarnings(as.integer(df$slot))
  if (all(!is.na(num)) && all(grepl("^\\d+$", df$slot))) {
    width <- max(4L, nchar(as.character(max(num))))
    df$slot <- sprintf(paste0("%0", width, "d"), num)
  }
  df
}

read_profile <- function(path, root = project_root()) {
  defaults <- PROFILE_DEFAULTS
  defaults$images_dir <- file.path(root, "images")
  if (!file.exists(path)) {
    return(as.list(defaults))
  }
  d <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(d) < 1L) return(as.list(defaults))
  row <- as.list(d[1, , drop = FALSE])
  out <- defaults
  for (nm in names(row)) {
    if (!is.null(row[[nm]]) && !(length(row[[nm]]) == 1L && is.na(row[[nm]]))) {
      out[[nm]] <- row[[nm]]
    }
  }
  # Resolve relative images_dir against project root
  id <- as.character(out$images_dir)
  if (!grepl("^[A-Za-z]:[/\\\\]", id) && !startsWith(id, "/")) {
    out$images_dir <- canonical_fs_path(file.path(root, id))
  } else {
    out$images_dir <- canonical_fs_path(id)
  }
  out$rows <- as.integer(out$rows)
  out$cols <- as.integer(out$cols)
  out$spacing_mm <- as.numeric(out$spacing_mm)
  out$dpi <- as.integer(out$dpi)
  # YYMM fields may be NA for non-yymm schemes; keep defaults if missing
  if (!is.null(out$start_yymm) && !is.na(out$start_yymm) && nzchar(as.character(out$start_yymm))) {
    out$start_yymm <- sprintf("%04d", as.integer(out$start_yymm))
  } else {
    out$start_yymm <- PROFILE_DEFAULTS$start_yymm
  }
  if (!is.null(out$end_yymm) && !is.na(out$end_yymm) && nzchar(as.character(out$end_yymm))) {
    out$end_yymm <- sprintf("%04d", as.integer(out$end_yymm))
  } else {
    out$end_yymm <- PROFILE_DEFAULTS$end_yymm
  }
  if (is.null(out$slot_scheme) || is.na(out$slot_scheme) || !nzchar(as.character(out$slot_scheme))) {
    out$slot_scheme <- PROFILE_DEFAULTS$slot_scheme
  } else {
    out$slot_scheme <- tolower(as.character(out$slot_scheme))
  }
  out
}

write_profile <- function(profile, path) {
  row <- data.frame(
    images_dir = as.character(profile$images_dir),
    start_yymm = as.character(profile$start_yymm),
    end_yymm = as.character(profile$end_yymm),
    rows = as.integer(profile$rows),
    cols = as.integer(profile$cols),
    spacing_mm = as.numeric(profile$spacing_mm),
    dpi = as.integer(profile$dpi),
    paper = as.character(profile$paper),
    orientation = as.character(profile$orientation),
    slot_scheme = as.character(if (is.null(profile$slot_scheme) || is.na(profile$slot_scheme)) "yymm" else profile$slot_scheme),
    stringsAsFactors = FALSE
  )
  write.csv(row, path, row.names = FALSE)
  invisible(row)
}

read_slots <- function(path, wanted = NULL, start_yymm = "0801", end_yymm = "2612") {
  if (is.null(wanted)) {
    wanted <- month_prefixes(start_yymm, end_yymm)
  }
  wanted <- as.character(wanted)
  if (!file.exists(path)) {
    return(empty_slots_df(wanted))
  }
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("NA", ""))
  df <- ensure_slot_columns(df)
  # Ensure all wanted slots exist; keep extras at end
  have <- as.character(df$slot)
  missing <- setdiff(wanted, have)
  if (length(missing) > 0) {
    df <- rbind(df, empty_slots_df(missing))
  }
  # Order by calendar / wanted list
  ord <- match(wanted, as.character(df$slot))
  rest <- which(!(as.character(df$slot) %in% wanted))
  df <- df[c(ord[!is.na(ord)], rest), , drop = FALSE]
  rownames(df) <- NULL
  df
}

write_slots <- function(df, path) {
  df <- ensure_slot_columns(df)
  # Stable column order
  cols <- intersect(SLOT_ALL_COLS, names(df))
  extra <- setdiff(names(df), cols)
  write.csv(df[, c(cols, extra), drop = FALSE], path, row.names = FALSE)
  invisible(df)
}

row_has_backup <- function(row) {
  if (is.null(row) || nrow(row) != 1L) return(FALSE)
  fn <- row$file_name_bak[[1L]]
  !is.na(fn) && nzchar(as.character(fn))
}

#' Copy current image+crop fields into *_bak columns.
archive_current_to_bak <- function(df, i) {
  df$file_name_bak[i] <- df$file_name[i]
  for (cn in SLOT_CROP_COLS) {
    df[[paste0(cn, "_bak")]][i] <- df[[cn]][i]
  }
  df
}

#' Swap current ↔ backup for one row.
restore_from_bak <- function(df, i) {
  if (!row_has_backup(df[i, , drop = FALSE])) {
    stop("No backup to restore for this slot.", call. = FALSE)
  }
  cur_fn <- df$file_name[i]
  bak_fn <- df$file_name_bak[i]
  cur_vals <- lapply(SLOT_CROP_COLS, function(cn) df[[cn]][i])
  names(cur_vals) <- SLOT_CROP_COLS
  bak_vals <- lapply(SLOT_CROP_COLS, function(cn) df[[paste0(cn, "_bak")]][i])
  names(bak_vals) <- SLOT_CROP_COLS

  df$file_name[i] <- bak_fn
  df$file_name_bak[i] <- cur_fn
  for (cn in SLOT_CROP_COLS) {
    df[[cn]][i] <- bak_vals[[cn]]
    df[[paste0(cn, "_bak")]][i] <- cur_vals[[cn]]
  }
  df$use_cropped[i] <- isTRUE(as.logical(df$use_cropped[i]))
  df
}

clear_crop_fields <- function(df, i) {
  df$mag_pct[i] <- 100
  df$rotate_cw[i] <- 0
  df$shift_x_pct[i] <- 0
  df$shift_y_pct[i] <- 0
  df$shift_x_px[i] <- 0
  df$shift_y_px[i] <- 0
  df$img_w[i] <- NA_real_
  df$img_h[i] <- NA_real_
  df$S0[i] <- NA_real_
  df$crop_side_px[i] <- NA_real_
  df$crop_left[i] <- NA_real_
  df$crop_top[i] <- NA_real_
  df$crop_out_px[i] <- 0
  df$use_cropped[i] <- FALSE
  # src_* refreshed by refresh_slot_src_meta after assign
  df$src_w[i] <- NA_real_
  df$src_h[i] <- NA_real_
  df$src_bytes[i] <- NA_real_
  df
}

#' Write original-file dimensions into the slot row (and seed img_* if empty).
refresh_slot_src_meta <- function(df, i, images_dir) {
  p <- resolve_image_path(images_dir, df$file_name[i])
  meta <- read_image_meta(p)
  df$src_w[i] <- meta$w
  df$src_h[i] <- meta$h
  df$src_bytes[i] <- meta$bytes
  if (is.na(df$img_w[i]) && !is.na(meta$w)) {
    df$img_w[i] <- meta$w
    df$img_h[i] <- meta$h
    df$S0[i] <- min(meta$w, meta$h)
  }
  df
}

update_slot_source <- function(df, i, new_rel_path, archive = TRUE, reset_crop = TRUE, images_dir = NULL) {
  if (isTRUE(archive)) {
    df <- archive_current_to_bak(df, i)
  }
  df$file_name[i] <- new_rel_path
  if (isTRUE(reset_crop)) {
    df <- clear_crop_fields(df, i)
  }
  if (!is.null(images_dir)) {
    df <- refresh_slot_src_meta(df, i, images_dir)
  }
  df
}

slot_status_summary <- function(df, images_dir, cropped_dir) {
  n <- nrow(df)
  filled <- 0L
  cropped_n <- 0L
  demo_n <- 0L
  blank_n <- 0L
  for (i in seq_len(n)) {
    rel <- df$file_name[i]
    p <- resolve_image_path(images_dir, rel)
    if (!is.na(p) && file.exists(p)) filled <- filled + 1L
    if (isTRUE(df$use_cropped[i]) && file.exists(file.path(cropped_dir, paste0(df$slot[i], ".jpg")))) {
      cropped_n <- cropped_n + 1L
    } else if (isTRUE(df$use_cropped[i]) && !is.na(rel) &&
               file.exists(file.path(cropped_dir, basename(rel)))) {
      cropped_n <- cropped_n + 1L
    }
    if (!is.na(rel) && grepl("^demos/", gsub("\\\\", "/", rel))) {
      if (grepl("blank", basename(rel), ignore.case = TRUE)) {
        blank_n <- blank_n + 1L
      } else {
        demo_n <- demo_n + 1L
      }
    }
  }
  list(n = n, filled = filled, cropped = cropped_n, demo = demo_n, blank = blank_n)
}
