# Square crop math and image I/O (ported/simplified from prepare_images/app.R).

suppressPackageStartupMessages({
  library(magick)
})

crop_bounds <- function(w, h, S0, mag_pct, shift_x, shift_y) {
  z <- mag_pct / 100
  if (z <= 0) z <- 1e-6
  T <- S0 / z
  T_max <- min(w, h)
  if (T > T_max) T <- T_max
  cx <- w / 2 + shift_x
  cy <- h / 2 + shift_y
  left <- cx - T / 2
  top <- cy - T / 2
  left <- max(0, min(left, w - T))
  top <- max(0, min(top, h - T))
  list(left = left, top = top, width = T, height = T)
}

crop_geometry <- function(b) {
  sprintf("%.0fx%.0f+%.0f+%.0f", b$width, b$height, b$left, b$top)
}

read_source_image <- function(path) {
  if (!image_path_ok(path)) {
    stop("Image file missing or unreadable: ", if (length(path)) path else "(empty)", call. = FALSE)
  }
  suppressWarnings(tryCatch(
    image_read(path, strip = TRUE),
    error = function(e) {
      stop("Could not read image: ", path, " — ", conditionMessage(e), call. = FALSE)
    }
  ))
}

read_edit_image <- function(src_path, rotate_cw = 0) {
  im <- read_source_image(src_path)
  if (!is.null(rotate_cw) && !is.na(rotate_cw) && rotate_cw != 0) {
    im <- image_rotate(im, -rotate_cw)
  }
  im
}

write_cropped_copy <- function(src_path, out_path, b, rotate_cw = 0, out_side_px = 0) {
  im <- read_edit_image(src_path, rotate_cw = rotate_cw)
  im <- image_crop(im, crop_geometry(b))
  # Optional downscale of the square crop only (original source file untouched).
  side <- suppressWarnings(as.integer(out_side_px))
  if (!is.na(side) && side > 0L) {
    info <- image_info(im)
    cur <- max(info$width[1], info$height[1])
    if (cur > side) {
      im <- image_resize(im, sprintf("%dx%d!", side, side))
    }
  }
  ext <- tolower(tools::file_ext(out_path))
  if (ext %in% c("jpg", "jpeg")) {
    image_write(im, path = out_path, format = "jpeg", quality = 92)
  } else {
    image_write(im, path = out_path)
  }
  invisible(out_path)
}

shift_px_from_pct <- function(shift_x_pct, shift_y_pct, w, h) {
  list(sx = (shift_x_pct / 100) * w, sy = (shift_y_pct / 100) * h)
}

shift_pct_from_row <- function(row, w, h) {
  sx_pct <- NA_real_
  sy_pct <- NA_real_
  if ("shift_x_pct" %in% names(row) && !is.na(row$shift_x_pct)) {
    sx_pct <- row$shift_x_pct
  } else if ("shift_x_px" %in% names(row) && !is.na(row$shift_x_px) && !is.na(w) && w > 0) {
    sx_pct <- 100 * row$shift_x_px / w
  }
  if ("shift_y_pct" %in% names(row) && !is.na(row$shift_y_pct)) {
    sy_pct <- row$shift_y_pct
  } else if ("shift_y_px" %in% names(row) && !is.na(row$shift_y_px) && !is.na(h) && h > 0) {
    sy_pct <- 100 * row$shift_y_px / h
  }
  if (is.na(sx_pct)) sx_pct <- 0
  if (is.na(sy_pct)) sy_pct <- 0
  list(sx_pct = sx_pct, sy_pct = sy_pct)
}

dims_row_has_saved_crop <- function(row) {
  if (is.null(row) || nrow(row) != 1L) return(FALSE)
  if (!("crop_side_px" %in% names(row))) return(FALSE)
  side <- row$crop_side_px[[1L]]
  if (is.na(side) || side <= 0) return(FALSE)
  if (!("crop_left" %in% names(row)) || !("crop_top" %in% names(row))) return(FALSE)
  !is.na(row$crop_left[[1L]]) && !is.na(row$crop_top[[1L]])
}
