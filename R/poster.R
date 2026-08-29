# Poster composition from slots + images/cropped.

suppressPackageStartupMessages({
  library(magick)
})

paper_mm <- function(paper = "A1", orientation = "portrait") {
  sizes <- list(
    A1 = c(594, 841),
    A2 = c(420, 594),
    A3 = c(297, 420),
    A4 = c(210, 297)
  )
  paper <- toupper(as.character(paper))
  if (!(paper %in% names(sizes))) paper <- "A1"
  wh <- sizes[[paper]]
  if (identical(tolower(orientation), "landscape")) {
    return(list(w_mm = wh[2], h_mm = wh[1]))
  }
  list(w_mm = wh[1], h_mm = wh[2])
}

#' Path to use for a slot cell: cropped if flagged and present, else source.
slot_render_path <- function(row, images_dir, cropped_dir) {
  slot <- row$slot[[1L]]
  rel <- row$file_name[[1L]]
  src <- resolve_image_path(images_dir, rel)
  if (isTRUE(as.logical(row$use_cropped[[1L]]))) {
    candidates <- c(
      file.path(cropped_dir, paste0(slot, ".jpg")),
      file.path(cropped_dir, paste0(slot, ".jpeg")),
      file.path(cropped_dir, paste0(slot, ".png")),
      if (!is.na(rel) && nzchar(as.character(rel))) file.path(cropped_dir, basename(rel)) else NULL
    )
    for (c in candidates) {
      if (!is.null(c) && file.exists(c)) return(canonical_fs_path(c))
    }
    # Cropped requested but missing — fall back to source if available
  }
  if (image_path_ok(src)) return(src)
  NA_character_
}

#' Build poster JPEG (and optional PDF) into output_dir.
#' Returns list with jpg, pdf, placed count, and missing_slots.
write_poster <- function(slots_df, images_dir, cropped_dir, output_dir,
                         rows = 19L, cols = 12L, spacing_mm = 1, dpi = 300,
                         paper = "A1", orientation = "portrait",
                         export_pdf = FALSE, progress = NULL) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  sz <- paper_mm(paper, orientation)
  canvas_w <- as.integer(round((sz$w_mm / 25.4) * dpi))
  canvas_h <- as.integer(round((sz$h_mm / 25.4) * dpi))
  spacing <- max(0L, as.integer(round((spacing_mm / 25.4) * dpi)))
  rows <- max(1L, as.integer(rows))
  cols <- max(1L, as.integer(cols))

  usable_w <- canvas_w - (cols - 1L) * spacing
  usable_h <- canvas_h - (rows - 1L) * spacing
  cell <- as.integer(floor(min(usable_w / cols, usable_h / rows)))
  if (cell < 1L) stop("Spacing too large for rows/cols on this paper.", call. = FALSE)

  grid_w <- cols * cell + (cols - 1L) * spacing
  grid_h <- rows * cell + (rows - 1L) * spacing
  margin_x <- as.integer(floor((canvas_w - grid_w) / 2))
  margin_y <- as.integer(floor((canvas_h - grid_h) / 2))

  canvas <- image_blank(width = canvas_w, height = canvas_h, color = "white")
  n_slots <- min(nrow(slots_df), rows * cols)
  missing <- character(0)
  placed <- 0L

  for (i in seq_len(n_slots)) {
    if (is.function(progress)) progress(i, n_slots)
    r <- ((i - 1L) %/% cols)
    c <- ((i - 1L) %% cols)
    x <- margin_x + c * (cell + spacing)
    y <- margin_y + r * (cell + spacing)

    src_file <- slot_render_path(slots_df[i, , drop = FALSE], images_dir, cropped_dir)
    if (!image_path_ok(src_file)) {
      missing <- c(missing, as.character(slots_df$slot[i]))
      next
    }
    im <- tryCatch(
      {
        im0 <- read_source_image(src_file)
        info <- image_info(im0)
        side0 <- max(info$width[1], info$height[1])
        im0 <- image_extent(im0, geometry = sprintf("%dx%d", side0, side0), gravity = "center", color = "white")
        image_resize(im0, sprintf("%dx%d!", cell, cell))
      },
      error = function(e) {
        missing <<- c(missing, as.character(slots_df$slot[i]))
        NULL
      }
    )
    if (is.null(im)) next
    canvas <- image_composite(canvas, im, offset = sprintf("+%d+%d", x, y))
    placed <- placed + 1L
  }

  out_jpg <- file.path(output_dir, "poster.jpg")
  image_write(canvas, path = out_jpg, format = "jpeg", quality = 95)
  out <- list(
    jpg = canonical_fs_path(out_jpg),
    pdf = NA_character_,
    placed = placed,
    missing_slots = unique(missing)
  )

  if (isTRUE(export_pdf)) {
    out_pdf <- file.path(output_dir, "poster.pdf")
    tryCatch(
      {
        image_write(canvas, path = out_pdf, format = "pdf")
        out$pdf <- canonical_fs_path(out_pdf)
      },
      error = function(e) NULL
    )
  }
  out
}
