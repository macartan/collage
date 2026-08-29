# Smoke test — run from collage root; prints aggregates only (no image bytes).
setwd("c:/Dropbox/01_life/04 AOIFE/18/prepare_images/collage")
for (f in c("paths.R", "slots.R", "crop.R", "images.R", "poster.R")) {
  source(file.path("R", f))
}

lay <- default_layout()
prof <- read_profile(lay$profile, root = lay$root)
slots <- read_slots(lay$slots, prof$start_yymm, prof$end_yymm)

stopifnot(nrow(slots) == 228L)
stopifnot(identical(slots$file_name[1], "demos/0801.jpg"))
stopifnot(file.exists(file.path(lay$demos, "0801.jpg")))
stopifnot(file.exists(file.path(lay$demos, "blank.jpg")))

src <- resolve_image_path(file.path(lay$root, "images"), slots$file_name[1])
im <- read_edit_image(src, 0)
info <- image_info(im)
w <- info$width[1]
h <- info$height[1]
S0 <- min(w, h)
b <- crop_bounds(w, h, S0, 100, 0, 0)
write_cropped_copy(src, file.path(lay$cropped, "0801.jpg"), b, 0)
slots$use_cropped[1] <- TRUE
slots$crop_side_px[1] <- b$width
slots$crop_left[1] <- b$left
slots$crop_top[1] <- b$top
write_slots(slots, lay$slots)

slots <- update_slot_source(slots, 2L, "demos/blank.jpg", archive = TRUE, reset_crop = TRUE)
stopifnot(row_has_backup(slots[2, , drop = FALSE]))
slots <- restore_from_bak(slots, 2L)
stopifnot(grepl("demos/0802", slots$file_name[2]))

res <- write_poster(
  slots,
  file.path(lay$root, "images"),
  lay$cropped,
  lay$output,
  rows = 19,
  cols = 12,
  spacing_mm = 1,
  dpi = 72,
  paper = "A1",
  orientation = "portrait"
)
sz <- file.info(res$jpg)$size
cat("slots:", nrow(slots), "\n")
cat("poster_bytes:", sz, "\n")
cat("SMOKE_OK\n")
