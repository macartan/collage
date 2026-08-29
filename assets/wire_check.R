setwd("c:/Dropbox/01_life/04 AOIFE/18/prepare_images/collage")
parse("app.R")
cat("PARSE_OK\n")

for (f in c("paths.R", "slots.R", "crop.R", "images.R", "poster.R", "archive.R")) {
  source(file.path("R", f))
}
source("assets/setup.R")

ids <- make_slot_ids("yymm", rows = 10, cols = 12, start_yymm = "1501", end_yymm = "2412")
stopifnot(length(ids) == 120L, identical(ids[1], "1501"), identical(ids[120], "2412"))
cat("IDS_OK", length(ids), "\n")

lay <- default_layout()
if (!file.exists(lay$profile) || !file.exists(lay$slots)) {
  setup_collage(root = lay$root, demo = FALSE, reset_data = TRUE)
}

nm <- paste0("_wiretest_", format(Sys.time(), "%H%M%S"))
res <- save_archive(nm, root = lay$root, include_cropped = FALSE, notes = "wire")
stopifnot(dir.exists(res$path))
res2 <- restore_archive(nm, root = lay$root, restore_cropped = FALSE, missing_demos = "regenerate")
cat("ARCHIVE_OK", res2$name, "\n")
unlink(res$path, recursive = TRUE)

# Confirm Setup inputs exist in parsed app (string check)
app_txt <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
needed <- c(
  "btn_restore_defaults_ok",
  "btn_reshape_grid_ok",
  "btn_save_archive",
  "btn_restore_archive_ok",
  "reload_live_state",
  "setup_preview_ids",
  "setup_auto_rows"
)
for (s in needed) {
  if (!grepl(s, app_txt, fixed = TRUE)) stop("Missing wiring: ", s)
}
cat("WIRING_OK\n")
cat("ALL_OK\n")
