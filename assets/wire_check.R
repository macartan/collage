setwd("c:/Dropbox/01_life/04 AOIFE/18/prepare_images/collage")
parse("app.R")
cat("PARSE_OK\n")

for (f in c("paths.R", "slots.R", "crop.R", "images.R", "poster.R", "archive.R")) {
  source(file.path("R", f))
}
source("assets/setup.R")

dir.create("archive", showWarnings = FALSE)
if (!file.exists("archive/.gitkeep")) writeLines("", "archive/.gitkeep")

lay <- default_layout()
if (!file.exists(lay$profile)) setup_collage(root = lay$root, demo = FALSE, reset_data = TRUE)

nm <- paste0("wire_", format(Sys.time(), "%H%M%S"))
res <- save_archive(nm, root = lay$root, include_cropped = FALSE, include_output = FALSE, notes = "wire")
stopifnot(dir.exists(file.path("archive", res$name, "data")))
stopifnot(dir.exists(file.path("archive", res$name, "output")))
stopifnot(dir.exists(file.path("archive", res$name, "cropped")))

sum <- archive_summary(res$name, lay$root)
cat("SUMMARY", format_archive_summary_line(sum), "\n")

wl <- workspace_layout(lay$root, res$name)
stopifnot(isTRUE(wl$is_archive), grepl("archive", wl$data, fixed = TRUE))

res2 <- restore_archive(res$name, root = lay$root, restore_cropped = FALSE, restore_output = FALSE)
delete_archive(res$name, root = lay$root)
stopifnot(!dir.exists(file.path("archive", res$name)))

# missing image path
stopifnot(!image_path_ok(NA_character_))
stopifnot(!image_path_ok("no/such/file.jpg"))

app_txt <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
for (s in c("btn_open_archive", "btn_delete_archive_ok", "btn_use_live", "archive_list_ui", "missing_slots")) {
  if (!grepl(s, app_txt, fixed = TRUE)) stop("Missing: ", s)
}
cat("ALL_OK\n")
