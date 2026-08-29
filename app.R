# Collage — square-crop monthly poster for non-technical users.
# Open this file in RStudio and click Run App, or: shiny::runApp()

suppressPackageStartupMessages({
  library(shiny)
  library(magick)
})

app_root <- tryCatch(
  {
    # When running via shiny::runApp(), getwd() is the app directory
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  },
  error = function(e) normalizePath(".", winslash = "/", mustWork = FALSE)
)

for (f in c("paths.R", "slots.R", "crop.R", "images.R", "poster.R", "archive.R")) {
  source(file.path(app_root, "R", f), local = FALSE)
}
# setup_collage / restore / reshape / demo tile helpers
source(file.path(app_root, "assets", "setup.R"), local = FALSE)

layout0 <- default_layout(app_root)
ensure_project_dirs(layout0)
keep_arch <- file.path(layout0$archive, ".gitkeep")
if (!file.exists(keep_arch)) writeLines("", keep_arch)

MAX_PREVIEW <- 900L

load_state <- function(root = app_root, archive_name = NULL) {
  lay <- workspace_layout(root, archive_name)
  if (isTRUE(lay$is_archive)) {
    if (!dir.exists(lay$archive_dir)) {
      stop("Archive not found: ", archive_name, call. = FALSE)
    }
    ensure_archive_dirs(lay$archive_dir)
  } else {
    ensure_project_dirs(lay)
  }
  if (!file.exists(lay$profile) || !file.exists(lay$slots)) {
    if (isTRUE(lay$is_archive)) {
      stop("Archive is incomplete (missing data/profile.csv or data/slots.csv).", call. = FALSE)
    }
    source(file.path(root, "assets", "setup.R"), local = TRUE)
    setup_collage(root = root, demo = FALSE)
    lay <- workspace_layout(root, NULL)
  }
  profile <- read_profile(lay$profile, root = root)
  img <- profile$images_dir
  for (d in c(img, file.path(img, "demos"), file.path(img, "import"))) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  wanted <- tryCatch(
    make_slot_ids(
      scheme = profile$slot_scheme,
      rows = profile$rows,
      cols = profile$cols,
      start_yymm = profile$start_yymm,
      end_yymm = profile$end_yymm
    ),
    error = function(e) month_prefixes(profile$start_yymm, profile$end_yymm)
  )
  slots <- read_slots(lay$slots, wanted = wanted)
  list(layout = lay, profile = profile, slots = slots)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', system-ui, sans-serif; background: #f6f5f2; }
      .title-bar { margin: 12px 0 4px; }
      .title-bar h2 { margin: 0; font-size: 1.35rem; font-weight: 650; color: #222; }
      .subtitle { color: #666; font-size: 0.92rem; margin-bottom: 10px; }
      .panel-card { background: #fff; border: 1px solid #e6e3dc; border-radius: 10px;
                    padding: 14px 16px; margin-bottom: 12px; }
      .panel-h { font-size: 0.78rem; font-weight: 650; letter-spacing: .04em;
                 text-transform: uppercase; color: #555; margin: 0 0 10px; }
      .status-line { font-size: 0.95rem; color: #333; margin: 6px 0 12px; }
      .poster-grid { display: grid; gap: 2px; background: #ddd; padding: 2px; border-radius: 6px; }
      .poster-grid img, .poster-grid .empty-cell {
        width: 100%; aspect-ratio: 1/1; object-fit: cover; display: block; cursor: pointer;
      }
      .poster-grid .empty-cell { background: #eee; }
      .poster-grid .cell-wrap { position: relative; }
      .poster-grid .cell-label {
        position: absolute; left: 2px; bottom: 2px; font-size: 9px; color: #fff;
        text-shadow: 0 0 3px #000; pointer-events: none;
      }
      .preview-box { background: #111; border-radius: 8px; }
      .rot-btn { font-size: 18px; min-width: 44px; }
      .muted { color: #777; font-size: 12px; }
      .source-choice .shiny-input-radiogroup .radio { margin-bottom: 4px; }
      code { background: #f0eee8; padding: 1px 5px; border-radius: 3px; font-size: 0.85em; }
      .welcome { max-width: 720px; }
      .welcome h3 { margin-top: 0; font-size: 1.15rem; font-weight: 650; }
      .welcome ol, .welcome ul { line-height: 1.55; color: #333; }
      .welcome .two-ways { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; margin: 14px 0; }
      @media (max-width: 700px) { .welcome .two-ways { grid-template-columns: 1fr; } }
      .welcome .way { background: #faf9f6; border: 1px solid #e6e3dc; border-radius: 8px; padding: 12px 14px; }
      .welcome .way h4 { margin: 0 0 6px; font-size: 0.95rem; }
      .welcome .way p { margin: 0; font-size: 0.9rem; color: #444; line-height: 1.45; }
      .meta-box { font-size: 12px; line-height: 1.45; color: #333; background: #faf9f6;
                  border: 1px solid #e6e3dc; border-radius: 6px; padding: 8px 10px; margin-top: 8px; }
      .meta-box .meta-k { color: #666; }
      .meta-box .meta-poster-orig { color: #356096; font-weight: 600; }
      .meta-box .meta-poster-crop { color: #1e9e78; font-weight: 600; }
      .meta-box .meta-warn { color: #a65c00; }
      .danger-zone { border: 2px solid #c0392b; background: #fdf5f4; }
      .danger-zone .panel-h { color: #922b21; }
      .danger-banner { background: #922b21; color: #fff; border-radius: 6px;
                       padding: 8px 12px; margin-bottom: 12px; font-size: 0.92rem; }
      .default-tag { color: #888; font-size: 11px; font-weight: 400; }
    ")),
    tags$script(HTML("
      $(document).on('keydown', function(e) {
        if ($(e.target).is('input, textarea, select')) return;
        if (e.key === 'ArrowLeft') {
          Shiny.setInputValue('key_nav', {dir:'prev', nonce:Math.random()}, {priority:'event'});
          e.preventDefault();
        }
        if (e.key === 'ArrowRight') {
          Shiny.setInputValue('key_nav', {dir:'next', nonce:Math.random()}, {priority:'event'});
          e.preventDefault();
        }
      });
    "))
  ),
  div(
    class = "title-bar",
    tags$h2("Collage maker"),
    div(class = "subtitle", "Put photos in slots · crop · generate a poster")
  ),
  uiOutput("status_summary_ui"),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Poster",
      br(),
      fluidRow(
        column(
          3,
          div(
            class = "panel-card",
            div(class = "panel-h", "Generate"),
            actionButton("btn_make_poster", "Generate poster", class = "btn-primary", width = "100%"),
            checkboxInput("chk_pdf", "Also save PDF", value = FALSE),
            div(class = "muted", style = "margin-top:8px;", "Saves to ", tags$code("output/")),
            textOutput("poster_msg"),
            tags$hr(style = "margin:12px 0;"),
            div(
              style = "display:flex; align-items:center; gap:8px;",
              actionButton("btn_smart_load", "Smart load", class = "btn-success", style = "flex:1;"),
              actionButton(
                "btn_smart_load_help",
                label = NULL,
                icon = icon("circle-question"),
                class = "btn-default btn-sm",
                title = "What is Smart load?",
                style = "min-width:36px;"
              )
            ),
            div(class = "muted", style = "margin-top:6px;",
                "Fill demo slots from matching filenames in ", tags$code("images/"), "."),
            textOutput("smart_load_msg")
          ),
          div(
            class = "panel-card",
            div(class = "panel-h", "Layout"),
            fluidRow(
              column(6, numericInput("grid_rows", "Rows", value = 19, min = 1, max = 40, step = 1)),
              column(6, numericInput("grid_cols", "Columns", value = 12, min = 1, max = 40, step = 1))
            ),
            fluidRow(
              column(6, numericInput("spacing_mm", "Gap (mm)", value = 1, min = 0, max = 20, step = 0.5)),
              column(6, numericInput("dpi", "DPI", value = 300, min = 72, max = 600, step = 10))
            ),
            selectInput("paper", "Paper", choices = c("A1", "A2", "A3", "A4"), selected = "A1"),
            selectInput("orientation", "Orientation", choices = c("portrait", "landscape"), selected = "portrait"),
            actionButton("btn_save_layout", "Save layout", class = "btn-default btn-sm", width = "100%")
          ),
          div(
            class = "panel-card",
            div(class = "panel-h", "Images folder"),
            textInput("images_dir_txt", NULL, value = "", width = "100%"),
            actionButton("btn_save_images_dir", "Save path", class = "btn-default btn-sm", width = "100%"),
            div(class = "muted", style = "margin-top:8px;",
                "Default is this project's ", tags$code("images/"),
                ". Demos live in ", tags$code("images/demos/"), " and are never overwritten.")
          )
        ),
        column(
          9,
          div(
            class = "panel-card",
            div(class = "panel-h", "Grid preview — click a cell to edit it"),
            div(class = "muted", "Filled cells show a thumbnail; empty or missing files are light grey."),
            br(),
            uiOutput("poster_grid_ui")
          )
        )
      )
    ),
    tabPanel(
      "Edit slot",
      br(),
      # --- Assign (top) ---
      div(
        class = "panel-card",
        div(class = "panel-h", "Assign image"),
        fluidRow(
          column(
            3,
            selectInput("slot_pick", "Slot", choices = character(0), width = "100%"),
            fluidRow(
              column(6, actionButton("btn_prev", "←", class = "btn-default", width = "100%")),
              column(6, actionButton("btn_next", "→", class = "btn-default", width = "100%"))
            ),
            div(class = "muted", style = "margin-top:8px;", "Arrow keys move between slots."),
            uiOutput("image_meta_ui"),
            conditionalPanel(
              "input.source_choice == 'new'",
              uiOutput("candidate_meta_ui")
            )
          ),
          column(
            5,
            div(
              class = "source-choice",
              radioButtons(
                "source_choice",
                NULL,
                choices = c(
                  "Keep current image" = "current",
                  "Blank (white)" = "blank",
                  "Demo color tile" = "demo",
                  "Choose new image…" = "new",
                  "Restore previous" = "restore"
                ),
                selected = "current"
              ),
              conditionalPanel(
                "input.source_choice == 'new'",
                textInput("new_label", "Optional label (e.g. vacation → 1205_vacation.jpg)", value = ""),
                fileInput("new_file", "Pick a file", accept = c("image/jpeg", "image/png", "image/webp", ".jpg", ".jpeg", ".png", ".webp")),
                selectInput("import_pick", "Or pick from images/import/", choices = c("(none)" = ""), width = "100%"),
                actionButton("btn_refresh_import", "Refresh import list", class = "btn-default btn-sm"),
                checkboxInput("overwrite_img", "Overwrite if file exists", value = FALSE)
              ),
              actionButton("btn_apply_source", "Apply", class = "btn-primary", width = "100%"),
              div(style = "margin-top:8px;", textOutput("assign_msg"))
            )
          ),
          column(
            4,
            div(class = "panel-h", "Import inbox"),
            div(class = "muted",
                "Drop photos into ", tags$code("images/import/"),
                ". Chosen files are copied into ", tags$code("images/"),
                " as ", tags$code("1205_label.jpg"), "."),
            br(),
            uiOutput("import_thumbs_ui")
          )
        )
      ),
      # --- Crop (bottom) ---
      fluidRow(
        column(
          4,
          div(
            class = "panel-card",
            div(class = "panel-h", "Crop"),
            numericInput("mag_pct", "Zoom % (100 = full square)", value = 100, min = 10, max = 10000, step = 1),
            fluidRow(
              column(5, actionButton("btn_rot_ccw", HTML("&#8635;"), class = "rot-btn btn-default", width = "100%", title = "Rotate CCW")),
              column(5, actionButton("btn_rot_cw", HTML("&#8634;"), class = "rot-btn btn-default", width = "100%", title = "Rotate CW")),
              column(2, div(style = "text-align:center;padding-top:8px;font-weight:600;", textOutput("rotate_label")))
            ),
            fluidRow(
              column(6, numericInput("shift_x_pct", "Shift X %", value = 0, step = 0.5)),
              column(6, numericInput("shift_y_pct", "Shift Y %", value = 0, step = 0.5))
            ),
            selectInput(
              "crop_out_px",
              "Saved size (px)",
              choices = c(
                "Full (no resize)" = "0",
                "2400" = "2400",
                "1600" = "1600",
                "1200" = "1200",
                "800" = "800"
              ),
              selected = "0",
              width = "100%"
            ),
            div(class = "muted", style = "margin-top:-6px;margin-bottom:8px;",
                "Only the file in ", tags$code("cropped/"), " is resized. The original stays untouched."),
            actionButton("btn_save_crop", "Save crop", class = "btn-primary", width = "100%"),
            div(class = "muted", style = "margin-top:8px;",
                "Writes a square crop into ", tags$code("cropped/"), " and remembers settings in ", tags$code("data/"), ".")
          )
        ),
        column(
          8,
          div(
            class = "panel-card preview-box",
            plotOutput("preview_plot", height = "520px")
          )
        )
      )
    ),
    tabPanel(
      "Setup",
      br(),
      div(
        class = "danger-banner",
        tags$strong("Danger zone"),
        " — these actions rewrite ", tags$code("data/"),
        " and can wipe crops / rebuild demos. They run through ",
        tags$code("assets/setup.R"), "."
      ),
      fluidRow(
        column(
          5,
          div(
            class = "panel-card danger-zone",
            div(class = "panel-h", "1 · Restore default data"),
            tags$p(
              "Reset to the shared starter layout: ",
              tags$code("19×12"), " on ", tags$code("A1"),
              ", slot names ", tags$code("0801…2612"),
              ", clear ", tags$code("cropped/"),
              ", recreate demos."
            ),
            actionButton("btn_restore_defaults", "Restore defaults…", class = "btn-danger", width = "100%"),
            div(style = "margin-top:8px;", textOutput("setup_restore_msg"))
          )
        ),
        column(
          7,
          div(
            class = "panel-card danger-zone",
            div(class = "panel-h", "2 · Reshape grid (resets all slot data)"),
            tags$p(
              class = "muted",
              "Changes rows/cols, gap, paper, and how slots are named. ",
              tags$strong("All assignments and crop settings are replaced."),
              " Demo tiles are rebuilt for the new cell ids."
            ),
            fluidRow(
              column(
                6,
                numericInput(
                  "setup_rows",
                  HTML(sprintf("Rows <span class='default-tag'>(default %s)</span>", PROFILE_DEFAULTS$rows)),
                  value = PROFILE_DEFAULTS$rows, min = 1, max = 40, step = 1
                )
              ),
              column(
                6,
                numericInput(
                  "setup_cols",
                  HTML(sprintf("Columns <span class='default-tag'>(default %s)</span>", PROFILE_DEFAULTS$cols)),
                  value = PROFILE_DEFAULTS$cols, min = 1, max = 40, step = 1
                )
              )
            ),
            fluidRow(
              column(
                6,
                numericInput(
                  "setup_spacing",
                  HTML(sprintf("Gap / cell spacing mm <span class='default-tag'>(default %s)</span>", PROFILE_DEFAULTS$spacing_mm)),
                  value = PROFILE_DEFAULTS$spacing_mm, min = 0, max = 20, step = 0.5
                )
              ),
              column(
                6,
                numericInput(
                  "setup_dpi",
                  HTML(sprintf("DPI <span class='default-tag'>(default %s)</span>", PROFILE_DEFAULTS$dpi)),
                  value = PROFILE_DEFAULTS$dpi, min = 72, max = 600, step = 10
                )
              )
            ),
            fluidRow(
              column(
                6,
                selectInput(
                  "setup_paper",
                  HTML(sprintf("Paper <span class='default-tag'>(default %s)</span>", PROFILE_DEFAULTS$paper)),
                  choices = c("A1", "A2", "A3", "A4"),
                  selected = PROFILE_DEFAULTS$paper
                )
              ),
              column(
                6,
                selectInput(
                  "setup_orientation",
                  HTML(sprintf("Orientation <span class='default-tag'>(default %s)</span>", PROFILE_DEFAULTS$orientation)),
                  choices = c("portrait", "landscape"),
                  selected = PROFILE_DEFAULTS$orientation
                )
              )
            ),
            selectInput(
              "setup_scheme",
              "Slot name matrix",
              choices = c(
                "Year–month calendar (0801, 0802, …) — default" = "yymm",
                "Row × column (r01c01, r01c02, …)" = "rowcol",
                "Sequential (0001, 0002, … 1:n)" = "sequential"
              ),
              selected = "yymm"
            ),
            conditionalPanel(
              "input.setup_scheme == 'yymm'",
              fluidRow(
                column(
                  6,
                  textInput(
                    "setup_start_yymm",
                    HTML(sprintf("Start YYMM <span class='default-tag'>(default %s)</span>", PROFILE_DEFAULTS$start_yymm)),
                    value = PROFILE_DEFAULTS$start_yymm
                  )
                ),
                column(
                  6,
                  textInput(
                    "setup_end_yymm",
                    HTML(sprintf("End YYMM <span class='default-tag'>(default %s)</span>", PROFILE_DEFAULTS$end_yymm)),
                    value = PROFILE_DEFAULTS$end_yymm
                  )
                )
              ),
              div(class = "muted",
                  "Example for 10 years from 2015: start ", tags$code("1501"),
                  ", end ", tags$code("2412"), ", rows ", tags$code("10"),
                  ", columns ", tags$code("12"),
                  ". Changing start/end auto-sets rows×12 when the range is whole years."),
              checkboxInput("setup_auto_rows", "Auto-set rows/cols from year–month range", value = TRUE)
            ),
            checkboxInput("setup_demo", "Recreate demo pictures for new cells", value = TRUE),
            checkboxInput("setup_clear_cropped", "Clear cropped/ folder", value = TRUE),
            uiOutput("setup_preview_ids"),
            br(),
            actionButton("btn_reshape_grid", "Reshape grid…", class = "btn-danger", width = "100%"),
            div(style = "margin-top:8px;", textOutput("setup_reshape_msg"))
          )
        )
      ),
      br(),
      div(
        class = "panel-card",
        div(class = "panel-h", "3 · Archives"),
        tags$p(
          "Archives live in ", tags$code("archive/<project_name>/"),
          " (not zipped): ", tags$code("data/"), ", ", tags$code("cropped/"),
          ", and ", tags$code("output/"),
          ". Photos in ", tags$code("images/"), " are shared and not copied."
        ),
        uiOutput("archive_list_ui"),
        br(),
        fluidRow(
          column(
            4,
            textInput("archive_name", "New archive name", value = "", placeholder = "e.g. family_2010_2020"),
            textInput("archive_notes", "Notes (optional)", value = ""),
            checkboxInput("archive_include_cropped", "Include cropped/", value = TRUE),
            checkboxInput("archive_include_output", "Include output/ (posters)", value = TRUE),
            actionButton("btn_save_archive", "Save current → archive", class = "btn-primary", width = "100%"),
            div(style = "margin-top:8px;", textOutput("archive_save_msg"))
          ),
          column(
            4,
            selectInput("archive_pick", "Select archive", choices = c("(none)" = ""), width = "100%"),
            actionButton("btn_refresh_archives", "Refresh list", class = "btn-default btn-sm", width = "100%"),
            br(), br(),
            checkboxInput("archive_restore_cropped", "When restoring: copy cropped/", value = TRUE),
            checkboxInput("archive_restore_output", "When restoring: copy output/", value = TRUE),
            radioButtons(
              "archive_missing_demos",
              "Missing demos on restore/open",
              choices = c("Regenerate" = "regenerate", "Use blank.jpg" = "blank"),
              selected = "regenerate"
            ),
            actionButton("btn_open_archive", "Open archive (edit in place)", class = "btn-info", width = "100%"),
            actionButton("btn_restore_archive", "Restore into live workspace…", class = "btn-warning", width = "100%", style = "margin-top:6px;"),
            actionButton("btn_use_live", "Back to live workspace", class = "btn-default", width = "100%", style = "margin-top:6px;"),
            actionButton("btn_delete_archive", "Delete archive…", class = "btn-danger", width = "100%", style = "margin-top:6px;"),
            div(style = "margin-top:8px;", textOutput("archive_load_msg"))
          ),
          column(
            4,
            div(uiOutput("archive_detail_ui")),
            tags$hr(),
            tags$p(
              class = "muted",
              tags$strong("Open"), " reads/writes that archive folder directly. ",
              tags$strong("Restore"), " copies it into the live ", tags$code("data/"),
              " / ", tags$code("cropped/"), " / ", tags$code("output/"), ". ",
              "Generate poster while an archive is open → saves under that archive’s ",
              tags$code("output/"), "."
            )
          )
        )
      )
    ),
    tabPanel(
      "Help",
      br(),
      div(
        class = "panel-card welcome",
        tags$h3("Make a monthly photo poster"),
        tags$p(
          "This app helps you place photos on a calendar grid (one slot per month), ",
          "crop them to squares, and build a print-ready poster."
        ),
        tags$h3("Quick start"),
        tags$ol(
          tags$li(tags$strong("Add photos"), " — drop files into ", tags$code("images/"),
                  " or ", tags$code("images/import/"),
                  " named for their slots (e.g. ", tags$code("1205.jpg"), " or ",
                  tags$code("1205_vacation.jpg"), ")."),
          tags$li(tags$strong("Smart load"), " — on the Poster tab, click ", tags$strong("Smart load"),
                  " (use the ", tags$strong("?"), " help beside it). It finds slots still on demo/blank fill and, when exactly one matching filename exists, assigns it. No match or several matches → that slot is left alone."),
          tags$li(tags$strong("Edit slot"), " — fine-tune any cell (blank / demo / new / restore), then crop."),
          tags$li(tags$strong("Poster"), " — Generate poster, or render ", tags$code("poster.qmd"), " separately."),
          tags$li(tags$strong("Setup"), " — reshape the year range / grid, or archive projects.")
        ),
        div(
          class = "two-ways",
          div(
            class = "way",
            tags$h4("Option A — all in the app"),
            tags$p(
              "Use the Poster tab and click ", tags$strong("Generate poster"),
              ". The file appears in ", tags$code("output/"), " (usually ", tags$code("poster.jpg"), ")."
            ),
            actionButton("btn_goto_poster", "Go to Poster", class = "btn-primary btn-sm", style = "margin-top:10px;")
          ),
          div(
            class = "way",
            tags$h4("Option B — prep here, build separately"),
            tags$p(
              "Assign and crop in this app (saves decisions in ", tags$code("data/"),
              "), then open ", tags$code("poster.qmd"),
              " to fine-tune DPI, gaps, labels, and export. Render it in RStudio / Quarto; output still goes to ",
              tags$code("output/"), "."
            )
          )
        ),
        tags$h3("Tips"),
        tags$ul(
          tags$li("Demo tiles in ", tags$code("images/demos/"), " are safe placeholders — the app never overwrites them during normal editing."),
          tags$li(tags$strong("Smart load"), " only changes slots that still point at ", tags$code("demos/"),
                  "; unique matches win, zero or many matches are skipped."),
          tags$li("New photos are copied into ", tags$code("images/"), " as names like ",
                  tags$code("1205.jpg"), " or ", tags$code("1205_vacation.jpg"), "."),
          tags$li("Choosing a new image keeps one backup so you can Restore once."),
          tags$li("Click any cell on the Poster grid to jump to Edit slot for that month."),
          tags$li("Use ", tags$strong("Setup"), " only when starting over or changing the grid size / naming scheme.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  st0 <- load_state(app_root)

  rv <- reactiveValues(
    profile = st0$profile,
    slots = st0$slots,
    layout = st0$layout,
    idx = 1L,
    rot_cw = 0L,
    img_w = NA_real_,
    img_h = NA_real_,
    S0 = NA_real_,
    assign_msg = "",
    poster_msg = "",
    smart_load_msg = "",
    grid_nonce = 0L,
    setup_restore_msg = "",
    setup_reshape_msg = "",
    archive_save_msg = "",
    archive_load_msg = "",
    archive_nonce = 0L
  )

  images_dir <- reactive({
    canonical_fs_path(rv$profile$images_dir)
  })

  cropped_dir <- reactive(rv$layout$cropped)
  slots_path <- reactive(rv$layout$slots)
  profile_path <- reactive(rv$layout$profile)

  persist_slots <- function() {
    write_slots(rv$slots, slots_path())
  }

  persist_profile <- function() {
    # Prefer relative images path when under project
    p <- rv$profile
    img <- canonical_fs_path(p$images_dir)
    root <- rv$layout$root
    def_img <- canonical_fs_path(file.path(root, "images"))
    if (paths_equivalent(img, def_img)) {
      p$images_dir <- "images"
    }
    write_profile(p, profile_path())
    # Re-read so images_dir is absolute again in memory
    rv$profile <- read_profile(profile_path(), root = root)
  }

  observe({
    updateTextInput(session, "images_dir_txt", value = images_dir())
    updateNumericInput(session, "grid_rows", value = rv$profile$rows)
    updateNumericInput(session, "grid_cols", value = rv$profile$cols)
    updateNumericInput(session, "spacing_mm", value = rv$profile$spacing_mm)
    updateNumericInput(session, "dpi", value = rv$profile$dpi)
    updateSelectInput(session, "paper", selected = rv$profile$paper)
    updateSelectInput(session, "orientation", selected = rv$profile$orientation)
  })

  sync_slot_selectors <- function() {
    ch <- rv$slots$slot
    names(ch) <- paste0(rv$slots$slot, " — ", basename(as.character(rv$slots$file_name)))
    sel <- rv$slots$slot[min(max(rv$idx, 1L), length(ch))]
    updateSelectInput(session, "slot_pick", choices = ch, selected = sel)
  }

  observe({
    req(rv$slots)
    sync_slot_selectors()
  })

  output$status_summary_ui <- renderUI({
    s <- slot_status_summary(rv$slots, images_dir(), cropped_dir())
    ws <- if (isTRUE(rv$layout$is_archive)) {
      paste0("Archive: ", rv$layout$archive_name)
    } else {
      "Live workspace"
    }
    div(
      class = "status-line",
      tags$span(ws),
      " · ",
      sprintf(
        "Slots %d · files present %d · demos %d · blank %d · cropped %d",
        s$n, s$filled, s$demo, s$blank, s$cropped
      )
    )
  })

  # ---- navigation ----
  set_idx <- function(i) {
    n <- nrow(rv$slots)
    if (n < 1L) return(invisible(NULL))
    rv$idx <- min(max(as.integer(i), 1L), n)
    slot <- rv$slots$slot[rv$idx]
    updateSelectInput(session, "slot_pick", selected = slot)
    sync_crop_inputs()
  }

  observeEvent(input$slot_pick, {
    w <- which(rv$slots$slot == input$slot_pick)
    if (length(w) == 1L && w != rv$idx) set_idx(w)
  }, ignoreInit = TRUE)

  observeEvent(input$btn_prev, set_idx(rv$idx - 1L))
  observeEvent(input$btn_next, set_idx(rv$idx + 1L))
  observeEvent(input$key_nav, {
    if (identical(input$key_nav$dir, "prev")) set_idx(rv$idx - 1L)
    if (identical(input$key_nav$dir, "next")) set_idx(rv$idx + 1L)
  })

  observeEvent(input$btn_goto_poster, {
    updateTabsetPanel(session, "tabs", selected = "Poster")
  })

  observeEvent(input$grid_pick_slot, {
    w <- which(rv$slots$slot == input$grid_pick_slot)
    if (length(w) == 1L) {
      set_idx(w)
      updateTabsetPanel(session, "tabs", selected = "Edit slot")
    }
  })

  # Greeting popup once per session
  session$onFlushed(function() {
    showModal(modalDialog(
      title = "Welcome to Collage",
      easyClose = TRUE,
      footer = modalButton("Got it"),
      tags$p("Place one photo per month, crop to a square, then make a print-ready poster."),
      tags$ul(
        tags$li(tags$strong("Poster"), " — see the grid and generate ", tags$code("output/poster.jpg"), "."),
        tags$li(tags$strong("Edit slot"), " — assign a photo (or blank / demo), then crop."),
        tags$li(tags$strong("Help"), " — full walkthrough anytime.")
      ),
      tags$p(
        style = "color:#777;font-size:12px;",
        "You can build the poster here, or prep images in the app and render ",
        tags$code("poster.qmd"), " separately for fine-tuning."
      )
    ))
  }, once = TRUE)

  current_row <- reactive({
    req(rv$slots, nrow(rv$slots) >= 1L)
    rv$slots[min(max(rv$idx, 1L), nrow(rv$slots)), , drop = FALSE]
  })

  current_src_path <- reactive({
    row <- current_row()
    resolve_image_path(images_dir(), row$file_name[[1L]])
  })

  sync_crop_inputs <- function() {
    row <- isolate(current_row())
    mag <- row$mag_pct[[1L]]
    rot <- row$rotate_cw[[1L]]
    if (is.na(mag)) mag <- 100
    if (is.na(rot)) rot <- 0
    p <- shift_pct_from_row(row, row$img_w[[1L]], row$img_h[[1L]])
    updateNumericInput(session, "mag_pct", value = mag)
    rv$rot_cw <- as.integer(rot %% 360)
    updateNumericInput(session, "shift_x_pct", value = p$sx_pct)
    updateNumericInput(session, "shift_y_pct", value = p$sy_pct)
    out_px <- row$crop_out_px[[1L]]
    if (is.na(out_px) || out_px <= 0) {
      updateSelectInput(session, "crop_out_px", selected = "0")
    } else {
      presets <- c(
        "Full (no resize)" = "0",
        "2400" = "2400",
        "1600" = "1600",
        "1200" = "1200",
        "800" = "800"
      )
      sel <- as.character(as.integer(out_px))
      if (!(sel %in% unname(presets))) {
        presets <- c(presets, setNames(sel, paste0(sel, " (saved)")))
      }
      updateSelectInput(session, "crop_out_px", choices = presets, selected = sel)
    }
  }

  observe({
    p <- current_src_path()
    if (is.na(p) || !file.exists(p)) {
      rv$img_w <- NA_real_
      rv$img_h <- NA_real_
      rv$S0 <- NA_real_
      return(NULL)
    }
    info <- tryCatch(
      image_info(read_edit_image(p, rotate_cw = rv$rot_cw)),
      error = function(e) NULL
    )
    if (is.null(info)) {
      rv$img_w <- NA_real_
      rv$img_h <- NA_real_
      rv$S0 <- NA_real_
      return(NULL)
    }
    rv$img_w <- info$width[1]
    rv$img_h <- info$height[1]
    rv$S0 <- min(info$width[1], info$height[1])
    # Persist original (unrotated) source size into slots when missing/stale
    i <- isolate(rv$idx)
    if (!is.null(i) && i >= 1L && i <= nrow(rv$slots)) {
      meta0 <- read_image_meta(p)
      need <- is.na(rv$slots$src_w[i]) || is.na(rv$slots$src_h[i]) ||
        (!is.na(meta0$w) && (rv$slots$src_w[i] != meta0$w || rv$slots$src_h[i] != meta0$h))
      if (isTRUE(need) && meta0$exists) {
        rv$slots$src_w[i] <- meta0$w
        rv$slots$src_h[i] <- meta0$h
        rv$slots$src_bytes[i] <- meta0$bytes
        persist_slots()
      }
    }
  })

  output$rotate_label <- renderText(paste0(rv$rot_cw, "°"))
  observeEvent(input$btn_rot_cw, { rv$rot_cw <- as.integer((rv$rot_cw + 90) %% 360) })
  observeEvent(input$btn_rot_ccw, { rv$rot_cw <- as.integer((rv$rot_cw + 270) %% 360) })

  output$image_meta_ui <- renderUI({
    row <- current_row()
    rel <- as.character(row$file_name[[1L]])
    src <- current_src_path()
    meta <- read_image_meta(src)
    kind <- source_kind_label(rel)
    crop_path <- cropped_file_for_slot(row, cropped_dir())
    uses_crop <- isTRUE(row$use_cropped[[1L]]) && !is.na(crop_path)
    crop_meta <- if (!is.na(crop_path)) read_image_meta(crop_path) else NULL

    bak <- row$file_name_bak[[1L]]
    bak_line <- if (!is.na(bak) && nzchar(as.character(bak))) {
      tags$div(tags$span(class = "meta-k", "Backup: "), as.character(bak))
    } else {
      NULL
    }

    poster_line <- if (uses_crop) {
      tags$div(
        tags$span(class = "meta-poster-crop", "Poster uses: cropped copy"),
        tags$div(
          class = "muted",
          paste0(basename(crop_path), " — ", fmt_px(crop_meta$w, crop_meta$h),
                 " · ", fmt_bytes(crop_meta$bytes))
        )
      )
    } else if (isTRUE(row$use_cropped[[1L]]) && is.na(crop_path)) {
      tags$div(
        tags$span(class = "meta-warn", "Poster flag says cropped, but crop file is missing"),
        tags$div(class = "muted", "Will fall back to the original until you Save crop again.")
      )
    } else {
      tags$div(
        tags$span(class = "meta-poster-orig", "Poster uses: original"),
        tags$div(class = "muted", "No saved crop — square-fitted from the source at render time.")
      )
    }

    work_line <- if (!is.na(rv$img_w) && !is.na(rv$img_h) && rv$rot_cw != 0) {
      tags$div(
        tags$span(class = "meta-k", "Working view (rotated): "),
        fmt_px(rv$img_w, rv$img_h)
      )
    } else {
      NULL
    }

    if (!isTRUE(meta$exists)) {
      return(div(
        class = "meta-box",
        tags$div(tags$strong("Source missing")),
        tags$div(rel),
        poster_line,
        bak_line
      ))
    }

    div(
      class = "meta-box",
      tags$div(tags$span(class = "meta-k", "Source: "), tags$code(rel)),
      tags$div(tags$span(class = "meta-k", "Kind: "), kind),
      tags$div(
        tags$span(class = "meta-k", "Original size: "),
        fmt_px(meta$w, meta$h),
        " · ", fmt_bytes(meta$bytes),
        if (!is.na(meta$format)) paste0(" · ", meta$format) else ""
      ),
      work_line,
      poster_line,
      bak_line
    )
  })

  output$candidate_meta_ui <- renderUI({
    src <- NULL
    label <- NULL
    if (!is.null(input$new_file) && is.data.frame(input$new_file) && nrow(input$new_file) >= 1L &&
        nzchar(input$new_file$datapath[1])) {
      src <- input$new_file$datapath[1]
      label <- input$new_file$name[1]
    } else if (!is.null(input$import_pick) && nzchar(input$import_pick)) {
      src <- input$import_pick
      label <- basename(src)
    }
    if (is.null(src)) {
      return(div(class = "meta-box muted", "Pick a file or import to see its size before Apply."))
    }
    meta <- read_image_meta(src)
    if (!meta$exists) {
      return(div(class = "meta-box meta-warn", "Selected file not found."))
    }
    div(
      class = "meta-box",
      tags$div(tags$strong("About to assign")),
      tags$div(tags$span(class = "meta-k", "File: "), label),
      tags$div(
        tags$span(class = "meta-k", "Size: "),
        fmt_px(meta$w, meta$h), " · ", fmt_bytes(meta$bytes),
        if (!is.na(meta$format)) paste0(" · ", meta$format) else ""
      )
    )
  })

  output$preview_plot <- renderPlot({
    p <- current_src_path()
    if (!image_path_ok(p) || is.na(rv$S0)) {
      plot.new()
      msg <- if (!image_path_ok(p)) {
        paste0("Missing image\n", current_row()$file_name[[1L]])
      } else {
        "No image for this slot"
      }
      text(0.5, 0.5, msg, col = "white", cex = 1.2)
      return(invisible(NULL))
    }
    ok <- TRUE
    err <- NULL
    mag <- if (is.null(input$mag_pct)) 100 else input$mag_pct
    sx_pct <- if (is.null(input$shift_x_pct)) 0 else input$shift_x_pct
    sy_pct <- if (is.null(input$shift_y_pct)) 0 else input$shift_y_pct
    w <- rv$img_w
    h <- rv$img_h
    S0 <- rv$S0
    shift <- shift_px_from_pct(sx_pct, sy_pct, w, h)
    b <- crop_bounds(w, h, S0, mag, shift$sx, shift$sy)
    im <- tryCatch(
      {
        im0 <- read_edit_image(p, rotate_cw = rv$rot_cw)
        im0 <- image_crop(im0, crop_geometry(b))
        disp <- min(MAX_PREVIEW, max(64L, as.integer(S0)))
        image_resize(im0, sprintf("%dx%d!", disp, disp))
      },
      error = function(e) {
        ok <<- FALSE
        err <<- conditionMessage(e)
        NULL
      }
    )
    par(mar = c(0, 0, 0, 0), bg = "black", pty = "s")
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1, xaxs = "i", yaxs = "i")
    if (is.null(im)) {
      text(0.5, 0.5, paste0("Could not load image\n", err), col = "white", cex = 1.1)
    } else {
      rasterImage(as.raster(im), 0, 0, 1, 1, interpolate = FALSE)
    }
  })

  observeEvent(input$btn_save_crop, {
    row <- current_row()
    i <- rv$idx
    slot <- row$slot[[1L]]
    src <- current_src_path()
    if (is.na(src) || !file.exists(src)) {
      showNotification("Cannot crop: source image missing.", type = "error")
      return(invisible(NULL))
    }
    if (is.na(rv$S0)) {
      showNotification("Cannot crop: image size unknown.", type = "error")
      return(invisible(NULL))
    }
    mag <- if (is.null(input$mag_pct)) 100 else input$mag_pct
    sx_pct <- if (is.null(input$shift_x_pct)) 0 else input$shift_x_pct
    sy_pct <- if (is.null(input$shift_y_pct)) 0 else input$shift_y_pct
    out_px <- suppressWarnings(as.integer(if (is.null(input$crop_out_px)) 0 else input$crop_out_px))
    if (is.na(out_px) || out_px < 0L) out_px <- 0L
    shift <- shift_px_from_pct(sx_pct, sy_pct, rv$img_w, rv$img_h)
    b <- crop_bounds(rv$img_w, rv$img_h, rv$S0, mag, shift$sx, shift$sy)

    out_name <- paste0(slot, ".jpg")
    out_path <- file.path(cropped_dir(), out_name)
    tryCatch(
      {
        write_cropped_copy(src, out_path, b, rotate_cw = rv$rot_cw, out_side_px = out_px)
        rv$slots$mag_pct[i] <- mag
        rv$slots$rotate_cw[i] <- rv$rot_cw
        rv$slots$shift_x_pct[i] <- sx_pct
        rv$slots$shift_y_pct[i] <- sy_pct
        rv$slots$shift_x_px[i] <- shift$sx
        rv$slots$shift_y_px[i] <- shift$sy
        rv$slots$img_w[i] <- rv$img_w
        rv$slots$img_h[i] <- rv$img_h
        rv$slots$S0[i] <- rv$S0
        rv$slots$crop_side_px[i] <- b$width
        rv$slots$crop_left[i] <- b$left
        rv$slots$crop_top[i] <- b$top
        rv$slots$crop_out_px[i] <- out_px
        rv$slots$use_cropped[i] <- TRUE
        meta0 <- read_image_meta(src)
        if (meta0$exists) {
          rv$slots$src_w[i] <- meta0$w
          rv$slots$src_h[i] <- meta0$h
          rv$slots$src_bytes[i] <- meta0$bytes
        }
        persist_slots()
        rv$grid_nonce <- rv$grid_nonce + 1L
        size_note <- if (out_px > 0L) paste0(" at ", out_px, " px") else " (full crop pixels)"
        showNotification(paste0("Saved crop for ", slot, size_note, "."), type = "message")
      },
      error = function(e) {
        showNotification(paste0("Save crop failed: ", conditionMessage(e)), type = "error", duration = NULL)
      }
    )
  })

  # ---- Assign / source choices ----
  refresh_import_choices <- function() {
    files <- list_import_files(file.path(images_dir(), "import"))
    if (length(files) == 0) {
      updateSelectInput(session, "import_pick", choices = c("(none)" = ""), selected = "")
    } else {
      ch <- setNames(files, basename(files))
      updateSelectInput(session, "import_pick", choices = c("(none)" = "", ch))
    }
  }

  observe({
    images_dir()
    refresh_import_choices()
  })
  observeEvent(input$btn_refresh_import, refresh_import_choices())

  output$assign_msg <- renderText(rv$assign_msg)

  output$import_thumbs_ui <- renderUI({
    files <- list_import_files(file.path(images_dir(), "import"))
    if (length(files) == 0) {
      return(div(class = "muted", "Import folder is empty."))
    }
    cells <- lapply(files, function(p) {
      uri <- thumb_data_uri(p, 100L)
      if (is.null(uri)) {
        return(tags$div(basename(p), class = "muted"))
      }
      tags$div(
        style = "display:inline-block; width:100px; margin:4px; vertical-align:top;",
        tags$img(src = uri, style = "width:100%; aspect-ratio:1/1; object-fit:cover; border-radius:4px;"),
        tags$div(basename(p), style = "font-size:10px; word-break:break-all;")
      )
    })
    do.call(tagList, cells)
  })

  observeEvent(input$btn_apply_source, {
    i <- rv$idx
    slot <- rv$slots$slot[i]
    choice <- input$source_choice
    img <- images_dir()
    demos <- file.path(img, "demos")

    tryCatch(
      {
        if (identical(choice, "current")) {
          rv$assign_msg <- "Kept current image."
          return(invisible(NULL))
        }

        if (identical(choice, "restore")) {
          rv$slots <- restore_from_bak(rv$slots, i)
          persist_slots()
          sync_slot_selectors()
          sync_crop_inputs()
          rv$grid_nonce <- rv$grid_nonce + 1L
          rv$assign_msg <- paste0("Restored previous image for ", slot, ".")
          showNotification(rv$assign_msg, type = "message")
          return(invisible(NULL))
        }

        if (identical(choice, "blank")) {
          blank_rel <- "demos/blank.jpg"
          blank_abs <- file.path(demos, "blank.jpg")
          if (!file.exists(blank_abs)) {
            make_blank <- function() {
              im <- image_blank(640, 640, "white")
              image_write(im, blank_abs, format = "jpeg", quality = 90)
            }
            if (!dir.exists(demos)) dir.create(demos, recursive = TRUE)
            make_blank()
          }
          rv$slots <- update_slot_source(rv$slots, i, blank_rel, archive = TRUE, reset_crop = TRUE, images_dir = img)
          persist_slots()
          sync_slot_selectors()
          sync_crop_inputs()
          rv$grid_nonce <- rv$grid_nonce + 1L
          rv$assign_msg <- paste0("Slot ", slot, " set to blank.")
          showNotification(rv$assign_msg, type = "message")
          return(invisible(NULL))
        }

        if (identical(choice, "demo")) {
          demo_rel <- paste0("demos/", slot, ".jpg")
          demo_abs <- file.path(demos, paste0(slot, ".jpg"))
          if (!file.exists(demo_abs)) {
            stop("Demo tile missing for ", slot, ". Run assets/setup.R with demo = TRUE.")
          }
          rv$slots <- update_slot_source(rv$slots, i, demo_rel, archive = TRUE, reset_crop = TRUE, images_dir = img)
          persist_slots()
          sync_slot_selectors()
          sync_crop_inputs()
          rv$grid_nonce <- rv$grid_nonce + 1L
          rv$assign_msg <- paste0("Slot ", slot, " set to demo tile.")
          showNotification(rv$assign_msg, type = "message")
          return(invisible(NULL))
        }

        if (identical(choice, "new")) {
          src <- NULL
          if (!is.null(input$new_file) && nzchar(input$new_file$datapath)) {
            src <- input$new_file$datapath
          } else if (!is.null(input$import_pick) && nzchar(input$import_pick)) {
            src <- input$import_pick
          }
          if (is.null(src) || !file.exists(src)) {
            stop("Pick a file or choose one from the import folder.")
          }
          label <- if (is.null(input$new_label)) "" else input$new_label
          rel <- copy_into_images(
            src, img, slot, label = label,
            overwrite = isTRUE(input$overwrite_img)
          )
          rv$slots <- update_slot_source(rv$slots, i, rel, archive = TRUE, reset_crop = TRUE, images_dir = img)
          persist_slots()
          sync_slot_selectors()
          sync_crop_inputs()
          rv$grid_nonce <- rv$grid_nonce + 1L
          rv$assign_msg <- paste0("Assigned ", rel, " to slot ", slot, " (previous saved to backup).")
          showNotification(rv$assign_msg, type = "message")
          return(invisible(NULL))
        }
      },
      error = function(e) {
        rv$assign_msg <- conditionMessage(e)
        showNotification(rv$assign_msg, type = "error", duration = NULL)
      }
    )
  })

  # ---- Layout / images path ----
  observeEvent(input$btn_save_layout, {
    rv$profile$rows <- as.integer(input$grid_rows)
    rv$profile$cols <- as.integer(input$grid_cols)
    rv$profile$spacing_mm <- as.numeric(input$spacing_mm)
    rv$profile$dpi <- as.integer(input$dpi)
    rv$profile$paper <- input$paper
    rv$profile$orientation <- input$orientation
    persist_profile()
    showNotification("Layout saved to data/profile.csv", type = "message")
  })

  observeEvent(input$btn_save_images_dir, {
    p <- trimws(input$images_dir_txt)
    if (!nzchar(p)) {
      showNotification("Path is empty.", type = "error")
      return(invisible(NULL))
    }
    if (!dir.exists(p)) {
      showNotification("Folder does not exist.", type = "error")
      return(invisible(NULL))
    }
    rv$profile$images_dir <- canonical_fs_path(p)
    for (d in c("demos", "import")) {
      dd <- file.path(rv$profile$images_dir, d)
      if (!dir.exists(dd)) dir.create(dd, recursive = TRUE, showWarnings = FALSE)
    }
    persist_profile()
    rv$grid_nonce <- rv$grid_nonce + 1L
    showNotification("Images folder saved.", type = "message")
  })

  # ---- Poster grid + generate ----
  output$poster_grid_ui <- renderUI({
    rv$grid_nonce
    rows <- max(1L, as.integer(if (is.null(input$grid_rows)) rv$profile$rows else input$grid_rows))
    cols <- max(1L, as.integer(if (is.null(input$grid_cols)) rv$profile$cols else input$grid_cols))
    n <- min(nrow(rv$slots), rows * cols)
    # Cap preview thumbs for UI speed: show smaller grid if huge
    thumb <- if (n > 120) 48L else if (n > 60) 64L else 80L

    cells <- lapply(seq_len(rows * cols), function(i) {
      if (i > nrow(rv$slots)) {
        return(div(class = "empty-cell"))
      }
      row <- rv$slots[i, , drop = FALSE]
      slot <- row$slot[[1L]]
      p <- slot_render_path(row, images_dir(), cropped_dir())
      uri <- if (!is.na(p) && file.exists(p)) thumb_data_uri(p, thumb) else NULL
      onclick <- sprintf(
        "Shiny.setInputValue('grid_pick_slot', '%s', {priority:'event'});",
        slot
      )
      if (is.null(uri)) {
        tags$div(
          class = "cell-wrap",
          div(class = "empty-cell", onclick = onclick, title = slot),
          span(class = "cell-label", slot)
        )
      } else {
        tags$div(
          class = "cell-wrap",
          tags$img(src = uri, title = slot, alt = slot, onclick = onclick),
          span(class = "cell-label", slot)
        )
      }
    })
    div(
      class = "poster-grid",
      style = sprintf("grid-template-columns: repeat(%d, 1fr);", cols),
      cells
    )
  })

  output$poster_msg <- renderText(rv$poster_msg)
  output$smart_load_msg <- renderText(rv$smart_load_msg)

  observeEvent(input$btn_smart_load_help, {
    showModal(modalDialog(
      title = "Smart load",
      easyClose = TRUE,
      footer = modalButton("Got it"),
      tags$p(
        "Smart load gives you a head start when photos are already named for their slots."
      ),
      tags$ol(
        tags$li("It only looks at slots still on the ", tags$strong("default fill"),
                " (anything under ", tags$code("demos/"), ", including blank)."),
        tags$li("In ", tags$code("images/"), " and ", tags$code("images/import/"),
                ", it looks for filenames that start with that slot id — e.g. slot ",
                tags$code("1205"), " matches ", tags$code("1205.jpg"), " or ",
                tags$code("1205_vacation.jpg"), "."),
        tags$li(tags$strong("Exactly one match"), " → that image is assigned (import files are copied into ",
                tags$code("images/"), ")."),
        tags$li(tags$strong("No match"), " → slot unchanged."),
        tags$li(tags$strong("Several matches"), " → slot unchanged (ambiguous); reported in the status message.")
      ),
      tags$p(
        class = "muted",
        "Slots you already assigned manually are never overwritten. Safe to run more than once."
      )
    ))
  })

  observeEvent(input$btn_smart_load, {
    tryCatch(
      {
        res <- smart_load_slots(rv$slots, images_dir())
        rv$slots <- res$slots
        persist_slots()
        sync_slot_selectors()
        if (nrow(rv$slots) >= 1L) sync_crop_inputs()
        rv$grid_nonce <- rv$grid_nonce + 1L

        msg <- sprintf(
          "Smart load: %d assigned · %d demo slots with no match · %d ambiguous (skipped).",
          res$n_assigned, res$n_skipped_none, res$n_skipped_multi
        )
        if (res$n_skipped_multi > 0 && length(res$ambiguous) > 0) {
          amb <- names(res$ambiguous)
          if (length(amb) > 8) amb <- c(head(amb, 8), "…")
          msg <- paste0(msg, " Ambiguous slots: ", paste(amb, collapse = ", "), ".")
        }
        if (res$n_assigned == 0L && res$n_skipped_multi == 0L) {
          msg <- paste0(
            msg,
            " Tip: put files like 1205.jpg in images/ (or import/), then try again."
          )
        }
        rv$smart_load_msg <- msg
        showNotification(msg, type = if (res$n_assigned > 0) "message" else "warning", duration = 10)
      },
      error = function(e) {
        rv$smart_load_msg <- conditionMessage(e)
        showNotification(rv$smart_load_msg, type = "error", duration = NULL)
      }
    )
  })

  observeEvent(input$btn_make_poster, {
    rows <- max(1L, as.integer(input$grid_rows))
    cols <- max(1L, as.integer(input$grid_cols))
    withProgress(message = "Building poster…", value = 0, {
      tryCatch(
        {
          res <- write_poster(
            rv$slots,
            images_dir(),
            cropped_dir(),
            rv$layout$output,
            rows = rows,
            cols = cols,
            spacing_mm = as.numeric(input$spacing_mm),
            dpi = as.integer(input$dpi),
            paper = input$paper,
            orientation = input$orientation,
            export_pdf = isTRUE(input$chk_pdf),
            progress = function(i, n) {
              setProgress(value = i / n, detail = paste(i, "/", n))
            }
          )
          msg <- paste0("Saved ", basename(res$jpg))
          if (!is.na(res$pdf) && nzchar(res$pdf)) msg <- paste0(msg, " and ", basename(res$pdf))
          out_label <- if (isTRUE(rv$layout$is_archive)) {
            paste0("archive/", rv$layout$archive_name, "/output/")
          } else {
            "output/"
          }
          msg <- paste0(msg, " in ", out_label)
          if (length(res$missing_slots) > 0) {
            miss <- res$missing_slots
            if (length(miss) > 12) {
              miss_txt <- paste0(paste(head(miss, 12), collapse = ", "), " … +", length(miss) - 12, " more")
            } else {
              miss_txt <- paste(miss, collapse = ", ")
            }
            msg <- paste0(
              msg, " — ", length(res$missing_slots),
              " slot(s) skipped (missing image/crop): ", miss_txt
            )
            showNotification(
              paste0(length(res$missing_slots), " slot(s) had missing files and were left blank."),
              type = "warning",
              duration = 10
            )
          }
          rv$poster_msg <- msg
          showNotification(msg, type = "message", duration = 8)
        },
        error = function(e) {
          rv$poster_msg <- conditionMessage(e)
          showNotification(rv$poster_msg, type = "error", duration = NULL)
        }
      )
    })
  })

  # Initial crop sync
  isolate({
    if (nrow(rv$slots) >= 1L) sync_crop_inputs()
  })

  # ---- Setup / archive (danger zone) ----
  reload_live_state <- function(archive_name = NULL) {
    st <- load_state(app_root, archive_name = archive_name)
    rv$layout <- st$layout
    rv$profile <- st$profile
    rv$slots <- st$slots
    rv$idx <- 1L
    rv$rot_cw <- 0L
    rv$grid_nonce <- rv$grid_nonce + 1L
    rv$archive_nonce <- rv$archive_nonce + 1L
    sync_slot_selectors()
    if (nrow(rv$slots) >= 1L) sync_crop_inputs()
    updateNumericInput(session, "grid_rows", value = rv$profile$rows)
    updateNumericInput(session, "grid_cols", value = rv$profile$cols)
    updateNumericInput(session, "spacing_mm", value = rv$profile$spacing_mm)
    updateNumericInput(session, "dpi", value = rv$profile$dpi)
    updateSelectInput(session, "paper", selected = rv$profile$paper)
    updateSelectInput(session, "orientation", selected = rv$profile$orientation)
    updateNumericInput(session, "setup_rows", value = rv$profile$rows)
    updateNumericInput(session, "setup_cols", value = rv$profile$cols)
    updateNumericInput(session, "setup_spacing", value = rv$profile$spacing_mm)
    updateNumericInput(session, "setup_dpi", value = rv$profile$dpi)
    updateSelectInput(session, "setup_paper", selected = rv$profile$paper)
    updateSelectInput(session, "setup_orientation", selected = rv$profile$orientation)
    updateSelectInput(session, "setup_scheme", selected = rv$profile$slot_scheme)
    updateTextInput(session, "setup_start_yymm", value = rv$profile$start_yymm)
    updateTextInput(session, "setup_end_yymm", value = rv$profile$end_yymm)
  }

  refresh_archive_choices <- function() {
    names <- list_archives(app_root)
    if (length(names) == 0) {
      updateSelectInput(session, "archive_pick", choices = c("(none)" = ""), selected = "")
    } else {
      labels <- vapply(names, function(nm) {
        format_archive_summary_line(archive_summary(nm, app_root))
      }, character(1))
      updateSelectInput(session, "archive_pick", choices = c("(none)" = "", setNames(names, labels)))
    }
  }

  observe({
    rv$archive_nonce
    refresh_archive_choices()
  })
  observeEvent(input$btn_refresh_archives, {
    rv$archive_nonce <- rv$archive_nonce + 1L
    refresh_archive_choices()
  })

  output$archive_list_ui <- renderUI({
    rv$archive_nonce
    names <- list_archives(app_root)
    if (length(names) == 0) {
      return(div(class = "muted", "No archives yet. Save the current project below → ", tags$code("archive/<name>/"), "."))
    }
    rows <- lapply(names, function(nm) {
      s <- archive_summary(nm, app_root)
      tags$li(format_archive_summary_line(s))
    })
    tagList(
      tags$strong("Saved projects"),
      tags$ul(style = "margin-top:6px;", rows)
    )
  })

  # Auto rows/cols from YYMM range (e.g. 10 years → 10×12)
  observeEvent(
    list(input$setup_start_yymm, input$setup_end_yymm, input$setup_scheme, input$setup_auto_rows),
    {
      if (!isTRUE(input$setup_auto_rows)) return(invisible(NULL))
      if (!identical(input$setup_scheme, "yymm")) return(invisible(NULL))
      start <- trimws(as.character(input$setup_start_yymm))
      end <- trimws(as.character(input$setup_end_yymm))
      prefs <- tryCatch(month_prefixes(start, end), error = function(e) NULL)
      if (is.null(prefs) || length(prefs) < 1L) return(invisible(NULL))
      if (length(prefs) %% 12L == 0L) {
        updateNumericInput(session, "setup_rows", value = as.integer(length(prefs) / 12L))
        updateNumericInput(session, "setup_cols", value = 12L)
      }
    },
    ignoreInit = TRUE
  )

  output$setup_preview_ids <- renderUI({
    scheme <- input$setup_scheme
    rows <- as.integer(input$setup_rows)
    cols <- as.integer(input$setup_cols)
    start <- trimws(as.character(input$setup_start_yymm))
    end <- trimws(as.character(input$setup_end_yymm))
    ids <- tryCatch(
      make_slot_ids(scheme, rows, cols, start, end),
      error = function(e) e$message
    )
    if (is.character(ids) && length(ids) == 1L && !grepl("^[0-9r]", ids[1])) {
      return(div(class = "meta-box meta-warn", ids))
    }
    show <- ids
    if (length(show) > 24L) {
      show <- c(head(show, 12L), "…", tail(show, 8L))
    }
    div(
      class = "meta-box",
      tags$div(tags$strong(sprintf("%d slots", length(ids))),
               sprintf(" (%d × %d)", rows, cols)),
      tags$div(class = "muted", paste(show, collapse = ", "))
    )
  })

  output$setup_restore_msg <- renderText(rv$setup_restore_msg)
  output$setup_reshape_msg <- renderText(rv$setup_reshape_msg)
  output$archive_save_msg <- renderText(rv$archive_save_msg)
  output$archive_load_msg <- renderText(rv$archive_load_msg)

  output$archive_detail_ui <- renderUI({
    rv$archive_nonce
    nm <- input$archive_pick
    if (is.null(nm) || !nzchar(nm)) {
      return(div(class = "muted", "Pick an archive for details."))
    }
    s <- tryCatch(archive_summary(nm, app_root), error = function(e) NULL)
    if (is.null(s)) return(div(class = "meta-warn", "Could not read archive."))
    div(
      class = "meta-box",
      tags$div(tags$strong(s$name)),
      tags$div(class = "muted", "Created: ", if (is.na(s$created)) "?" else s$created),
      tags$div(class = "muted",
               sprintf("Grid %s · %s slots · %s",
                       if (!is.na(s$rows)) paste0(s$rows, "×", s$cols) else "?",
                       if (!is.na(s$n_slots)) s$n_slots else "?",
                       if (!is.null(s$slot_scheme) && !is.na(s$slot_scheme)) s$slot_scheme else "?")),
      tags$div(class = "muted", "Cropped files: ", s$n_cropped,
               if (isTRUE(s$has_poster)) " · poster present" else " · no poster yet"),
      tags$div(class = "muted", tags$code(paste0("archive/", s$name, "/"))),
      if (nzchar(s$notes)) tags$div(class = "muted", "Notes: ", s$notes)
    )
  })

  observeEvent(input$btn_restore_defaults, {
    showModal(modalDialog(
      title = "Restore defaults?",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_restore_defaults_ok", "Yes, reset everything", class = "btn-danger")
      ),
      tags$p("This replaces live ", tags$code("data/"), " with the starter 19×12 / 0801–2612 layout, clears ",
             tags$code("cropped/"), ", and recreates demos."),
      tags$p(tags$strong("Photos in images/ are not deleted."),
             " Archive first if you need the current project.")
    ))
  })

  observeEvent(input$btn_restore_defaults_ok, {
    removeModal()
    withProgress(message = "Restoring defaults…", value = 0.3, {
      tryCatch(
        {
          restore_collage_defaults(root = app_root, demo = TRUE)
          setProgress(0.8)
          reload_live_state(NULL)
          rv$setup_restore_msg <- "Restored live defaults (19×12, 0801–2612) and rebuilt demos."
          showNotification(rv$setup_restore_msg, type = "message")
        },
        error = function(e) {
          rv$setup_restore_msg <- conditionMessage(e)
          showNotification(rv$setup_restore_msg, type = "error", duration = NULL)
        }
      )
    })
  })

  observeEvent(input$btn_reshape_grid, {
    rows <- as.integer(input$setup_rows)
    cols <- as.integer(input$setup_cols)
    scheme <- input$setup_scheme
    start <- trimws(as.character(input$setup_start_yymm))
    end <- trimws(as.character(input$setup_end_yymm))
    n <- rows * cols
    showModal(modalDialog(
      title = "Reshape grid?",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_reshape_grid_ok", "Yes, reshape and reset data", class = "btn-danger")
      ),
      tags$p(sprintf("New grid: %d × %d (%d slots), scheme “%s”.", rows, cols, n, scheme)),
      if (identical(scheme, "yymm")) tags$p(sprintf("Year–month range: %s … %s", start, end)),
      tags$p(tags$strong("Applies to the live workspace."), " Archive first if needed.")
    ))
  })

  observeEvent(input$btn_reshape_grid_ok, {
    removeModal()
    withProgress(message = "Reshaping grid…", value = 0.2, {
      tryCatch(
        {
          # Reshape always targets live workspace
          reshape_collage(
            root = app_root,
            rows = as.integer(input$setup_rows),
            cols = as.integer(input$setup_cols),
            spacing_mm = as.numeric(input$setup_spacing),
            paper = input$setup_paper,
            orientation = input$setup_orientation,
            dpi = as.integer(input$setup_dpi),
            scheme = input$setup_scheme,
            start_yymm = trimws(as.character(input$setup_start_yymm)),
            end_yymm = trimws(as.character(input$setup_end_yymm)),
            demo = isTRUE(input$setup_demo),
            clear_cropped = isTRUE(input$setup_clear_cropped)
          )
          setProgress(0.85)
          reload_live_state(NULL)
          rv$setup_reshape_msg <- sprintf(
            "Reshaped live workspace to %d×%d (%s). %d slots.",
            rv$profile$rows, rv$profile$cols, rv$profile$slot_scheme, nrow(rv$slots)
          )
          showNotification(rv$setup_reshape_msg, type = "message")
          updateTabsetPanel(session, "tabs", selected = "Poster")
        },
        error = function(e) {
          rv$setup_reshape_msg <- conditionMessage(e)
          showNotification(rv$setup_reshape_msg, type = "error", duration = NULL)
        }
      )
    })
  })

  observeEvent(input$btn_save_archive, {
    nm <- trimws(as.character(input$archive_name))
    if (!nzchar(nm)) {
      showNotification("Enter an archive name.", type = "error")
      return(invisible(NULL))
    }
    tryCatch(
      {
        persist_slots()
        persist_profile()
        # Always snapshot from live layout paths — if currently in an archive, save that archive's files under a new name via copying layout dirs
        res <- if (isTRUE(rv$layout$is_archive)) {
          # Copy current archive workspace to a new archive name
          src_lay <- rv$layout
          dest <- archive_dir(nm, app_root)
          if (dir.exists(dest)) stop("Archive already exists: ", nm, call. = FALSE)
          ensure_archive_dirs(dest)
          file.copy(src_lay$profile, file.path(dest, "data", "profile.csv"), overwrite = TRUE)
          file.copy(src_lay$slots, file.path(dest, "data", "slots.csv"), overwrite = TRUE)
          n_crop <- if (isTRUE(input$archive_include_cropped)) {
            copy_dir_images(src_lay$cropped, file.path(dest, "cropped"))
          } else {
            0L
          }
          n_out <- if (isTRUE(input$archive_include_output)) {
            copy_dir_images(src_lay$output, file.path(dest, "output"))
          } else {
            0L
          }
          man <- data.frame(
            name = sanitize_archive_name(nm),
            created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            notes = as.character(input$archive_notes),
            includes_cropped = isTRUE(input$archive_include_cropped),
            n_cropped_files = n_crop,
            n_output_files = n_out,
            n_slots = nrow(rv$slots),
            rows = rv$profile$rows,
            cols = rv$profile$cols,
            slot_scheme = rv$profile$slot_scheme,
            has_poster = file.exists(file.path(dest, "output", "poster.jpg")),
            stringsAsFactors = FALSE
          )
          write.csv(man, file.path(dest, "manifest.csv"), row.names = FALSE)
          list(path = dest, name = sanitize_archive_name(nm), n_cropped = n_crop, n_output = n_out)
        } else {
          save_archive(
            name = nm,
            root = app_root,
            include_cropped = isTRUE(input$archive_include_cropped),
            include_output = isTRUE(input$archive_include_output),
            notes = input$archive_notes
          )
        }
        rv$archive_save_msg <- sprintf(
          "Saved archive/%s/ (crops %d, output files %d).",
          res$name, res$n_cropped, if (is.null(res$n_output)) 0L else res$n_output
        )
        rv$archive_nonce <- rv$archive_nonce + 1L
        refresh_archive_choices()
        updateSelectInput(session, "archive_pick", selected = res$name)
        updateTextInput(session, "archive_name", value = "")
        showNotification(rv$archive_save_msg, type = "message")
      },
      error = function(e) {
        rv$archive_save_msg <- conditionMessage(e)
        showNotification(rv$archive_save_msg, type = "error", duration = NULL)
      }
    )
  })

  observeEvent(input$btn_open_archive, {
    nm <- input$archive_pick
    if (is.null(nm) || !nzchar(nm)) {
      showNotification("Pick an archive to open.", type = "error")
      return(invisible(NULL))
    }
    tryCatch(
      {
        reload_live_state(nm)
        if (identical(input$archive_missing_demos, "regenerate")) {
          ensure_demos_for_slots(rv$slots, rv$layout)
        }
        rv$archive_load_msg <- paste0("Opened archive/", nm, "/ (edit in place).")
        showNotification(rv$archive_load_msg, type = "message")
        updateTabsetPanel(session, "tabs", selected = "Poster")
      },
      error = function(e) {
        rv$archive_load_msg <- conditionMessage(e)
        showNotification(rv$archive_load_msg, type = "error", duration = NULL)
      }
    )
  })

  observeEvent(input$btn_use_live, {
    tryCatch(
      {
        reload_live_state(NULL)
        rv$archive_load_msg <- "Back on live workspace (data/, cropped/, output/)."
        showNotification(rv$archive_load_msg, type = "message")
      },
      error = function(e) {
        rv$archive_load_msg <- conditionMessage(e)
        showNotification(rv$archive_load_msg, type = "error", duration = NULL)
      }
    )
  })

  observeEvent(input$btn_restore_archive, {
    nm <- input$archive_pick
    if (is.null(nm) || !nzchar(nm)) {
      showNotification("Pick an archive to restore.", type = "error")
      return(invisible(NULL))
    }
    showModal(modalDialog(
      title = paste0("Restore “", nm, "” into live workspace?"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_restore_archive_ok", "Yes, replace live data", class = "btn-warning")
      ),
      tags$p("Copies this archive into live ", tags$code("data/"),
             " (and optionally cropped/output). Current live project is overwritten."),
      tags$p("Photos in ", tags$code("images/"), " stay put.")
    ))
  })

  observeEvent(input$btn_restore_archive_ok, {
    removeModal()
    nm <- input$archive_pick
    withProgress(message = "Restoring archive…", value = 0.3, {
      tryCatch(
        {
          res <- restore_archive(
            name = nm,
            root = app_root,
            restore_cropped = isTRUE(input$archive_restore_cropped),
            restore_output = isTRUE(input$archive_restore_output),
            missing_demos = input$archive_missing_demos
          )
          setProgress(0.8)
          reload_live_state(NULL)
          rv$archive_load_msg <- paste0(
            "Restored “", res$name, "” into live workspace.",
            if (length(res$demos_made) > 0) paste0(" Regenerated ", length(res$demos_made), " demo(s).") else ""
          )
          showNotification(rv$archive_load_msg, type = "message")
          updateTabsetPanel(session, "tabs", selected = "Poster")
        },
        error = function(e) {
          rv$archive_load_msg <- conditionMessage(e)
          showNotification(rv$archive_load_msg, type = "error", duration = NULL)
        }
      )
    })
  })

  observeEvent(input$btn_delete_archive, {
    nm <- input$archive_pick
    if (is.null(nm) || !nzchar(nm)) {
      showNotification("Pick an archive to delete.", type = "error")
      return(invisible(NULL))
    }
    showModal(modalDialog(
      title = paste0("Delete archive “", nm, "”?"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_delete_archive_ok", "Yes, delete permanently", class = "btn-danger")
      ),
      tags$p("Removes ", tags$code(paste0("archive/", nm, "/")), " including its data, crops, and output."),
      tags$p("Does not delete photos under ", tags$code("images/"), ".")
    ))
  })

  observeEvent(input$btn_delete_archive_ok, {
    removeModal()
    nm <- input$archive_pick
    tryCatch(
      {
        if (isTRUE(rv$layout$is_archive) && identical(rv$layout$archive_name, sanitize_archive_name(nm))) {
          reload_live_state(NULL)
        }
        delete_archive(nm, root = app_root)
        rv$archive_nonce <- rv$archive_nonce + 1L
        refresh_archive_choices()
        rv$archive_load_msg <- paste0("Deleted archive/", nm, "/")
        showNotification(rv$archive_load_msg, type = "message")
      },
      error = function(e) {
        rv$archive_load_msg <- conditionMessage(e)
        showNotification(rv$archive_load_msg, type = "error", duration = NULL)
      }
    )
  })
}

shinyApp(ui, server)
