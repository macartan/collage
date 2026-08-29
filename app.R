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

for (f in c("paths.R", "slots.R", "crop.R", "images.R", "poster.R")) {
  source(file.path(app_root, "R", f), local = FALSE)
}

layout0 <- default_layout(app_root)
ensure_project_dirs(layout0)

MAX_PREVIEW <- 900L

load_state <- function(root = app_root) {
  lay <- default_layout(root)
  ensure_project_dirs(lay)
  if (!file.exists(lay$profile) || !file.exists(lay$slots)) {
    # Auto-bootstrap data (no demos) so the app always opens
    source(file.path(root, "assets", "setup.R"), local = TRUE)
    setup_collage(root = root, demo = FALSE)
  }
  profile <- read_profile(lay$profile, root = root)
  # Ensure images subdirs under configured images_dir
  img <- profile$images_dir
  for (d in c(img, file.path(img, "demos"), file.path(img, "import"))) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  slots <- read_slots(lay$slots, profile$start_yymm, profile$end_yymm)
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
            textOutput("poster_msg")
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
          tags$li(tags$strong("Add photos"), " — drop files into ", tags$code("images/import/"),
                  ", or pick any file when you assign a slot."),
          tags$li(tags$strong("Edit slot"), " — choose a month, set the image (current / blank / demo / new / restore), then zoom, pan, rotate and Save crop."),
          tags$li(tags$strong("Poster"), " — generate from the Poster tab, or render ", tags$code("poster.qmd"), " separately.")
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
          tags$li("Demo tiles in ", tags$code("images/demos/"), " are safe placeholders — the app never overwrites them."),
          tags$li("New photos are copied into ", tags$code("images/"), " as names like ",
                  tags$code("1205.jpg"), " or ", tags$code("1205_vacation.jpg"), "."),
          tags$li("Choosing a new image keeps one backup so you can Restore once."),
          tags$li("Click any cell on the Poster grid to jump to Edit slot for that month.")
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
    grid_nonce = 0L
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
    div(
      class = "status-line",
      sprintf(
        "Slots %d · files present %d · using demos %d · blank %d · cropped %d",
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
    if (is.na(p) || !file.exists(p) || is.na(rv$S0)) {
      plot.new()
      text(0.5, 0.5, "No image for this slot", col = "white", cex = 1.4)
      return(invisible(NULL))
    }
    mag <- if (is.null(input$mag_pct)) 100 else input$mag_pct
    sx_pct <- if (is.null(input$shift_x_pct)) 0 else input$shift_x_pct
    sy_pct <- if (is.null(input$shift_y_pct)) 0 else input$shift_y_pct
    w <- rv$img_w
    h <- rv$img_h
    S0 <- rv$S0
    shift <- shift_px_from_pct(sx_pct, sy_pct, w, h)
    b <- crop_bounds(w, h, S0, mag, shift$sx, shift$sy)
    im <- read_edit_image(p, rotate_cw = rv$rot_cw)
    im <- image_crop(im, crop_geometry(b))
    disp <- min(MAX_PREVIEW, max(64L, as.integer(S0)))
    im <- image_resize(im, sprintf("%dx%d!", disp, disp))
    par(mar = c(0, 0, 0, 0), bg = "black", pty = "s")
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1, xaxs = "i", yaxs = "i")
    rasterImage(as.raster(im), 0, 0, 1, 1, interpolate = FALSE)
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
    shift <- shift_px_from_pct(sx_pct, sy_pct, rv$img_w, rv$img_h)
    b <- crop_bounds(rv$img_w, rv$img_h, rv$S0, mag, shift$sx, shift$sy)

    out_name <- paste0(slot, ".jpg")
    out_path <- file.path(cropped_dir(), out_name)
    tryCatch(
      {
        write_cropped_copy(src, out_path, b, rotate_cw = rv$rot_cw)
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
        rv$slots$use_cropped[i] <- TRUE
        meta0 <- read_image_meta(src)
        if (meta0$exists) {
          rv$slots$src_w[i] <- meta0$w
          rv$slots$src_h[i] <- meta0$h
          rv$slots$src_bytes[i] <- meta0$bytes
        }
        persist_slots()
        rv$grid_nonce <- rv$grid_nonce + 1L
        showNotification(paste0("Saved crop for ", slot, "."), type = "message")
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
          msg <- paste0(msg, " in output/")
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
}

shinyApp(ui, server)
