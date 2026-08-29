# Collage

Build a **month-by-month photo poster** from a folder of pictures. Assign each photo to a calendar slot, crop it to a square, then generate a print-ready poster.

**Full guide (with TOC):** <https://macartan.github.io/collage/>

Good for family timelines, project archives, “one photo per month” walls — anything where the story is a grid over time.

---

## What you get

- A simple **Shiny app** to assign photos, crop them, and generate a poster into `output/`
- Optional **`poster.qmd`** if you want to fine-tune DPI, gaps, labels, or re-render without opening the app
- Safe **demo tiles** in `images/demos/` so you can try the workflow before using real photos

---

## Requirements

- [R](https://cran.r-project.org/) (4.x recommended)
- Packages:

```r
install.packages(c("shiny", "magick", "base64enc"))
```

For `poster.qmd` you also want [Quarto](https://quarto.org/) (or Knit from RStudio).

---

## First open (shared copy)

If this folder already has `images/demos/` and `data/`, you can skip setup and go straight to the app.

If demos or data are missing:

```r
setwd("...")  # this collage folder
source("assets/setup.R")
setup_collage(demo = TRUE)
```

---

## Walkthrough

For the longer guide (Smart load, Setup, archives, folder map), see the [live site](https://macartan.github.io/collage/). Short version:

### 1. Put photos in

Drop files into `images/import/` (any names), or name them for slots (`1205.jpg` / `1205_vacation.jpg`) in `images/` and use **Smart load** on the Poster tab.

### 2. Open the app

In RStudio: open `app.R` → **Run App**  
Or in R:

```r
shiny::runApp(".")
```

Start on the **Poster** tab. A short greeting pops up once; **Help** (last tab) has the walkthrough anytime.

### 3. Assign & crop

Open **Edit slot**. Pick a month (e.g. `1205` = May 2012). At the top, choose what goes in the slot; below, crop it.

| Choice | What it does |
|--------|----------------|
| Keep current | Leave the mapped image as-is |
| Blank | White placeholder |
| Demo | Color demo tile for that month |
| Choose new image… | Copy a photo into `images/` as `1205.jpg` or `1205_vacation.jpg` |
| Restore previous | Undo the last replace (one step) |

Choosing a new image stores the previous mapping in backup fields so Restore works once. Then use **Crop** on the same tab (zoom / pan / rotate → **Save crop**). Clicking a cell on the Poster grid opens that slot here.

### 4. Make the poster

**Option A — in the app**  
Poster tab → **Generate poster** → find `output/poster.jpg`.

**Option B — fine-tune separately**  
Edit the options at the top of `poster.qmd` (DPI, gap, labels, …), then render it. Output still goes to `output/`. Use this when you want print control without the Shiny UI.

### 5. Setup, year range, and archives

On the **Setup** tab (danger zone):

- **Restore defaults** — back to 19×12 / `0801`–`2612` with fresh demos  
- **Reshape grid** — change rows/cols, gap, paper, and slot naming. For a **10-year** calendar: start `1501`, end `2412`, leave auto rows/cols on (becomes 10×12), recreate demos  
- **Archive** — projects live under `archive/<name>/` with `data/`, `cropped/`, and `output/` (photos in `images/` are shared, not copied). From Setup you can save, open in place, restore into live, or delete.

Reshape and restore **replace** live slot data — archive first if you care about the current project.

---

## Folder map

| Path | Role |
|------|------|
| `app.R` | Main app — open this |
| `poster.qmd` | Optional high-control poster render |
| `images/demos/` | Demo tiles + `blank.jpg` (app never overwrites these) |
| `images/import/` | Drop zone for new photos |
| `images/` | Your assigned photos (`1205_label.jpg`) |
| `cropped/` | Square crops used when a slot is marked cropped |
| `data/profile.csv` | Images folder path + layout (rows, cols, paper, dpi) |
| `data/slots.csv` | Slot ↔ file map, crop settings, one backup |
| `archive/` | Saved projects |
| `output/` | Generated poster |

You can point the app at another images folder on the Poster tab; that path is saved in the profile.

Image sizes for each **assigned** slot live in `data/slots.csv` (`src_w` / `src_h` / `src_bytes`). There is no separate image catalogue — import-folder files are measured on demand when you select them.

---

## Sharing this project

This folder is meant to be its own git repository. Typical flow:

1. Clone or copy `collage/`
2. Install R packages (above)
3. Run the app; replace demos with real photos via Assign
4. Commit *your* `data/` and `images/` if you want versioned layouts (keep large originals out of git if needed — see `.gitignore`)

Demo JPEGs are small and included so a fresh clone works immediately.

Docs: <https://macartan.github.io/collage/>
