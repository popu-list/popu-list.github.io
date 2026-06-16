# ==========================================================
# 1. SETUP: Load Packages and Data
# ==========================================================
library(tidyverse)
library(reactable)
library(gt)

populist_cleaned <- read_csv("Data/processed_populist.csv")

populist_cleaned <- populist_cleaned |> 
  mutate(across(everything(), ~replace_na(., "")))
  

# ==========================================================
# 6. PDF TABLE
# ==========================================================

populist_cleaned |> 
  gt(groupname_col = "Country") |> 
  tab_header(
    title = md("<img src='/Users/lukefischer/Dropbox/The PopuList Repo/Visualizations/dashboard/images/logo_narrow.jpeg' style='height:30px;'> The PopuList, Version 4.0 (May 2026)")
  ) |> 
  tab_source_note(
    source_note = md("*Note.* ●: Characteristic met; ◐: Borderline case")
  ) |> 
  opt_css(
    css = "
    @page {
      size: A4 landscape;
      margin: 1cm;
    }
    * {
      -webkit-print-color-adjust: exact !important;
      print-color-adjust: exact !important;
    }
    thead {
      display: table-header-group;
    }
    tr {
      page-break-inside: avoid;
    }
    "
  ) |>
  tab_options(table.width = pct(100)) |> 
  tab_style(
    style = list(
      cell_text(
        weight = "bold", 
        size = px(24) 
      )
    ),
    locations = cells_title()) |> 
  tab_style(
    style = list(
      cell_fill(color = "#E8E8E8"),
      cell_text(color = "#363636", weight = "bold")
    ),
    locations = cells_row_groups()
  ) |> 
  tab_style(
    style = list(
      cell_fill(color = "#787276"),
      cell_text(color = "white", weight = "bold")
    ),
    locations = cells_column_labels()
  ) |> 
  cols_width(
    "Eurosceptic" ~ px(120),
    "In Parliament" ~ px(120),
    "Far-Left" ~ px(120),
    "Far-Right" ~ px(120),
    "Populist" ~ px(120)
  ) |> 
  fmt_markdown(columns = everything()) |>
  opt_align_table_header(align = "left") |>
  opt_table_font(
    font = list(
      google_font(name = "Lato")), 
    size = px(12)
  ) |> 
  gtsave(
    "Visualizations/table/table.pdf"
  )
