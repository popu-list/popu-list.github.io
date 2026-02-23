# ==========================================================
# 1. LOAD PACKAGES AND SPATIAL DATA
# ==========================================================
library(eurostat)
library(sf)
library(tidyverse)
library(ggplot2)
library(ggiraph)

# Fetch European geospatial data (NUTS 0 - Country Level)
european <- get_eurostat_geospatial(resolution = 20, 
                                    nuts_level = 0, 
                                    year = 2016)

european <- european |> 
  select(id, geometry)

view(european)

# ==========================================================
# 2. DATA PREPARATION: Populist Votes
# ==========================================================
populist_votes <- P |> 
  filter(year == 2022) |> 
  select(country, c2_populist_votes) 

# Map country names to Eurostat ID codes
populist_votes <- populist_votes |> mutate(
  id = case_when(
    country == "Austria" ~ "AT",
    country == "Belgium" ~ "BE",
    country == "Bulgaria" ~ "BG",
    country == "Croatia" ~ "HR",
    country == "Cyprus" ~ "CY",
    country == "Czech_Republic" ~ "CZ",
    country == "Denmark" ~ "DK",
    country == "Estonia" ~ "EE",
    country == "Finland" ~ "FI",
    country == "France" ~ "FR",
    country == "Germany" ~ "DE",
    country == "Greece" ~ "GR",
    country == "Hungary" ~ "HU",
    country == "Iceland" ~ "IS",
    country == "Ireland" ~ "IE",
    country == "Italy" ~ "IT",
    country == "Latvia" ~ "LV",
    country == "Lithuania" ~ "LT",
    country == "Luxembourg" ~ "LU",
    country == "Netherlands" ~ "NL",
    country == "Norway" ~ "NO",
    country == "Poland" ~ "PL",
    country == "Portugal" ~ "PT",
    country == "Romania" ~ "RO",
    country == "Slovakia" ~ "SK",
    country == "Slovenia" ~ "SI",
    country == "Spain" ~ "ES",
    country == "Sweden" ~ "SE",
    country == "Switzerland" ~ "CH",
    country == "United_Kingdom" ~ "UK",
    TRUE ~ NA_character_
  )
)

# Join spatial data and clean names for display
populist_votes <- populist_votes |> 
  left_join(european)

populist_votes <- populist_votes |> 
  mutate(country = case_when(
    country == "United_Kingdom" ~ "United Kingdom", 
    country == "Czech_Republic" ~ "Czech Republic", 
    TRUE ~ country
  ))

# ==========================================================
# 3. INTERACTIVE DIRECTORY LINKS
# ==========================================================
populist_votes$links <- rep("placeholder", 30)

populist_votes <- populist_votes |> 
  mutate(links = case_when(
    country == "France" ~ "https://popu-list.github.io/Countries/France/France.html",
    TRUE ~ links
  )) 

# ==========================================================
# 4. DATA VISUALIZATION: ggplot2 and ggiraph
# ==========================================================

# Create the static map with interactive aesthetics
directory_map <- populist_votes |>
  ggplot(aes(geometry = geometry, 
             data_id = country, 
             tooltip = paste0(country), 
             onclick = paste0('window.open("', links , '")'))) +
  geom_sf_interactive(fill = "#6FB6F2") +
  coord_sf(
    xlim = c(-28, 35),
    ylim = c(32, 73),
    expand = FALSE
  ) +
  labs(title = "") +
  theme_void() +
  theme(aspect.ratio = 0.9, 
        legend.position = "top", 
        legend.margin = margin(t = -20, r = 0, b = -30, l = 0),
        legend.title = element_text(hjust = 0.5, face = "bold", size = 8),
        legend.title.position = "top",
        legend.key.width = unit(0.8, "cm"), 
        legend.key.height = unit(0.3, "cm"))

# Render the interactive Girafe object
directory_giraph <- girafe(
  directory_map,
  width_svg = 5,
  height_svg = 5,
  options = list(
    opts_hover(css = "fill:#af69ee;"),
    opts_hover_inv(css = "opacity:0.3;"),
    opts_selection(type = "multiple", css = "fill:#FF851B;stroke:black;"),
    opts_tooltip(
      css = "background-color:black;color:black;padding:10px;border-radius:10px;box-shadow:10px 10px 10px rgba(0,0,0,0.3);font-family:Arial;font-size:15px;",
      opacity = 0.9,
      use_fill = TRUE
    ),
    opts_sizing(rescale = TRUE)
  ))

# Save the final visualization as HTML
htmltools::save_html(directory_giraph, "Visualizations/directory/directory.html")