# ==========================================================
# 0. Load Packages and Data
# ==========================================================
library(tidyverse)
library(gganimate)
library(ggiraph)
library(ggplot2)
library(grid)

G_long <- read_csv("Data/G_long.csv")

# ==========================================================
# 1. Data Preparation and Factoring
# ==========================================================

# Reorder Party Levels
G_long <- G_long |>
  mutate(party = factor(party, levels = c(
    "far-right",
    "far-right populist",
    "populist",
    "far-left populist",
    "far-left"
  )))

# Add ID factor
G_long <- G_long |>
  mutate(id = as.factor(rep(1:150)))

# Calculate Annual Vote Share Sums
G_long <- G_long |>
  group_by(year) |>
  mutate(sum = sum(vote_share)) |>
  ungroup()

max_long <- G_long |>
  count(year, sum)

# ==========================================================
# 2. Core Interactive Bar Chart (ggplot2 + ggiraph)
# ==========================================================

core_figure <- G_long |>
  ggplot(aes(
    x = year, 
    y = vote_share, 
    fill = party, 
    data_id = id, 
    tooltip = paste0(" Vote share of ", party, " parties in ", year, ": ", vote_share, "%")
  )) +
  # Reference Segments
  annotate("segment", x = 1992.5, xend = 2023, y = 5, yend = 5, color = "#D3D3D3") +
  annotate("segment", x = 1992.5, xend = 2023, y = 10, yend = 10, color = "#D3D3D3") +
  annotate("segment", x = 1992.5, xend = 2023, y = 15, yend = 15, color = "#D3D3D3") +
  annotate("segment", x = 1992.5, xend = 2023, y = 20, yend = 20, color = "#D3D3D3") +
  annotate("segment", x = 1992.5, xend = 2023, y = 25, yend = 25, color = "#D3D3D3") +
  annotate("segment", x = 1992.5, xend = 2023, y = 30, yend = 30, color = "#D3D3D3") +
  # Geoms and Scales
  geom_bar_interactive(position = "stack", stat = "identity", width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = c('#1E88E5', '#6FB6F2', "#D6D6D6", '#F06292', '#D81B60'),
    labels = c(
      "far-left" = "Far-Left", 
      "far-left populist" = "Far-Left Populist", 
      "populist" = "Populist", 
      "far-right populist" = "Far-Right Populist", 
      "far-right" = "Far-Right"
    )
  ) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  labs(
    x = "", y = "", fill = "",
    caption = "Note: Vote shares of (1) far-left, (2) far-left populist, (3) populist, (4) far-right populist, and (5) far-right parties in\n31 European countries, weighted by population size."
  ) +
  # Theme and Styling
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text.position = "top",
    plot.caption = element_text(hjust = 0, size = 14),
    legend.text = element_text(size = 13),
    legend.key.width = unit(2.1, 'cm'),
    legend.key.height = unit(0.3, 'cm'),
    legend.key.spacing.x = unit(1, 'cm'),
    legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  guides(fill = guide_legend(reverse = TRUE, byrow = TRUE)) +
  # Axis and Data Labels
  annotate("text", x = 2022, y = -0.1, label = "2022", hjust = 1, size = 5.4, fontface = "bold") +
  annotate("text", x = 1993, y = -0.1, label = "1993", hjust = 1, size = 5) +
  annotate("text", x = 2007, y = -0.1, label = "2007", hjust = 1, size = 5) +
  annotate("text", x = 2000, y = -0.2, label = "2000", hjust = 1, size = 5) +
  annotate("text", x = 2014, y = -0.2, label = "2014", hjust = 1, size = 5) +
  annotate("text", x = 2023.5, y = 5, label = "5%", size = 5) +
  annotate("text", x = 2023.5, y = 10, label = "10%", size = 5) +
  annotate("text", x = 2023.5, y = 15, label = "15%", size = 5) +
  annotate("text", x = 2023.5, y = 20, label = "20%", size = 5) +
  annotate("text", x = 2023.5, y = 25, label = "25%", size = 5) +
  annotate("text", x = 2023.5, y = 30, label = "30%", size = 5) +
  annotate("text", x = 2022, y = 30.1, label = "29.9%", hjust = 0, size = 6, fontface = "bold") +
  annotate("text", x = 1993, y = 12.7, label = "12.5%", hjust = 0, size = 5)

# Preview and Export
max_long |> filter(year %in% c(1993, 2007, 2022))

core_figure

girafe_object <- girafe(
  core_figure, 
  width_svg = 10, 
  height_svg = 13,
  options = list(
    opts_hover(css = "fill:#af69ee;"),
    opts_hover_inv(css = "opacity:0.7;"),
    opts_selection(type = "multiple", css = "fill:#FF851B;stroke:black;"),
    opts_tooltip(
      css = "background-color:black;color:black;padding:10px;border-radius:10px;box-shadow:10px 10px 10px rgba(0,0,0,0.3);font-family:Arial;font-size:12px;",
      opacity = 0.9,
      use_fill = TRUE
    ),
    opts_sizing(rescale = TRUE)
  )
)

htmltools::save_html(girafe_object, 'Visualizations/bar/bar.html')

# ==========================================================
# 3. Populism Only Time Series (Animated)
# ==========================================================

library(gganimate)

populist_time <- G_long |>
  mutate(populist = case_when(
    party == "far-right populist" ~ "Populist",
    party == "populist" ~ "Populist",
    party == "far-left populist" ~ "Populist",
    TRUE ~ "Not Populist"
  )) |>
  filter(populist == "Populist") |>
  group_by(year) |>
  summarize(share = sum(vote_share)) |>
  ungroup() |>
  mutate(share_label = sprintf("%.2f%%", share))

populist_moving <- populist_time |>
  ggplot(aes(x = year, y = share)) +
  geom_area(stat = "identity", fill = "#E4A0f7") +
  theme_minimal() +
  labs(x = "Populist", y = "") +
  scale_x_continuous(breaks = c(1993, 2000, 2007, 2014, 2022)) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
  geom_text(aes(label = sprintf("%.2f", share)), hjust = -0.1, size = 1) +
  transition_reveal(year, keep_last = FALSE) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 3),
    axis.title.x = element_text(size = 3)
  )

animpopulist <- animate(
  populist_moving,
  width = 316, height = 200, res = 300,
  fps = 20, nframes = 200,
  renderer = gifski_renderer()
)

anim_save("Visualizations/time/timepopulist.gif", animation = animpopulist)

# ==========================================================
# 4. Far-Right Plot (Animated)
# ==========================================================

right_time <- G_long |>
  mutate(far_right = case_when(
    party == "far-right populist" ~ "Far-Right",
    party == "far-right" ~ "Far-Right",
    TRUE ~ "Not Far-Right"
  )) |>
  filter(far_right == "Far-Right") |>
  group_by(year) |>
  summarize(share = sum(vote_share)) |>
  ungroup() |>
  mutate(share_label = sprintf("%.2f%%", share))

right_moving <- right_time |>
  ggplot(aes(x = year, y = share)) +
  geom_area(stat = "identity", fill = "#1E88E5") +
  theme_minimal() +
  labs(x = "Far-Right", y = "") +
  scale_x_continuous(breaks = c(1993, 2000, 2007, 2014, 2022)) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
  geom_text(aes(label = sprintf("%.2f", share)), hjust = -0.1, size = 1) +
  transition_reveal(year, keep_last = FALSE) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 3),
    axis.title.x = element_text(size = 3)
  )

anim_right <- animate(
  right_moving,
  width = 316, height = 200, res = 300,
  fps = 20, nframes = 200,
  renderer = gifski_renderer()
)

anim_save("Visualizations/time/timeright.gif", animation = anim_right)

# ==========================================================
# 5. Far-Left Plot (Animated)
# ==========================================================

left_time <- G_long |>
  mutate(far_right = case_when(
    party == "far-left populist" ~ "Far-Left",
    party == "far-left" ~ "Far-Left",
    TRUE ~ "Not Far-Left"
  )) |>
  filter(far_right == "Far-Left") |>
  group_by(year) |>
  summarize(share = sum(vote_share)) |>
  ungroup() |>
  mutate(share_label = sprintf("%.2f%%", share))

left_moving <- left_time |>
  ggplot(aes(x = year, y = share)) +
  geom_area(stat = "identity", fill = "#D81B60") +
  theme_minimal() +
  labs(x = "Far-Left", y = "") +
  scale_x_continuous(breaks = c(1993, 2000, 2007, 2014, 2022)) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
  geom_text(aes(label = sprintf("%.2f", share)), hjust = -0.1, size = 1) +
  transition_reveal(year, keep_last = FALSE) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 3),
    axis.title.x = element_text(size = 3)
  )

anim_left <- animate(
  left_moving,
  width = 316, height = 200, res = 300,
  fps = 20, nframes = 200,
  renderer = gifski_renderer()
)

anim_save("Visualizations/time/timeleft.gif", animation = anim_left)
