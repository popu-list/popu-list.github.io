# Note: Additional Visualizations for the PopuList

# ==========================================================
# Load Packages
# ==========================================================

library(ggtext)
library(ggchicklet)

# ==========================================================
# Core Interactive Bar Chart - Horizontal Layout
# ==========================================================


core_figure_horizontal <- G_long |>
  ggplot(aes(
    x = year, 
    y = vote_share, 
    fill = party, 
    data_id = id, 
    tooltip = paste0(" Vote share of ", party, " parties in ", year, ": ", vote_share, "%")
  )) +
  # Geoms and Scales
  geom_bar_interactive(position = "stack", stat = "identity", width = 0.7)+
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
  scale_y_continuous(breaks = seq(0, 30, 5), 
                     limits = c(0,32),
                     labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%")) +
  
  labs(
    x = "", y = "", fill = "",
    caption = "*Note*. Vote shares of (1) far-left, (2) far-left populist, (3) populist, (4) far-right populist, and (5) far-right parties in 31 European countries,<br>weighted by population size."
  ) +
  # Theme and Styling
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text.position = "top",
    plot.caption = element_markdown(hjust = 0, size = 14),
    legend.text = element_text(size = 13),
    legend.key.width = unit(2.1, 'cm'),
    legend.key.height = unit(0.3, 'cm'),
    legend.key.spacing.x = unit(1, 'cm'),
    legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
    #axis.text.y = element_blank(),
    #axis.text.x = element_blank(),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  guides(fill = guide_legend(reverse = TRUE, byrow = TRUE))



horizontal_girafe_object <- girafe(
  core_figure_horizontal, 
  width_svg = 13, 
  height_svg = 9,
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

# ==========================================================
# Populist Only As Bar Chart
# ==========================================================

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

populist_time |>
  ggplot(aes(x = year, y = share)) +
  geom_chicklet(radius = grid::unit(1, "mm"), fill = "#E4A0f7") +
  theme_minimal() +
  scale_x_continuous(breaks = c(1993, 2000, 2007, 2014, 2022)) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 30))+
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()
  )

# ==========================================================
# Far-Right Plot 
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

right_time |>
  ggplot(aes(x = year, y = share)) +
  geom_chicklet(radius = grid::unit(1, "mm"), fill = '#1E88E5') +
  theme_minimal() +
  scale_x_continuous(breaks = c(1993, 2000, 2007, 2014, 2022)) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()
  )

# ==========================================================
# Far-Left Plot
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

left_time |>
  ggplot(aes(x = year, y = share)) +
  geom_chicklet(radius = grid::unit(1, "mm"), fill = '#D81B60') +
  theme_minimal() +
  scale_x_continuous(breaks = c(1993, 2000, 2007, 2014, 2022)) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank()
  )
