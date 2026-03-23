# Note: Additional Visualizations for the PopuList

# ==========================================================
# Load Packages
# ==========================================================

library(tidyverse)
library(gganimate)
library(ggiraph)
library(ggplot2)
library(grid)
library(ggtext)
library(sysfonts)
# ==========================================================
# Core Interactive Bar Chart - Horizontal Layout
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
                     labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%"), 
                     expand = c(0,0)) +
  scale_x_continuous(breaks = c(1993, 2000,2007,2015, 2022))+
  labs(
    x = "", y = "", fill = "",
    caption = "<br>*Note*. Vote shares of (1) far-left, (2) far-left populist, (3) populist, (4) far-right populist, and (5) far-right parties in 31 European countries,<br>weighted by population size."
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
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  guides(fill = guide_legend(reverse = TRUE, byrow = TRUE))



horizontal_girafe_object <- girafe(
  core_figure_horizontal, 
  width_svg = 13, 
  height_svg = 7,
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

htmltools::save_html(horizontal_girafe_object, 'Visualizations/additional_visualizations/bar_horizontal.html')

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

# ==========================================================
# Ridge Plot
# ==========================================================


aggregate <- G_long |>
  mutate(aggregate = case_when(
    party == "far-left populist" ~ "Far-Left (Populist)",
    party == "far-left" ~ "Far-Left (Populist)",
    party == "far-right" ~ "Far-Right (Populist)",
    party == "far-right populist" ~ "Far-Right (Populist)",
    TRUE ~ "Populist (Only)"
  )) |> 
  group_by(year, aggregate) |>
  summarize(share = sum(vote_share)) |> 
  ungroup()|>
  mutate(share_label = sprintf("%.2f%%", share), 
         aggregate = factor(aggregate, levels = c("Far-Right (Populist)", "Populist (Only)", "Far-Left (Populist)"))) 

aggregate2 <- aggregate |>
  bind_rows(
    aggregate |>
      group_by(year) |>
      summarize(share = 100 - sum(share),
                share_label = sprintf("%.2f%%", share)) |>
      mutate(aggregate = factor("Other")) |> 
      ungroup()
  ) |> 
  mutate(aggregate = factor(aggregate, levels = c("Other", "Far-Right (Populist)", "Populist (Only)", "Far-Left (Populist)")))


aggregate |> 
  ggplot(aes(x = year, y = share, fill = aggregate)) + 
  geom_area(position = "stack") +
  geom_line(position = "stack", color = "white") +
  scale_y_continuous(labels = scales::label_percent(scale = 1))
  


aggregate_max <- aggregate |> 
  mutate(last_share = if_else(year == max(year), share, NA)) |> 
  filter(!is.na(last_share)) |> 
  arrange(aggregate) |> 
  mutate(lag = lead(share), 
         lag2 = lead(share, n = 2), 
         cumulative = (rowSums(across(c(last_share, lag, lag2)), na.rm = TRUE))/100, 
         share = share/100)

aggregate |> 
  ggplot(aes(x = year, y = share, fill = aggregate)) + 
  geom_area(position = "stack") +
  geom_line(position = "stack", color = "white")+
  scale_fill_manual(
    values = c('#1E88E5', "#D6D6D6", '#F06292'))+
  scale_x_continuous(breaks = c(1995, 2000, 2005, 2010, 2015, 2020)
  )+
scale_y_continuous(expand = c(0, 0.5)
)+
  geom_text(
    data = aggregate_max,
    aes(x = year, y = cumulative, label = share_label),
    hjust = -0.1
  )+
  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_line(color = "gray"),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    legend.key.width = unit(1.5, 'cm'),
    legend.key.height = unit(0.4, 'cm'),
    legend.key.spacing.x = unit(1, 'cm'),
    legend.position = "bottom", 
    legend.text.position = "bottom",
    legend.title = element_blank()
  )


aggregate <- G_long |>
  mutate(aggregate = case_when(
    party == "far-left populist" ~ "Far-Left (Populist)",
    party == "far-left" ~ "Far-Left (Populist)",
    party == "far-right" ~ "Far-Right (Populist)",
    party == "far-right populist" ~ "Far-Right (Populist)",
    TRUE ~ "Populist (Only)"
  )) |> 
  group_by(year, aggregate) |>
  summarize(share = sum(vote_share)) |> 
  ungroup()|>
  mutate(share_label = sprintf("%.2f%%", share), 
         aggregate = factor(aggregate, levels = c("Far-Right (Populist)", "Populist (Only)", "Far-Left (Populist)"))) 



aggregate2 <- aggregate |>
  bind_rows(
    aggregate |>
      filter(aggregate == "Populist (Only)") |> 
      group_by(year, aggregate) |>
      summarize(share = -share/2,
                share_label = sprintf("%.2f%%", share)) |>
      mutate(aggregate = "Populist (Only) Negative", 
             aggregate = factor("Populist (Only) Negative")) |> 
      ungroup()
  ) |> 
  mutate(aggregate = factor(aggregate, levels = c("Far-Right (Populist)", "Populist (Only)",  "Far-Left (Populist)", "Populist (Only) Negative"))) |> 
  arrange(year, aggregate) |> 
  mutate(share = if_else(aggregate == "Populist (Only)", share/2, share), 
         share = if_else(aggregate == "Far-Left (Populist)", -share, share))




stacked_plot <- aggregate2 |> 
  ggplot(aes(x = year, y = share, fill = aggregate)) + 
  geom_area(position = "identity") +
  geom_line(color = "white") +
  scale_fill_manual(
    values = c('#1E88E5', "#D6D6D6", '#F06292', "#D6D6D6"),
    labels = c("Far-Right (Populist)", "Populist (Only)", "Far-Left (Populist)", "")
  )+
  annotate("segment", x = 1993, xend = 2022, y = 0, yend = 0, color = "#D6D6D6")+
  annotate("segment", x = 2022, xend = 2022, y = -15, yend = 17.62, color = "black")+
  annotate("segment", x = 1993, xend = 1993, y = -15, yend = 6.65, color = "black")+
  annotate("label", x = 2022, y = 0, label = "5.39%\nof Votes", hjust = -0.2, size = 5, fill = "#D6D6D6", color = "#5A5A5A") +
  annotate("label", x = 2022, y = 17.62, label = "17.62%\nof Votes", hjust = -0.2, size = 5, fill = "#1E88E5", color = "white") +
  annotate("label", x = 2022, y = -6.88, label = "6.88%\nof Votes", hjust = -0.2, size = 5, fill = "#F06292", color = "white") +
  scale_y_continuous(limits = c(-15, 19),
                     breaks = c(-15, -10, -5, 0, 5, 10, 15),
                     labels = c("15%", "10%", "5%", "0%", "5%", "10%", "15%"), 
                     expand = c(0,0)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1),
                                        add = c(0.8, 0)), 
                     breaks = c(1993, 2022),
                     labels = c("1993", "2022"))+
  guides(
    fill = guide_legend(
      override.aes = list(
        fill = c('#1E88E5', "#D6D6D6", '#F06292', NA)
      )
    )
  ) +
  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_line(color = "gray"),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    legend.key.width = unit(1.5, 'cm'),
    legend.key.height = unit(0.5, 'cm'),
    legend.key.spacing.x = unit(2, 'cm'),
    legend.position = "top", 
    legend.text.position = "top",
    legend.text = element_text(size = 15),
    legend.title = element_blank()
  )
                     
ggsave("Visualizations/additional_visualizations/stacked_plot.png", 
       stacked_plot,
       width = 17.5, 
       height = 8)

?ggsave

