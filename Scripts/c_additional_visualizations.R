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
library(gridtext)
library(sysfonts)
library(patchwork)
# ==========================================================
# Additional Interactive Bar Charts
# ==========================================================
G_long <- read_csv("Data/G_long.csv")

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
  mutate(id = as.factor(rep(1:170)))

# Calculate Annual Vote Share Sums
G_long <- G_long |>
  group_by(year) |>
  mutate(sum = sum(vote_share)) |>
  ungroup()

max_long <- G_long |>
  count(year, sum)

## Activate Font
sysfonts::font_add_google(name = "Lato", family = "Lato")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# ==========================================================
# Flat Horizontal Graph
# ==========================================================

core_figure_horizontal_flat <- G_long |>
  ggplot(aes(
    x = year, 
    y = vote_share, 
    fill = party
  )) +
  # Geoms and Scales
  geom_col(position = "stack", width = 0.7)+
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
                     limits = c(0,31),
                     labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%"), 
                     expand = c(0,0)) +
  scale_x_continuous(breaks = c(1993, 2001,2009,2017, 2026))+
  labs(
    x = "", y = "", fill = "",
    caption = "*Note*. Vote shares of (1) far-left, (2) far-left populist, (3) populist, (4) far-right populist, and (5) far-right parties in 31 European countries, weighted by<br>population size."
  ) +
  # Theme and Styling
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text.position = "top",
    plot.caption = element_markdown(hjust = 0, size = 14, lineheight = 1.4, margin = margin(t = 10)),
    legend.text = element_text(size = 14),
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

ggsave("Visualizations/additional_visualizations/bar_flat.png", 
       core_figure_horizontal_flat,
       width = 14, 
       height = 8)

# ==========================================================
## Far-right (populist)
# ==========================================================

far_right_populist <- G_long |> 
  filter(party %in% c("far-right", "far-right populist")) |> 
  mutate(party = if_else(party == "far-right", "far-right (populist)", "far-right (populist)")) |> 
  group_by(year) |> 
  summarize(vote_share = sum(vote_share)) |> 
  ungroup() |> 
  mutate(party = rep("Far-Right (Populist)", 34))|>
  mutate(id = as.factor(rep(1:34)))


far_right_populist_plot <- far_right_populist|>
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
     values = c('#1E88E5')) +
  scale_y_continuous(breaks = seq(0, 30, 5), 
                     limits = c(0,31),
                     labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%"), 
                     expand = c(0,0)) +
  scale_x_continuous(breaks = c(1993, 2001,2009,2017, 2026))+
  labs(
    x = "", y = "", fill = "", title = "Far-Right (Populist)* Vote-Share",
    caption = "<br>*Note*. *Vote shares of far-right populist and far-right parties in 31 European countries, weighted by population size."
  ) +
  # Theme and Styling
  theme_minimal() +
  guides(
    fill = guide_legend(
      override.aes = list(
        width = unit(0.3, "cm"),
        height = unit(0.2, "cm")
      )
    )
  )+
  theme(
    text = element_text(family = "Lato"),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_markdown(hjust = 0, size = 14)
  )


far_right_populist_interactive <- girafe(
  far_right_populist_plot, 
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

htmltools::save_html(far_right_populist_interactive, 'Visualizations/additional_visualizations/bar_horizontal_far_right.html')

far_right_flat <- far_right_populist |>
  ggplot(aes(
    x = year, 
    y = vote_share, 
    fill = party
  )) +
  # Geoms and Scales
  geom_col(position = "stack", width = 0.7)+
  scale_fill_manual(
    values = c('#1E88E5')) +
  scale_y_continuous(breaks = seq(0, 30, 5), 
                     limits = c(0,31),
                     labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%"), 
                     expand = c(0,0)) +
  scale_x_continuous(breaks = c(1993, 2001,2009,2017, 2026))+
  labs(
    x = "", y = "", fill = "", title = "Far-Right (Populist)* Vote-Share",
    caption = "*Note*. *Vote shares of far-right populist and far-right parties in 31 European countries, weighted by population size."
  ) +
  # Theme and Styling
  theme_minimal() +
  theme_minimal() +
  guides(
    fill = guide_legend(
      override.aes = list(
        width = unit(0.3, "cm"),
        height = unit(0.2, "cm")
      )
    )
  )+
  theme(
    text = element_text(family = "Lato"),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_markdown(hjust = 0, size = 14, lineheight = 1.4, margin = margin(t = 10)),
  )


ggsave("Visualizations/additional_visualizations/bar_horizontal_farright_flat.png", 
       far_right_flat,
       width = 14, 
       height = 8)

# ==========================================================
## Populist (far-left and far-right)
# ==========================================================

populist_all <- G_long |> 
  filter(party %in% c("far-right populist", "far-left populist", "populist")) |> 
  mutate(party = if_else(party == "far-right populist", "populist", "populist")) |> 
  group_by(year) |> 
  summarize(vote_share = sum(vote_share)) |> 
  ungroup() |> 
  mutate(party = rep("populist", 34))|>
  mutate(id = as.factor(rep(1:34)))

populist_plot <- populist_all|>
  ggplot(aes(
    x = year, 
    y = vote_share, 
    fill = party, 
    data_id = id, 
    tooltip = paste0(" Vote share of ", party, " parties in ", year, ": ", vote_share, "%")
  )) +
  # Geoms and Scales
  geom_bar_interactive(stat = "identity", width = 0.7)+
  scale_fill_manual(
    values = c("#D6D6D6")) +
  scale_y_continuous(breaks = seq(0, 30, 5), 
                     limits = c(0,31),
                     labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%"), 
                     expand = c(0,0)) +
  scale_x_continuous(breaks = c(1993, 2001,2009,2017, 2026))+
  labs(
    x = "", y = "", fill = "", title = "(Far-Right and Far-Left)* Populist Vote-Share",
    caption = "<br>*Note*. *Vote shares of far-right populist, far-left populist, and populist parties in 31 European countries, weighted by population size."
    
  ) +
  # Theme and Styling
  theme_minimal() +
  guides(
    fill = guide_legend(
      override.aes = list(
        width = unit(0.3, "cm"),
        height = unit(0.2, "cm")
      )
    )
  )+
  theme(
    text = element_text(family = "Lato"),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_markdown(hjust = 0, size = 14)
  )


populist_interactive <- girafe(
  populist_plot, 
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


htmltools::save_html(populist_interactive, 'Visualizations/additional_visualizations/bar_horizontal_populist.html')

populist_flat <- populist_all |>
  ggplot(aes(
    x = year, 
    y = vote_share, 
    fill = party
  )) +
  # Geoms and Scales
  # Geoms and Scales
  geom_bar_interactive(stat = "identity", width = 0.7)+
  scale_fill_manual(
    values = c("#D6D6D6")) +
  scale_y_continuous(breaks = seq(0, 30, 5), 
                     limits = c(0,31),
                     labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%"), 
                     expand = c(0,0)) +
  scale_x_continuous(breaks = c(1993, 2001,2009,2017, 2026))+
  labs(
    x = "", y = "", fill = "", title = "(Far-Right and Far-Left)* Populist Vote-Share",
    caption = "<br>*Note*. *Vote shares of far-right populist, far-left populist, and populist parties in 31 European countries, weighted by population size."
    
  ) +
  # Theme and Styling
  theme_minimal() +
  guides(
    fill = guide_legend(
      override.aes = list(
        width = unit(0.3, "cm"),
        height = unit(0.2, "cm")
      )
    )
  )+
  theme(
    text = element_text(family = "Lato"),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_markdown(hjust = 0, size = 14, lineheight = 1.4, margin = margin(t = 10)),
  )


ggsave("Visualizations/additional_visualizations/bar_horizontal_populist_flat.png", 
       populist_flat,
       width = 14, 
       height = 8)
# ==========================================================
## Far-Let (Populist) Interactive
# ==========================================================

far_left_populist <- G_long |> 
  filter(party %in% c("far-left populist", "far-left")) |> 
  mutate(party = if_else(party == "far-left populist", "Far-Left Populist", "Far-Left Populist")) |> 
  group_by(year) |> 
  summarize(vote_share = sum(vote_share)) |> 
  ungroup() |> 
  mutate(party = rep("Far-Left Populist", 34))|>
  mutate(id = as.factor(rep(1:34)))

far_left_populist_plot <- far_left_populist|>
  ggplot(aes(
    x = year, 
    y = vote_share, 
    fill = party, 
    data_id = id, 
    tooltip = paste0(" Vote share of ", party, " parties in ", year, ": ", vote_share, "%")
  )) +
  # Geoms and Scales
  geom_bar_interactive(stat = "identity", width = 0.7)+
  scale_fill_manual(
    values = c("#F06292")) +
  scale_y_continuous(breaks = seq(0, 30, 5), 
                     limits = c(0,31),
                     labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%"), 
                     expand = c(0,0)) +
  scale_x_continuous(breaks = c(1993, 2001,2009,2017, 2026))+
  labs(
    x = "", y = "", fill = "", title = "Far-Left (Populist)* Vote-Share",
    caption = "<br>*Note*. *Vote shares of far-left and far-left populist parties in 31 European countries, weighted by population size."
    
  ) +
  # Theme and Styling
  theme_minimal() +
  guides(
    fill = guide_legend(
      override.aes = list(
        width = unit(0.3, "cm"),
        height = unit(0.2, "cm")
      )
    )
  )+
  theme(
    text = element_text(family = "Lato"),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_markdown(hjust = 0, size = 14)
  )


far_left_interactive <- girafe(
  far_left_populist_plot, 
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


htmltools::save_html(far_left_interactive, 'Visualizations/additional_visualizations/bar_horizontal_far_left.html')

far_left_flat <- far_left_populist |>
  ggplot(aes(
    x = year, 
    y = vote_share, 
    fill = party
  )) +
  # Geoms and Scales
  geom_bar_interactive(stat = "identity", width = 0.7)+
  scale_fill_manual(
    values = c("#F06292")) +
  scale_y_continuous(breaks = seq(0, 30, 5), 
                     limits = c(0,31),
                     labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%"), 
                     expand = c(0,0)) +
  scale_x_continuous(breaks = c(1993, 2001,2009,2017, 2026))+
  labs(
    x = "", y = "", fill = "", title = "Far-Left (Populist)* Vote-Share",
    caption = "<br>*Note*. *Vote shares of far-left and far-left populist parties in 31 European countries, weighted by population size."
    
  ) +
  # Theme and Styling
  theme_minimal() +
  guides(
    fill = guide_legend(
      override.aes = list(
        width = unit(0.3, "cm"),
        height = unit(0.2, "cm")
      )
    )
  )+
  theme(
    text = element_text(family = "Lato"),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = element_markdown(hjust = 0, size = 14, lineheight = 1.4, margin = margin(t = 10)),
  )


ggsave("Visualizations/additional_visualizations/bar_horizontal_far_left_flat.png", 
       far_left_flat,
       width = 14, 
       height = 8)

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

aggregate2 |> 
  filter(year == 2026)


stacked_plot <- aggregate2 |> 
  ggplot(aes(x = year, y = share, fill = aggregate)) + 
  geom_area(position = "identity") +
  geom_line(color = "white") +
  scale_fill_manual(
    values = c('#1E88E5', "#D6D6D6", '#F06292', "#D6D6D6"),
    labels = c("Far-Right (Populist)", "Populist (Only)", "Far-Left (Populist)", "")
  )+
  annotate("segment", x = 1993, xend = 2026, y = 0, yend = 0, color = "white")+
  annotate("segment", x = 2026, xend = 2026, y = -15, yend = 23.28, color = "black")+
  annotate("segment", x = 1993, xend = 1993, y = -15, yend = 6.56, color = "black")+
  annotate("label", x = 2026, y = 0, label = "5.65%", hjust = -0.2, size = 20, fill = "#D6D6D6", color = "#5A5A5A") +
  annotate("label", x = 2026, y = 13.05, label = "23.3%", hjust = -0.2, size = 20, fill = "#1E88E5", color = "white") +
  annotate("label", x = 2026, y = -4.125, label = "5.43%", hjust = -0.2, size = 20, fill = "#F06292", color = "white") +
  scale_y_continuous(limits = c(-15, 25),
                     breaks = c(-15, -10, -5, 0, 5, 10, 15, 20, 25),
                     labels = c("15%", "10%", "5%", "0%", "5%", "10%", "15%", "20%", "25%"), 
                     expand = c(0,0)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1),
                                        add = c(0.8, 0)), 
                     breaks = c(1993, 2026),
                     labels = c("1993", "2026"))+
  guides(
    fill = guide_legend(
      override.aes = list(
        fill = c('#1E88E5', "#D6D6D6", '#F06292', NA)
      )
    )
  ) +
  theme_minimal()+
  theme(
    text = element_text(family = "Lato", size = 65),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    #panel.grid.major.y = element_line(color = "gray"),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    # axis.text.y = element_text(size = 15),
    # axis.text.x = element_text(size = 15),
    legend.key.width = unit(1.5, 'cm'),
    legend.key.height = unit(0.5, 'cm'),
    legend.key.spacing.x = unit(2, 'cm'),
    legend.position = "top", 
    legend.text.position = "top",
    # legend.text = element_text(size = 15),
    legend.title = element_blank()
  )
                     
ggsave("Visualizations/additional_visualizations/stacked_plot.png", 
       stacked_plot,
       width = 17.5, 
       height = 8)

