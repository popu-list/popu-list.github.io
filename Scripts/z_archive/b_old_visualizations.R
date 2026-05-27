# ==========================================================
# Far-Right Plot 
# ==========================================================

right_time <- G_long |>
  mutate(far_right = case_when(
    #party == "far-right populist" ~ "Far-Right",
    party == "far-right" ~ "Far-Right",
    TRUE ~ "Not Far-Right"
  )) |>
  filter(far_right == "Far-Right") |>
  group_by(year) |>
  summarize(share = sum(vote_share)) |>
  ungroup() |>
  mutate(share_label = sprintf("%.2f%%", share))

right_time_plot <- right_time |>
  ggplot(aes(x = year, y = share)) +
  geom_chicklet(radius = grid::unit(1, "mm"), fill = '#1E88E5') +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  labs(subtitle = "\n[A] Far-Right\n")+
  scale_x_continuous(breaks = c(1993, 2000, 2007, 2014, 2022)) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    plot.title.position = "plot"
  )

# ==========================================================
# Far-Right Populist Plot 
# ==========================================================

right_pop_time <- G_long |>
  mutate(far_right = case_when(
    party == "far-right populist" ~ "Far-Right",
    #party == "far-right" ~ "Far-Right",
    TRUE ~ "Not Far-Right"
  )) |>
  filter(far_right == "Far-Right") |>
  group_by(year) |>
  summarize(share = sum(vote_share)) |>
  ungroup() |>
  mutate(share_label = sprintf("%.2f%%", share))

right_pop_time_plot <- right_pop_time |>
  ggplot(aes(x = year, y = share, fill = "Far-Right Populist")) +
  geom_chicklet(radius = grid::unit(1, "mm")) +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  labs(subtitle = "\n")+
  scale_x_continuous(breaks = c(1993, 2000, 2007, 2014, 2022)) +
  scale_fill_manual(values = c("Far-Right Populist" = "#6FB6F2"),  
                    name = "Political Group") +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.title = element_blank(),
    plot.title.position = "plot"
  )

# ==========================================================
# Far-Left Populist Plot
# ==========================================================

left_pop_time <- G_long |>
  mutate(far_right = case_when(
    party == "far-left populist" ~ "Far-Left",
    #party == "far-left" ~ "Far-Left",
    TRUE ~ "Not Far-Left"
  )) |>
  filter(far_right == "Far-Left") |>
  group_by(year) |>
  summarize(share = sum(vote_share)) |>
  ungroup() |>
  mutate(share_label = sprintf("%.2f%%", share))

left_pop_time_plot <- left_pop_time |>
  ggplot(aes(x = year, y = share, fill = "Far-Left Populist")) +
  geom_chicklet(radius = grid::unit(1, "mm")) +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  labs(subtitle = "\n")+
  scale_x_continuous(breaks = c(1993, 2000, 2007, 2014, 2022)) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
  scale_fill_manual(values = c("Far-Left Populist" = "#F06292"),  
                    name = "Political Group") +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.title = element_blank(),
    plot.title.position = "plot"
  )

# ==========================================================
# Far-Left Plot
# ==========================================================

left_time <- G_long |>
  mutate(far_right = case_when(
    #party == "far-left populist" ~ "Far-Left",
    party == "far-left" ~ "Far-Left",
    TRUE ~ "Not Far-Left"
  )) |>
  filter(far_right == "Far-Left") |>
  group_by(year) |>
  summarize(share = sum(vote_share)) |>
  ungroup() |>
  mutate(share_label = sprintf("%.2f%%", share))

left_time_plot <- left_time |>
  ggplot(aes(x = year, y = share)) +
  geom_chicklet(radius = grid::unit(1, "mm"), fill = '#D81B60') +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  labs(subtitle = "\n[E] Far-Left\n")+
  scale_x_continuous(breaks = c(1993, 2000, 2007, 2014, 2022)) +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 30)) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    plot.title.position = "plot"
  )

# bar_separate_plot <- (right_time_plot/right_pop_time_plot/populist_time_plot/left_pop_time_plot/left_time_plot) +
#   plot_layout(axes = "collect_x")&
#   theme(legend.position = "none")

# ggsave("Visualizations/additional_visualizations/bar_separate_all.png", 
#        bar_separate_plot,
#        width = 7, 
#        height = 12)
# 
bar_separate_populist <- (right_pop_time_plot/populist_time_plot/left_pop_time_plot) +
  plot_layout(axes = "collect_x", guides = "collect")&
  theme(legend.position = "top", 
        legend.text.position = "top", 
        legend.key.width = unit(2.3, "cm"))

ggsave("Visualizations/additional_visualizations/bar_separate_populist.png", 
       bar_separate_populist,
       width = 5, 
       height = 6)
