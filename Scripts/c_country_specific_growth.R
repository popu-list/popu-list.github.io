library(tidyverse)
library(gganimate)
library(ggiraph)
library(ggplot2)
library(grid)
library(ggtext)
library(gridtext)
library(sysfonts)
library(patchwork)


nrc_selection <- P |> 
select(c(year:populist_votes)) |> 
  pivot_longer(cols = right_votes:populist_votes, 
               names_to = "party", 
               values_to = "vote_share") |> 
  mutate(party = factor(party, levels = c(
    "right_votes",
    "right_populist_votes",
    "populist_votes",
    "left_populist_votes",
    "left_votes"
  ))) |> 
  mutate(country_name = case_when(
    country_name == "Germany" ~ "Duitsland", 
    country_name == "United_Kingdom" ~ "Verenigd Koninkrijk",
    country_name  == "France" ~ "Frankrijk",
    country_name  == "Italy" ~ "Italië",
    country_name  == "Spain" ~ "Spanje",
    country_name  == "Poland" ~ "Polen",
    country_name  == "Netherlands" ~ "Nederland",
    country_name  == "Sweden" ~ "Zweden",
    country_name == TRUE ~ country_name
  ))



nrc_countries <- c("Duitsland", "Verenigd Koninkrijk", "Frankrijk", "Italië", "Spanje", "Polen", "Nederland", "Zweden")

for (country in nrc_countries) {
  selected <- nrc_selection |> 
    filter(country_name == paste0(country))
  
  p <- selected |> 
    ggplot(aes(
      x = year, 
      y = vote_share, 
      fill = party
    )) +
    geom_col(position = "stack", width = 0.7)+
    scale_fill_manual(
      values = c('#1E88E5', '#6FB6F2', "#D6D6D6", '#F06292', '#D81B60'),
      labels = c(
        "left_votes" = "Uiterst links", 
        "left_populist_votes" = "Populistisch uiterst links", 
        "populist_votes" = "Populistisch", 
        "right_populist_votes" = "Populistisch uiterst rechts", 
        "right_votes" = "Uiterst rechts"
      )
    ) +
    scale_y_continuous(breaks = seq(0, 60, 10), 
    limits = c(0,62),
                        labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%"), 
                        expand = c(0,0)) +
    scale_x_continuous(breaks = c(1993, 2001,2009,2017, 2026))+
    labs(
      x = "", y = "", fill = "",
      caption = paste0("*Opmerking*. Stemmenaandelen van (1) uiterst linkse, (2) populistisch uiterst linkse, (3) populistische, (4) populistisch uiterst rechtse en (5) uiterst 
                       \nrechtse partijen - ", country)
    ) +
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
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    guides(fill = guide_legend(reverse = TRUE, byrow = TRUE))
  
  ggsave(paste0("Visualizations/country_bars/", country, ".png"), 
         p,
         width = 14, 
         height = 8)
  
  write_csv(selected, paste0('/Users/lukefischer/Dropbox/The PopuList Repo/Data/NRC_Data/', country, '.csv'))
}


