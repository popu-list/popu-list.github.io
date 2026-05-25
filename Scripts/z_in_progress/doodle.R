data

data <- data |> 
  arrange(country_name, party_name, year) 


far_right_contribution <- data %>%
  filter(farright == 1 & populist == 0) %>%
  group_by(country_name, year) %>%
  mutate(right_votes_total = sum(vote_share, na.rm = TRUE)) %>%
  arrange(country_name, year, desc(vote_share)) %>%
  select(country_name, year, party_name, vote_share, right_votes_total) 

far_right_contribution |> arrange(country_name, desc(-year)) |> ungroup()

test <- left_join(expand.grid(year=c(1990:2026), country_name=unique(P3$country_name)), P3,
          by=c("year" = "year", "country_name" = "country_name"))


data %>%
  filter(farright == 1 & populist == 1) %>%
  group_by(country_name, year) %>%
  mutate(populist_right_votes_total = sum(vote_share, na.rm = TRUE)) %>%
  arrange(country_name, year, desc(vote_share)) %>%
  select(country_name, year, party_name, vote_share, populist_right_votes_total) |> 
  view()



