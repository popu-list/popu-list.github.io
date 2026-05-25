# election_results <- read_csv('/Users/lukefischer/Dropbox/The PopuList Repo/Data/ppeg_parl_2025v1.csv')
# 
# country_codes <- c(
#   "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
#   "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ITA",
#   "LVA", "LTU", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU",
#   "SVK", "SVN", "ESP", "SWE", "CHE", "GBR"
# )
# 
# ppeg_results <- election_results |> filter(iso3c %in% country_codes) |> 
#   select(iso3c, pname_or, party_id, pinitials, edate, v_share, pname_en, cname_en) |> 
#   arrange(iso3c)
# 
# ppeg_data <- merging |> 
#   left_join(ppeg_results, by = "party_id") |> 
#   filter(
#     edate > as.Date("1988-12-31"),
#     party_name %in% populist_names, 
#   ) |> 
#   rename(
#     vote_share = v_share, 
#     election_date = edate, 
#     country_name_short = iso3c) |> 
#   select(party_name,
#          vote_share,
#          election_date,
#          country_name_short,
#          parlgov_id,
#          party_id,
#          country_name) 
# 
# # Missing from all datasets
# past_parlgov_supp <- merging |> filter(party_id == "parl_gov") |> 
#   filter(!party_name %in% c("Prisaha", "Volný Blok"))
# 
# # For old parties that don't exist anymore and have a parlgov ID, add information from parlgov
# pre_2025_parlgov <- read_csv("Data/view_election.csv")
# 
# pre_2025_parlgov <- pre_2025_parlgov |> 
#   select(-c(party_name, party_name_short, country_name)) |> 
#   filter(election_type == "parliament") |> 
#   mutate(vote_share = vote_share/100)
# 
# past_parlgov_supp <- past_parlgov_supp |> 
#   left_join(pre_2025_parlgov, by = c("parlgov_id" = "party_id")) |> 
#   select(party_name, vote_share,election_date,  country_name_short, parlgov_id, party_id, country_name) |> 
#   filter(!is.na(election_date))


# Fill missing party names
# data <- data |> 
#   group_by(party_id) |> 
#   fill(party_name_short, party_name) 


# Add party names for Map Hoverinfo
# The maps contain Hoverinfo on the current parties and their vote share
# These variables are created here
# right_parties <- data %>% filter(farright==1) %>% group_by(country_name, year) %>%
#   summarise(farright_parties=paste0(party_name_short, " ", vote_share, "%",collapse = "\n"))
# left_parties <- data %>% filter(farleft==1) %>% group_by(country_name, year) %>%
#   summarise(farleft_parties=paste0(party_name_short, " ", vote_share, "%",collapse = "\n"))
# pop_parties <- data %>% filter(populist==1) %>% group_by(country_name, year) %>%
#   summarise(populist_parties=paste0(party_name_short, " ", vote_share, "%",collapse = "\n"))
# 
# # join together
# P3 <- P3 %>% left_join(right_parties) %>% left_join(left_parties) %>% left_join(pop_parties)

# set parties to none if NA, otherwise problems occur when filling the data for the maps
# P3$farleft_parties <- if_else(is.na(P3$farleft_parties), "none", P3$farleft_parties)
# P3$farright_parties <- if_else(is.na(P3$farright_parties), "none", P3$farright_parties)
# P3$populist_parties <- if_else(is.na(P3$populist_parties), "none", P3$populist_parties)

# if I simply use the data from 1989 onwards, the problem occurs that some countries already
# held elections and other did not, so the mean value is heavily skewed, as many countries
# are still NA


#rm(data, left_parties, pop_parties, right_parties)