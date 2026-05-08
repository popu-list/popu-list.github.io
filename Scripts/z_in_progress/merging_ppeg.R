library(tidyverse)
library(stringi)

election_results <- read_csv('/Users/lukefischer/Dropbox/The PopuList Repo/Data/ppeg_parl_2025v1.csv')
populist_parties_elections_raw <- read_csv('/Users/lukefischer/Dropbox/The PopuList Repo/Data/populist_parlgov_id.csv')

country_codes <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
  "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ITA",
  "LVA", "LTU", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU",
  "SVK", "SVN", "ESP", "SWE", "CHE", "GBR"
)

fake_others_id <- 9900:9930

others_id <- tibble(
  CountryCode = country_codes, 
  parlgov_id = fake_others_id,
  Party = rep("others", 31)
)

populist_parties_elections_raw <- bind_rows(populist_parties_elections_raw, others_id) |> 
  arrange(CountryCode) 

ppeg_ids <- election_results |> filter(iso3c %in% country_codes) |> 
  select(iso3c, pname_or, party_id, pinitials, edate, v_share, pname_en, cname_en) |> 
  arrange(iso3c)

str_cleaning <- function(df, column) {
  df |>
    mutate({{ column }} := gsub("[|/''.,:;!?@#&*—\\-]", " ", {{ column }})) |>
    mutate({{ column }} := gsub(" - ", " ", {{ column }})) |>
    mutate({{ column }} := gsub("\\s+", " ", {{ column }})) |>
    mutate({{ column }} := tolower({{ column }})) |> 
    mutate({{ column }} := stri_trans_general({{ column }}, "Latin-ASCII"))
}

ppeg_ids <- str_cleaning(ppeg_ids, pinitials)
ppeg_ids <- str_cleaning(ppeg_ids, pname_or)

#ppeg_ids <- ppeg_ids |> 
  #filter(pname_or != "others")

ppeg_ids_merger <- ppeg_ids |> 
  count(iso3c, pname_or, party_id, pinitials, pname_en, cname_en) |> 
  select(-n)

populist_parties_elections <- str_cleaning(populist_parties_elections_raw,Party)
populist_parties_elections <- str_cleaning(populist_parties_elections,Name_Short_y)
populist_parties_elections <- str_cleaning(populist_parties_elections,Name_Short_x)


merged1 <- populist_parties_elections |> 
  left_join(ppeg_ids_merger, by = c("CountryCode" = "iso3c", "Party" = "pname_or"))|>
  group_by(Party, CountryCode) |> 
  distinct(Party, .keep_all = TRUE) |> 
  ungroup()

merged2 <- populist_parties_elections |> 
  left_join(ppeg_ids_merger, by = c("CountryCode" = "iso3c", "Name_Short_x" = "pinitials"))|> 
  group_by(Party, CountryCode) |> 
  distinct(Party, .keep_all = TRUE) |> 
  ungroup()


merged3 <- merged1 |>
  left_join(merged2, by = c("Party", "CountryCode"), suffix = c("", ".y")) |>
  mutate(
    parlgov_id = coalesce(parlgov_id, parlgov_id.y),
    party_id = coalesce(party_id, party_id.y)
  ) |>
  select(-ends_with(".y")) 


merged4 <- populist_parties_elections |> 
  left_join(ppeg_ids_merger, by = c("CountryCode" = "iso3c", "Name_Short_y" = "pinitials"))|> 
  group_by(Party, CountryCode) |> 
  distinct(Party, .keep_all = TRUE) |> 
  ungroup()

merged <- merged3 |>
  left_join(merged4, by = c("Party", "CountryCode"), suffix = c("", ".y")) |>
  mutate(
    parlgov_id = coalesce(parlgov_id, parlgov_id.y),
    party_id = coalesce(party_id, party_id.y)
  ) |>
  select(-ends_with(".y")) 


# Check which 
merged |> 
  filter(is.na(party_id))

# Manual Matching of Parlgov ID and PPEG ID where possible. Otherwise, supplent with "parl_gov" and manually check which parties are still relevant 
merged <- merged |> 
  mutate(party_id = case_when(
    Party == "liste dr hans peter martin" ~ "parl_gov", # Irrelevant after 2022
    Party == "team stronach" ~ "AT26",
    Party == "front national" ~ "BE33",
    Party == "lijst dedecker libertair direct democratisch" ~ "BE4",
    Party == "izpravi se " ~ "BG613",
    Party == "volya" ~ "BG36",
    Party == "partei der arbeit der schweiz" ~ "CH9",
    Party == "kinima allileggiis" ~ "CY18",
    Party == "symmaxia" ~ "CY20",
    Party == "akce nespokojenych obcanu 2011" ~ "CZ22",
    Party == "komunisticka strana ceskoslovenska" ~ "CZ1",
    Party == "prisaha" ~ "parl_gov", # MANUAL ADDITION OF ELECTION DATA AFTER 2022
    Party == "volny blok" ~ "parl_gov", # MANUAL ADDITION OF ELECTION DATA AFTER 2022
    Party == "pds die linke" ~ "DE3",
    Party == "enhedslisten - de rod gronne" ~ "DK601",
    Party == "erakond res publica" ~ "parl_gov", # Irrelevant after 2022
    Party == "debout la republique debout la france" ~ "FR58",
    Party == "mouvement pour la france" ~ "parl_gov", # Irrelevant after 2022
    Party == "democratic unionist party" ~ "GB24",
    Party == "respect - the unity coalition" ~ "GB35",
    Party == "laikos syndesmos - chrysi avg" ~ "GR61",
    Party == "oikolόgoi enallaktikoί" ~ "GR48",
    Party == "mozemo - politicka platforma" ~ "HR49",
    Party == "borgarahreyfingin - hreyfingin" ~ "IS40",
    Party == "sosialistaflokkur islands" ~ "IS14",
    Party == "centro destra" ~ "IT880",
    Party == "fiamma tricolore" ~ "IT55",
    Party == "forza italia - il popolo della liberta" ~ "IT50",
    Party == "italia dei valori" ~ "IT87",
    Party == "lega d azione meridionale" ~ "IT53",
    Party == "movimento per la democrazia - la rete" ~ "IT48",
    Party == "jaunoji lietuva" ~ "LT34",
    Party == "lietuviu tautininku sajunga" ~ "parl_gov", # Irrelevant after 2022
    Party == "tvarka ir teisingumas - liberalu demokratu partija" ~ "LT24",
    Party == "kommunistesch partei letzebuerg" ~ "LU7",
    Party == "national bewegong" ~ "LU27",
    Party == "latvijas komunistiska partija" ~ "parl_gov",
    Party == "nacionala apvieniba tevzemei un brivibai lnnk" ~ "LV720",
    Party == "reformu partija" ~ "LV23",
    Party == "zjednoczenie chrzescijansko narodowe" ~ "parl_gov", # Irrelevant after 2022
    Party == "partidul romania unita" ~ "parl_gov", # Irrelevant after 2022
    Party == "prava slovenska narodna strana" ~ "SK4",
    Party == "vlast" ~ "parl_gov", # Irrelevant after 2022
    Party == "slovenija je nasa" ~ "parl_gov", # Irrelevant after 2022
    Party == "vansterpartiet (kommunisterna)" ~ "SE10",
    TRUE ~ party_id
  ))

# Check if there are still some parties without a corresponding ID
merged |> 
  filter(is.na(party_id))

## If necessary, try again to manuall match them to a ppeg_id

# Save parlgov_id to ppeg_id matching where it was possible
ppeg_merging <- merged |> 
  select(party_id, parlgov_id)

raw_names_populist <-populist_parties_elections_raw |> 
  select(Party, parlgov_id)

ppeg_merging <- ppeg_merging |> 
  left_join(raw_names_populist)

write_csv(ppeg_merging, "Data/ppeg_merge_ids.csv")

# make column naming consistent to merge later with populist
post_2022_data <- ppeg_ids |> 
  left_join(ppeg_merging, by = "party_id") |> 
  filter(edate > as.Date("2022-12-31"), 
         !is.na(parlgov_id)) |> 
  rename(party_name = Party,
         vote_share = v_share, 
         election_date = edate, 
         country_name_short = iso3c,
         country_name = cname_en) |> 
  select(party_name,
         vote_share,
         election_date,
         country_name_short,
         parlgov_id,
         country_name)

# Only keep last election results from a single year
post_2022_data <-post_2022_data |> 
  mutate(election_year = format(election_date, "%Y")) |> 
  group_by(country_name_short, election_year) |> 
  mutate(n_elections = dense_rank(election_date)) |> 
  filter(n_elections == max(n_elections)) |> 
  ungroup() |> 
  select(-c(n_elections, election_year))

## Malta Data Missing 2022 from parlgov dataset
malta_missing <- ppeg_ids |> 
  left_join(ppeg_merging, by = "party_id") |> 
  filter(edate > as.Date("2022-01-01"), 
         !is.na(parlgov_id), 
         iso3c == "MLT") |> 
  rename(party_name = Party,
         vote_share = v_share, 
         election_date = edate, 
         country_name_short = iso3c,
         country_name = cname_en) |> 
  select(party_name,
         vote_share,
         election_date,
         country_name_short,
         parlgov_id,
         country_name)

# Manual Addition of data until October 2025 (only Czech Republic Missing)
# SOURCES: 
# https://www.volby.cz/app/ps2025/en/results

manual_addition_missing_cze <- tibble(
  party_name = c(
    "Přísaha", 
    "Volný blok"),
  vote_share = c(
    0.0107, 
    0.0018), 
  election_date = rep(as.Date("2025-10-04"), 2), 
  country_name_short = rep("CZE", 2),
  country_name = rep("Czech Republic", 2),
  parlgov_id = c(
    2831, 
    2854)
  )

post_2022_data<-bind_rows(post_2022_data, manual_addition_missing_cze, malta_missing) |> 
  arrange(country_name_short)

# Manual Addition of 2026 data up until PopuList 4.0 release
# SOURCES: 
# https://volitve.dvk-rs.si/dz2026/#/rezultati


slovenia_2026 <- tibble(
  party_name = c(
    "Slovenska nacionalna stranka", 
    "Nova Slovenija – Krščanska ljudska stranka",
    "Slovenska demokratska stranka",
    "Levica",# left running with greens
    "others"), # add other category
  vote_share = c(
    0.0224,
    0.0926,
    0.2788,
    0.0569,
    0
  ), 
  election_date = rep(as.Date("2026-03-22"), 5), 
  country_name_short = rep("SVN", 5),
  country_name = rep("Slovenia", 5),
  parlgov_id = c(
    981, 
    1047,
    179,
    2670,
    9926)
)

# https://www.dr.dk/nyheder/politik/folketingsvalg/resultater
denmark_2026 <- tibble(
  party_name = c(
    "Socialistisk Folkeparti",
    "Dansk Folkeparti",
    "Enhedslisten – De Rød-Grønne",
    "others"
  ),
  vote_share = c(
    0.116,
    0.091,
    0.063,
    0
  ),
  election_date = rep(as.Date("2026-03-26"),4),
  country_name_short = rep("DNK", 4),
  country_name = rep("Denmark", 4),
  parlgov_id = c(
    1644,
    1418,
    306,
    9906
  )
)


# Hungary: https://www.valasztas.hu/home

# Bulgaria: https://results.cik.bg/

# to identify relevant parties
ppeg_ids |> 
  left_join(ppeg_merging, by = "party_id") |> 
  filter(edate > as.Date("2010-12-31"), 
         !is.na(parlgov_id),
         iso3c == "SVN") |> # insert country code
  count(Party)


# Combine into final list 
post_2022_data <-bind_rows(
    post_2022_data,
    slovenia_2026,
    denmark_2026) |> 
    arrange(country_name_short)

write_csv(post_2022_data, "Data/post_2022_data.csv")

