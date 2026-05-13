
test <- read_csv("Data/ppeg_merge_ids.csv")
old <- read_csv2("Data/The PopuList 3.0.csv")


populist <- read_csv2("Data/The PopuList 4.0.csv")

populist_parties <- populist |> 
  ungroup() |> 
  count(party_name, party_name_short, country_name, partyfacts_id, parlgov_id) |> 
  select(-n) |> 
  arrange(country_name) 

populist_names <- populist_parties |> pull(party_name)

test2 <- left_join(populist_parties, test) |> 
  select(party_name, party_name_short, country_name, partyfacts_id, parlgov_id, party_id)


test2 <- test2[!duplicated(test2), ]


fake_others_id <- 9900:9930


test2 <- test2 |> 
  mutate(party_id = case_when(
    party_name == "Kommunistische Partei Österreichs Plus" ~ "AT5", 
    party_name == "Bulgarski Vazhod" ~ "9900",
    party_name == "MECh" ~ "BG45",
    party_name == "Velichie" ~ "BG44",
    party_name == "Левицата!" ~ "BG623",
    party_name == "Прогресивна България" ~ "9001",
    party_name == "Blok za Hrvatsku" ~ "HR54",
    party_name == "Dom i Nacionalno Okupljanje" ~ "9002",
    party_name == "Hrvatska Cista Stranka Prava" ~ "9003",
    party_name == "Hrvatska Konzervativna Stranka" ~ "HR48",
    party_name == "Hrvatska Stranka Prava Dr. Ante Starcevic" ~ "HR29",
    party_name == "Hrvatski Suverenisti" ~ "HR57",
    party_name == "Pravo i Pravda" ~ "HR56",
    party_name == "Radnička Fronta" ~ "HR44",
    party_name == "Movement of Hunters" ~ "9004", # missing data for all IDs
    party_name == "Motoristé Sobe" ~ "CZ29",
    party_name == "Strana Práv Občanů" ~ "CZ19",
    party_name == "Borgernes Parti" ~ "9005", # not yet in PPEG due to recency
    party_name == "Danmarksdemokraterne" ~ "DK32",
    party_name == "Les Républicains" ~ "FR33",
    party_name == "Reconquete" ~ "FR80",
    party_name == "Union des droites pour la République" ~ "9006", # not yet in PPEG due to recency
    party_name == "Bündnis Sahra Wagenknecht" ~ "DE62",
    party_name == "Δημοκρατικό Πατριωτικό Κίνημα \"ΝΙΚΗ\"" ~ "GR74",
    party_name == "Νέα Αριστερά" ~ "9007", # not yet in PPEG due to recency
    party_name == "Πλεύση Ελευθερίας - Ζωή Κωνσταντοπούλου" ~ "GR75",
    party_name == "Σπαρτιάτες" ~ "GR76",
    party_name == "Independent Ireland" ~ "IE43",
    party_name == "Workers' Party" ~ "IE19",
    party_name == "De Luca Sindaco d’Italia" ~ "9008", # missing data for all IDs
    party_name == "Forza Italia (2013-)" ~ "IT50",
    party_name == "La Destra - Fiamma Tricolore" ~ "9009", # missing data for parlgov and ppeg
    party_name == "La Sinistra Arcobaleno" ~ "9010", # missing data for all IDs
    party_name == "Liberi e Uguali" ~ "IT641",
    party_name == "Rivoluzione Civile" ~ "IT632",
    party_name == "Dzimtene (now Jaunā Saskaņa)" ~ "9011", # missing data for parlgov and ppeg
    party_name == "Katram un Katrai" ~ "parl_gov",
    party_name == "Latvija Pirmajā Vietā" ~ "LV32",
    party_name == "Stabilitāte!" ~ "LV31",
    party_name == "Suverēnā Vara" ~ "9012", # missing data for all IDs
    party_name == "Front Party" ~ "9013", # missing data for parlgov and ppeg
    party_name == "Nacionalinis susivienijimas" ~ "LT38",
    party_name == "Nemuno Aušra" ~ "LT39",
    party_name == "Imperium Europa" ~ "9014", # missing data for all IDs
    party_name == "Pasientfokus" ~ "NO23",
    party_name == "Senterpartiet" ~ "NO7",
    party_name == "Konfederacja Korony Polskiej" ~ "9015", # missing data for all IDs
    party_name == "Konfederacja Wolność i Niepodległość" ~ "PL61", # party or alliance -> partyfacts is 9026
    party_name == "Kongres Nowej Prawicy" ~ "9016", # parlgov id 1549
    party_name == "Nowa Nadzieja" ~ "PL40",
    party_name == "Suwerenna Polska" ~ "parl_gov",
    party_name == "Partidul Național Conservator Român" ~ "RO49",
    party_name == "Partidul Oamenilor Tineri" ~ "RO48",
    party_name == "Patriotii Poporului Roman" ~ "9017", # missing data for all IDs
    party_name == "S.O.S Romǎnia" ~ "RO47",
    party_name == "Republika" ~ "9018",  # missing data for all IDs
    party_name == "Povežimo Slovenijo" ~ "SI602",
    party_name == "Resni.ca" ~ "SI38",
    party_name == "Sumar" ~ "ES880",
    party_name == "Unidas Podemos" ~ "ES806",
    TRUE ~ party_id
  ))
    
election_results <- read_csv('/Users/lukefischer/Dropbox/The PopuList Repo/Data/ppeg_parl_2025v1.csv')

country_codes <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
  "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ITA",
  "LVA", "LTU", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU",
  "SVK", "SVN", "ESP", "SWE", "CHE", "GBR"
)

ppeg_results <- election_results |> filter(iso3c %in% country_codes) |> 
  select(iso3c, pname_or, party_id, pinitials, edate, v_share, pname_en, cname_en) |> 
  arrange(iso3c)

post_2022 <- test2 |> 
  left_join(ppeg_results, by = "party_id") |> 
  filter(
    edate > as.Date("2022-12-31"),
    party_name %in% populist_names, 
  ) |> 
  rename(
         vote_share = v_share, 
         election_date = edate, 
         country_name_short = iso3c) |> 
  select(party_name,
         vote_share,
         election_date,
         country_name_short,
         parlgov_id,
         party_id,
         country_name) 


post_2022 <- post_2022 |> 
  mutate(parlgov_id = as.character(parlgov_id)) |> 
  mutate(parlgov_id = if_else(is.na(parlgov_id), party_id, parlgov_id))

# Only keep last election results from a single year
post_2022 <- post_2022 |> 
  mutate(election_year = format(election_date, "%Y")) |> 
  group_by(country_name_short, election_year) |> 
  mutate(n_elections = dense_rank(election_date)) |> 
  filter(n_elections == max(n_elections)) |> 
  ungroup() |> 
  select(-c(n_elections, election_year))


manual_addition_missing_cze <- tibble(
  party_name = c(
    "Prisaha", 
    "Volný Blok"),
  vote_share = c(
    0.0107, 
    0.0018 
    ), 
  election_date = rep(as.Date("2025-10-04"), 2), 
  country_name_short = rep("CZE", 2),
  country_name = rep("Czech Republic", 2),
  parlgov_id = c(
    "2831", 
    "2854")
)

post_2022 <-bind_rows(post_2022, manual_addition_missing_cze) |> 
  arrange(country_name_short)

# Manual Addition of 2026 data up until PopuList 4.0 release
# SOURCES: 
# https://volitve.dvk-rs.si/dz2026/#/rezultati


slovenia_2026 <- tibble(
  party_name = c(
    "Slovenska Nacionalna Stranka", 
    "Nova Slovenija – Krščanski Demokrati",
    "Slovenska Demokratska Stranka",
    "Levica",# left running with greens
    "Resni.ca",
    "others"), # add other category
  vote_share = c(
    0.0224,
    0.0926,
    0.2788,
    0.0569,
    0.0549,
    0
  ), 
  election_date = rep(as.Date("2026-03-22"), 6), 
  country_name_short = rep("SVN", 6),
  country_name = rep("Slovenia", 6),
  parlgov_id = c(
    "981", 
    "1047",
    "179",
    "2670",
    NA,
    NA
    )
)


# https://www.dr.dk/nyheder/politik/folketingsvalg/resultater
denmark_2026 <- tibble(
  party_name = c(
    "Socialistisk Folkeparti",
    "Dansk Folkeparti",
    "Enhedslisten – De Rød-Grønne",
    "Borgernes Parti",
    "Danmarksdemokraterne",
    "others"
  ),
  vote_share = c(
    0.116,
    0.091,
    0.063,
    0.021,
    0.058,
    0
  ),
  election_date = rep(as.Date("2026-03-26"),6),
  country_name_short = rep("DNK", 6),
  country_name = rep("Denmark", 6),
  parlgov_id = c(
    "1644",
    "1418",
    "306",
    NA, 
    "DK32",
    NA
  )
)


# Hungary: https://www.valasztas.hu/home

hungary_2026 <- tibble(
  party_name = c(
    "Fidesz",
    "Mi Hazánk Mozgalom",
    "others"
  ),
  vote_share = c(
    0.3861,
    0.0563,
    0
  ),
  election_date = rep(as.Date("2026-04-12"),3),
  country_name_short = rep("HUN", 3),
  country_name = rep("Hungary", 3),
  parlgov_id = c(
    921,
    2745,
    NA
  )
)

# Bulgaria: https://results.cik.bg/

bulgaria_2026 <- tibble(
  party_name = c(
    "Balgarska Sotsialisticheska Partiya",
    "Grazhdani za Evropeysko Razvitie na Bulgariya",
    "Ima Takav Narod",
    "Vazrazhdane",
    "Velichie",
    "Прогресивна България",
    "others"
  ),
  vote_share = c(
    0.03017,
    0.13387,
    0.0736,
    0.04257,
    0.03104,
    0.44594,
    0
  ),
  election_date = rep(as.Date("2026-04-19"),7),
  country_name_short = rep("BGR", 7),
  country_name = rep("Bulgaria", 7),
  parlgov_id = c(
    "982",
    "1541",
    "2836",
    "2640", 
    "BG44",
    "Прогресивна",
    0
  )
)

post_2022_data <-bind_rows(
  post_2022,
  slovenia_2026,
  denmark_2026,
  hungary_2026, 
  bulgaria_2026) |> 
  arrange(country_name_short, party_name)

write_csv(post_2022_data, "Data/post_2022_data.csv")


missing_data_new <-  test2 |> 
  anti_join(ppeg_results, by = "party_id") |> 
  filter(party_id != "parl_gov")

missing_data_new_names <- missing_data_new |> pull(party_name)

missing_data_old <- old |> 
  filter(is.na(parlgov_id))
