
library(tidyverse) 
 
pre_2025_parlgov <- read_csv('/Users/lukefischer/Downloads/dataverse_files (1)/view_election.csv')

populist <- read_csv2("Data/The PopuList 4.0.csv")

populist_countries <- unique(populist |> pull(country_name))


country_codes <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
  "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ITA",
  "LVA", "LTU", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU",
  "SVK", "SVN", "ESP", "SWE", "CHE", "GBR"
)

pre_2025_parlgov <- pre_2025_parlgov |> 
  filter(election_date >= as.Date("1988-12-31")) |> 
  filter(country_name_short %in%country_codes) |> 
  filter(election_type == "parliament")

pre_2025_parlgov %>% group_by(country_name) %>% slice_min(election_date) %>% arrange(desc(election_date)) # Croatian election Data must be added manually for before 2000

pre_2025_parlgov <- pre_2025_parlgov |> 
  select(party_id, vote_share, election_date) # Join key later: ID

manual_data <- read_csv2("/Users/lukefischer/Desktop/manual_election_data.csv")

manual_data <- manual_data |> 
  mutate(election_date = as.Date(election_date, format = "%d.%m.%y")) |> 
  mutate(vote_share = vote_share/100) 

# Manual addition Croatia before 2000 because it was missing from parlgov

croatia_manual <- tibble(
  party_name = c(
    "Hrvatska Demokratska Zajednica", 
    "Hrvatska Demokratska Zajednica",
    "Hrvatska Stranka Prava", 
    "Hrvatska Demokratska Zajednica",
    "Hrvatska Stranka Prava"),
  vote_share = c(
    0.4193,
    0.4471,
    0.0707,
    0.4523, 
    0.0501
  ), 
  election_date = as.Date(c("1990-04-22", "1992-08-2", "1992-08-2", "1995-10-29", "1995-10-29")), 
  country_name = rep("Croatia", 5)
)



# Manual addition of 2026 data
# Manual Addition of 2026 data up until PopuList 4.0 release
# SOURCES: 
# https://volitve.dvk-rs.si/dz2026/#/rezultati


slovenia_2026 <- tibble(
  party_name = c(
    "Slovenska Nacionalna Stranka", 
    "Nova Slovenija – Krščanski Demokrati",
    "Slovenska Demokratska Stranka",
    "Levica",# left running with greens
    "Resni.ca"),
  vote_share = c(
    0.0224,
    0.0926,
    0.2788,
    0.0569,
    0.0549
  ), 
  election_date = rep(as.Date("2026-03-22"), 5), 
  country_name = rep("Slovenia", 5)
)


# https://www.dr.dk/nyheder/politik/folketingsvalg/resultater
denmark_2026 <- tibble(
  party_name = c(
    "Socialistisk Folkeparti",
    "Dansk Folkeparti",
    "Enhedslisten – De Rød-Grønne",
    "Borgernes Parti",
    "Danmarksdemokraterne"
  ),
  vote_share = c(
    0.116,
    0.091,
    0.063,
    0.021,
    0.058
  ),
  election_date = rep(as.Date("2026-03-26"),5),
  country_name = rep("Denmark", 5),
)


# Hungary: https://www.valasztas.hu/home

hungary_2026 <- tibble(
  party_name = c(
    "Fidesz",
    "Mi Hazánk Mozgalom"
  ),
  vote_share = c(
    0.3861,
    0.0563
  ),
  election_date = rep(as.Date("2026-04-12"),2),
  country_name = rep("Hungary", 2)
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
    "MECh"
  ),
  vote_share = c(
    0.03017,
    0.13387,
    0.0736,
    0.04257,
    0.03104,
    0.44594, 
    0.032
  ),
  election_date = rep(as.Date("2026-04-19"),7),
  country_name = rep("Bulgaria", 7)
)


manual_data_complete <-bind_rows(
  croatia_manual,
  manual_data,
  slovenia_2026,
  denmark_2026,
  hungary_2026, 
  bulgaria_2026) |> 
  arrange(country_name, party_name) |> 
  select(-country_name)

populist <- read_csv2("Data/The PopuList 4.0.csv")

populist <- populist |> 
  filter(party_name != "Forza Italia (2013-)") |> 
  mutate(parlgov_id = if_else(party_name == "Latvija Pirmajā Vietā", 2876, parlgov_id), 
         parlgov_id = if_else(party_name == "Katram un Katrai", 2880, parlgov_id), 
         parlgov_id = if_else(party_name == "Stabilitāte!", 2875, parlgov_id), 
         parlgov_id = if_else(party_name =="Povežimo Slovenijo", 2884, parlgov_id), 
         parlgov_id = if_else(party_name =="Partidul Socialist Democrat", 1120, parlgov_id), 
         parlgov_id = if_else(party_name == "Σπαρτιάτες", 2902, parlgov_id), 
         parlgov_id = if_else(party_name_english == "Democratic Patriotic Movement NIKI", 2901, parlgov_id), 
         parlgov_id = if_else(party_name == "Suverēnā Vara", 2882, parlgov_id), 
         parlgov_id = if_else(party_name == "Reconquete", 2860, parlgov_id), 
         parlgov_id = if_else(party_name == "Kommunistische Partei Österreichs Plus",769, parlgov_id), 
         parlgov_id = if_else(party_name == "Πλεύση Ελευθερίας - Ζωή Κωνσταντοπούλου", 2596, parlgov_id))

populist <- populist |>
  bind_rows(
    populist |>
      filter(party_name == "Fidesz") |>
      mutate(parlgov_id = 437)
  ) |> 
  arrange(country_name,party_name ) 

data_pre_2024 <- populist |> 
  left_join(pre_2025_parlgov, by = c("parlgov_id" = "party_id")) |> 
  filter(!is.na(election_date)) |> 
  filter(!is.na(vote_share)) 

data_post_2023 <- manual_data_complete |> 
  left_join(populist, by = "party_name") |> 
  mutate(vote_share = vote_share*100)

data <- bind_rows(data_pre_2024, data_post_2023)


data <- data |> 
  arrange(country_name, party_name)

data <- data |> # get rid of fidez duplicate
  slice(-396)

data$year <- str_extract(data$election_date, "^.{4}")
data$year <- as.numeric(data$year)

data <- data |> 
  mutate(election_year = format(election_date, "%Y")) |> 
  group_by(country_name, election_year) |> 
  mutate(n_elections = dense_rank(election_date)) |> 
  filter(n_elections == max(n_elections)) |> 
  ungroup() |> 
  select(-c(n_elections, election_year)) 


# Check missing party information

all_parties <- populist |> count(party_name) |> select(-n)

parties_data <- data |> count(party_name)|> select(-n)


all_parties |> 
  anti_join(parties_data) 

# add filler data for election years
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

ppeg_results <- ppeg_results |> 
  filter(
    edate > as.Date("1988-12-31"))


# remove duplicate elections
ppeg_results <- ppeg_results |> 
  mutate(election_year = format(edate, "%Y")) |> 
  group_by(iso3c, election_year) |> 
  mutate(n_elections = dense_rank(edate)) |> 
  filter(n_elections == max(n_elections)) |> 
  ungroup() |> 
  select(-c(n_elections, election_year))

ppeg_results$year <- str_extract(ppeg_results$edate, "^.{4}")
ppeg_results$year <- as.numeric(ppeg_results$year)

# Make sure all election years are present
election_fill<-ppeg_results |> 
  count(iso3c, cname_en, year) |> 
  select(-n, -iso3c) |> 
  mutate(party_name = rep("other", 307)) |> 
  rename("country_name" = cname_en)


# Fill up each election year with "other"; this also makes sure that parties countries with no parties in populist are taken into account
data <- bind_rows(data, election_fill) |> 
  arrange(country_name, year)


data$country_name <- ifelse(data$country_name=="Czech Republic", "Czech_Republic", data$country_name)
data$country_name <- ifelse(data$country_name=="United Kingdom", "United_Kingdom", data$country_name)


# Now add others with ppeg others

# Take into account the time dynamic: election year has to be after start and before end
data <- data %>% 
  mutate(populist = ifelse(year >= populist_start & year <= populist_end, 1,0),
         farright = ifelse(year >= farright_start & year <= farright_end, 1,0),
         farleft = ifelse(year >= farleft_start & year <= farleft_end, 1,0),
         eurosceptic = ifelse(year >= eurosceptic_start & year <= eurosceptic_end, 1,0))

data <- data |> 
    mutate(populist = if_else(year<populist_startnobl | year > populist_endnobl, 0, populist), 
           farright = if_else(year<farright_startnobl | year > farright_endnobl, 0, farright), 
        farleft = if_else(year<farleft_startnobl | year > farleft_endnobl, 0, farleft), 
         eurosceptic = if_else(year<eurosceptic_startnobl | year > eurosceptic_endnobl, 0, eurosceptic)) 

# recode NA to 0
# this is important for the coding of "other parties"
data$populist[is.na(data$populist)] <- 0
data$farright[is.na(data$farright)] <- 0
data$farleft[is.na(data$farleft)] <- 0

# Create aggregated dataset for plotting
# there is probably a much cleaner way of doing this, but I haven't found it
# Create the Party types for coding scheme 1
# 1. Farright (only)
right <- data %>% filter(farright==1 & populist==0) %>% group_by(country_name, year) %>%
  summarise(right_votes=sum(vote_share, na.rm = T)) |> 
  ungroup()
# 2. Farright Populist
right_populist <- data %>% filter(farright==1 & populist==1) %>% group_by(country_name, year) %>%
  summarise(right_populist_votes=sum(vote_share, na.rm = T))|> 
  ungroup()
# 3. Farleft (only)
left <- data %>% filter(farleft==1 & populist==0) %>% group_by(country_name, year) %>%
  summarise(left_votes=sum(vote_share, na.rm = T))|> 
  ungroup()
# 4. Farleft Populist
left_populist <- data %>% filter(farleft==1 & populist==1) %>% group_by(country_name, year) %>%
  summarise(left_populist_votes=sum(vote_share, na.rm = T))|> 
  ungroup()
# 5. Populist (not farright or farleft)
populist <- data %>% filter(populist==1 & farright==0 & farleft==0) %>% group_by(country_name, year) %>%
  summarise(populist_votes=sum(vote_share, na.rm = T))|> 
  ungroup()
# Other Parties: I include these, simply to retain data for every election
# Otherwise, if there is no populist party, we lose the data for that election
# this is problematic, because the country wouldn't count into mean, although it should
# be counted as 0 (which is important)
other <- data %>% filter(populist==0 & farright==0 & farleft==0) %>% group_by(country_name, year) %>%
  summarise(other_votes=sum(vote_share, na.rm = T))|> 
  ungroup()

other <- other |> filter(!is.na(country_name))

# join these together
P3 <- full_join(right, right_populist) %>% full_join(left) %>% full_join(left_populist) %>%
  full_join(populist) %>% full_join(other)

P3 <- P3 %>% arrange(country_name, desc(-year)) |> ungroup()
#rm(right, right_populist, left, left_populist, populist, other)

# recode NA to 0
P3[is.na(P3)] <- 0

# Create Coding Scheme 2
# This creates the three category scheme used on the maps and for one line plot
P3$c2_left_votes <- P3$left_populist_votes + P3$left_votes
P3$c2_right_votes <- P3$right_populist_votes + P3$right_votes
P3$c2_populist_votes <- P3$right_populist_votes + P3$left_populist_votes + P3$populist_votes


# "Wait" until every country has held one election
# this code is just here to find that year
# that is 1993 (except croatia)

# rename to country
colnames(P3)[1] <- "country_name"

# ungroup
P3<- ungroup(P3)


# For the barplot, I need a filled Dataset (The year after an election still needs to have the same vote share)
# create "fillable" dataset
# this adds each year for each country, so far we only have the election years
P <- left_join(expand.grid(year=c(1989:2026), country_name=unique(P3$country_name)), P3,
               by=c("year" = "year", "country_name" = "country_name"))



# fill data
# Note: I need to group by countries, otherwise the last values of another country
# appear before the first election
# fill all variables which I need for this plot
P <- P %>% group_by(country_name) %>% fill(right_votes, right_populist_votes, left_votes,
                                           left_populist_votes, populist_votes, other_votes, c2_populist_votes)

# now subset to years in which every country held an election
# croatia only has data from 2000 on, otherwise all countries had elections until 1993
P <- P %>% filter(year>=1993)


P |> 
  select(country_name, year, populist_votes) |> 
  pivot_wider(
    names_from = year,
    values_from = populist_votes,
    id_cols = country_name
  ) |> 
  ungroup() |> 
  view()


#################################################
### add population sizes
#################################################

# recreate country code to merge with population data
library(countrycode)
P$country_code <- countrycode(P$country_name, origin = "country.name", destination = "iso3c")

# read dataset
# population dataset obtained from world bank: https://data.worldbank.org/indicator/SP.POP.TOTL?most_recent_year_desc=false&view=map
populations <- readxl::read_excel("Data/population_2024.xls")

populations <- populations |> 
  slice(-c(1:2)) |> 
  rename_with(~ as.character(populations[3, ])) |> 
  slice(-1)

#Add up until latest year if necessary
populations <- populations |> 
  mutate(`2026` = NA)

# remove blanks in colnames
colnames(populations) <- str_replace_all(colnames(populations), " ", "_")
# remove unnecessary columns
populations[,c("Indicator_Name", "Indicator_Code", "Country_Name")] <- NULL

#filter to our countries
countries <- unique(P$country_code)
populations <- filter(populations, Country_Code %in% countries)
#rm(countries)

# change to long format
populations <- populations %>% pivot_longer(cols = !c(Country_Code),
                                            names_to = "year", values_to = "pop")
# filter to 1989
populations <- filter(populations, year>=1989)
populations$year <- as.numeric(populations$year)
populations$pop <- as.numeric(populations$pop)

# Fill to last year
populations <- populations %>% fill(pop)

# compute total population per country per year
populations <- populations %>% group_by(year) %>% mutate(total_population=sum(pop))
# compute population share of total sample, i.e. the country weight
populations <- populations %>% group_by(Country_Code, year) %>% mutate(weight=pop/total_population)

# join the datasets
P <- P %>% left_join(populations,
                     by = c("country_code"="Country_Code",
                            "year" = "year"))

write_csv(P, "Data/P.csv")
#################################################################################


# create dataset to plot
# computes the total vote share and uses the population weights
# uses weighted mean function
# round data by 2 decimal
G <- P %>% group_by(year) %>% summarise(
  right_votes=round(weighted.mean(right_votes, weight, na.rm=T),2),
  right_populist_votes=round(weighted.mean(right_populist_votes, weight, na.rm=T),2),
  left_votes=round(weighted.mean(left_votes, weight, na.rm=T),2),
  left_populist_votes=round(weighted.mean(left_populist_votes, weight, na.rm=T),2),
  populist_votes=round(weighted.mean(populist_votes, weight, na.rm=T),2)
)

# make long dataset
G_long <- gather(G, party, vote_share, 
                 right_votes:populist_votes, factor_key=TRUE)

# change names
G_long <- G_long %>%
  mutate(party = case_when(party == "left_votes" ~ "far-left",
                           party == "left_populist_votes" ~ "far-left populist",
                           party == "populist_votes" ~ "populist",
                           party == "right_populist_votes" ~ "far-right populist",
                           party == "right_votes" ~ "far-right"))

write_csv(G_long, "Data/G_long.csv")
