
# INTRO

# Clear all variables
rm(list = ls(all = TRUE))

# Load packages
library(readxl)
library(tidyverse)
library(rio)

# Set working directory
#setwd("") # set your wd here

# Import data: PopuList and Parlgov
populist <- read_csv2("Data/The PopuList 3.0.csv")

elections <- read_csv("Data/view_election.csv") # Download from https://www.parlgov.org/data-info/

elections_post_2022 <- read_csv("Data/post_2022_data.csv") |> 
  select(-party_name) |> 
  rename(party_id = parlgov_id) |> 
  mutate(vote_share = 100*vote_share)

elections_new <- elections |> 
  bind_rows(elections_post_2022) |> 
  arrange(country_name_short, party_id)



# Get populist parties and corresponding information
populist_parties <- populist |> 
  ungroup() |> 
  count(party_name, party_name_short, country_name, partyfacts_id, parlgov_id) |> 
  select(-n) |> 
  arrange(country_name) 

# join: adds populist info to the respective cases in parlov
data <- left_join(elections_new, select(populist, populist:parlgov_id), by=c("party_id"="parlgov_id"))

populist_parties_elections <- left_join(populist_parties, elections, by=c("parlgov_id"="party_id"))



# create year variable
populist_parties_elections$year <- str_extract(populist_parties_elections$election_date, "^.{4}")
populist_parties_elections$year <- as.numeric(populist_parties_elections$year)


data$year <- str_extract(data$election_date, "^.{4}")
data$year <- as.numeric(data$year)

data <- data |> 
  mutate(election_type = if_else(is.na(election_type), "parliament", election_type))


# filter to national elections and data after 1990
data <- data %>% 
  filter(year>1989, 
         election_type=="parliament")

populist_parties_elections <- populist_parties_elections |> 
  filter(year > 1989, # 2015
         election_type=="parliament") |> 
  count(party_name.y, party_name_short.y, party_name_short.x, country_name_short, parlgov_id) |> 
  select(-n) |> 
  arrange(country_name_short) |> 
  rename(
    "Party" = party_name.y, 
    "Name_Short_y" = party_name_short.y, 
    "Name_Short_x" =party_name_short.x,  
    "CountryCode" = country_name_short
  )

write_csv(populist_parties_elections, "Data/populist_parlgov_id.csv")


# Problem: If a country had multiple elections per year, they stack (vote share adds up)
# Create a variable that indicates how many elections per year were held per country
# technically this variable is not needed, but useful to understand
#data <- data %>% group_by(year, country_name) %>% mutate(n_elections=n_distinct(election_id))
# create variable indicating whether a given election is the last in that year,
#data <- data %>% group_by(year, country_name) %>% 
  #mutate(last_election = if_else(election_id==max(election_id),1,0))

data <- data |> 
  mutate(election_year = format(election_date, "%Y")) |> 
  group_by(country_name_short, election_year) |> 
  mutate(n_elections = dense_rank(election_date)) |> 
  filter(n_elections == max(n_elections)) |> 
  ungroup() |> 
  select(-c(n_elections, election_year)) 

# filter by this variable
#data <- data %>% filter(last_election==1)

# filter to countries covered by PopuList
country_codes <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
  "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ITA",
  "LVA", "LTU", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU",
  "SVK", "SVN", "ESP", "SWE", "CHE", "GBR"
)

data <- data %>% filter(country_name_short %in% country_codes)
rm(elections, populist, countries)

# change country name in case of blank
data$country_name <- ifelse(data$country_name=="Czech Republic", "Czech_Republic", data$country_name)
data$country_name <- ifelse(data$country_name=="United Kingdom", "United_Kingdom", data$country_name)


# Take into account the time dynamic: election year has to be after start and before end
data <- data %>% 
  mutate(populist = ifelse(year >= populist_start & year <= populist_end, 1,0),
         farright = ifelse(year >= farright_start & year <= farright_end, 1,0),
         farleft = ifelse(year >= farleft_start & year <= farleft_end, 1,0),
         eurosceptic = ifelse(year >= eurosceptic_start & year <= eurosceptic_end, 1,0))


# recode NA to 0
# this is important for the coding of "other parties"
data$populist[is.na(data$populist)] <- 0
data$farright[is.na(data$farright)] <- 0
data$farleft[is.na(data$farleft)] <- 0

# Fill missing party names
data <- data |> 
  group_by(party_id) |> 
  fill(party_name_short, party_name) 

# Create aggregated dataset for plotting
# there is probably a much cleaner way of doing this, but I haven't found it
# Create the Party types for coding scheme 1
# 1. Farright (only)
right <- data %>% filter(farright==1 & populist==0) %>% group_by(country_name, year) %>%
  summarise(right_votes=sum(vote_share, na.rm = T))
# 2. Farright Populist
right_populist <- data %>% filter(farright==1 & populist==1) %>% group_by(country_name, year) %>%
  summarise(right_populist_votes=sum(vote_share, na.rm = T))
# 3. Farleft (only)
left <- data %>% filter(farleft==1 & populist==0) %>% group_by(country_name, year) %>%
  summarise(left_votes=sum(vote_share, na.rm = T))
# 4. Farleft Populist
left_populist <- data %>% filter(farleft==1 & populist==1) %>% group_by(country_name, year) %>%
  summarise(left_populist_votes=sum(vote_share, na.rm = T))
# 5. Populist (not farright or farleft)
populist <- data %>% filter(populist==1 & farright==0 & farleft==0) %>% group_by(country_name, year) %>%
  summarise(populist_votes=sum(vote_share, na.rm = T))
# Other Parties: I include these, simply to retain data for every election
# Otherwise, if there is no populist party, we lose the data for that election
# this is problematic, because the country wouldn't count into mean, although it should
# be counted as 0 (which is important)
other <- data %>% filter(populist==0 & farright==0 & farleft==0) %>% group_by(country_name, year) %>%
  summarise(other_votes=sum(vote_share, na.rm = T))

# join these together
P3 <- full_join(right, right_populist) %>% full_join(left) %>% full_join(left_populist) %>%
  full_join(populist) %>% full_join(other)
P3 <- P3 %>% arrange(country_name, desc(-year))
rm(right, right_populist, left, left_populist, populist, other)

# recode NA to 0
P3[is.na(P3)] <- 0

# Create Coding Scheme 2
# This creates the three category scheme used on the maps and for one line plot
P3$c2_left_votes <- P3$left_populist_votes + P3$left_votes
P3$c2_right_votes <- P3$right_populist_votes + P3$right_votes
P3$c2_populist_votes <- P3$right_populist_votes + P3$left_populist_votes + P3$populist_votes

# Add party names for Map Hoverinfo
# The maps contain Hoverinfo on the current parties and their vote share
# These variables are created here
right_parties <- data %>% filter(farright==1) %>% group_by(country_name, year) %>%
  summarise(farright_parties=paste0(party_name_short, " ", vote_share, "%",collapse = "\n"))
left_parties <- data %>% filter(farleft==1) %>% group_by(country_name, year) %>%
  summarise(farleft_parties=paste0(party_name_short, " ", vote_share, "%",collapse = "\n"))
pop_parties <- data %>% filter(populist==1) %>% group_by(country_name, year) %>%
  summarise(populist_parties=paste0(party_name_short, " ", vote_share, "%",collapse = "\n"))

# join together
P3 <- P3 %>% left_join(right_parties) %>% left_join(left_parties) %>% left_join(pop_parties)

# set parties to none if NA, otherwise problems occur when filling the data for the maps
P3$farleft_parties <- if_else(is.na(P3$farleft_parties), "none", P3$farleft_parties)
P3$farright_parties <- if_else(is.na(P3$farright_parties), "none", P3$farright_parties)
P3$populist_parties <- if_else(is.na(P3$populist_parties), "none", P3$populist_parties)

# if I simply use the data from 1989 onwards, the problem occurs that some countries already
# held elections and other did not, so the mean value is heavily skewed, as many countries
# are still NA
# Solution: "Wait" until every country has held one election
# this code is just here to find that year
P3 %>% group_by(country_name) %>% slice_min(year) %>% arrange(desc(year))
# that is 1993 (except croatia)

# rename to country
colnames(P3)[1] <- "country"

# ungroup
P3<- ungroup(P3)

rm(data, left_parties, pop_parties, right_parties)

# For the barplot, I need a filled Dataset (The year after an election still needs to have the same vote share)
# create "fillable" dataset
# this adds each year for each country, so far we only have the election years
P <- left_join(expand.grid(year=c(1989:2026), country=unique(P3$country)), P3,
               by=c("year" = "year", "country" = "country"))

# fill data
# Note: I need to group by countries, otherwise the last values of another country
# appear before the first election
# fill all variables which I need for this plot
P <- P %>% group_by(country) %>% fill(right_votes, right_populist_votes, left_votes,
                                      left_populist_votes, populist_votes, other_votes, c2_populist_votes)

# now subset to years in which every country held an election
# croatia only has data from 2000 on, otherwise all countries had elections until 1993
P <- P %>% filter(year>=1993)


#################################################
### add population sizes
#################################################

# recreate country code to merge with population data
library(countrycode)
P$country_code <- countrycode(P$country, origin = "country.name", destination = "iso3c")

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
rm(countries)

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

# save csv
write_csv(G_long, "Data/G_long.csv")

