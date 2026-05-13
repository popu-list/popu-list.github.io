covered_parties <- data |> 
  count(party_name) |> 
  filter(party_name != "other")

all_parties<-populist_parties |> count(party_name, country_name)


uncovered_parties <- all_parties |> 
  anti_join(covered_parties, by = "party_name") |> 
  filter(party_name != "Forza Italia (2013-)") 
