library(tidyverse)

# download and read Party Facts mapping table
file_name <- "Data/partyfacts-mapping.csv"
if( ! file_name %in% list.files()) {
  url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  download.file(url, file_name)
}
partyfacts_raw <- read_csv(file_name, guess_max = 50000)
partyfacts <- partyfacts_raw |> filter(! is.na(partyfacts_id))

partyfacts |> select(country, name, partyfacts_id) |> view()

write_csv(partyfacts, "Data/partyfacts_raw.csv")



# link datasets (select only linked parties)
dataset_1 <- partyfacts |> filter(dataset_key == "manifesto")
dataset_2 <- partyfacts |> filter(dataset_key == "parlgov")
link_table <-
  dataset_1 |>
  inner_join(dataset_2, by = c("partyfacts_id" = "partyfacts_id"))

# write results into file with dataset names in file name
file_out <- "Data/partyfacts-linked.csv"
write_csv(link_table, file_out)
