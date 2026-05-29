library(tidyverse)

populist <- read_csv2("Data/The PopuList 4.0.csv")

arranged_check <- populist |> 
  arrange(country_name, desc(in_parliament), party_name) 


library(writexl)

write_xlsx(arranged_check, "/Users/lukefischer/Downloads/arranged_check.xlsx")


populist |> 
  select(party_name, 
         country_name, 
         party_name_english, 
         party_name_short, 
         populist:populist_endnobl) 

populist_cleaned

grouping_vars <- c("Populist", "Far-Right", "Far-Left", "Eurosceptic", "In Parliament")

c <- "Austria"




populist_cleaned |> 
  filter(Country == c) |> 
  select(`Party Name`, `Name En.`, Abbr., {{column}}, `In Parliament`) |> 
  filter(Populist != "") |> 
  rename(Classification = {{column}})

library(dplyr)
library(purrr)
library(reactable)


grouping_vars <- c("Populist", "Far-Right", "Far-Left", "Eurosceptic")

c <- "Croatia"

binded_result <- map_dfr(grouping_vars, function(var) {
  
  populist_cleaned |>
    filter(Country == c) |>
    select(`Party Name`, all_of(var), `In Parliament`) |>
    filter(.data[[var]] != "") |>
    rename("Status" = all_of(var), 
           "Party" = `Party Name`, 
            "Parliament" = `In Parliament`) |>
    mutate(Classification = var)  
})


reactable(
  binded_result,
  groupBy = "Classification", 
  columns = list(
    "Party" = 
      colDef(html = TRUE, width = 140, headerStyle = list(backgroundColor = "#f0f5f9")),
    "Status" = 
      colDef(html = TRUE, width = 140, headerStyle = list(backgroundColor = "#f0f5f9")),
    "Parliament" = 
      colDef(html = TRUE, width = 140, headerStyle = list(backgroundColor = "#f0f5f9")),
    "Classification" = 
      colDef(
        width = 140,
        headerStyle = list(backgroundColor = "#f0f5f9"),
        grouped = JS("function(cellInfo) {
        return cellInfo.value
      }")
      )),
  highlight = TRUE,
  sortable = TRUE,
  defaultExpanded = FALSE,
  defaultPageSize = 6,
  searchable = TRUE,
  theme = reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "8px 12px",
    style = list(
      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
    searchInputStyle = list(width = "100%")
  )
)




in_parliament <- populist_cleaned |>
  filter(Country == c, `In Parliament` == "●") |>
  select(`Party Name`, all_of(grouping_vars)) |> 
  mutate(`In Parliament` = "Yes")

not_parliament <- populist_cleaned |>
  filter(Country == c, `In Parliament` == "") |>
  select(`Party Name`, all_of(grouping_vars)) |> 
  mutate(`In Parliament` = "No")

country_party_summary <- bind_rows(in_parliament, not_parliament)

reactable(
  country_party_summary,
  groupBy = "In Parliament",
  highlight = TRUE,
  sortable = TRUE,
  defaultExpanded = FALSE,
  defaultPageSize = 6,
  searchable = TRUE,
  columns = list(
    "Populist" = 
      colDef(html = TRUE, width = 120, headerStyle = list(backgroundColor = "#f0f5f9")),
    "Far-Right" = 
      colDef(html = TRUE, width = 120, headerStyle = list(backgroundColor = "#f0f5f9")),
    "Far-Left" = 
      colDef(html = TRUE, width = 120, headerStyle = list(backgroundColor = "#f0f5f9")),
    "Eurosceptic" = 
      colDef(html = TRUE, width = 120, headerStyle = list(backgroundColor = "#f0f5f9")),
    "Party Name" = 
      colDef( width = 140, headerStyle = list(backgroundColor = "#f0f5f9")),
    "In Parliament" = 
      colDef(
        width = 120,
        headerStyle = list(backgroundColor = "#f0f5f9"),
        grouped = JS("function(cellInfo) {
        return cellInfo.value
      }")
      )),
  theme = reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5f9",
    cellPadding = "8px 12px",
    style = list(
      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
    searchInputStyle = list(width = "100%")
  )
)

far_right_populist <- populist_cleaned |> 
  filter(Country == c) |> 
  filter(Populist != "" & `Far-Right` != "") |> 
  select()



library(tidyverse)
populist_cleaned <- read_csv("/Users/lukefischer/Dropbox/The PopuList Repo/Data/processed_populist.csv")

test1 <- populist_cleaned |>   
  filter(
  if_any(
    c(Populist, `Far-Right`, `Far-Left`, Eurosceptic, `In Parliament`),
    ~ str_detect(as.character(.), fixed("("))
  )
) 

write_xlsx(test2, "/Users/lukefischer/Downloads/parties_with_bounds.xlsx")



test2<-populist |> 
  filter(
    populist_start > 1900 & populist_start != 2100 |
      populist_end < 2100 |
      populist_startnobl > 1900 & populist_startnobl != 2100 |
      populist_endnobl < 2100 |
      
      farright_start > 1900 & farright_start != 2100 |
      farright_end < 2100 |
      farright_startnobl > 1900 & farright_startnobl != 2100 |
      farright_endnobl < 2100 |
      
      farleft_start > 1900 & farleft_start != 2100 |
      farleft_end < 2100 |
      farleft_startnobl > 1900 & farleft_startnobl != 2100 |
      farleft_endnobl < 2100 |
      
      eurosceptic_start > 1900 & eurosceptic_start != 2100 |
      eurosceptic_end < 2100 |
      eurosceptic_startnobl > 1900 & eurosceptic_startnobl != 2100 |
      eurosceptic_endnobl < 2100
  ) |> 
  filter(
    !party_name %in% c("Forza Italia (1994-2009)", "Forza Italia (2013-)")
  )

test1 <- test1 |> arrange(`Party Name`)
test2 <- test2|> arrange(party_name)

library(writexl)
write_xlsx(test2, "/Users/lukefischer/Downloads/parties_with_bounds.xlsx")


namestest1 <- test1 |>  pull(`Party Name`)

namestest2 <- test2 |> pull(party_name)

namestest1==namestest2

