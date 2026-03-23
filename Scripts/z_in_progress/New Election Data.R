test <- read_csv("/Users/lukefischer/Downloads/ppeg_parl_2025v1/ppeg_parl_2025v1.csv")

test <- test %>% filter(cname_en %in% countries)

test |> 
  count(cname_en)

test$year <- str_extract(test$edate, "^.{4}")
test$year <- as.numeric(test$year)