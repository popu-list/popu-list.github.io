# ==========================================================
# 1. SETUP: Load Packages and Data
# ==========================================================
library(tidyverse)

populist <- read_csv2("/Users/lukefischer/Dropbox/The PopuList Repo/Data/The PopuList 4.0.csv")

# ==========================================================
# 2. DEFINITIONS: Date Formatting Functions
# ==========================================================

# Function to handle standard (non-borderline) date labeling
from_til_noborderline <- function(df, variable, start, end, start_nobl, end_nobl) {
  df |>
    mutate(
      {{variable}} := case_when(
        # From specific dare until now
        {{start_nobl}} != 1900 & {{start_nobl}} != 2100 & {{end_nobl}} == 2100 ~ 
          paste0(as.character({{variable}}), " (", {{start_nobl}}, "-)"),
        
        # From specific date until specific date
        {{start_nobl}} != 1900 & {{start_nobl}} != 2100 & {{end_nobl}} != 2100 ~ 
          paste0(as.character({{variable}}), " (", {{start_nobl}}, "-", {{end_nobl}}, ")"),
        
        # From beginning until specific date (after end -> borderline)
        {{start}} == 1900 & {{end}} == 2100 & {{end_nobl}} != 2100 ~ 
          paste0(as.character({{variable}}), " (", "-", {{end_nobl}}, ")"),
        
        # From beginning until specific date (after end -> neither borderline nor full)
        {{start}} == 1900 & {{end}} != 2100 & {{end_nobl}} == {{end}} ~ 
          paste0(as.character({{variable}}), " (", "-", {{end_nobl}}, ")"),
        
        # Fail safe, from beginning until specific end yet where it could be borderline after end
        {{start_nobl}} == 1900 & {{end_nobl}} != 2100 ~ 
          paste0(as.character({{variable}}), " (", "-", {{end_nobl}}, ")"),
        
        TRUE ~ as.character({{variable}})
      )
    )
}

# Function to handle borderline date labeling
from_til_borderline <- function(df, variable, start, end, start_nobl, end_nobl) {
  df |>
    mutate(
      {{variable}} := case_when(
        
        # borderline after the full period ends and borderline ends as well (2002-2008)
        {{variable}} == 1 & {{start_nobl}} != 1900 & {{start_nobl}} != 2100 & 
          {{end_nobl}} < {{end}} & {{end}} != 2100 ~ 
          paste0(as.character({{variable}})," (", {{end_nobl}}, "-", {{end}}, ")"), 
        
        # borderline from specific start that is not beginning of party until a specific time after which it is full (2002-2008)
        {{variable}} == 1 & {{start_nobl}} != 1900 & {{start_nobl}} != 2100 & 
          {{start}} < {{start_nobl}} & {{start}} != 1900 ~ 
          paste0(as.character({{variable}})," (",{{start}}, "-", {{start_nobl}}, ")"), 
        
        # only borderline from some starting year onward, with no non-borderline interval at all
        {{variable}} ==1 & {{start_nobl}} > {{start}} & 
          {{end}} == {{end_nobl}} & 
          {{start_nobl}} == 2100 & 
          {{start}} != 1900 ~ 
          paste0(as.character({{variable}})," (",{{start}}, "-)"), 
        
        # borderline from specific time onward - before it was no borderline
        {{variable}} ==1 & {{end_nobl}} < {{end}} & 
          {{start}} == {{start_nobl}} ~ 
          paste0(as.character({{variable}})," (",{{end_nobl}}, "-)"),
        
        # borderline until classification applies
        {{variable}} ==1 & {{start_nobl}} > {{start}} & 
          {{end}} == {{end_nobl}} & 
          {{end_nobl}} != 2100 ~ 
          paste0(as.character({{variable}})," (","-", {{start_nobl}}, ")"), 
        
        # borderline until classification applies,  but full classification is open ended
        {{variable}} ==1 & {{start_nobl}} > {{start}} & 
          {{end}} == {{end_nobl}} & 
          {{end_nobl}} == 2100 & 
          {{start_nobl}} != {{end_nobl}}~ 
          paste0(as.character({{variable}})," (","-", {{start_nobl}}, ")"),
        
        # Was only ever borderline from specific start date but not anymore
        {{variable}} == 1 & {{start_nobl}} > {{start}} & 
          {{start}} != 1900 &
          {{end}} < {{end_nobl}} & 
          {{end_nobl}} == 2100 & 
          {{start_nobl}} == 2100 ~ 
          paste0(as.character({{variable}})," (",{{start}}, "-", {{end}}, ")"),
        
        # Was only ever borderline since founding but not anymore
        {{variable}} == 1 & {{start_nobl}} > {{start}} & 
          {{start}} == 1900 &
          {{end}} < {{end_nobl}} & 
          {{end_nobl}} == 2100 & 
          {{start_nobl}} == 2100 ~ 
          paste0(as.character({{variable}})," (", "-", {{end}}, ")"),
        
        # classification is always borderline, no time interval
        {{variable}} ==1 & {{start_nobl}} == 2100 & 
          {{start_nobl}} == {{end_nobl}} & 
          {{start}} == 1900 & 
          {{end}} == 2100 ~ as.character({{variable}}),
        TRUE ~ as.character({{variable}})
      )
    )
}

# ==========================================================
# 3. PROCESSING: Apply Date Functions to Ideologies
# ==========================================================

# Populism
populist <- from_til_noborderline(populist, populist, populist_start, populist_end, populist_startnobl, populist_endnobl) 
populist <- from_til_borderline(populist, populist_bl, populist_start, populist_end, populist_startnobl, populist_endnobl) 

# Far-right
populist <- from_til_noborderline(populist, farright, farright_start, farright_end, farright_startnobl, farright_endnobl) 
populist <- from_til_borderline(populist, farright_bl, farright_start, farright_end, farright_startnobl, farright_endnobl) 

# Far-left
populist <- from_til_noborderline(populist, farleft, farleft_start, farleft_end, farleft_startnobl, farleft_endnobl) 
populist <- from_til_borderline(populist, farleft_bl, farleft_start, farleft_end, farleft_startnobl, farleft_endnobl) 

# Euroskeptic
populist <- from_til_noborderline(populist, eurosceptic, eurosceptic_start, eurosceptic_end, eurosceptic_startnobl, eurosceptic_endnobl) 
populist <- from_til_borderline(populist, eurosceptic_bl, eurosceptic_start, eurosceptic_end, eurosceptic_startnobl, eurosceptic_endnobl) 

# ==========================================================
# 4. CLEANING: Selection and Manual Fixes
# ==========================================================

populist_cleaned <- populist |> 
  select(country_name, party_name, party_name_english, party_name_short, populist, populist_bl, farright, farright_bl, farleft, farleft_bl, eurosceptic, eurosceptic_bl, in_parliament) |> 
  mutate(populist = case_when(
    party_name_short == "FI1" ~ "1", 
    TRUE ~ populist
  ), 
  populist_bl = case_when(
    party_name_short == "FI1" ~ "1", 
    TRUE ~ populist_bl
  ), 
  eurosceptic_bl = case_when(
    party_name_short == "FI1" ~ "0", 
    party_name_short == "FI2" ~ "1", 
    TRUE ~ eurosceptic_bl
  )
  )

columns <- c("populist", "populist_bl", "farright", "farright_bl", "farleft", "farleft_bl", "eurosceptic", "eurosceptic_bl")

populist_cleaned <- populist_cleaned |>
  mutate(
    across(
      all_of(columns),
      ~ str_replace(.x, "^0 \\(", "1 (")
    )
  )

populist_cleaned <- populist_cleaned |>
  arrange(country_name, desc(in_parliament), party_name)

# ==========================================================
# 5. VISUALS: Replace Numeric Indicators with Symbols
# ==========================================================

populist_cleaned <- populist_cleaned |> 
  mutate(populist = str_replace(populist, "^1", "●"), 
         populist_bl = str_replace(populist_bl, "^1", "◐"), 
         farright = str_replace(farright, "^1", "●"), 
         farright_bl = str_replace(farright_bl, "^1", "◐"), 
         farleft = str_replace(farleft, "^1", "●"), 
         farleft_bl = str_replace(farleft_bl, "^1", "◐"), 
         eurosceptic = str_replace(eurosceptic, "^1", "●"), 
         eurosceptic_bl = str_replace(eurosceptic_bl, "^1", "◐"), 
         in_parliament = str_replace(in_parliament, "^1", "●"), 
         populist = str_replace(populist, "^0", ""), 
         populist_bl = str_replace(populist_bl, "^0", ""), 
         farright = str_replace(farright, "^0", ""), 
         farright_bl = str_replace(farright_bl, "^0", ""), 
         farleft = str_replace(farleft, "^0", ""), 
         farleft_bl = str_replace(farleft_bl, "^0", ""), 
         eurosceptic = str_replace(eurosceptic, "^0", ""), 
         eurosceptic_bl = str_replace(eurosceptic_bl, "^0", ""), 
         in_parliament = str_replace(in_parliament, "^0", "")) 

# ==========================================================
# 6. FINAL TABLE FORMATTING: Merging and Renaming
# ==========================================================

# Helper function to merge borderline and non-borderline columns for display
clean_table <- function(df, variable, variable_bl) {
  df |> 
    mutate(
      {{variable}} := case_when(
        str_detect({{variable}}, "^●$") & str_detect({{variable_bl}}, "^◐$") ~ {{variable_bl}},
        str_detect({{variable}}, "^●$") & str_detect({{variable_bl}}, "^◐ \\(") ~ {{variable_bl}},
        str_detect({{variable}}, "^● \\(") & str_detect({{variable_bl}}, "^◐ \\(") ~ paste0({{variable}}, "<br>", {{variable_bl}}),
        TRUE ~ {{variable}}
      )
    ) |> 
    select(!{{variable_bl}})
}

populist_cleaned <- clean_table(populist_cleaned, populist, populist_bl) 
populist_cleaned <- clean_table(populist_cleaned, farright, farright_bl) 
populist_cleaned <- clean_table(populist_cleaned, farleft, farleft_bl) 
populist_cleaned <- clean_table(populist_cleaned, eurosceptic, eurosceptic_bl) 

# Final column naming
names <- c("Country", "Party Name", "Name En.", "Abbr.", "Populist", "Far-Right", "Far-Left", "Eurosceptic", "In Parliament")
colnames(populist_cleaned) <- names

populist_cleaned <- populist_cleaned |> 
  relocate("Far-Right", .before = "Far-Left")

# Save for Dashboard and PDF
write_csv(populist_cleaned, "Data/processed_populist.csv")


