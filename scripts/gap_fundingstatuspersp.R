################################################################################
# Purpose: Wrangling data from funding status per supply plan in the GFPVAN 
#           for quarterly commodity funding analysis
# Author: Arati Krishnamoorthy
# Version Control:
#    - June 2023  restyling per tidyverse style guide linked in epir
#                 remove reference to magrittr as it is included in tidyverse
#                 replace the magrittr pipe with the base R pipe |> 
#                 use rio package for import/export
#                 use janitor to for data cleaning
#                 use across to perform operations across multiple columns
#                 use rename_with to rename columns using a formula
#                 Changed PRH country tiers
################################################################################

#### General notes for R ####

# use installr::updateR() to update R
# use sessioninfo::session_info() to get a list of packages used with versions
# use reprex() to replicate code for problem solving
# to install a package from github use pacman -
# install.packages("pacman") 
# pacman::p_load_gh("appliedepi/epirhandbook") 
#this will install from https://github.com/appliedepi/epirhandbook and load
# note p_load_gh will install if necessary and load
# note p_load will both install (if necessary) and load a package if it is on CRAN
# TinyTeX (for compiling an RMarkdown document to PDF)
# pacman::p_load("styler") #styler is a package to restyle existing code

devtools::install_github("ekothe/trackdown")
library(googledrive, trackdown)
#### Load packages ####

pacman::p_load("tidyverse",    # p_load will both install (if necessary) and load a package if it is on CRAN
               "here",         # relative file paths
               "rio",          # import/export
               "janitor",      # tables and data cleaning
             # "matchmaker",   # dictionary-based cleaning
               "jsonlite",
               "httr")
                 
#### Data import ####

supplyplan_raw <- import(file = here("raw_data", "supplyplan.xlsx"),
                         na = c("", " ","NA"))

#### Data wrangling - cleaning column names ####

glimpse(supplyplan_raw) # explore

#clean column names using janitor
supplyplan <- supplyplan_raw |> 
  clean_names() |>  
  glimpse() 

#changing column class
supplyplan <- supplyplan |> 
  mutate(across(.cols = where(is.POSIXct), .fns = as.Date)) |> 
  glimpse()

supplyplan <- supplyplan |>  
  # rename columns
  rename(   
    # NEW       = OLD
    country     = country_name,
    supply_plan = supply_plan_description) |> 
  # create new columns
  mutate(across(ends_with("_supply_plan"), 
                ~ .*estimated_unit_value, 
              # ~ means there is a function without a name
              # tidyverse now recommends \(x) x instead of ~.
              # period refers to the cols selected in the previous line
                .names = "value_{str_remove(.col,'_supply_plan')}"))


### Data wrangling - pivot longer to make supplyplan tidy ####
# funding status needs to be a column, not part of a column name

supplyplan <- supplyplan |> 
  # rename the columns that need to be gathered
    rename_with( ~ paste0("quantity_", str_remove(.x, "_supply_plan")) ,
              .cols = ends_with("_supply_plan")) |> 
  # then pivot_longer to gather
  pivot_longer(
    cols = starts_with("quantity_") | starts_with("value_"),
    names_to = c(".value", "funding_status"),
    names_sep = "_",
    values_drop_na = TRUE
  ) |> 
  glimpse()

#### Data Wrangling - Analyzing categorical columns ####

skimr::skim(supplyplan)

# basic categorical check
unique(supplyplan$country)
unique(supplyplan$supply_plan)
unique(supplyplan$l3_method)
unique(supplyplan$l5_product)
unique(supplyplan$collab_review_status) # note per Maggie - is used for a product that is no longer managed
unique(supplyplan$supplier)
unique(supplyplan$funding_source)
unique(supplyplan$funding_status)

supplyplan |>
  select(country, supply_plan,l3_method, collab_review_status) |>
  distinct() |>
  print(n = 100)

# Check if there are unfunded lines that have a source.
supplyplan |>
  filter(
    funding_status == "unfunded",
    collab_review_status == "Ready To Use Approved"
  ) |>
  select(country, funding_source, funding_status) |>
  distinct(country, funding_source, funding_status)

sort(unique(supplyplan$funding_source))

# clean up funding source
supplyplan <- supplyplan |>
  mutate(funding_source = str_replace(
    string = funding_source,
    pattern = "To be determined|To Be Determined|ToBeDetermined|None Selected|Unknown",
    replacement = "TBD"
  ))

#not including this in the previous TBD replacement as that would result in all - being replaced with TBD even if they are a part of a name
supplyplan <- supplyplan |> 
  mutate(funding_source = case_when((funding_source == "-") ~ "TBD", 
                                     TRUE ~ funding_source))

# Check funding status for funding source TBD
supplyplan |>
  filter(
    funding_source == "TBD",
    collab_review_status == "Ready To Use Approved|In Review|Ready to Use Estimate"
  ) |>
  select(country, funding_source, funding_status) |>
  distinct(country, funding_source, funding_status)

sort(unique(supplyplan$funding_source))

# Check if there are planned/received/funded lines with funding source TBD
supplyplan |>
  filter(funding_status != "unfunded",
         funding_source == "TBD",
         collab_review_status == "Ready To Use Approved|In Review|Ready to Use Estimate") |>
  select(country, funding_source, funding_status) |>
  distinct(country, funding_source, funding_status)

# Note that GF global fund and GFF global financing facility are separate entities
supplyplan <- supplyplan |>
  mutate(funding_source = str_replace(string = funding_source,
                                      pattern = "Global Fund to Fight AIDS, Tuberculosis and Malaria|^GF$",
                                      replacement = "Global Fund" ))

supplyplan <- supplyplan |> 
  mutate(funding_source = case_when(
                            str_detect(funding_source, regex("^Banque Mondiale", ignore_case = TRUE)) ~ "World Bank",
                            str_detect(funding_source,regex("United Nations International Children",ignore_case = TRUE)) ~ "UNICEF",
                            str_detect(funding_source,regex("President's Emergency Plan For AIDS Relief",ignore_case = TRUE)) ~ "PEPFAR",
                            str_detect(funding_source,regex("BMGFF|Bill & Melinda Gates Foundation",ignore_case = TRUE)) ~ "BMGF",
                            str_detect(funding_source,"^DOHS") ~ "DOHS",
                            str_detect(funding_source, regex("DOHS|GOK|MOH|government|CMAM|Ministry|GOT|PNA|CAMEG|ETAT",ignore_case = TRUE)) ~ "Government",
                            str_detect(funding_source,regex("kingdom of netherlands",ignore_case = TRUE)) ~ "Kingdom of Netherlands",
                            str_detect(funding_source,regex("Planned Parenthood Federation",ignore_case = TRUE)) ~ "IPPF",
                            str_detect(funding_source,regex("United States Agency for International Development|Most likely USAID",ignore_case = TRUE)) ~ "USAID",
                            str_detect(funding_source,regex("United Nations Population Fund",ignore_case = TRUE)) ~ "UNFPA",
                            str_detect(funding_source,regex("West African Health Organization",ignore_case = TRUE)) ~ "WAHO",
                            str_detect(funding_source,regex("Populations Services International",ignore_case = TRUE)) ~ "PSI",
                            str_detect(funding_source,regex("others|other",ignore_case = TRUE)) ~ "Other",
                            TRUE ~ funding_source))
  
sort(unique(supplyplan$funding_source))

# get a pivot of L5 and L3 from supply plan
productpivot <- supplyplan |>
  distinct(L3.Method, L5.Product)

#### Defining PRH country categories ####
tier1 <- c("Afghanistan",
               "Angola",
               "Benin",
               "Burkina Faso",
               "Burundi",
               "Cameroon",
               "Côte d’Ivoire", 
               "Cote d'Ivoire",
              "DRC", "Congo (the Democratic Republic of the)",
              "Ethiopia", 
              "Ghana", 
              "Haiti", 
              "Guinea",
              "Kenya", 
              "Liberia", 
              "Madagascar", 
              "Mali", 
              "Mozambique",
              "Niger",
              "Nigeria", 
              "Pakistan",
              "Philippines", 
              "Senegal", 
              "South Sudan", 
              "Tanzania", 
              "Togo",
              "Uganda",
              "Yemen",
              "Zambia")

tier2 <- c("Cambodia", 
          "Egypt",  
          "Guatemala", 
          "Jordan", 
          "Malawi", 
          "Nepal", 
          "Rwanda",
          "Sierra Leone",
          "Timor Leste",
          "Zimbabwe")

tier3 <- c("Bangladesh",
           "India")

supplyplan <- supplyplan |>
  mutate(tier = case_when(country %in% tier1 ~ "Tier1 High Need",
                          country %in% tier2 ~ "Tier2 Strategic Support",
                          country %in% tier3 ~ "Tier3 Strategic Transition",
                          TRUE ~ "other"))

#### Data Wrangling - Analyzing numeric and date columns ####

supplyplan |>
  select_if(is.numeric) |>
  skimr::skim()

#### Exporting data ####

export(supplyplan, file = here::here("wrangled_data", "supplyplanwrangled.csv"))

#### import mcpr projections from https://www.un.org/development/desa/pd/node/3288 ####

callAPI <- function(rel_path, topics_list = FALSE) {
  base_url <- "https://population.un.org/dataportalapi/api/v1"
  target <- paste0(base_url, rel_path)
  response <- fromJSON(target)
  print(status_code(GET(target)))
# Checks if response was a flat file or a list (indicating pagination)
# If response is a list, we may need to loop through the pages to get all of the data
  if (class(response) == "list") {
# Create a data frame from the first page of the response using the `data` attribute
    df <- response$data
    while (!is.null(response$nextPage)) {
      response <- fromJSON(response$nextPage)
      df_temp <- response$data
      df <- rbind(df, df_temp)
    }
    return(df)
  }
# load the data directly from the API into a data frame
  else {
    if (topics_list == TRUE) {
      df <- fromJSON(target, flatten = TRUE)
      return(df[[5]][[1]])
    } else {
      df <- fromJSON(target)
      return(df)
    }
  }
}

# Returning a list of indicators
relative_path <- "/indicators/"
indicators <- callAPI(rel_path = relative_path)
export(indicators, file = here::here("wrangled_data", "indicators.csv"))

glimpse(indicators)
unique(indicators$name)

# Get all locations
relative_path <- "/locations/?sort=id"

locations <- callAPI(rel_path = relative_path)
glimpse(locations)

locations_subset <- locations 
country_codes <- paste(as.character(locations_subset$id), collapse = ",")

# Get indicator 2 for all location_subset, 1970 to most recent year - CHECK
relative_path <- paste0("/data/indicators/2/locations/", country_codes, "/start/1970/end/2023")

df <- callAPI(rel_path = relative_path)
glimpse(df)

# check country name differences between API data and countrysummary2023

rename(locations, country = name)
distinct(locations, country)
distinct(countrysummary2023, country) |> pull()
setdiff(distinct(countrysummary2023, country), distinct(locations, country))
rename(df, country = location)
setdiff(distinct(countrysummary2023, country), distinct(df, country))

locations |>
  filter(str_detect(country, regex("tanzania", ignore_case = TRUE))) |>
  select(country)

locations |>
  filter(str_detect(country, regex("congo", ignore_case = TRUE))) |>
  select(country)

locations |>
  filter(str_detect(country, regex("Ivoire", ignore_case = TRUE))) |>
  select(country)

locations |>
  filter(str_detect(country, regex("verde", ignore_case = TRUE))) |>
  select(country)

df <- df |>
  mutate_at(
    vars(location),
    ~ str_replace(
      string = .,
      pattern = "Democratic Republic of the Congo|Dem. Rep. of the Congo",
      replacement = "Congo (the Democratic Republic of the)"
    )
  )

df <- df |>
  mutate(location = case_when(
    str_detect(location, regex("tanzania", ignore_case = TRUE)) ~ "Tanzania",
    TRUE ~ location
  ))

df <- df |> 
  mutate_at(
    vars(location),
    ~ str_replace(
      string = .,
      pattern = "Côte d'Ivoire",
      replacement = "Cote d'Ivoire"
    )
  )

df <- df |>
  mutate_at(
    vars(location),
    ~ str_replace(
      string = .,
      pattern = "Cabo Verde",
      replacement = "Cape Verde"
    )
  )

locations |>
  filter(str_detect(country, regex("Eswatini", ignore_case = TRUE))) |>
  select(country)


supplyplan <- supplyplan |>
  mutate_at(
    vars(country),
    ~ str_replace(
      string = .,
      pattern = "Eswantini",
      replacement = "Eswatini"
    )
  )

setdiff(distinct(countrysummary2023, country), distinct(df, country))

export(df, file = here::here("wrangled_data", "mcpr.csv"))

df |>
  distinct(timeLabel)

# To do change from 2021 to most recent year
dfrecent <- df |>
  filter(timeLabel == "2023" & category == "All women" & variant == "Median") |>
  select(country, indicator, source, revision, variantLabel, timeLabel, category, estimateType, estimateMethod, sex, ageLabel, value)

rename(df, mCPRprojection = value)

#### Exporting data ####

export(dfrecent, file = here::here("wrangled_data", "dfrecent.csv"))

# dfrecent <- readr::read_csv(here("wrangled_data", "dfrecent.csv"))
# TO DO check if number of locations in df and dfrecent are the same

save(supplyplan, df, 
  file = here::here("wrangled_data", "wrangled_data.rda")
)
