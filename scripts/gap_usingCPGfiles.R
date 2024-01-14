
#for data import
library(readr) 
#for data tidying
 library(tibble)
 library(tidyr)
#for data wrangling
 library(dplyr)
 library(stringr)
#for data vis
 library(ggplot2)
#for functional programming
 library(purrr)
#other
 library(here)
 library(skimr)
 library(magrittr) #for the pipe operator
#dates
  library(lubridate)

#Data import with name repair
#reading in march supply plans instead of most recent as this file will be used only to get the product pivots.
#previously this file was used to get the non rationalized implant requirements, which is not needed now.

supplyplan <- readr::read_csv(here("wrangled_data", "supplyplanwrangled_03032022.csv"),
                              col_names = TRUE, name_repair="universal")

#get a pivot of L5 and L3 from supply plan
productpivot <- supplyplan %>%
  distinct(L3.Method, L5.Product)

####################################################
#Define priority and ouag countries
priority = c('Afghanistan','Bangladesh','DRC', 'Congo (the Democratic Republic of the)',
             'Ethiopia','Ghana','Haiti','India','Kenya','Liberia','Madagascar','Malawi','Mali','Mozambique',
             'Nigeria','Nepal','Pakistan','Philippines','Rwanda','Senegal','South Sudan','Tanzania','Uganda',
             'Yemen','Zambia')

ouag = c("Benin","Burkina Faso","Côte d’Ivoire","Cote d'Ivoire","Guinea","Mali","Mauritania","Niger","Senegal","Togo")

#########################################################################
#code to import file from CPG's funding gap analysis
cpg_gap <- readr::read_csv(here("raw_data", "CPG_fundinggap_Aug2022.csv"),
                              col_names = TRUE, name_repair="universal")


cpg_gap %<>%
  mutate(IsPriority = if_else(condition = country %in% priority,
                              true ="y",
                              false = "n")) 
cpg_gap %<>%
  mutate(IsOuag = if_else(condition = country %in% ouag,
                          true ="y",
                          false = "n"))
cpg_gap %<>%
  mutate(IsPriorityOrOuag = if_else(condition = IsPriority=="y"|IsOuag=="y",
                                    true ="y",
                                    false = "n"))

cpg_gap %<>% purrr::modify_at("total.requirement.2022", ~NULL)
cpg_gap %<>% purrr::modify_at("Total", ~NULL)

#add L3 product.
cpg_gap <- left_join(cpg_gap, productpivot, by = "L5.Product", na_matches = "never")

#pivot longer with funding source and Value
cpg_gap %<>% 
  pivot_longer(
    cols =  Government:TBD,
    names_to = "Funding.Source", 
    values_to = "Value",
    values_drop_na = TRUE
  )

unique(cpg_gap$Funding.Source)

cpg_gap %<>%
  mutate(Funding.Source = str_replace(
    string = Funding.Source,
    pattern = "Global.Fund",
    replacement = "Global Fund"
  ))

#add funding status based on funding source
cpg_gap%<>% 
  mutate(Funding_Status = case_when(Funding.Source != "TBD" ~ "Funded",
                                    TRUE~ "Unfunded"))

#change value to number
cpg_gap %<>%
  mutate(Value=parse_number(Value, na=c("$-",NA)))

#remove rows with value =0
cpg_gap %<>%
  filter(Value !=0)

write_csv(cpg_gap, file = here::here("wrangled_data", "cpg_gapwrangled.csv"))
write_csv(cpg_gap, file = here::here("wrangled_data", "supplyplanwrangled.csv"))
##########################################################################

#get the %unfunded by country for 2022
cpg_gap %<>%
  mutate(unfunded.value = if_else(condition = Funding_Status == "Unfunded",
                                    true = Value,
                                    false = 0))

cpg_gap %<>%
  mutate(Funder.exists = na_if(Funding.Source,"TBD"))

countrysummary2022 <- cpg_gap %>%
                    group_by(country)%>%
                    summarize(total.value.2022 = sum(Value, na.rm = TRUE),
                            unfunded.value.2022 = sum(unfunded.value, na.rm = TRUE)  ,
                            unfunded.percent.2022 = sum(unfunded.value, na.rm = TRUE)/sum(Value,na.rm = TRUE) ,
                            funder.count.2022 = n_distinct(Funder.exists, na.rm=TRUE))

distinct(countrysummary2022, country) %>% pull()

#include priority oug status
countrysummary2022 %<>%
  mutate(IsPriority = if_else(condition = country %in% priority,
                              true ="y",
                              false = "n")) 
countrysummary2022 %<>%
  mutate(IsOuag = if_else(condition = country %in% ouag,
                          true ="y",
                          false = "n"))
countrysummary2022 %<>%
  mutate(IsPriorityOrOuag = if_else(condition = IsPriority=="y"|IsOuag=="y",
                                    true ="y",
                                    false = "n"))

print(countrysummary2022)

write_csv(countrysummary2022, file = here::here("wrangled_data", "countrysummary2022.csv"))

####################################################################################################
#import mcpr projections from https://www.un.org/development/desa/pd/node/3288

library(jsonlite)

library(httr)

callAPI <- function(rel_path, topics_list=FALSE){
  base_url <- "https://population.un.org/dataportalapi/api/v1"
  target <- paste0(base_url, rel_path)
  response <- fromJSON(target)
  print(status_code(GET(target)))
    # Checks if response was a flat file or a list (indicating pagination)
  # If response is a list, we may need to loop through the pages to get all of the data
  if (class(response)=="list"){
    # Create a dataframe from the first page of the response using the `data` attribute
    df <- response$data
    while (!is.null(response$nextPage)){
      response <- fromJSON(response$nextPage)
      df_temp <- response$data
      df <- rbind(df, df_temp)
    }
    return(df)}
  # Otherwise, we will simply load the data directly from the API into a dataframe
  else{
    if (topics_list==TRUE){
      df <- fromJSON(target, flatten = TRUE)
      return(df[[5]][[1]])
    }
    else{
      df <- fromJSON(target)        
      return(df)
    }
  }
}

#Returning a list of indicators
relative_path <- "/indicators/"
indicators <- callAPI(rel_path = relative_path)
write_csv(indicators, file = here::here("wrangled_data", "indicators.csv"))

glimpse(indicators)
unique(indicators$name)

#Get all locations
relative_path <- "/locations/"

locations <- callAPI(rel_path = relative_path)
glimpse(locations)

country_codes <- paste(as.character(locations$id),collapse = ",")

# Get indicator 2 for all locations, all years
#CHANGE TO MOST RECENT YEAR

relative_path <- paste0("/data/indicators/2/locations/",country_codes,"/start/1970/end/2022")

df <- callAPI(rel_path = relative_path)
glimpse(df)

#check country name differences between API data and countrysummary2022

locations %<>% rename(country = name)
distinct(locations,country)
distinct(countrysummary2022,country) %>% pull()
setdiff(distinct(countrysummary2022,country), distinct(locations,country))
df %<>% rename(country = location)
setdiff(distinct(countrysummary2022,country), distinct(df,country))

locations %>%
  filter(str_detect(country,regex("tanzania",ignore_case = TRUE))) %>%
  select(country)

locations %>%
  filter(str_detect(country,regex("congo",ignore_case = TRUE))) %>%
  select(country)

locations %>%
  filter(str_detect(country,regex("Ivoire",ignore_case = TRUE))) %>%
  select(country)

locations %>%
  filter(str_detect(country,regex("verde",ignore_case = TRUE))) %>%
  select(country)

df %<>%
  mutate_at(
    vars(country),
    ~str_replace(
      string = .,
      pattern = "Democratic Republic of the Congo|Dem. Rep. of the Congo",
      replacement = "Congo (the Democratic Republic of the)"
    ))

df %<>% 
  mutate(country = case_when(str_detect(country,regex("tanzania",ignore_case = TRUE)) ~ "Tanzania",
                             TRUE~ country))

df %<>%
  mutate_at(
    vars(country),
    ~str_replace(
      string = .,
      pattern = "Côte d'Ivoire",
      replacement = "Cote d'Ivoire"
    ))

df %<>%
  mutate_at(
    vars(country),
    ~str_replace(
      string = .,
      pattern = "Cabo Verde",
      replacement = "Cape Verde"
    ))

setdiff(distinct(countrysummary2022,country), distinct(df,country))

write_csv(df, file = here::here("wrangled_data", "df.csv"))

df %>%
  distinct(timeLabel)

#To do change from 2021 to most recent year
dfrecent <- df %>%
  filter(timeLabel=="2022" & category=="All women"& variant=="Median") %>%
  select(country,indicator,source,revision, variantLabel,timeLabel,category,estimateType,estimateMethod,sex,ageLabel,value)

dfrecent %<>% rename(mCPRprojection= value)
write_csv(dfrecent, file = here::here("wrangled_data", "dfrecent.csv"))

#dfrecent <- readr::read_csv(here("wrangled_data", "dfrecent.csv"))
#TO DO check if number of locations in df and dfrecent are the same

###################################################################################################
countrysummary2022 <- left_join(countrysummary2022, dfrecent, by = "country")
write_csv(countrysummary2022, file = here::here("wrangled_data", "countrysummary2022.csv"))

save(supplyplan, supplyplan2022, countrysummary2022, shipment,df, dfrecent,
     file = here::here("wrangled_data", "wrangled_data.rda"))

################################################################################################
