# Scraping covid 19 data
# date: 4 april 2020
# author: sayed farhad nabizada

library(RCurl)
library(dplyr)
library(rvest)


################# CSSE Data Upates once in 24 hours ###################

reshap_csse_data <- function(data, case_death){
  
  vars <- c(state = "Province/State", country ="Country/Region")
  data <- dplyr::rename(data, !!vars)
  data$country<-as.character(data$country)
  long <- tidyr::pivot_longer(data, names_to = "Date", values_to = case_death, cols = contains("/"))
  long$Date <- as.Date(long$Date, tryFormats = "%m/%d/%y")
  return(long)
}

join_csse_data <- function(cases_data, death_data, recov_data){
  # create unique key
  cases_data$key <- paste(cases_data$country, cases_data$Date, cases_data$Long, cases_data$Lat, cases_data$state)
  death_data$key <- paste(death_data$country, death_data$Date, death_data$Long, death_data$Lat, death_data$state)
  recov_data$key <- paste(recov_data$country, recov_data$Date, recov_data$Long, recov_data$Lat, recov_data$state)
  
  #subset the columns
  death_data_sub <- death_data %>% 
    select(key, deaths)
  
  recov_data_sub <- recov_data %>% 
    select(key, recov)
  
  #join
  all_dfs <- list(cases_data, death_data_sub, recov_data_sub)
  all_dfs_merged <- all_dfs %>% purrr::reduce(dplyr::left_join, by = "key") %>% select(-c(key))
  all_dfs_merged$recov[is.na(all_dfs_merged$recov)] <- 0
  
  
  return(all_dfs_merged)
  
}

# read data from github repo
cases_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
death_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recov_url <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

cases_data <- read.csv(text = cases_url, check.names = F)
death_data <- read.csv(text = death_url, check.names = F)
recov_data <- read.csv(text = recov_url, check.names = F)

# reshape
cases_data <- reshap_csse_data(cases_data, "cases")
death_data <- reshap_csse_data(death_data, "deaths")
recov_data <- reshap_csse_data(recov_data, "recov")


# join cases , death and recovered datasets 
joined_csse_data <- join_csse_data(cases_data, death_data, recov_data)

# Calculate Active cases
joined_csse_data$active_cases <- joined_csse_data$cases - joined_csse_data$deaths - joined_csse_data$recov

# aggregate data country and date
csse_data_by_country_date <- joined_csse_data %>% 
  group_by(country, Date) %>% 
  summarise(
    cases = sum(cases),
    deaths = sum(deaths),
    recov = sum(recov),
    active_cases = sum(active_cases)
  ) 

# Aggregate by country
aggregate_by_country <- csse_data_by_country_date %>% group_by(country) %>%
  summarise(
    cases = max(cases),
    deaths = max(deaths),
    recov = max(recov),
    active_cases = max(active_cases),
  )


# CSSE Data,  long format
csse_long_format <- csse_data_by_country_date  %>% tidyr::pivot_longer(names_to = "case_type", cols = c(active_cases, cases, deaths, recov)) %>%
  mutate(
    case_type = case_when(
    case_type == "active_cases" ~ "Active Cases",
    case_type == "cases" ~ "Total Cases",
    case_type == "deaths" ~ "Deaths",
    case_type == "recov" ~ "Recovered",
    )
  )

write.csv(joined_csse_data, "output/CSSE/joined_csse_data.csv", row.names = F)
write.csv(csse_data_by_country_date, "output/CSSE/csse_data_by_country_date.csv", row.names = F)
write.csv(aggregate_by_country, "output/CSSE/aggregate_by_country.csv", row.names = F)
write.csv(csse_long_format, "output/CSSE/csse_long_format.csv", row.names = F)



######################## worldometers.info - Updates more frequently ####################### 

# scraping function 

scrape_worldometers <- function(){

  url <- "https://www.worldometers.info/coronavirus/?mc_cid=d17a0161cb&mc_eid=ea1b6e3dc9#countries"

  covid_df <- url %>%
    html() %>%
    html_nodes(xpath='//*[@id="main_table_countries_today"]') %>%
    html_table()
  covid_df <- covid_df[[1]]

  covid_df$TotalCases <- gsub(",", "", covid_df$TotalCases)
  covid_df$TotalDeaths <- gsub(",", "", covid_df$TotalDeaths)
  covid_df$TotalRecovered <- gsub(",", "", covid_df$TotalRecovered)
  covid_df$ActiveCases <- gsub(",", "", covid_df$ActiveCases)
  covid_df$`Serious,Critical` <- gsub(",", "", covid_df$`Serious,Critical`)

  covid_df$TotalCases <- as.numeric(covid_df$TotalCases)
  covid_df$TotalDeaths <- as.numeric(covid_df$TotalDeaths)
  covid_df$TotalRecovered <- as.numeric(covid_df$TotalRecovered)
  covid_df$ActiveCases <- as.numeric(covid_df$ActiveCases)
  covid_df$`Serious,Critical` <- as.numeric(covid_df$`Serious,Critical`)

  return(covid_df)

}

# get worldometers data
worldometers_data <- scrape_worldometers()

write.csv(worldometers_data, "output/Worldmeters/worldometers_data.csv", row.names = F)

