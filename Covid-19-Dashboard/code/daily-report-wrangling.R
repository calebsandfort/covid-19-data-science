library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)

filterMunicipality <- function(data){
  str_detect(data, "Out of", negate = TRUE)
}

filterStates <- function(data){
  str_detect(data, "District of Columbia|Northern Mariana Islands|Recovered|U.S.|Diamond Princess|Grand Princess|Guam|Virgin Islands|Puerto Rico|Wuhan|American Samoa", negate = TRUE)
}

reportDateRange <- seq(mdy('3/1/2020'), lubridate::today(), by = "days")
# reportDateRange <- seq(mdy('3/12/2020'), mdy('3/13/2020'), by = "days")

masterReportMessy <- NULL
currentReportMessy <- NULL
currentReportTidy <- NULL

for(i in seq(1, length(reportDateRange) - 1)){
  currentDate <- reportDateRange[i] %>% format('%m-%d-%Y')
  
  print(currentDate)
  
  url <- str_interp("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/${currentDate}.csv")
  
  currentReportMessy <- read_csv(url, col_types = cols())
  
  if(length(colnames(currentReportMessy)) == 8){
    currentReportTidy <- currentReportMessy %>%
      filter(`Country/Region` == "US" &
               filterStates(`Province/State`) &
               !is.na(`Province/State`)) %>%
      mutate(Confirmed = replace_na(Confirmed, 0)) %>%
      select(`Province/State`, `Country/Region`, Confirmed)
    
    colnames(currentReportTidy)[1:3] <- c("State", "Country", as.character(currentDate))
    
    if(currentReportTidy %>% filter(any(str_detect(State, ","))) %>% nrow() > 0) {
      currentReportTidy <- currentReportTidy %>%
      separate(col = State, into = c("Municipality", "State"), sep = ", ") %>%
      mutate(State = openintro::abbr2state(State))
    }
    else{
      currentReportTidy <- currentReportTidy %>%
        mutate(Municipality = "None")
    }
  }
  else{
    currentReportTidy <- currentReportMessy %>%
      filter(Country_Region == "US" &
               filterStates(Province_State) &
               !is.na(Province_State)) %>%
      mutate(Confirmed = replace_na(Confirmed, 0)) %>%
      select(Admin2, Province_State, Country_Region, Confirmed)
    
    colnames(currentReportTidy)[1:4] <- c("Municipality", "State", "Country", as.character(currentDate))
  }

  currentReportTidy <- currentReportTidy %>%
    unite(col = "Key", Municipality, State, Country, sep = "_", remove = FALSE) %>%
    filter(filterMunicipality(Municipality)) %>%
    mutate(Key = str_replace_all(Key, " ", "_"))

  if(is.null(masterReportMessy)){
    masterReportMessy <- currentReportTidy
  }
  else{
    # currentReportTidy <- currentReportTidy %>% select(-c(Municipality, State, Country))

    masterReportMessy <- masterReportMessy %>%
      full_join(currentReportTidy, by = c('Key', 'Municipality', 'State', 'Country'))
  }
}

masterReportMessy[is.na(masterReportMessy)] = 0

masterReportMessy <- masterReportMessy %>%
  filter(str_detect(State, "0|US", negate = TRUE))

masterReportTidy <- masterReportMessy %>%
  select(-Country, -Key, -Municipality) %>%
  gather(Date, Count, -State) %>%
  mutate(Date=mdy(Date)) %>%
  group_by(State, Date) %>% 
  summarise(Total = sum(Count)) %>%
  mutate(New = 0) %>%
  arrange(Date)

uniqueStates <- masterReportMessy %>% select(State) %>% distinct() %>% arrange(State)

tempDF <- NULL

for(s in uniqueStates$State){
  state_rows <- masterReportTidy %>% filter(State == s) %>% arrange(Date)
  state_rows$New[1] = 0;
  for(i in 2:nrow(state_rows)){
    state_rows$New[i] = state_rows$Total[i] - state_rows$Total[i-1]
  }
  
  if(is.null(tempDF)){
    tempDF <- state_rows
  }
  else{
    tempDF <- bind_rows(tempDF, state_rows)
  }
}

masterReportTidy <- tempDF


summaryReportTidy <- masterReportTidy %>%
  group_by(State) %>%
  summarise(Total = max(Total), MostNew = max(New)) %>%
  arrange(State)

getStates <- function(){
  uniqueStates$State
}

getStateRows <- function(s){
  masterReportTidy %>% filter(State == s) %>% arrange(Date)
}

getNewCasesMinMax <- function(){
  c(min(masterReportTidy$New), max(masterReportTidy$New))
}

getSummaryReport <- function(){
  summaryReportTidy
}


