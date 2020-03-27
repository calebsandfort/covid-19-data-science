library(tidyverse)
library(dplyr)
library(lubridate)

wide_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

colnamesList <- colnames(wide_confirmed)
startCol <- colnamesList[5]
endCol <- colnamesList[length(colnamesList)]

tidy_states_confirmed <- wide_confirmed %>%
  filter(`Country/Region` == "US" & `Province/State` %in% state.name) %>%
  select(-c(`Country/Region`)) %>%
  rename(State = `Province/State`) %>%
  gather(Date, Count, Date, Count) %>%
  mutate(Date=mdy(Date)) %>%
  group_by(State, Date) %>% 
  summarise(Total =sum(Count)) %>%
  filter(Total > 0) %>%
  mutate(New = 0) %>%
  arrange(Date)


