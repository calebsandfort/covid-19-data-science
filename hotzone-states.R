library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(gridExtra)
library(poliscidata)

data(states)

# stateNames <- states$state
# 
# stateNames

state.name

hotzone_states <- c("California", "New York", "New Jersey", "Washington")

wide_hotzone_states_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

washington <- wide_hotzone_states_confirmed %>%
  filter(`Country/Region` == "US" & `Province/State` == "Washington")

tidy_hotzone_states_confirmed <- wide_hotzone_states_confirmed %>%
  filter(`Country/Region` == "US" & `Province/State` %in% hotzone_states) %>%
  select(-c(`Country/Region`, Lat, Long)) %>%
  rename(State = `Province/State`) %>%
  gather(Date, Count, -State) %>%
  mutate(Date=mdy(Date)) %>%
  group_by(State, Date) %>% 
  summarise(Total =sum(Count)) %>%
  filter(Total > 0) %>%
  mutate(New = 0) %>%
  arrange(Date)

hotzone_states_df <- NULL

for(state in hotzone_states){
  state_rows = tidy_hotzone_states_confirmed %>% filter(State == state) %>% arrange(Date)
  state_rows$New[1] = 0;
  for(i in 2:nrow(state_rows)){
    state_rows$New[i] = state_rows$Total[i] - state_rows$Total[i-1]
  }
  
  if(is.null(hotzone_states_df)){
    hotzone_states_df <- state_rows
  }
  else{
    hotzone_states_df <- bind_rows(hotzone_states_df, state_rows)
  }
}

get_plot_states_by_day <- function(){
  hotzone_states_df %>%
    ggplot(aes(Date, Total, color = State, fill = State)) +
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle("Coronavirus Cases - Hotzone States") +
    ylab("Confirmed Cases")
}

get_plot_new_cases_by_state <- function(){ 
  hotzone_states_df %>%
    ggplot(aes(Date, Total, color = State, fill = State)) +
    geom_bar(stat = "identity") +
    ggtitle("New Cases by State") +
    ylab("New Case") +
    facet_wrap(.~State, scales = "free", ncol = 2)
}

grid.arrange(get_plot_states_by_day(), get_plot_new_cases_by_state(), nrow = 2)

