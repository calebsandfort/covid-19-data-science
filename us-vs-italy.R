library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)

wide_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

country_populations <- data.frame(c("US", "Italy"), c(327200000, 60480000), stringsAsFactors = FALSE) %>%
  setNames(c("Country", "Population"))

#drop columns
tidy_confirmed <- wide_confirmed %>% select(-c(`Province/State`, Lat, Long)) %>%
  rename(Country = `Country/Region`) %>%
  gather(Date, Count, -Country) %>%
  mutate(Date=mdy(Date)) %>%
  group_by(Country, Date) %>% 
  summarise(Total =sum(Count))


us_and_italy <- tidy_confirmed %>%
  filter(Country == "US" | Country == "Italy")  %>%
  left_join(country_populations, by = "Country") %>%
  arrange(Date)

us_and_italy %>%
  ggplot(aes(Date, Total, color = Country, fill = Country)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Coronavirus Cases - US vs Italy") +
  ylab("Cases") +
  theme_fivethirtyeight()

