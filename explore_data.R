library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(lubridate)


phe <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv") %>% clean_names()


ldn_bs <- c("Barking and Dagenham",
            "Barnet",
            "Bexley",
            "Brent",
            "Bromley",
            "Camden",
            "Croydon",
            "Ealing",
            "Enfield",
            "Greenwich",
            "Hackney",
            "Hammersmith and Fulham",
            "Haringey",
            "Harrow",
            "Havering",
            "Hillingdon",
            "Hounslow",
            "Islington",
            "Kensington and Chelsea",
            "Kingston upon Thames",
            "Lambeth",
            "Lewisham",
            "Merton",
            "Newham",
            "Redbridge",
            "Richmond upon Thames",
            "Southwark",
            "Sutton",
            "Tower Hamlets",
            "Waltham Forest",
            "Wandsworth",
            "Westminster")

thing <- phe %>% 
  filter(area_name %in% ldn_bs) %>% 
  # filter(area_type == "Lower tier local authority") %>% 
  group_by(specimen_date, area_name) %>% 
  summarise(daily_lab_confirmed_cases = sum(daily_lab_confirmed_cases), 
            cumulative_lab_confirmed_cases = sum(cumulative_lab_confirmed_cases)) 

ggplot(thing, aes(x = ymd(specimen_date), y = cumulative_lab_confirmed_cases, group = area_name, colour = area_name)) +
  geom_line()
