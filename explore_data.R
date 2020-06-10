library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(tidyr)


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




phe_d <- read.xlsx("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/06/COVID-19-total-announced-deaths-9-June-2020.xlsx", 
                   sheet = "Tab4 Deaths by trust", 
                   startRow = 16, 
                   colNames = TRUE) %>% clean_names()


phe_d[1,5:(ncol(phe_d)-2)] <- as.character(as.Date(as.numeric(gsub("x","",names(phe_d[5:(ncol(phe_d)-2)]))), origin = "1900-01-01"))
phe_d[1,c(1:4,(ncol(phe_d)-1):ncol(phe_d))] <- names(phe_d[1,c(1:4,(ncol(phe_d)-1):ncol(phe_d))] )

phe_d <- phe_d %>%
  row_to_names(row_number = 1)


thing <- phe_d %>% 
  filter(nhs_england_region == "London") %>% 
  pivot_longer(-c(nhs_england_region, code, name, awaiting_verification, total),"date") %>% 
  mutate(date = ymd(ifelse(grepl("up_to_01_mar_20", date), "2020-03-01", date)), 
         csum = ave(value, name, FUN=cumsum))
 
ggplot(thing, aes(x = ymd(date), y = as.numeric(csum), group = name, colour = name)) +
  geom_line()


