# Visualize the growth of Covid-19 cases
# Load libraries ----
library(data.table)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)

# Import ----
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# Data wrangling ----
covid_data_tbl <- covid_data_tbl %>% 
  mutate(dateRep = as.Date(dateRep, "%d/%m/%Y")) %>%
  arrange(dateRep)



covid_2020_tbl <- covid_data_tbl %>%
  # Select relevant columns
  select(countriesAndTerritories, dateRep, cases, year, month) %>% 
  filter(year == 2020) %>% 
  # Filter countries, add formatted date, gorup and arrange
  filter(countriesAndTerritories %in% c("Germany", "United_Kingdom", "Spain", "France", "United_States_of_America") ) %>%
  arrange(countriesAndTerritories) %>% 
  group_by(countriesAndTerritories) %>%
  # Add cumulative sum of cases
  mutate(cumulative_sum = cumsum(cases)) %>% 
  ungroup()

# Visualization ----
covid_2020_tbl %>%    
  ggplot(aes(dateRep, cumulative_sum, color = countriesAndTerritories)) +
  geom_line(size = 1) +
  
  scale_x_date(date_breaks = "1 month", date_labels = "%B" )  + scale_colour_manual(values = c("#cb75e0", "#75e0d2", "#7ae075", "#dee075", "#e07575")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels =  number_format(scale = 1e-6, suffix = " M")) +
  theme_minimal() +
  labs(
    title = "COVID-19 Confirmed cases Worldwide",
    subtitle = "USA the most affected country",
    tag = "Challenge 1",
    x = "Year 2020",
    y = "Cummulative Cases",
    color = "Countries" # Legend text
  ) +
  
  
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    plot.tag = element_text(face = "bold"),
    plot.tag.position =  "bottom"
  ) +
  
  geom_label( label = max(covid_2020_tbl %>% select(cumulative_sum)),
              vjust = 0.5,
              hjust = 1.5,
              size  = 3,
              data  = covid_2020_tbl %>% 
                filter(countriesAndTerritories %in% c("United_States_of_America")) %>% 
                filter(dateRep == max(covid_2020_tbl$dateRep))
  )

# Mortality rate
# Load libraries ----
library(tidyverse)
library(maps)

# Import ----
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
world <- map_data("world")

# Data wrangling ----
covid_tbl <- covid_data_tbl %>%
  
  # Select relevant columns
  select(deaths, popData2019, countriesAndTerritories) %>%
  
  # Calculate total deaths
  group_by(countriesAndTerritories) %>%
  mutate(total_deaths = sum(deaths)) %>%
  ungroup() %>%
  
  select(popData2019, countriesAndTerritories, total_deaths) %>%
  unique() %>%
  
  # Add deaths with respect to total population
  mutate(deaths_percent = total_deaths/popData2019) %>%
  select(countriesAndTerritories, deaths_percent) %>% 
  
  # Handle differences in country names
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))
# Merge data
combined_data <- merge(x = covid_tbl, y = world, 
                       by.x    = "countriesAndTerritories", 
                       by.y    = "region",
                       all.x = FALSE, 
                       all.y = FALSE)
# 4.0 Visualization ----
ggplot(combined_data) +
  
  #Data representation & Legend 
  scale_fill_gradient(low = "red", high = "black", name = "Mortality Rate",
                      n.breaks = 4) +
  
  # Apply base layer for countries without data
  geom_map(dat=world, map = world,
           aes(map_id=region), fill="grey", color="white") +
  
  
  # Apply main map data layer
  geom_map(aes(fill = deaths_percent, map_id = countriesAndTerritories), map = world,
           color = "#ffffff", size=0.000001) +
  
  expand_limits(x = world$long, y = world$lat) +
  
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "More than 1.2 Million confirmed deaths worldwide",
    caption = "Date: 06-12-2020",
    x = "",
    y = ""
  ) + 
  
  # Remove Axis labels (long & lat)
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank())
