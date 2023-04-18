# Data acquisition using API
# Load libraries ----
library(httr)
library(jsonlite)
library(tidyverse)

# In this example, I worked with the Open Notify API, which opens up data on various NASA projects.
# Request Data ----

res = GET("http://api.open-notify.org/astros.json")

res

#3.0 Convert JSON to Data Structure ----

data = fromJSON(rawToChar(res$content))

names(data)

data$people


# WEBSCRAPING ----


# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# 1.1 COLLECT PRODUCT TYPES ----
url_home          <- "https://www.radon-bikes.de/"
xopen(url_home)


html_home         <- read_html(url_home)
bike_family_tbl <- html_home %>%
  html_nodes(css = ".megamenu__item > a") %>%  
  html_attr('href') %>%  
  discard(.p = ~stringr::str_detect(.x,"wear")) %>%  
  enframe(name = "position", value = "cat_subcat_url") %>%  
  mutate(family_id = str_glue("https://www.radon-bikes.de{cat_subcat_url}bikegrid"))
bike_family_tbl


# 2.0 COLLECT BIKE DATA ----

bike_category_url <- bike_family_tbl$family_id[1]
xopen(bike_category_url)


html_bike_category  <- read_html(bike_category_url)

bike_name_tbl        <- html_bike_category %>%
  html_nodes(css = ".m-bikegrid__info .a-heading--small") %>%
  html_text() %>%
  enframe(name = "position", value = "name")
bike_name_tbl 

bike_price_tbl <- html_bike_category %>%
  html_nodes(css = ".m-bikegrid__price.currency_eur .m-bikegrid__price--active") %>%  
  html_text() %>% 
  enframe(name = "position", value = "price")
bike_price_tbl

model_price_tbl <- left_join(bike_name_tbl, bike_price_tbl)%>% 
  select(name, price)
model_price_tbl