# Data Wrangling
## Challange I

library(vroom)
library(tidyverse)
library(readxl)
library(lubridate)
library("writexl")
library(stringr)
library(dplyr)
library(tidyr)
col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)
#load stuff
assignee_tbl <- vroom(
  file       = "C:/Users/harsh/Desktop/data_science/DS_101/00_data/02_data_wrangling/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
patent_assignee_tbl <- vroom(
  file       = "C:/Users/harsh/Desktop/data_science/DS_101/00_data/02_data_wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
#gen table
Patent_Dominance_tbl <- tibble()
#filter us companys
assignee_tbl <- assignee_tbl %>%
  filter(type == 2)
#assamble
Patent_Dominance_tbl <- assignee_tbl %>%
  left_join(patent_assignee_tbl, by = c("id" = "assignee_id")) %>%
  group_by(organization) %>%
  summarise(count = n())%>%
  arrange(desc(count))%>%
  slice(1:10)
#show
glimpse(Patent_Dominance_tbl)
```
## Challange II
```{r c3challenge_2, fig.width=14, fig.height=7}
library(vroom)
library(tidyverse)
library(readxl)
library(lubridate)
library("writexl")
library(stringr)
library(dplyr)
library(tidyr)
col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)
#load stuff
assignee_tbl <- vroom(
  file       = "C:/Users/harsh/Desktop/data_science/DS_101/00_data/02_data_wrangling//assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
patent_assignee_tbl <- vroom(
  file       = "C:/Users/harsh/Desktop/data_science/DS_101/00_data/02_data_wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
patent_tbl <- vroom(
  file       = "C:/Users/harsh/Desktop/data_science/DS_101/00_data/02_data_wrangling/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
#genearte stuff
Recent_patent_acitivity_tbl <- tibble()
#filter us companys
assignee_tbl <- assignee_tbl %>%
  filter(type == 2)
#assamble it
Recent_patent_acitivity_tbl <- assignee_tbl %>%
  left_join(patent_assignee_tbl, by = c("id" = "assignee_id")) %>%
  left_join(patent_tbl, by = c("patent_id" = "id")) %>%
  mutate(year = year(date)) %>%
  filter(year == 2019)%>%
  group_by(organization) %>%
  summarise(count = n())%>%
  arrange(desc(count))%>%
  slice(1:10)
#show it
glimpse(Recent_patent_acitivity_tbl)

## Challange III

library(vroom)
library(tidyverse)
library(stringr)
library(dplyr)
col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)
#load stuff
assignee_tbl <- vroom(
  file       = "C:/Users/harsh/Desktop/data_science/DS_101/00_data/02_data_wrangling/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
patent_assignee_tbl <- vroom(
  file       = "C:/Users/harsh/Desktop/data_science/DS_101/00_data/02_data_wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
uspc_tbl <- vroom(
  file       = "C:/Users/harsh/Desktop/data_science/DS_101/00_data/02_data_wrangling/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
mainclass_current_tbl <- vroom(
  file       = "C:/Users/harsh/Desktop/data_science/DS_101/00_data/02_data_wrangling/mainclass_current.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
#generate tibbles
getTenTable <- tibble()
wholeTable <- tibble()
filterdTable<- tibble()
filterdTable_summeried<- tibble()
#filter companys
assignee_tbl <- assignee_tbl %>%
  filter(type == 2 | type == 3)
#get the 10 biggest companys
getTenTable <- assignee_tbl %>%
  left_join(patent_assignee_tbl, by = c("id" = "assignee_id")) %>%
  left_join(uspc_tbl, by = c("patent_id" = "patent_id")) %>%
  #filter(!is.na(mainclass_id))%>%
  group_by(organization) %>%
  summarise(count = n())%>%
  arrange(desc(count)) %>%
  ungroup() %>%
  slice(1:10)
topTen <- getTenTable[,c("organization")]
#put together everything
wholeTable <- assignee_tbl %>%
  left_join(patent_assignee_tbl, by = c("id" = "assignee_id")) %>%
  left_join(uspc_tbl, by = c("patent_id" = "patent_id"))
#filter the big 10 
filterdTable <- subset(wholeTable, wholeTable$organization %in% c(topTen[1,1],topTen[2,1],topTen[3,1],topTen[4,1],topTen[5,1],topTen[6,1],topTen[7,1],topTen[8,1],topTen[9,1],topTen[10,1]))
#assable the summary
filterdTable_summeried <- filterdTable %>%
  filter(!is.na(mainclass_id))%>%
  group_by(mainclass_id) %>%
  summarise(count = n())%>%
  arrange(desc(count)) %>%
  ungroup() %>%
  left_join(mainclass_current_tbl, by = c("mainclass_id" = "id")) %>%
  slice(1:10)
#show 
glimpse(filterdTable_summeried)