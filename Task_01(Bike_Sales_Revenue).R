library(readxl)
library(dplyr)
library(stringr)
library(rlang)
library(tidyr)
library(lubridate)
library(ggplot2)
library(writexl)
library(readr)


bikes_tbl <- read_excel("C:/Users/harsh/Desktop/data_science/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("C:/Users/harsh/Desktop/data_science/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel("C:/Users/harsh/Desktop/data_science/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate (col= category,into   = c("category.1", "category.2", "category.3"),sep= " - ")%>% 
  mutate(total.price = price * quantity) %>%
  select(-...1, -gender) %>%select(-ends_with(".id")) %>%
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>%
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))



# Challenge task 1 

sales_by_state_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  tidyr::separate(col = location,
                  into = c("city","state"),
                  sep = ",") %>% 
  # Select columns and add a year
  select(state, total_price) %>%
  
  # Group by and summarize year and main catgegory
  group_by(state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() 

# Step 2 - Visualize
sales_by_state_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = state, y = sales)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " ???")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )

# Challenge task 2

sales_by_year_state_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  tidyr::separate(col = location,
                  into = c("city","state"),
                  sep = ",") %>% 
  
  # Select columns and add a year
  select(state,order_date, total_price) %>%
  mutate(year = year(order_date)) %>% 
  
  # Group by and summarize year and main catgegory
  group_by(year,state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() 

# Step 2 - Visualize
sales_by_year_state_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state )) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  facet_wrap(~ state)+
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = "ϵ")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )

bike_orderlines_wrangled_tbl %>% 
  write_rds("C:/Users/harsh/Desktop/data_science/DS_101/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")