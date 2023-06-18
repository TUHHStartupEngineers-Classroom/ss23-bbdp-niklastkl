library(tidyverse)
library(lubridate)
library(DT)
library(raster)
library(sf)


# Bike data
bikes_tbl      <- readRDS("01_data/bikes_tbl.rds")
bikeshops_tbl  <- readRDS("01_data/bikeshops_tbl.rds")
orderlines_tbl <- readRDS("01_data/orderlines_tbl.rds")

bike_orderlines_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl,     by = c("product_id" = "bike_id")) %>%
  left_join(bikeshops_tbl, by = c("customer_id" = "bikeshop_id")) %>%
  mutate(total_price = price_euro * quantity)

# German spatial data
#germany_sp <- getData('GADM', country='DE', level=1) 
# Convert SpatialPolygonsDataFrame to an sf dataframe
#germany_sf <- st_as_sf(germany_sp) %>% 
#  # Add english names
#  mutate(VARNAME_1 = ifelse(is.na(VARNAME_1), NAME_1, VARNAME_1)) 

category_1_labels <- bike_orderlines_tbl %>%
  dplyr::select(category_1) %>% unique() %>% c()

category_2_labels <- bike_orderlines_tbl %>% dplyr::filter(category_1 %in% category_1_labels[[1]]) %>% 
  dplyr::select(category_2) %>% unique() %>% c()

selection_category_1 = category_1_labels[[1]][1:3]

selection_category_2 = category_2_labels[[1]][1:3]

default_category_1 = category_1_labels[[1]][1]
default_category_2 = bike_orderlines_tbl %>% dplyr::filter(category_1 %in% default_category_1) %>% 
  dplyr::select(category_2) %>% unique() %>% c()

min_date = ymd(20190131)
max_date = ymd(20190228)
geo_plot_tbl <- bike_orderlines_tbl %>%
  dplyr::filter(category_1 %in% selection_category_1) %>%
  dplyr::filter(category_2 %in% selection_category_2) %>%
  dplyr::filter(order_date > min_date) %>%
  dplyr::filter(order_date < max_date) %>%
  group_by(state) %>%
  summarise(total_revenue = sum(total_price))


unit = "month"
date_format = "%B %Y"
sales_data_reactive <- bike_orderlines_tbl %>%
  
  #data_tbl <- bike_orderlines_tbl %>%
  #dplyr::filter(category_1 %in% input$bike_type_group) %>%
  #dplyr::filter(category_2 %in% input$bike_family_group) %>%
  #dplyr::filter(order_date > input$date_range[1]) %>%
  #dplyr::filter(order_date < input$date_range[2]) %>%
  
  dplyr::select(order_date, total_price) %>%
  
  mutate(date_rounded = floor_date(order_date, unit = unit)) %>%
  
  group_by(date_rounded) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format(date_format)}"))




