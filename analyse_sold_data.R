# SETUP ENVIRONMENT -------------------------------------------------------
library(readr)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

# 1. IMPORT DATA ----------------------------------------------------------
# import data extracted from www.realestate.com
sold_data <- read_csv("sold_data.csv")

# 2. WRANGLE DATA ---------------------------------------------------------
# create new data frame
sold_data_2 <- sold_data

# extract price
sold_data_2 <- sold_data_2 %>%
  mutate(price = str_extract(Column1, "[0-9]{0,3},[0-9]{0,3}")) %>%
  mutate(price = str_remove(price, ",")) %>%
  mutate(price = as.numeric(price))
  
# extract data
sold_data_2 <- sold_data_2 %>%
  mutate(date = str_extract(Column1, "[0-9]{1,2} [aA-zZ]{3} [0-9]{4}")) %>%
  mutate(date = dmy(date))

# extract location
sold_data_2 <- sold_data_2 %>%
  mutate(location = str_remove_all(Column1, "\\n")) %>%
  mutate(location = str_remove(location, "\\$[0-9]{0,3},[0-9]{0,3}.*, ")) %>%
  mutate(location = str_remove_all(location, " [0-9] [0-9].*"))

# create locations string
sold_data_2_locations <- unique(sold_data_2$location)

# extract year
sold_data_2 <- sold_data_2 %>%
  mutate(date_year = year(date)) %>%
  mutate(date_year = factor(date_year, levels = c(2010:2020)))

# 3. VISUALISE DATA -------------------------------------------------------
# create new data frame
sold_data_3 <- sold_data_2

# create prices overtime table
plot_yearly_prices_overtime_table <- sold_data_3 %>%
  group_by(date_year) %>%
  summarise(average_price = median(price)) %>%
  mutate(average_price = as.integer(average_price) * 0.001) %>%
  filter(!is.na(date_year))

# create prices overtime plot
plot_yearly_prices_overtime <- ggplot(plot_yearly_prices_overtime_table) +
  aes(x = date_year, fill = average_price, weight = average_price) +
  geom_bar() +
  scale_fill_gradient() +
  labs(x = "Year", y = "Sold Price (Thousands)", 
       title = "Average Cost of 2 Bedroom + 1 Study Units in Fortitude Valley and Surrounding Suburbs",
       subtitle = str_c(append("Surround Suburbs include", sold_data_2_locations), collapse = " | "),
       caption = "source: www.realestate.com.au | searching 2 bedroom units with study and pool area") +
  scale_y_continuous(labels=scales::dollar_format()) +
  geom_text(aes(y = average_price , label = paste0("$", round(average_price), "k")), nudge_y = 15) +
  theme_minimal() +
  theme(legend.position="none")

# print plot
plot_yearly_prices_overtime

# create facet of location prices overtime table
plot_yearly_location_prices_overtime_table <- sold_data_3 %>%
  group_by(date_year, location) %>%
  summarise(average_price = median(price)) %>%
  mutate(average_price = as.integer(average_price) * 0.001) %>%
  filter(!is.na(date_year)) %>%
  filter(average_price > 10)

# create facet of location prices overtime plot
plot_yearly_location_prices_overtime <- ggplot(plot_yearly_location_prices_overtime_table) +
  aes(x = date_year, fill = average_price, weight = average_price) +
  geom_bar() +
  scale_fill_gradient() +
  labs(x = "Year", y = "Sold Price (Thousands)", 
       title = "Average Cost of 2 Bedroom + 1 Study Units in Fortitude Valley and Surrounding Suburbs",
       subtitle = str_c(append("Surround Suburbs include", sold_data_2_locations), collapse = " | "),
       caption = "source: www.realestate.com.au | searching 2 bedroom units with study and pool area") +
  scale_y_continuous(labels=scales::dollar_format()) +
  geom_text(aes(y = average_price , label = paste0("$", round(average_price), "k")), nudge_y = 45, size = 3) +
  theme_minimal() +
  theme(legend.position="none") + 
  facet_wrap(vars(location))

# print plot
plot_yearly_location_prices_overtime

