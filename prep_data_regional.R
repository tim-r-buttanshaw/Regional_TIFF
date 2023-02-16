###
# Project - tiff in the regions of England
# Title - Regions graphing
# Author - Tim Buttanshaw
# Description - Import the full data ste and filter it to be nice
# Date - 11/11/2022
###

if(!require(pacman)) {
  install.packages('pacman')
  library(pacman)
}

#load other packages
p_load(tidyverse, 
       glue, 
       fs, 
       readxl, 
       cli,
       scales,
       gridExtra,
       knitr,
       lubridate,
       janitor,
       here,
       ggfbs,
       snakecase,
       ggfbs)

# import the whole NUTS3 data set
everything <- read_xlsx(here("TIFF in the regions of England in 2021", "data", "Regional TIFF raw out of Access.xlsx"),
                        sheet = "NUTS3")

# clean up the nuts3 data set, making the names lowercase with _ and no spaces;
# removing names of regions to work with codes;
# making the data set long so you can more easily manipulated
everything_cleaned <- everything %>% 
  clean_names(use_make_names = FALSE) %>%
  mutate(nuts1_code = to_snake_case(nuts1_code)) %>% 
  mutate(nuts3_code = to_snake_case(nuts3_code)) %>% 
  select(-c("nuts1_name", "nuts3_name", "account_item_name_tiff")) %>%
  rename("account_item_order" = "account_item_order_tiff_nuts1_dataset") %>%
  pivot_longer(cols = `2010`:glue("{working_year}"), values_to = "value", names_to = "year") %>% 
  arrange(year, nuts3_code, account_item_order)

# create a lookup table so you can replace the codes with item names later
# rename some of the items from the data set so they match their publishing names
lookup_items <- everything %>%
  distinct(`Final Account Item code`, `Account Item name TIFF`) %>% 
  clean_names %>%
  rename(item_name = account_item_name_tiff) %>%
  mutate(item_name = {gsub('\\s*\\([^\\)]+\\)', '', .$item_name)}) %>% 
  mutate(item_name = {gsub('[0-9]+', '', .$item_name)}) %>%
  mutate(item_name = {gsub('of which', '', .$item_name)}) %>%
  mutate(item_name = {gsub('excl GFCF', '', .$item_name)}) %>%
  mutate(final_account_item_code = to_snake_case(final_account_item_code)) %>%
  mutate(item_name = to_snake_case(item_name)) %>% 
  mutate(item_name = str_replace_all(item_name, '_', ' ')) %>%
  
  mutate(item_name = str_replace_all(item_name,"cattle", "beef")) %>%
  mutate(item_name = str_replace_all(item_name,"pigs", "pigmeat")) %>%
  mutate(item_name = str_replace_all(item_name,"poultry", "poultry")) %>% 
  
  mutate(item_name = str_to_sentence(item_name)) %>% 
  
  mutate(item_name = str_replace_all(item_name, 'non agricultural', 'non-agricultural')) %>% 
  mutate(item_name = str_replace_all(item_name, 'Sheep and goats', 'Mutton, lamb and goat')) %>%
  mutate(item_name = str_replace_all(item_name, 'Energy lubricants', 'Energy')) %>%
  mutate(item_name = str_replace_all(item_name, 
                                     'Rents and other real estate rental charges to be paid', 
                                     'Rent and other associated costs')) %>% 
  mutate(item_name = str_replace_all(item_name,
                                     'Plant protection products herbicides insecticides and pesticides',
                                     'Plant protection products'))

# a list of colurs which match the Defra colour palette
colour_chart <- c("#12436d", "#28a197", "#801650", "#f46a25", "#3d3d3d", "#a285d1", "#2f8d2f", "##ffd92f")

main_fig1_colours <- c("#801650", "#f46a25", "#3d3d3d", "#28a197", "#12436d")

main_fig2_colours <- c("#801650", "#f46a25", "#28a197", "#12436d")

# split out into the nuts 1 regions in a data set ------------------------------
# north east
ukc <- everything_cleaned %>%
  filter(nuts1_code == "ukc") %>% 
  select(-"nuts1_code")

# north west
ukd <- everything_cleaned %>%
  filter(nuts1_code == "ukd") %>% 
  select(-"nuts1_code")

# yorkshire and the humber
uke <- everything_cleaned %>%
  filter(nuts1_code == "uke") %>% 
  select(-"nuts1_code")

# east midlands
ukf <- everything_cleaned %>%
  filter(nuts1_code == "ukf") %>% 
  select(-"nuts1_code")

# west midlands
ukg <- everything_cleaned %>%
  filter(nuts1_code == "ukg") %>% 
  select(-"nuts1_code")

# east of england
ukh <- everything_cleaned %>%
  filter(nuts1_code == "ukh") %>% 
  select(-"nuts1_code")

# london and the south east
ukij <- everything_cleaned %>%
  filter(str_detect(nuts1_code, "ukij")) %>% 
  select(-"nuts1_code")

# south west
ukk <- everything_cleaned %>%
  filter(nuts1_code == "ukk") %>% 
  select(-"nuts1_code")

# per hectare data -------------------------------------------------------------
itl1_per_ha <- read_xlsx(here("TIFF in the regions of England in 2021", "data", "Regional TIFF raw out of Access.xlsx"),
                        sheet = "NUTS1-ha")

itl1_per_ha_cleaned <- itl1_per_ha %>% 
  clean_names(use_make_names = FALSE) %>%
  rename(nuts1_code = nuts1_universal_london_south_east_combined_code) %>% 
  rename(nuts1_name = nuts1_universal_london_south_east_combined_name2) %>% 
  mutate(nuts1_code = to_snake_case(nuts1_code)) %>%
  select(-c("nuts1_name", "account_item_name_tiff")) %>%
  rename("account_item_order" = "account_item_order_tiff") %>%
  pivot_longer(cols = `2010`:glue("{working_year}"), values_to = "value", names_to = "year") %>% 
  arrange(year, nuts1_code, account_item_order)