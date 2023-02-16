###
# Project - tiff in the regions of England
# Title - Region maps
# Author - Tim Buttanshaw
# Description - Make the maps for the itl1 regions
# Date - 04/11/2022
###

p_load(sf,
       svglite)

# Load England NUTS1 and NUTS3 shapefile for overlay ---------------------------
eng_nuts1 <- st_read(here("TIFF - individual regions 2021", "data",
                          "NUTS1 - shape file", "NUTS_Level_1_(January_2018)_Boundaries.dbf")) %>% 
  filter(!nuts118cd %in% c("UKL", "UKM", "UKN")) # filter out NI, Scotland and Wales

eng_nuts3 <- st_read(here("TIFF - individual regions 2021", "data",
                          "NUTS3 - shape file", "NUTS_Level_3_(January_2018)_Boundaries.dbf")) %>% 
  mutate(nuts1 = substr(nuts318cd, 1, 3)) %>% # extract NUTS1 from NUTS3
  filter(!nuts1 %in% c("UKL", "UKM", "UKN"))

# highlight the itl1 region of the map for the region -------------------------- 
itl1_map_highlight <- function(region){
  if(region == "UKIJ"){
    itl1_select <- st_read(here("TIFF - individual regions 2021", "data",
                                  "NUTS1 - shape file", "NUTS_Level_1_(January_2018)_Boundaries.dbf")) %>%
      filter(nuts118cd %in% c("UKI", "UKJ")) # filter for London and the South East
  } 
  else{
    itl1_select <- st_read(here("TIFF - individual regions 2021", "data",
                                  "NUTS1 - shape file", "NUTS_Level_1_(January_2018)_Boundaries.dbf")) %>%
      filter(nuts118cd == region) # filter for only the region
  }
  return(itl1_select)
}

# pull out and highlight the itl1 region with itl3 detail ----------------------
itl3_map_highlight <- function(region){
  if(region == "UKIJ"){
    itl3_select <- st_read(here("TIFF - individual regions 2021", "data",
                                "NUTS3 - shape file", "NUTS_Level_3_(January_2018)_Boundaries.dbf")) %>% 
      mutate(nuts1 = substr(nuts318cd, 1, 3)) %>%
      filter(nuts1 %in% c("UKI", "UKJ")) # filter for London and the South East
  } 
  else{
    itl3_select <- st_read(here("TIFF - individual regions 2021", "data",
                                "NUTS3 - shape file", "NUTS_Level_3_(January_2018)_Boundaries.dbf")) %>% 
      mutate(nuts1 = substr(nuts318cd, 1, 3)) %>%
      filter(nuts1 == region) # filter for only the region
  }
  return(itl3_select)
}

# colour gradient map ----------------------------------------------------------
itl1_region <- st_read(here("TIFF - individual regions 2021", "data",
                            "NUTS3 - shape file", "NUTS_Level_3_(January_2018)_Boundaries.dbf")) %>% 
  mutate(nuts1 = substr(nuts318cd, 1, 3)) %>% 
  filter(!nuts1 %in% c("UKL", "UKM", "UKN")) %>% 
  mutate(nuts318cd = to_snake_case(nuts318cd))

tiff_itl3 <-  everything_cleaned %>% 
  filter(final_account_item_code == 99021, year == 2021) %>% 
  select(nuts3_code, value) %>% 
  rename(nuts318cd = nuts3_code)

tiff_region_areas <- left_join(itl1_region, tiff_itl3)

tiff_gradient <- ggplot() +
  geom_sf(data = eng_nuts3, fill = "NA", colour = "#0b0c0c", size = 0.7) +
  geom_sf(data = tiff_region_areas, aes(fill = value), size = 0.7) +
  theme_void()

# https://r-charts.com/spatial/choropleth-map-ggplot2/