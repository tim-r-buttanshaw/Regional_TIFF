###
# Project - tiff in the regions of England
# Title - Key messages
# Author - Tim Buttanshaw
# Description - Generate the key messages for the ITL1 region specific publications
# Date - 30/03/2022
###

km_bullet_data <- function(nuts1, account_item_code){
  data <- nuts1 %>% 
    filter(final_account_item_code == account_item_code) %>% 
    filter(year %in% c(2020, 2021)) %>% 
    group_by(year) %>% 
    summarise(value = sum(value)) %>% 
    mutate(change = round(value-lag(value), digits = 0)) %>% 
    mutate(percentage_change = round(100*(value-lag(value))/lag(value), digits = 0)) %>%
    mutate(up_or_down = if_else(change[2] > 0, "an increase", "a decrease")) %>% 
    mutate(value = dollar(value, accuracy = 1, prefix = "\u00A3"))
  
  return (data)
}

km_tiff_bullet_data <- km_bullet_data(region, 99021)

km_crops_bullet_data <- km_bullet_data(region, 10000)

km_livestock_bullet_data <- km_bullet_data(region, 99017)

km_int_consumption_bullet_data <- km_bullet_data(region, 19000)

km_bullet_text <- function(data, bullet_id){
  if(data$percentage_change[2] == "0%"){
    glue("{bullet_id} in {region_name} in {working_year} was {data$value[2]} million, {data$up_or_down[2]} 
         of \u00A3{data$change[2]} million (<1%) from {previous_year}.")
  }
  else{
    glue("{bullet_id} in {region_name} in {working_year} was {data$value[2]} million, {data$up_or_down[2]} 
         of \u00A3{abs(data$change[2])} million ({data$percentage_change[2]}%) from {previous_year}.")
  }
}
