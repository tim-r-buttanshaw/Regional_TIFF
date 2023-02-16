###
# Project - tiff in the regions of England
# Title - Regions misc.
# Author - Tim Buttanshaw
# Description - Do anything miscellaneous required for the regional tiff 
# Date - 30/03/2022
###

# subsidies as a percentage of TIFF --------------------------------------------
subsidies_percentage <- everything_cleaned %>% 
  filter(final_account_item_code %in% c(99025, 99021)) %>% 
  filter(year == working_year) %>% 
  group_by(nuts1_code, final_account_item_code) %>% 
  summarise(totals = sum(value)) %>% 
  pivot_wider(names_from = final_account_item_code, values_from = totals) %>%
  mutate(value = `99025`/`99021`) %>% 
  arrange(desc(value)) %>% 
  mutate(value = percent(value, accuracy = 0.1), accuracy = 0.1) %>% 
  select(nuts1_code, value)