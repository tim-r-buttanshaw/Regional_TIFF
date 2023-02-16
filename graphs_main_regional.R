###
# Project - tiff in the regions of England
# Title - Regions graphing
# Author - Tim Buttanshaw
# Description - Make the graphs 
# Date - 30/03/2022
###

# tiff in itl1 in 2021 ---------------------------------------------------------
tiff_itl1 <- everything_cleaned %>% 
  filter(final_account_item_code == 99021) %>%  # filter for the item code for TIFF
  filter(year %in% c(working_year, previous_year)) %>% # filter for the working year set in the knitting parameters
  group_by(nuts1_code, year) %>% # group by the nuts1 code so the appropriate values are summed together
  summarise(tiff = round(sum(value), 0)) %>%  # sum the values to give tiff in each itl1 region
  ungroup() %>% 
  pivot_wider(names_from = "year", values_from = "tiff")

tiff_per_ha_itl1 <- itl1_per_ha_cleaned %>% 
  filter(final_account_item_code == 99021) %>%
  filter(year %in% c(working_year, previous_year)) %>%
  group_by(nuts1_code, year) %>%
  summarise(tiff_per_ha = round(sum(value), 0)) %>% 
  ungroup()%>% 
  pivot_wider(names_from = "year", values_from = "tiff_per_ha")

itl1_hectares <- tiff_itl1 %>% 
  mutate(Hectares = round((.[[3]] / tiff_per_ha_itl1[[3]]) * 1000000, digits = 0)) %>% 
  select(nuts1_code, Hectares)
  
# outputs and subsidies overview -----------------------------------------------
outputs_and_subsidies_itl1 <- everything_cleaned %>%
  filter(final_account_item_code %in% c(10000, 99017, 15000, 17900, 99025)) %>% # filter for the item code for each of the outputs
  filter(year == working_year) %>% # filter for the working year set in the knitting parameters
  group_by(nuts1_code, final_account_item_code) %>% # group by the nuts1 and item codes so the appropriate values are summed
  summarise(value = sum(value), .groups = "drop") %>% # sum the values to give each item total for each itl1 region
  ungroup() %>% # ungroup all the items so nothing weird happens - this might be unnecessary given the above line
  group_by(nuts1_code) %>%
  mutate(total_value = sum(value)) %>% 
  mutate(label = if_else(value/total_value > 0.05, percent(value/total_value, accuracy = 0.1), '')) %>%
  ungroup() %>% 
  mutate(label = if_else(total_value < max(total_value)*0.25, '', label)) %>% # calculate the percentages to label each bar
  itl1_code_to_name() %>% # convert the itl1 codes to the region names
  item_code_to_name() # convert the item codes to the item names

outputs_and_subsidies_overview <- fbs_stackplot(data = outputs_and_subsidies_itl1,
                                                aes(x = itl1_name, y = value/1000, fill = item_name, group = value),
                                                # fill sets what the groups of colours on the chart will be
                                                # group makes sure the bars appear in the correct order
                                                font = "GDS Transport Website", value_name = "\u00A3 billion") +
  scale_fill_manual(values = main_fig1_colours) + # set the colours manually (shouldn't need with ggfbs)
  geom_text(aes(label = label, colour = "#ffffff"), size = 5, position = position_stack(vjust = 0.5)) +
  # set all the parameters for the labels on the graph
  scale_colour_identity() +
  labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
  theme(legend.position = "bottom",
        plot.subtitle = element_text(vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
        plot.caption = element_text(size = 16, family = "GDS Transport Website")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

# crop producers ---------------------------------------------------------------
top3_crops_itl1_across <- top3_finder(10000) # see functions

# chart for if we want it non-interactive and to flip the x and y
# top_3_crop_plot <- fbs_barplot(data = crop_across_years, aes(x = year, y = crop_out, fill = nuts1_code), 
#                                font = "GDS Transport Website", value_name = "\u00A3 million") +
#   scale_fill_manual(values = colour_chart[1:3]) +
#   labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
#   theme(legend.position = "bottom",
#         plot.subtitle = element_text(vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
#         plot.caption = element_text(size = 16, family = "GDS Transport Website"))

# livestock producers ----------------------------------------------------------
top3_livestock_itl1_across <- top3_finder(99017) # see functions

# other outputs and subsidies --------------------------------------------------
past_2_years_other_outputs_etc_years_across <- everything_cleaned %>% 
  filter(final_account_item_code %in% c('15000', '17900', '99025')) %>%
  filter(year >= previous_year) %>% 
  group_by(year, nuts1_code) %>% 
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(value = round(value, digits = 0)) %>% 
  pivot_wider(names_from = "year", values_from = value) %>% 
  arrange(desc(.[3])) %>% 
  itl1_code_to_name()
  

# inputs and costs overview ----------------------------------------------------
inputs_and_costs_itl1 <- everything_cleaned %>% 
  filter(final_account_item_code %in% c(19000, 99016, 23000, 28000)) %>% # filter for the item code for each of the inputs
  filter(year == 2021) %>% # filter for the working year set in the knitting params
  group_by(nuts1_code, final_account_item_code) %>% # group by the nuts1 and item codes so the appropriate values are summed 
  summarise(costs = sum(value), .groups = "drop") %>% # sum the values to give each item total for each itl1 region
  ungroup() %>% # ungroup all the items so nothing weird happens - this might be unnecessary given the above line 
  group_by(nuts1_code) %>%
  mutate(total_costs = sum(costs)) %>% 
  mutate(label = if_else(costs/total_costs > 0.05, percent(costs/total_costs, accuracy = 0.1), '')) %>%
  ungroup() %>% 
  mutate(label = if_else(total_costs < max(total_costs)*0.25, '', label)) %>% # calculate the percentages to label each bar
  itl1_code_to_name() %>% # convert the itl1 codes to the region names
  item_code_to_name() # convert the item codes to the item names

inputs_and_costs_overview <- fbs_stackplot(data = inputs_and_costs_itl1, 
                                           aes(x = itl1_name, y = costs/1000, fill = item_name, group = costs), 
                                                font = "GDS Transport Website", value_name = "\u00A3 billion") +
  scale_fill_manual(values = main_fig2_colours) +
  geom_text(aes(label = label, colour = "#ffffff"), size = 5, position = position_stack(vjust = 0.5)) +
  scale_colour_identity() +
  labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
  theme(legend.position = "bottom",
        plot.subtitle = element_text(vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
        plot.caption = element_text(size = 16, family = "GDS Transport Website")) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))



