################################################################################
#  @Organisation - Defra Agri Foodchain Directorate (Evidence & Analysis)
#  @Project - Regional TIFF Stats Notice  HTML
#  @Title - Save files
#  @Decription - Code for saving files for publishing
#  @Author - Tim Buttanshaw
#  @Date - 07/11/2022
################################################################################

save_chart_regional <- function(chart, title){
  ggsave(filename = glue("{title}-regions_of_england-{date_pub}.svg"),
         path = here::here("TIFF in the regions of England in 2021", "send to publishing"),
         plot = chart,
         width = 960,
         height = 640,
         units = "px",
         dpi = 72)
}

outputs_and_subsidies_to_save <- everything_cleaned %>%
  filter(final_account_item_code %in% c(10000, 99017, 15000, 17900, 99025)) %>% # filter for the item code for each of the outputs
  filter(year == working_year) %>% # filter for the working year set in the knitting parameters
  group_by(nuts1_code, final_account_item_code) %>% # group by the nuts1 and item codes so the appropriate values are summed
  summarise(value = sum(value), .groups = "drop") %>% # sum the values to give each item total for each itl1 region
  ungroup() %>% # ungroup all the items so nothing weird happens - this might be unnecessary given the above line
  group_by(nuts1_code)  %>%
  mutate(total_value = sum(value)) %>% 
  mutate(label = if_else(value/total_value > 0.07, percent(value/total_value, accuracy = 0.1), '')) %>%
  ungroup() %>% 
  mutate(label = if_else(total_value < max(total_value)*0.25, '', label)) %>% # calculate the percentages to label each bar
  itl1_code_to_name() %>% # convert the itl1 codes to the region names
  item_code_to_name() %>% 
  fbs_stackplot(aes(x = itl1_name, y = value/1000, fill = item_name, group = value),
                # fill sets what the groups of colours on the chart will be
                # group makes sure the bars appear in the correct order
                font = "GDS Transport Website", value_name = "\u00A3 billion", text_scale = 1.4) +
  scale_fill_manual(values = main_fig1_colours) + # set the colours manually (shouldn't need with ggfbs)
  geom_text(aes(label = label, colour = "#ffffff"), size = 7, position = position_stack(vjust = 0.5)) +
  # set all the parameters for the labels on the graph
  scale_colour_identity() +
  labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
  theme(legend.position = "bottom",
        plot.subtitle = element_text(size = 25, vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
        plot.caption = element_text(size = 20, family = "GDS Transport Website"),
        plot.title = element_text(hjust = 0.05)) +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))

save_chart_regional(outputs_and_subsidies_to_save, "chart1")

inputs_and_costs_to_save <- everything_cleaned %>% 
  filter(final_account_item_code %in% c(19000, 99016, 23000, 28000)) %>% # filter for the item code for each of the inputs
  filter(year == 2021) %>% # filter for the working year set in the knitting params
  group_by(nuts1_code, final_account_item_code) %>% # group by the nuts1 and item codes so the appropriate values are summed 
  summarise(costs = sum(value), .groups = "drop") %>% # sum the values to give each item total for each itl1 region
  ungroup() %>% # ungroup all the items so nothing weird happens - this might be unnecessary given the above line 
  group_by(nuts1_code) %>%
  mutate(total_costs = sum(costs)) %>% 
  mutate(label = if_else(costs/total_costs > 0.07, percent(costs/total_costs, accuracy = 0.1), '')) %>%
  ungroup() %>% 
  mutate(label = if_else(total_costs < max(total_costs)*0.25, '', label)) %>% # calculate the percentages to label each bar
  itl1_code_to_name() %>% # convert the itl1 codes to the region names
  item_code_to_name() %>% # convert the item codes to the item names
  fbs_stackplot(aes(x = itl1_name, y = costs/1000, fill = item_name, group = costs),
                font = "GDS Transport Website", value_name = "\u00A3 billion", text_scale = 1.4) +
  scale_fill_manual(values = main_fig2_colours) +
  geom_text(aes(label = label, colour = "#ffffff"), size = 7, position = position_stack(vjust = 0.5)) +
  scale_colour_identity() +
  labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
  theme(legend.position = "bottom",
        plot.subtitle = element_text(size = 25, vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
        plot.caption = element_text(size = 20, family = "GDS Transport Website"),
        plot.title = element_text(hjust = 0.05)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

save_chart_regional(inputs_and_costs_to_save, "chart2")

# itl1 current prices ----------------------------------------------------------
nuts1_for_publishing_function <- function(nuts1){
  output <- nuts1 %>% 
    group_by(account_item_order, final_account_item_code, year) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(value = round(value, digits = 3)) %>% 
    pivot_wider(names_from = year, values_from = value) %>% 
    item_code_to_name()
}

ITL1_cp_wb <-  createWorkbook()

itl1_cp_Sheet1 <- createSheet(wb = ITL1_cp_wb, sheetName = "TLC")
tlc_publication_data <- as.data.frame(nuts1_for_publishing_function(ukc))

itl1_cp_Sheet2 <- createSheet(wb = ITL1_cp_wb, sheetName = "TLD")
tld_publication_data <- as.data.frame(nuts1_for_publishing_function(ukd))

itl1_cp_Sheet3 <- createSheet(wb = ITL1_cp_wb, sheetName = "TLE")
tle_publication_data <- as.data.frame(nuts1_for_publishing_function(uke))

itl1_cp_Sheet4 <- createSheet(wb = ITL1_cp_wb, sheetName = "TLF")
tlf_publication_data <- as.data.frame(nuts1_for_publishing_function(ukf))

itl1_cp_Sheet5 <- createSheet(wb = ITL1_cp_wb, sheetName = "TLG")
tlg_publication_data <- as.data.frame(nuts1_for_publishing_function(ukg))

itl1_cp_Sheet6 <- createSheet(wb = ITL1_cp_wb, sheetName = "TLH")
tlh_publication_data <- as.data.frame(nuts1_for_publishing_function(ukh))

itl1_cp_Sheet7 <- createSheet(wb = ITL1_cp_wb, sheetName = "TLIJ")
tlij_publication_data <- as.data.frame(nuts1_for_publishing_function(ukij))

itl1_cp_Sheet8 <- createSheet(wb = ITL1_cp_wb, sheetName = "TLK")
tlk_publication_data <- as.data.frame(nuts1_for_publishing_function(ukk))


addDataFrame(tlc_publication_data, sheet = itl1_cp_Sheet1, startColumn = 1, row.names = FALSE)
addDataFrame(tld_publication_data, sheet = itl1_cp_Sheet2, startColumn = 1, row.names = FALSE)
addDataFrame(tle_publication_data, sheet = itl1_cp_Sheet3, startColumn = 1, row.names = FALSE)
addDataFrame(tlf_publication_data, sheet = itl1_cp_Sheet4, startColumn = 1, row.names = FALSE)
addDataFrame(tlg_publication_data, sheet = itl1_cp_Sheet5, startColumn = 1, row.names = FALSE)
addDataFrame(tlh_publication_data, sheet = itl1_cp_Sheet6, startColumn = 1, row.names = FALSE)
addDataFrame(tlij_publication_data, sheet = itl1_cp_Sheet7, startColumn = 1, row.names = FALSE)
addDataFrame(tlk_publication_data, sheet = itl1_cp_Sheet8, startColumn = 1, row.names = FALSE)

saveWorkbook(ITL1_cp_wb, here("send to publishing", glue("ITL1_regional_accounts_current_prices-{date_pub}.xlsx")))

# itl2 current prices ----------------------------------------------------------
nuts2_for_publishing_function <- function(nuts1){
  output <- nuts1 %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>% 
    group_by(nuts2_code, account_item_order, final_account_item_code, year) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(value = round(value, digits = 3)) %>% 
    pivot_wider(names_from = year, values_from = value) %>% 
    item_code_to_name() %>%
    itl2_code_to_name()
}

ITL2_cp_wb <-  createWorkbook()

itl2_cp_Sheet1 <- createSheet(wb = ITL2_cp_wb, sheetName = "TLC")
itl2_tlc_publication_data <- as.data.frame(nuts2_for_publishing_function(ukc))

itl2_cp_Sheet2 <- createSheet(wb = ITL2_cp_wb, sheetName = "TLD")
itl2_tld_publication_data <- as.data.frame(nuts2_for_publishing_function(ukd))

itl2_cp_Sheet3 <- createSheet(wb = ITL2_cp_wb, sheetName = "TLE")
itl2_tle_publication_data <- as.data.frame(nuts2_for_publishing_function(uke))

itl2_cp_Sheet4 <- createSheet(wb = ITL2_cp_wb, sheetName = "TLF")
itl2_tlf_publication_data <- as.data.frame(nuts2_for_publishing_function(ukf))

itl2_cp_Sheet5 <- createSheet(wb = ITL2_cp_wb, sheetName = "TLG")
itl2_tlg_publication_data <- as.data.frame(nuts2_for_publishing_function(ukg))

itl2_cp_Sheet6 <- createSheet(wb = ITL2_cp_wb, sheetName = "TLH")
itl2_tlh_publication_data <- as.data.frame(nuts2_for_publishing_function(ukh))

itl2_cp_Sheet7 <- createSheet(wb = ITL2_cp_wb, sheetName = "TLIJ")
itl2_tlij_publication_data <- as.data.frame(nuts2_for_publishing_function(ukij))

itl2_cp_Sheet8 <- createSheet(wb = ITL2_cp_wb, sheetName = "TLK")
itl2_tlk_publication_data <- as.data.frame(nuts2_for_publishing_function(ukk))

addDataFrame(itl2_tlc_publication_data, sheet = itl2_cp_Sheet1, startColumn = 1, row.names = FALSE)
addDataFrame(itl2_tld_publication_data, sheet = itl2_cp_Sheet2, startColumn = 1, row.names = FALSE)
addDataFrame(itl2_tle_publication_data, sheet = itl2_cp_Sheet3, startColumn = 1, row.names = FALSE)
addDataFrame(itl2_tlf_publication_data, sheet = itl2_cp_Sheet4, startColumn = 1, row.names = FALSE)
addDataFrame(itl2_tlg_publication_data, sheet = itl2_cp_Sheet5, startColumn = 1, row.names = FALSE)
addDataFrame(itl2_tlh_publication_data, sheet = itl2_cp_Sheet6, startColumn = 1, row.names = FALSE)
addDataFrame(itl2_tlij_publication_data, sheet = itl2_cp_Sheet7, startColumn = 1, row.names = FALSE)
addDataFrame(itl2_tlk_publication_data, sheet = itl2_cp_Sheet8, startColumn = 1, row.names = FALSE)

saveWorkbook(ITL2_cp_wb, here("send to publishing", glue("ITL2_regional_accounts_current_prices-{date_pub}.xlsx")))

# itl3 current prices ----------------------------------------------------------
nuts3_for_publishing_function <- function(nuts1){
  output <- nuts1 %>%
    filter(final_account_item_code %in% c('10000', '99017', '99011', '19000', '99024', '99021')) %>% 
    group_by(nuts3_code, account_item_order, final_account_item_code, year) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(value = round(value, digits = 3)) %>% 
    pivot_wider(names_from = year, values_from = value) %>% 
    item_code_to_name()
}

ITL3_cp_wb <-  createWorkbook()

itl3_cp_Sheet1 <- createSheet(wb = ITL3_cp_wb, sheetName = "TLC")
itl3_tlc_publication_data <- as.data.frame(nuts3_for_publishing_function(ukc))

itl3_cp_Sheet2 <- createSheet(wb = ITL3_cp_wb, sheetName = "TLD")
itl3_tld_publication_data <- as.data.frame(nuts3_for_publishing_function(ukd))

itl3_cp_Sheet3 <- createSheet(wb = ITL3_cp_wb, sheetName = "TLE")
itl3_tle_publication_data <- as.data.frame(nuts3_for_publishing_function(uke))

itl3_cp_Sheet4 <- createSheet(wb = ITL3_cp_wb, sheetName = "TLF")
itl3_tlf_publication_data <- as.data.frame(nuts3_for_publishing_function(ukf))

itl3_cp_Sheet5 <- createSheet(wb = ITL3_cp_wb, sheetName = "TLG")
itl3_tlg_publication_data <- as.data.frame(nuts3_for_publishing_function(ukg))

itl3_cp_Sheet6 <- createSheet(wb = ITL3_cp_wb, sheetName = "TLH")
itl3_tlh_publication_data <- as.data.frame(nuts3_for_publishing_function(ukh))

itl3_cp_Sheet7 <- createSheet(wb = ITL3_cp_wb, sheetName = "TLIJ")
itl3_tlij_publication_data <- as.data.frame(nuts3_for_publishing_function(ukij))

itl3_cp_Sheet8 <- createSheet(wb = ITL3_cp_wb, sheetName = "TLK")
itl3_tlk_publication_data <- as.data.frame(nuts3_for_publishing_function(ukk))

addDataFrame(itl3_tlc_publication_data, sheet = itl3_cp_Sheet1, startColumn = 1, row.names = FALSE)
addDataFrame(itl3_tld_publication_data, sheet = itl3_cp_Sheet2, startColumn = 1, row.names = FALSE)
addDataFrame(itl3_tle_publication_data, sheet = itl3_cp_Sheet3, startColumn = 1, row.names = FALSE)
addDataFrame(itl3_tlf_publication_data, sheet = itl3_cp_Sheet4, startColumn = 1, row.names = FALSE)
addDataFrame(itl3_tlg_publication_data, sheet = itl3_cp_Sheet5, startColumn = 1, row.names = FALSE)
addDataFrame(itl3_tlh_publication_data, sheet = itl3_cp_Sheet6, startColumn = 1, row.names = FALSE)
addDataFrame(itl3_tlij_publication_data, sheet = itl3_cp_Sheet7, startColumn = 1, row.names = FALSE)
addDataFrame(itl3_tlk_publication_data, sheet = itl3_cp_Sheet8, startColumn = 1, row.names = FALSE)

saveWorkbook(ITL3_cp_wb, here("send to publishing", glue("ITL3_regional_accounts_current_prices-{date_pub}.xlsx")))


# other stuf ----

# nuts1_publication_data <- nuts1_for_publishing_function(everything_cleaned)
# 
# write_csv(nuts1_publication_data, 
#           file = here::here("send to publishing", glue("regional_accounts_nuts1_level-{date_pub}.csv")))