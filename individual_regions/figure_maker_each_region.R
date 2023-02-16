###
# Project - tiff in the regions of England
# Title - Individual regions calculations
# Author - Tim Buttanshaw
# Description - do any data manipulation on the appropriate itl1 level data
# Date - 30/03/2022
###
if(!require(pacman)) {
  install.packages('pacman')
  library(pacman)
}

#load other packages
p_load(here)

source(here('TIFF in the regions of England in 2021', 'scripts', 'prep_data_regional.R'))
source(here('TIFF in the regions of England in 2021', 'scripts', 'functions_regional.R'))

# Figure 1.1 -------------------------------------------------------------------
itl1_tiff_years_function <- function(nuts1){
  figure <- nuts1 %>% 
    filter(final_account_item_code == 99021) %>%
    filter(year >= working_year - 5) %>% 
    group_by(year) %>% 
    summarise(tiff = sum(value)) %>%
    ungroup() %>% 
    fbs_barplot(aes(x = year, y = tiff), font = "GDS Transport Website", value_name = "\u00A3 million") +
    geom_bar(stat = "identity", fill = "#12436d") +
    labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
    theme(plot.subtitle = element_text(vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
          plot.caption = element_text(size = 16, family = "GDS Transport Website"))
  
  return(figure)
}

# Figure 2.1 -------------------------------------------------------------------
itl1_outputs_and_subsidies_years_function <- function(nuts1){
  figure <- nuts1 %>% 
    filter(final_account_item_code %in% c('10000', '99017', '15000', '17900', '99025')) %>%
    filter(year >= working_year - 5) %>% 
    group_by(year, final_account_item_code) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    group_by(year) %>%
    mutate(total_value = sum(value)) %>% 
    mutate(label = if_else(value/total_value > 0.05, percent(value/total_value, accuracy = 0.1), '')) %>%
    ungroup() %>% 
    mutate(label = if_else(total_value < max(total_value)*0.25, '', label)) %>%
    item_code_to_name() %>% 
    fbs_stackplot(aes(x = year, y = value, fill = item_name, group = value), 
                  font = "GDS Transport Website", value_name = "\u00A3 million") +
    scale_fill_govuk(limits = c('Total livestock output', 'Total crop output', 'Direct payments', 
                                'Inseparable non-agricultural activities', 'Other agricultural activities')) +
    scale_fill_manual(values = c("#801650", "#f46a25", "#3d3d3d", "#28a197", "#12436d")) +
    geom_text(aes(label = label, colour = "#ffffff"), size = 4, position = position_stack(vjust = 0.5)) +
    scale_colour_identity() +
    labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
          plot.caption = element_text(size = 16, family = "GDS Transport Website")) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  return(figure)
}

# Figure 2.2 -------------------------------------------------------------------
itl2_outputs_and_subsidies_current_year_function <- function(nuts1){
  figure <- nuts1 %>% 
    filter(final_account_item_code %in% c('10000', '99017', '15000', '17900', '99025')) %>%
    filter(year %in% c(working_year)) %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>% 
    group_by(nuts2_code, final_account_item_code) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    group_by(nuts2_code) %>%
    mutate(total_value = sum(value)) %>% 
    mutate(label = if_else(value/total_value > 0.05, percent(value/total_value, accuracy = 0.1), '')) %>%
    ungroup() %>% 
    mutate(label = if_else(total_value < max(total_value)*0.25, '', label)) %>%
    item_code_to_name() %>%
    itl2_code_to_name() %>% 
    fbs_stackplot(aes(x = itl2_name, y = value, fill = item_name, group = value), 
                  font = "GDS Transport Website", value_name = "\u00A3 million") +
    scale_fill_govuk(limits = c('Total livestock output', 'Total crop output', 'Direct payments', 
                                'Inseparable non-agricultural activities', 'Other agricultural activities')) +
    scale_fill_manual(values = c("#801650", "#f46a25", "#3d3d3d", "#28a197", "#12436d")) +
    geom_text(aes(label = label, colour = "#ffffff"), size = 4, position = position_stack(vjust = 0.5)) +
    scale_colour_identity() +
    labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
          plot.caption = element_text(size = 16, family = "GDS Transport Website")) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  return(figure)
}

# Figure 2.3 -------------------------------------------------------------------
itl1_crops_current_year <- function(nuts1){
  nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code < 10000) %>%
    filter(final_account_item_code != '01000', final_account_item_code != '02000', final_account_item_code != '04000') %>% 
    itl1_finisher()
}

# Figure 2.4 -------------------------------------------------------------------
itl2_total_crop_current_year <- function(nuts1){
  nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code == '10000') %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>% 
    itl2_finisher()
}

# Figure 2.5 -------------------------------------------------------------------
itl1_livestock_current_year <- function(nuts1){
  nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>%
    filter(final_account_item_code %in% c('99002', '99003', '99004', '11500', '32299', '99013', '12200')) %>% 
    itl1_finisher()
}

# Figure 2.6 -------------------------------------------------------------------
itl2_total_livestock_current_year <- function(nuts1){
  nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code == '99017') %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>% 
    itl2_finisher()
}

# Figure 2.7 -------------------------------------------------------------------
itl1_other_outputs_and_subsidies_current_year <- function(nuts1){
  nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>%
    filter(final_account_item_code %in% c('15000', '17900', '99025')) %>% 
    itl1_finisher()
}

# Figure ?? --------------------------------------------------------------------

# Figure 3.1 -------------------------------------------------------------------
itl1_inputs_and_costs_years_function <- function(nuts1){
  figure <- nuts1 %>% 
    filter(final_account_item_code %in% c('19000', '99016', '23000', '28000')) %>%
    filter(year >= working_year - 5) %>% 
    group_by(year, final_account_item_code) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    group_by(year) %>%
    mutate(total_value = sum(value)) %>% 
    mutate(label = if_else(value/total_value > 0.05, percent(value/total_value, accuracy = 0.1), '')) %>%
    ungroup() %>% 
    mutate(label = if_else(total_value < max(total_value)*0.25, '', label)) %>%
    item_code_to_name() %>% 
    fbs_stackplot(aes(x = year, y = value, fill = item_name, group = value), 
                  font = "GDS Transport Website", value_name = "\u00A3 million") + 
    scale_fill_govuk(limits = c('Total intermediate consumption', 'Total consumption of fixed capital', 
                                'Compensation of employees', 'Rent and other associated costs')) +
    scale_fill_manual(values = c("#801650", "#f46a25", "#28a197", "#12436d")) +
    geom_text(aes(label = label, colour = "#ffffff"), size = 4, position = position_stack(vjust = 0.5)) +
    scale_colour_identity() +
    labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
          plot.caption = element_text(size = 16, family = "GDS Transport Website")) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  return(figure)
}

# Figure 3.2 -------------------------------------------------------------------
itl2_inputs_and_costs_current_year_function <- function(nuts1){
  figure <- nuts1 %>% 
    filter(final_account_item_code %in% c('19000', '99016', '23000', '28000')) %>%
    filter(year %in% c(working_year)) %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>% 
    group_by(nuts2_code, final_account_item_code) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    group_by(nuts2_code) %>%
    mutate(total_value = sum(value)) %>% 
    mutate(label = if_else(value/total_value > 0.05, percent(value/total_value, accuracy = 0.1), '')) %>%
    ungroup() %>% 
    mutate(label = if_else(total_value < max(total_value)*0.25, '', label)) %>%
    item_code_to_name() %>%
    itl2_code_to_name() %>% 
    fbs_stackplot(aes(x = itl2_name, y = value, fill = item_name, group = value), 
                  font = "GDS Transport Website", value_name = "\u00A3 million") +
    scale_fill_govuk(limits = c('Total intermediate consumption', 'Total consumption of fixed capital', 
                                'Compensation of employees', 'Rent and other associated costs')) +
    scale_fill_manual(values = c("#801650", "#f46a25", "#28a197", "#12436d")) +
    geom_text(aes(label = label, colour = "#ffffff"), size = 4, position = position_stack(vjust = 0.5)) +
    scale_colour_identity() +
    labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
          plot.caption = element_text(size = 16, family = "GDS Transport Website")) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  return(figure)
}

# Figure 3.3 -------------------------------------------------------------------
itl1_intermediate_consumption_current_year <- function(nuts1){
  nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code %in% c(19010, 19020, 19030, 19040, 19050, 19060, 99007, 19090, 19900)) %>%
    itl1_finisher()
}

# Figure 3.4 -------------------------------------------------------------------
itl2_total_intermediate_consumption_current_year <- function(nuts1){
  nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code == '19000') %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>% 
    itl2_finisher()
}

# Figure 3.5 -------------------------------------------------------------------
itl1_other_inputs_and_costs_current_year <- function(nuts1){
  nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>%
    filter(final_account_item_code %in% c('99016', '23000', '28000')) %>% 
    itl1_finisher()
}

# Figure ?? --------------------------------------------------------------------
