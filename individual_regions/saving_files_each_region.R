################################################################################
#  @Organisation - Defra Agri Foodchain Directorate (Evidence & Analysis)
#  @Project - Regional TIFF Stats Notice  HTML
#  @Title - Save files
#  @Decription - Code for saving files for publishing
#  @Author - Tim Buttanshaw
#  @Date - 07/11/2022
################################################################################

save_chart_individual_regions <- function(chart, title){
  ggsave(filename = glue("{shout_ITL_region}-{title}-{date_pub}.svg"),
         path = here::here("TIFF - individual regions 2021", "send to publishing", glue("{shout_ITL_region}")),
         plot = chart,
         width = 960,
         height = 640,
         units = "px",
         dpi = 72)
}

# Figure 1 ---------------------------------------------------------------------
save_chart_individual_regions(grid.arrange(plot1, plot2, ncol = 2), "regional_accounts_chart1")

# Figure 1.1 ---------------------------------------------------------------------
itl1_tiff_years_across_fp <- function(nuts1){
  figure <- nuts1 %>% 
    filter(final_account_item_code == 99021) %>%
    filter(year >= working_year - 5) %>% 
    group_by(year) %>% 
    summarise(tiff = sum(value)) %>%
    ungroup() %>% 
    fbs_barplot(aes(x = year, y = tiff), font = "GDS Transport Website", value_name = "\u00A3 million") +
    geom_bar(stat = "identity", fill = "#12436d") +
    labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
    theme(plot.subtitle = element_text(size = 25, vjust = 1, hjust = -0.04, family = "GDS Transport Website"),
          plot.caption = element_text(size = 20, family = "GDS Transport Website"))
  
  return(figure)
}

save_chart_individual_regions(itl1_tiff_years_across_fp(region), "regional_accounts_chart2")

# Figure 2.1 ---------------------------------------------------------------------
itl1_outputs_etc_years_across_fp <- function(nuts1){
  figure <- nuts1 %>% 
    filter(final_account_item_code %in% c('10000', '99017', '15000', '17900', '99025')) %>%
    filter(year >= working_year - 5) %>% 
    group_by(year, final_account_item_code) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    group_by(year) %>%
    mutate(total_value = sum(value)) %>% 
    mutate(label = if_else(value/total_value > 0.07, percent(value/total_value, accuracy = 0.1), '')) %>%
    ungroup() %>% 
    mutate(label = if_else(total_value < max(total_value)*0.25, '', label)) %>%
    item_code_to_name() %>% 
    fbs_stackplot(aes(x = year, y = value, fill = item_name, group = value), 
                  font = "GDS Transport Website", value_name = "\u00A3 million", text_scale = 1.4) +
    scale_fill_govuk(limits = c('Total livestock output', 'Total crop output', 'Direct payments', 
                                'Inseparable non-agricultural activities', 'Other agricultural activities')) +
    scale_fill_manual(values = c("#801650", "#f46a25", "#3d3d3d", "#12436d", "#28a197")) +
    geom_text(aes(label = label, colour = "#ffffff"), size = 6, position = position_stack(vjust = 0.5)) +
    scale_colour_identity() +
    labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(size = 25, vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
          plot.caption = element_text(size = 20, family = "GDS Transport Website")) +
    guides(fill=guide_legend(nrow=3,byrow=TRUE))
  
  return(figure)
}

save_chart_individual_regions(itl1_outputs_etc_years_across_fp(region), "regional_accounts_chart3")

# Figure 2.2 ---------------------------------------------------------------------
itl2_outputs_etc_current_year_fp <- function(nuts2){
  figure <- nuts2 %>% 
    filter(final_account_item_code %in% c('10000', '99017', '15000', '17900', '99025')) %>%
    filter(year %in% c(working_year)) %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>%
    filter(!substr(nuts2_code, 1, 3) == "uki") %>% 
    group_by(nuts2_code, final_account_item_code) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    group_by(nuts2_code) %>%
    mutate(total_value = sum(value)) %>% 
    mutate(label = if_else(value/total_value > 0.07, percent(value/total_value, accuracy = 0.1), '')) %>%
    ungroup() %>% 
    mutate(label = if_else(total_value < max(total_value)*0.25, '', label)) %>% 
    item_code_to_name() %>%
    itl2_code_to_name() %>% 
    fbs_stackplot(aes(x = itl2_name, y = value, fill = item_name, group = value), 
                  font = "GDS Transport Website", value_name = "\u00A3 million", text_scale = 1.4) +
    scale_fill_govuk(limits = c('Total livestock output', 'Total crop output', 'Direct payments', 
                                'Inseparable non-agricultural activities', 'Other agricultural activities')) +
    scale_fill_manual(values = c("#801650", "#f46a25", "#3d3d3d", "#12436d", "#28a197")) +
    geom_text(aes(label = label, colour = "#ffffff"), size = 6, position = position_stack(vjust = 0.5)) +
    scale_colour_identity() +
    labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(size = 25, vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
          plot.caption = element_text(size = 20, family = "GDS Transport Website")) +
    guides(fill=guide_legend(nrow=3,byrow=TRUE))
  
  return(figure)
}

save_chart_individual_regions(itl2_outputs_etc_current_year_fp(region), "regional_accounts_chart4")

# Figure 3.1 --------------------------------------------------------------------
itl1_inputs_and_costs_years_across_fp <- function(nuts1){
  figure <- nuts1 %>% 
    filter(final_account_item_code %in% c('19000', '99016', '23000', '28000')) %>%
    filter(year >= working_year - 5) %>% 
    group_by(year, final_account_item_code) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    group_by(year) %>%
    mutate(total_value = sum(value)) %>% 
    mutate(label = if_else(value/total_value > 0.07, percent(value/total_value, accuracy = 0.1), '')) %>%
    ungroup() %>% 
    mutate(label = if_else(total_value < max(total_value)*0.25, '', label)) %>%
    item_code_to_name() %>% 
    fbs_stackplot(aes(x = year, y = value, fill = item_name, group = value), 
                  font = "GDS Transport Website", value_name = "\u00A3 million", text_scale = 1.4) + 
    scale_fill_govuk(limits = c('Total intermediate consumption', 'Total consumption of fixed capital', 
                                'Compensation of employees', 'Rent and other associated costs')) +
    scale_fill_manual(values = c("#801650", "#f46a25", "#28a197", "#12436d")) +
    geom_text(aes(label = label, colour = "#ffffff"), size = 6, position = position_stack(vjust = 0.5)) +
    scale_colour_identity() +
    labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(size = 25, vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
          plot.caption = element_text(size = 20, family = "GDS Transport Website")) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  return(figure)
}

save_chart_individual_regions(itl1_inputs_and_costs_years_across_fp(region), "regional_accounts_chart5")

# Figure 3.2 --------------------------------------------------------------------
itl2_inputs_and_costs_current_year_fp <- function(nuts2){
  figure <- nuts2 %>% 
    filter(final_account_item_code %in% c('19000', '99016', '23000', '28000')) %>%
    filter(year %in% c(working_year)) %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>%
    filter(!substr(nuts2_code, 1, 3) == "uki") %>% 
    group_by(nuts2_code, final_account_item_code) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    group_by(nuts2_code) %>%
    mutate(total_value = sum(value)) %>% 
    mutate(label = if_else(value/total_value > 0.07, percent(value/total_value, accuracy = 0.1), '')) %>%
    ungroup() %>% 
    mutate(label = if_else(total_value < max(total_value)*0.25, '', label)) %>%
    item_code_to_name() %>%
    itl2_code_to_name() %>% 
    fbs_stackplot(aes(x = itl2_name, y = value, fill = item_name, group = value), 
                  font = "GDS Transport Website", value_name = "\u00A3 million", text_scale = 1.4) +
    scale_fill_govuk(limits = c('Total intermediate consumption', 'Total consumption of fixed capital', 
                                'Compensation of employees', 'Rent and other associated costs')) +
    scale_fill_manual(values = c("#801650", "#f46a25", "#28a197", "#12436d")) +
    geom_text(aes(label = label, colour = "#ffffff"), size = 6, position = position_stack(vjust = 0.5)) +
    scale_colour_identity() +
    labs(caption = ("Source: Defra \u00A9 Crown copyright")) +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(size = 25, vjust = 1, hjust = -0.05, family = "GDS Transport Website"),
          plot.caption = element_text(size = 20, family = "GDS Transport Website")) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  
  return(figure)
}

save_chart_individual_regions(itl2_inputs_and_costs_current_year_fp(region), "regional_accounts_chart6")