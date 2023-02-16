###
# Project - tiff in the regions of England
# Title - Individual regions bullet point maker
# Author - Tim Buttanshaw
# Description - prep everything for the bullet points under each figure
# Date - 27/10/2022
###

# figure 1.1 tiff for the past 6 years -----------------------------------------
tiff_past_6_years_bullet_data <- function(nuts1){
  output <-  nuts1 %>% # start with the the regional data for that run
    filter(final_account_item_code == 99021) %>% # filter for just the TIFF item
    filter(year >= working_year - 5) %>% # select the last 6 years (-5 inclusive)
    group_by(year) %>% # group by the year
    summarise(tiff = sum(value)) %>% # sum all the TIFF values for the ITL3 regions in each year
    ungroup() %>%  # ungroup t stop any weirdness 
    mutate(change = tiff-lag(tiff)) %>% # calculate the change in TIFF in each year (lag means the row above)
    mutate(percentage_change = percent(change/lag(tiff), accuracy = 1)) %>% # calculate the percentage change in TIFF 
    mutate(rank = ordinal(rank(-tiff))) # this sets the order of the TIFFs with a 1st, 2nd... etc
  
  return(output)
}

tiff_past_6_years_bullet_1 <- function(data){
  data <- data %>% # start with the data input 
    filter(year == working_year) %>% # select on the working year
    mutate(up_down = if_else(change > 0, "an increase", "a decrease")) %>% # dictate whether TIFF is up or down from the previous year
    mutate(change = abs(change)) %>% # make the change value absolute for printing in the text
    mutate_if(is.numeric, dollar_format(accuracy = 1, prefix = "\u00A3")) # make everything over 1,000 have a comma
  
  if(data$percentage_change == "0%"){ # if the value rounds to 0 replace the 0% with a <1%
    data$percentage_change == ">1%"
  }
  
  bullet <- glue("TIFF in {region_name} in {working_year} was {data$tiff} million, {data$up_down} of {data$change} 
                 million ({data$percentage_change}) from {previous_year}.")
  
  return(bullet)
}

tiff_past_6_years_bullet_2 <- function(data){
  current_data <- data %>% # start with the data input 
    filter(year == working_year) %>% # filter for the working
    mutate_if(is.numeric, dollar_format(accuracy = 1, prefix = "\u00A3")) # make everything ov
  
  top_tiff <- data %>% # start with the input data
    filter(rank == "1st") # select the year with the highest TIFF of the past 6
  
  if(current_data$rank == "1st"){ # if TIFF in the current year was the highest in the last 6 
    bullet <- glue("TIFF in {working_year} was the highest value for TIFF in {region_name} in the last 6 years.")
  }
  else if(current_data$rank == "6th"){ # if TIFF in the current year was the lowest in the last 6 
    bullet <- glue("TIFF in {working_year} was the lowest value for TIFF in {region_name} in the last 6 years, with 
                   the highest value in {top_tiff$year}.")
  } 
  else{ # if TIFF in the current year 2nd - 5th highest 
    bullet <- glue("TIFF in {working_year} was the {current_data$rank} highest value for TIFF in {region_name} in the 
                   last 6 years, with the highest value in {top_tiff$year}.")
  }
  return(bullet)
}

# figure 2.1 outputs and subsidies in past 6 years -----------------------------
outs_and_subs_itl1_bullet_data <- function(nuts1){
  midway <- nuts1 %>% # start with the input data 
    filter(final_account_item_code %in% c('10000', '99017', '15000', '17900', '99025')) %>% # select the outputs and subsidies items
    filter(year >= working_year - 5) %>% # select the past 6 years
    group_by(year, final_account_item_code) %>% # group by year and item so the right items are summed
    summarise(value = sum(value)) %>% # sum the items from the ITL3 regions in each year
    ungroup() %>% # ungroup so theres no weirdness 
    group_by(year) %>% # regroup by year for more sums
    mutate(label = 100*value/sum(value)) %>% # make the labels for the graph by calculating each items percentage of the total
    item_code_to_name() %>% # replace the item codes with their names 
    ungroup() # ungroup to stop any weirdness
  
  biggest_contributor <- midway %>%  # data from above
    group_by(year) %>% # group by year
    mutate(max = max(value)) %>% # create a column with the max value in each year
    ungroup() %>% # ungroup
    filter(max == value) %>% # select the items with the largest value in each year
    group_by(item_name) %>% # re-group by item name
    summarise(count = n()) %>% # count the number of times each item was the biggest contributor to outputs
    ungroup() %>% # ungroup
    arrange(desc(count)) # arrange from top to bottom with the most common top contributor at the top
  
  rank_year_biggest_contributor <- midway %>% # data from above
    filter(item_name == biggest_contributor$item_name) %>% # select only items which have been the biggest percentage of outputs
    mutate(rank = ordinal(rank(-value))) # sort the items in order of their percentage share
  
  output <- midway %>% # data from above
    filter(item_name %in% c(biggest_contributor$item_name, "Direct payments")) %>% # select biggest contributors or direct payments
    group_by(item_name) %>% # group by item name
    mutate(rank = ordinal(rank(-label))) %>% # rank the items by when they contributed the most
    mutate(label = round(label, digits = 1)) %>% # round the label to 1dp
    ungroup() # ungroup
  
  return(output)
}

# calculate the total output for use in bullet points 
total_output_working_year <- region %>%
  filter(year == working_year, final_account_item_code %in% c('10000', '99017', '15000', '17900', '99025')) %>%
  summarise(total_output = sum(value)) %>% 
  mutate(total_output = round(total_output))

outs_and_subs_itl1_bullet_1 <- function(data){
  biggest_item <- data %>% 
    filter(item_name != "Direct payments")
  
  working_year_percent <- data %>% 
    filter(year == working_year)%>%
    filter(item_name != "Direct payments") %>% 
    select(label, rank)
  
  part1 <- glue("{biggest_item$item_name[1]} has made the biggest contribution to outputs and subsidies in {region_name} in")
  
  if(nrow(biggest_item) == 6){
    part2 <- glue("each of the past 6 years,")
  }
  else{
    part2 <- glue("{nrow(biggest_item)} out of the past 6 years,")
  }
  
  part3 <- glue("contributing at least {min(biggest_item$label)}%. In {working_year} {biggest_item$item_name[1]} contributed 
                {working_year_percent$label}% of the \u00A3{total_output_working_year} million of outputs and subsidies,")
  
    
  if(working_year_percent$rank == "1st"){
    part4 <- glue("the highest proportion of the past 6 years.")
  }
  else if(working_year_percent$rank == "6th"){
    part4 <- glue("the lowest proportion of the past 6 years.")
  }
  else{
    part4 <- glue("the {working_year_percent$rank} highest proportion of the past 6 years.")
  }
  
  bullet <- glue("{part1} {part2} {part3} {part4}")
  
  return(bullet)
}

outs_and_subs_itl1_bullet_2 <- function(data){
  min_payment <- data %>% 
    filter(item_name == "Direct payments") %>% 
    select(label) %>% 
    filter(label == min(label))
  
  max_payment <- data %>% 
    filter(item_name == "Direct payments") %>% 
    select(label) %>% 
    filter(label == max(label))
  
  current_payment <- data %>% 
    filter(item_name == "Direct payments") %>%
    filter(year == working_year) %>% 
    select(label)
  
  bullet <- glue("Direct payments have made up between {min_payment[1,1]}% and {max_payment[1,1]}% of the total 
  outputs and subsidies in the past 6 years for {region_name}. In {working_year}, direct payments made up {current_payment[1,1]}% 
                 of total outputs and subsidies.")
  
  return(bullet)
}

# figure 2.2 outputs and subsidies at itl2 -------------------------------------
outs_and_subs_itl2_bullet_data <- function(nuts1){
  midway <- nuts1 %>% 
    filter(final_account_item_code %in% c('10000', '99017', '15000', '17900', '99025')) %>%
    filter(year %in% c(working_year)) %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>% 
    group_by(nuts2_code, final_account_item_code) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    group_by(nuts2_code) %>% 
    mutate(label = percent(value/sum(value), accuracy = 0.1)) %>%
    item_code_to_name() %>%
    itl2_code_to_name() %>% 
    group_by(itl2_name) %>% 
    summarise(total = sum(value)) %>% 
    ungroup() %>% 
    arrange(total) %>% 
    mutate(increase = round(100*(total-lag(total))/lag(total), digits = 1)) %>% 
    mutate(total = round(total, digits = 0))%>% 
    mutate(increase = if_else(increase == 0, "<1", as.character(increase)))
  
  return(midway)
}

outs_and_subs_itl2_bullet_1 <- function(data){
  itl1_total <- data %>% 
    summarise(grand_total = sum(total))
  
  percentage_of_total <- round(100 * tail(data$total, 1) / itl1_total$grand_total, digits = 0)
    
  
  bullet <- glue("{tail(data$itl2_name, 1)} had the highest value for outputs and subsidies in {region_name} in {working_year} with a value of 
  \u00A3{tail(data$total, 1)} million, {percentage_of_total}%  of the total outputs and subsidies for {region_name}.")
  
  return(bullet)
}

outs_and_subs_itl2_bullet_2 <- function(data){
  itl1_total <- data %>% 
    summarise(grand_total = sum(total))
  
  percentage_of_total <- round(100 * head(data$total, 1) / itl1_total$grand_total, digits = 0)
  
  bullet <- glue("{head(data$itl2_name, 1)} had the lowest value for outputs and subsidies in {region_name} in {working_year} with a 
                 value of \u00A3{head(data$total, 1)} million, {percentage_of_total}%  of the total outputs and subsidies for 
                 {region_name}.")
  
  return(bullet)
}

# figure 2.3 ilt1 crops  -------------------------------------------------------
crops_data_each_region <- function(nuts1){
  midway <- nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code < 10000) %>%
    filter(final_account_item_code != '01000', final_account_item_code != '02000', 
           final_account_item_code != '04000') %>% 
    group_by(final_account_item_code, year) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    pivot_wider(names_from = 'year', values_from = 'value') %>% 
    arrange(desc(.[[3]])) %>% 
    item_code_to_name() %>%
    clean_names(case = 'title', use_make_names = FALSE) %>%
    mutate(change = .[[3]] - .[[2]]) %>% 
    mutate(percent_change = 100*change/.[[2]])  %>%
    mutate(up_down = if_else(change > 0, "an increase", "a decrease"))
  
  output <- midway %>% 
    mutate_if(is.numeric, round, digits = 0) %>% 
    mutate(percent_change = if_else(percent_change == 0, "<1", as.character(percent_change)))
  
  return(output)
}

crop_bullet_1 <- function(data){
    bullet <- glue("The largest crop output in {region_name} in {working_year} was {head(data[1], 1)} with a value of 
    \u00A3{head(data[3], 1)} million, {head(data$up_down, 1)} of \u00A3{abs(head(data$change, 1))} million 
                   ({head(data$percent_change, 1)}%) from {previous_year}.")
  
  return(bullet)
}

crop_livestock_bullet_2 <- function(data){
  data_sorted <- data %>% 
    arrange(desc(change))
  
  if(head(data_sorted$up_down, 1) == "a decrease"){ # if all values decreased
    if(head(data_sorted[1], 1) == head(data[1], 1)){ # if the smallest decrease was in the biggest output
      bullet <- glue("All outputs decreased in {region_name} in {working_year}, the smallest decrease in a value was also in 
      {data_sorted[1,1]}. The second smallest decrease was in {data_sorted[2,1]}, decreasing by \u00A3{abs(data_sorted[2,4])} 
                     million ({data_sorted[2,5]}%) from {previous_year} to \u00A3{data_sorted[2,3]} million in {working_year}.")
    }
    else{ # if the smallest decrease was not in the biggest output
      bullet <- glue("All outputs decreased in {region_name} in {working_year}, but the smallest decrease in a value was in 
      {data_sorted[1,1]}, decreasing by \u00A3{abs(data_sorted[1,4])} million ({data_sorted[1,5]}%) from {previous_year} to 
                     \u00A3{data_sorted[1,3]} million in {working_year}.")
    }
  } else{ # if not all the values decreased 
    if(head(data_sorted[1], 1) == head(data[1], 1)){ # if the largest increase was in the biggest output
      bullet <- glue("The largest increase in a value in {region_name} in {working_year} was also in {data_sorted[1,1]}. The second 
      largest increase was in {data_sorted[2,1]}, increasing by \u00A3{data_sorted[2,4]} million 
                     ({data_sorted[2,5]}%) from {previous_year} to \u00A3{data_sorted[2,3]} million in {working_year}.")
    }
    else{ # if the largest increase was not in the biggest output
      bullet <- glue("The largest increase in a value in {region_name} in {working_year} was in {data_sorted[1,1]}, increasing by 
      \u00A3{data_sorted[1,4]} million ({data_sorted[1,5]}%) from {previous_year} to \u00A3{data_sorted[1,3]} million in 
                     {working_year}.")
    }
  }
  return(bullet)
}

crop_livestock_bullet_3 <- function(data){
  data_sorted <- data %>% 
    arrange(change)
  if(head(data_sorted$up_down, 1) == "an increase"){ # if all values increased
    if(head(data_sorted[1], 1) == head(data[1], 1)){ # if the smallest increase was in the biggest output
      bullet <- glue("All outputs increased in {region_name} in {working_year}, the smallest increase in a value was also in 
      {data_sorted[1,1]}. The second smallest increase was in {data_sorted[2,1]}, increasing by \u00A3{data_sorted[2,4]} million 
                     ({data_sorted[2,5]}%) from {previous_year} to \u00A3{data_sorted[2,3]} million in {working_year}.")
    }
    else{ # if the smallest increase was not in the biggest output
      bullet <- glue("All outputs increased in {region_name} in {working_year}, but the smallest increase in a value was in 
      {data_sorted[1,1]}, which increased by \u00A3{abs(data_sorted[1,4])} million ({data_sorted[1,5]}%) from {previous_year} to 
                     \u00A3{data_sorted[1,3]} million in {working_year}.")
    }
  }else{ # if not all values increased
    if(head(data_sorted[1], 1) == head(data[1], 1)){ # if the largest decrease was in the biggest output
      bullet <- glue("The largest decrease in a value in {region_name} in {working_year}, was also in {data_sorted[1,1]}. The second 
      largest decrease was in {data_sorted[2,1]}, decreasing by \u00A3{abs(data_sorted[2,4])} million ({data_sorted[2,5]}%) from 
                     {previous_year} to \u00A3{data_sorted[2,3]} million in {working_year}.")
    }
    else{ # if the largest decrease was not in the biggest output
      bullet <- glue("The largest decrease in a value in {region_name} in {working_year}, was in {data_sorted[1,1]}, decreasing by 
                     \u00A3{abs(data_sorted[1,4])} million ({data_sorted[1,5]}%) from {previous_year} to 
                     \u00A3{data_sorted[1,3]} million in {working_year}.")
    }
  }
  
  return(bullet)
}

# figure 2.4 itl2 crops --------------------------------------------------------
crops_data_itl2 <- function(nuts1){
  midway <- nuts1 %>% filter(year %in% c(previous_year, working_year)) %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code == '10000') %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>% 
    group_by(nuts2_code, year) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    pivot_wider(names_from = 'year', values_from = 'value') %>% 
    arrange(desc(.[[3]])) %>%
    itl2_code_to_name() %>% 
    clean_names(case = 'title', use_make_names = FALSE) %>%
    mutate(change = .[[3]] - .[[2]]) %>% 
    mutate(percent_change = 100*change/.[[2]])  %>%
    mutate(up_down = if_else(change > 0, "an increase", "a decrease"))
  
  output <- midway %>% 
    mutate_if(is.numeric, round, digits = 0) %>% 
    mutate(percent_change = if_else(percent_change == 0, "<1", as.character(percent_change)))
  
  return(output)
}

crop_bullet_4 <- function(data){
  bullet <- glue("{data[1,1]} had the largest crop output of the ITL2 regions within {region_name} in {working_year}. This was 
                 {data[1,6]} of \u00A3{abs(data[1,4])} million ({data[1,5]}%) from {previous_year} to \u00A3{data[1,3]} million in 
  {working_year}.")
  
  return(bullet)
}

crop_bullet_5 <- function(data){
  bullet <- glue("{tail(data[1], 1)} had the smallest crop output of the ITL2 regions within {region_name} 
                  in {working_year}. This was {tail(data[6], 1)} of \u00A3{abs(tail(data[4], 1))} million 
                  ({tail(data[5], 1)}%) from {previous_year} to \u00A3{tail(data[3], 1)} million in {working_year}.")
  
  return(bullet)
}

# figure 2.5 itl1 livestock ----------------------------------------------------
livestock_data_each_region <- function(nuts1){
  midway <- nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code %in% c('99002', '99003', '99004', '11500', '32299', '99013', '12200')) %>% 
    group_by(final_account_item_code, year) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    pivot_wider(names_from = 'year', values_from = 'value') %>% 
    arrange(desc(.[[3]])) %>% 
    item_code_to_name() %>%
    clean_names(case = 'title', use_make_names = FALSE) %>%
    mutate(change = .[[3]] - .[[2]]) %>% 
    mutate(percent_change = 100*change/.[[2]])  %>%
    mutate(up_down = if_else(change > 0, "an increase", "a decrease"))
  
  output <- midway %>% 
    mutate_if(is.numeric, round, digits = 0) %>% 
    mutate(percent_change = if_else(percent_change == 0, "<1", as.character(percent_change)))
  
  return(output)
}

livestock_bullet_1 <- function(data){
  bullet <- glue("The largest livestock output in {region_name} in {working_year} was {head(data[1], 1)} with a 
                 value of \u00A3{head(data[3], 1)} million, which was {head(data$up_down, 1)} of 
                 \u00A3{abs(head(data$change, 1))} million ({head(data$percent_change, 1)}%) from {previous_year}.")
  
  return(bullet)
}
# figure 2.6 itl2 livestock ----------------------------------------------------
livestock_data_itl2 <- function(nuts1){
  midway <- nuts1 %>% filter(year %in% c(previous_year, working_year)) %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code == '99017') %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>% 
    group_by(nuts2_code, year) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    pivot_wider(names_from = 'year', values_from = 'value') %>% 
    arrange(desc(.[[3]])) %>%
    itl2_code_to_name() %>% 
    clean_names(case = 'title', use_make_names = FALSE) %>%
    mutate(change = .[[3]] - .[[2]]) %>% 
    mutate(percent_change = 100*change/.[[2]])  %>%
    mutate(up_down = if_else(change > 0, "an increase", "a decrease"))
  
  output <- midway %>% 
    mutate_if(is.numeric, round, digits = 0) %>% 
    mutate(percent_change = if_else(percent_change == 0, "<1", as.character(percent_change)))
  
  return(output)
}

livestock_bullet_4 <- function(data){
  bullet <- glue("{data[1,1]} had the largest livestock output of the ITL2 regions within {region_name} in {working_year}. This was 
  {data[1,6]} of \u00A3{abs(data[1,4])} million ({data[1,5]}%) from {previous_year} to \u00A3{data[1,3]} million in {working_year}.")
  
  return(bullet)
}

livestock_bullet_5 <- function(data){
  bullet <- glue("{tail(data[1], 1)} had the smallest livestock output of the ITL2 regions within {region_name} in {working_year}. 
  This was {tail(data[6], 1)} of \u00A3{abs(tail(data[4], 1))} million ({tail(data[5], 1)}%) from {previous_year} to 
                 \u00A3{tail(data[3],1)} million in {working_year}.")
  
  return(bullet)
}

# figure 2.7 itl1 other outputs and subsidies ----------------------------------
other_outputs_etc_itl1 <- function(nuts1){
  midway <- nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code %in% c('15000', '17900', '99025')) %>% 
    group_by(final_account_item_code, year) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    pivot_wider(names_from = 'year', values_from = 'value') %>% 
    arrange(desc(.[[3]])) %>% 
    item_code_to_name() %>%
    clean_names(case = 'title', use_make_names = FALSE) %>%
    mutate(change = .[[3]] - .[[2]]) %>% 
    mutate(percent_change = 100*change/.[[2]])  %>%
    mutate(up_down = if_else(change > 0, "an increase", "a decrease"))
  
  output <- midway %>%
    mutate(percent_change = if_else(percent_change < 0.5, "<1", as.character(percent_change)))
  
  return(output)
}

other_outputs_etc_bullet_1 <- function(data){
  working_year_total <- sum(data[3])
  previous_year_total <- sum(data[2])
  diff <- round(abs(working_year_total - previous_year_total), digits = 0)
  diff_percent <- round(100 * diff/previous_year_total, digits = 0)
  diff_percent <- if_else(diff_percent == 0, "<1", as.character(diff_percent))
  up_down <- if_else(working_year_total - previous_year_total > 0, "an increase", "a decrease")
  
  bullet <- glue("Other outputs and subsidies in {region_name} totalled \u00A3{round(working_year_total, digits = 0)} million in 
                 {working_year}, {up_down} of \u00A3{diff} million ({diff_percent}%) from {previous_year}.")
  
  return(bullet)
}

other_outputs_etc_bullet_2 <- function(data){
  direct_payments <- data %>%
    filter(`Item Name` == "Direct payments")
  
  working_payments <- direct_payments[3]
  previous_payments <- direct_payments[2]
  change_payments <- abs(working_payments - previous_payments)
  change_percent <- round(100 * change_payments/previous_payments, digits = 0)
  change_percent <- if_else(change_percent == 0, "<1", as.character(change_percent))
  up_down <- if_else(working_payments - previous_payments > 0, "an increase", "a decrease")
  
  bullet <- glue("Direct payments in {region_name} were \u00A3{round(working_payments, digits = 0)} million in {working_year}, 
                 {up_down} of \u00A3{round(change_payments, digits = 0)} million ({change_percent}%) from {previous_year}.")
  
  return(bullet)
}

# figure 3.1 Inputs and costs split by percentage ------------------------------
inputs_and_costs_itl1_bullet_data <- function(nuts1){
  midway <- nuts1 %>% 
    filter(final_account_item_code %in% c('19000', '99016', '23000', '28000')) %>%
    filter(year >= working_year - 5) %>% 
    group_by(year, final_account_item_code) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    group_by(year) %>% 
    mutate(label = 100*value/sum(value)) %>%
    item_code_to_name() %>% 
    ungroup()
  
  biggest_contributor <- midway %>%
    group_by(year) %>% 
    mutate(max = max(value)) %>%
    ungroup() %>% 
    filter(max == value) %>% 
    group_by(item_name) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    arrange(desc(count))
  
  rank_year_biggest_contributor <- midway %>% 
    filter(item_name == biggest_contributor$item_name) %>% 
    mutate(rank = ordinal(rank(-value)))
  
  output <- midway %>% 
    filter(item_name %in% c(biggest_contributor$item_name, "Compensation of employees")) %>% 
    group_by(item_name) %>% 
    mutate(rank = ordinal(rank(-label))) %>%
    mutate(label = round(label, digits = 1)) %>% 
    ungroup()
  
  return(output)
}

total_costs_working_year <- region %>%
  filter(year == working_year, final_account_item_code %in% c('19000', '99016', '23000', '28000')) %>%
  summarise(total_output = sum(value)) %>% 
  mutate(total_output = round(total_output))

inputs_and_costs_itl1_bullet_1 <- function(data){
  biggest_item <- data %>% 
    filter(item_name != "Compensation of employees")
  
  working_year_percent <- data %>% 
    filter(year == working_year)%>%
    filter(item_name != "Compensation of employees") %>% 
    select(label, rank)
  
  if(working_year_percent$rank == "1st"){
    bullet <- glue("{biggest_item$item_name[1]} has contributed at least {min(biggest_item$label)}% of total costs 
                   in {region_name} in the last 6 years. In {working_year}, {biggest_item$item_name[1]} contributed 
                   {working_year_percent$label}% of the \u00A3{total_costs_working_year} million of 
                   inputs and costs, the highest proportion of the past 6 years.")
  }
  else if(working_year_percent$rank == "6th"){
    bullet <- glue("{biggest_item$item_name[1]} has contributed at least {min(biggest_item$label)}% of costs in 
                   {region_name} in the last 6 years. In {working_year} {biggest_item$item_name[1]} contributed 
                   {working_year_percent$label}% of the \u00A3{total_costs_working_year} million of 
                   inputs and costs, the lowest proportion of the past 6 years.")
  }
  else{
    bullet <- glue("{biggest_item$item_name[1]} has contributed at least {min(biggest_item$label)}% of costs in 
                   {region_name} in the last 6 years. In {working_year} {biggest_item$item_name[1]} contributed 
                   {working_year_percent$label}% of the \u00A3{total_costs_working_year} million of 
                   inputs and costs, the {working_year_percent$rank} highest proportion of the past 6 years.")
  }
  
  return(bullet)
}

inputs_and_costs_itl1_bullet_2 <- function(data){
  min_payment <- data %>% 
    filter(item_name == "Compensation of employees") %>% 
    select(label) %>% 
    filter(label == min(label))
  
  max_payment <- data %>% 
    filter(item_name == "Compensation of employees") %>% 
    select(label) %>% 
    filter(label == max(label))
  
  current_payment <- data %>% 
    filter(item_name == "Compensation of employees") %>%
    filter(year == working_year) %>% 
    select(label)
  
  bullet <- glue("Compensation of employees has made up between {min_payment[1,1]}% and {max_payment[1,1]}% of the
                 total inputs and costs in the past 6 years for {region_name}. In {working_year}, Compensation of 
                 employees made up {current_payment[1,1]}% of total inputs and costs.")
  
  return(bullet)
}

# figure 3.2 inputs and costs at itl2 ------------------------------------------
inputs_and_costs_itl2_bullet_data <- function(nuts1){
  midway <- nuts1 %>% 
    filter(final_account_item_code %in% c('19000', '99016', '23000', '28000')) %>%
    filter(year %in% c(working_year)) %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>% 
    group_by(nuts2_code, final_account_item_code) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    group_by(nuts2_code) %>% 
    mutate(label = percent(value/sum(value), accuracy = 0.1)) %>%
    item_code_to_name() %>%
    itl2_code_to_name() %>% 
    group_by(itl2_name) %>% 
    summarise(total = sum(value)) %>% 
    ungroup() %>% 
    arrange(total) %>% 
    mutate(increase = round(100*(total-lag(total))/lag(total), digits = 1)) %>% 
    mutate(total = round(total, digits = 0)) %>% 
    mutate(increase = if_else(increase == 0, "<1", as.character(increase)))
  
  return(midway)
}

inputs_and_costs_itl2_bullet_1 <- function(data){
  itl1_total <- data %>% 
    summarise(grand_total = sum(total))
  
  percentage_of_total <- round(100 * tail(data$total, 1) / itl1_total$grand_total, digits = 0)
  
  bullet <- glue("{tail(data$itl2_name, 1)} had the highest total inputs and costs of {region_name} in 
                 {working_year} with \u00A3{tail(data$total, 1)} million, {percentage_of_total}% of the total inputs
                 and costs in {region_name}.")
  
  return(bullet)
}

inputs_and_costs_itl2_bullet_2 <- function(data){
  itl1_total <- data %>% 
    summarise(grand_total = sum(total))
  
  percentage_of_total <- round(100 * head(data$total, 1) / itl1_total$grand_total, digits = 0)
  
  bullet <- glue("{head(data$itl2_name, 1)} had the lowest total inputs and costs of {region_name} in 
                 {working_year} with \u00A3{head(data$total, 1)} million, {percentage_of_total}% of the total inputs
                 and costs in {region_name}.")
  
  return(bullet)
}

# figure 3.3 intermediate consumption itl1 -------------------------------------
int_consumption_each_region <- function(nuts1){
  midway <- nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code %in% c(19010, 19020, 19030, 19040, 19050, 19060, 99007, 19090, 19900)) %>% 
    group_by(final_account_item_code, year) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    pivot_wider(names_from = 'year', values_from = 'value') %>% 
    arrange(desc(.[[3]])) %>% 
    item_code_to_name() %>%
    clean_names(case = 'title', use_make_names = FALSE) %>%
    mutate(change = .[[3]] - .[[2]]) %>% 
    mutate(percent_change = 100*change/.[[2]])  %>%
    mutate(up_down = if_else(change > 0, "an increase", "a decrease"))
  
  output <- midway %>% 
    mutate_if(is.numeric, round, digits = 0) %>% 
    mutate(percent_change = if_else(percent_change == 0, "<1", as.character(percent_change)))
  
  return(output)
}

int_consumption_bullet_1 <- function(data){
  bullet <- glue("The largest intermediate consumption in {region_name} in {working_year} was {head(data[1], 1)} 
                 with a value of \u00A3{abs(head(data[3], 1))} million, this was an {head(data$up_down, 1)} of 
                 \u00A3{abs(head(data$change, 1))} million ({head(data$percent_change, 1)}%) from {previous_year}.")
  
  return(bullet)
}

int_consumption_bullet_2 <- function(data){
  data_sorted <- data %>% 
    arrange(desc(change))
  if(head(data_sorted$up_down, 1) == "a decrease"){ # if all total intermediate consumption costs fell
    if(head(data_sorted[1], 1) == head(data[1], 1)){ # if the smallest fall was in the biggest cost
      bullet <- glue("The value of all intermediate consumption costs in {region_name} in {working_year} fell, the smallest
                     of these decreases was also in {data_sorted[1,1]}. The second smallest decrease was 
                     in {data_sorted[2,1]} which decreased by \u00A3{abs(data_sorted[2,4])} million 
                     ({data_sorted[2,5]}%) from {previous_year} to \u00A3{data_sorted[2,3]} million in 
                     {working_year}.")
    }
    else{ # if the smallest fall was not in the biggest cost
      bullet <- glue("The value of all intermediate consumption costs in {region_name} in {working_year} fell, the smallest
                     of these decreases was in {data_sorted[1,1]} which decreased by \u00A3{abs(data_sorted[1,4])} 
                     million ({data_sorted[1,5]}%) from {previous_year} to \u00A3{data_sorted[1,3]} million in 
                     {working_year}.")
    }
  } else{ # if not all of the total intermediate consumption costs fell
    if(head(data_sorted[1], 1) == head(data[1], 1)){ # if the biggest increase was also in the biggest cost
      bullet <- glue("The largest value increase in a cost in {region_name} in {working_year} was also in 
                     {data_sorted[1,1]}. The second largest increase was in {data_sorted[2,1]} which increased by 
                     \u00A3{data_sorted[2,4]} million ({data_sorted[2,5]}%) from {previous_year} to 
                     \u00A3{data_sorted[2,3]} million in {working_year}.")
    }
    else{ # if the biggest increase was not also in the biggest cost
      bullet <- glue("The largest value increase in a cost in {region_name} in {working_year} was in {data_sorted[1,1]} 
                     which increased by \u00A3{data_sorted[1,4]} million ({data_sorted[1,5]}%) from {previous_year} 
                     to \u00A3{data_sorted[1,3]} million in {working_year}.")
    }
  }
  
  return(bullet)
}

int_consumption_bullet_3 <- function(data){
  data_sorted <- data %>% 
    arrange(change)
  if(head(data_sorted$up_down, 1) == "an increase"){ # if all intermediate consumption costs rose
    if(head(data_sorted[1], 1) == head(data[1], 1)){ # if the biggest increase was also in the biggest cost
      bullet <- glue("The value of all costs in {region_name} in {working_year} increased, the smallest increase in a cost
                     was also in {data_sorted[1,1]}. The second largest increase was in {data_sorted[2,1]} 
                     which increased by \u00A3{data_sorted[2,4]} million ({data_sorted[2,5]}%) from {previous_year}
                     to \u00A3{data_sorted[2,3]} million in {working_year}.")
    }
    else{ # if the biggest increase was not in the biggest cost
      bullet <- glue("The value of all costs in {region_name} in {working_year} increased, the smallest increase in a cost was in 
                     {data_sorted[1,1]} which increased by \u00A3{data_sorted[1,4]} million ({data_sorted[1,5]}%) 
                     from {previous_year} to \u00A3{data_sorted[1,3]} million in {working_year}.")
    }
  } else{ # if not all the intermediate consumption costs rose
    if(head(data_sorted[1], 1) == head(data[1], 1)){ # if the biggest fall was also in the biggest cost
      bullet <- glue("The largest value decrease in a cost in {region_name} in {working_year} was also in 
                     {data_sorted[1,1]}. The second largest decrease was in {data_sorted[2,1]} decreasing by 
                     \u00A3{data_sorted[2,4]} million ({data_sorted[2,5]}%) from {previous_year} to 
                     \u00A3{data_sorted[2,3]} million in {working_year}.")
    }
    else{ # if the biggest fall was not in the biggest cost
      bullet <- glue("The largest value decrease in a cost in {region_name} in {working_year} was in {data_sorted[1,1]} 
                     which decreased by \u00A3{abs(data_sorted[1,4])} million ({data_sorted[1,5]}%) from 
                     {previous_year} to \u00A3{data_sorted[1,3]} million in {working_year}.")
    }
  }
  return(bullet)
}

# figure 3.4 intermediate consumption itl2 -------------------------------------
int_consumption_data_itl2 <- function(nuts1){
  midway <- nuts1 %>% filter(year %in% c(previous_year, working_year)) %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code == '19000') %>%
    mutate(nuts2_code = substr(nuts3_code, 1, nchar(nuts3_code)-1)) %>% 
    group_by(nuts2_code, year) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    pivot_wider(names_from = 'year', values_from = 'value') %>% 
    arrange(desc(.[[3]])) %>%
    itl2_code_to_name() %>% 
    clean_names(case = 'title', use_make_names = FALSE) %>%
    mutate(change = .[[3]] - .[[2]]) %>% 
    mutate(percent_change = 100*change/.[[2]])  %>%
    mutate(up_down = if_else(change > 0, "an increase", "a decrease"))
  
  output <- midway %>% 
    mutate_if(is.numeric, round, digits = 0) %>% 
    mutate(percent_change = if_else(percent_change == 0, "<1", as.character(percent_change)))
  
  return(output)
}

int_consumption_bullet_4 <- function(data){
  bullet <- glue("{data[1,1]} had the largest intermediate consumption of the ITL2 regions within 
                 {region_name} in {working_year}. This was {data[1,6]} of \u00A3{abs(data[1,4])} million 
                 ({data[1,5]}%) from {previous_year} to \u00A3{data[1,3]} million in {working_year}.")
  
  return(bullet)
}

int_consumption_bullet_5 <- function(data){
  bullet <- glue("{tail(data[1], 1)} had the smallest intermediate consumption of the ITL2 regions within 
                 {region_name} in {working_year}. This was {tail(data[6], 1)} of \u00A3{abs(tail(data[4], 1))} 
                 million ({tail(data[5], 1)}%) from {previous_year} to \u00A3{tail(data[3],1)} million in {working_year}.")
  
  return(bullet)
}

# figure 3.5 itl1 other inputs and costs ---------------------------------------
other_intputs_etc_itl1 <- function(nuts1){
  midway <- nuts1 %>% 
    filter(year %in% c(previous_year, working_year)) %>% 
    filter(final_account_item_code %in% c('99016', '23000', '28000')) %>% 
    group_by(final_account_item_code, year) %>% 
    summarise(value = sum(value)) %>%
    ungroup() %>% 
    pivot_wider(names_from = 'year', values_from = 'value') %>% 
    arrange(desc(.[[3]])) %>% 
    item_code_to_name() %>%
    clean_names(case = 'title', use_make_names = FALSE) %>%
    mutate(change = .[[3]] - .[[2]]) %>% 
    mutate(percent_change = 100*change/.[[2]])  %>%
    mutate(up_down = if_else(change > 0, "an increase", "a decrease"))
  
  output <- midway %>%
    mutate(percent_change = round(percent_change, digits = 0)) %>% 
    mutate(percent_change = if_else(percent_change == 0, "<1", as.character(percent_change)))
  
  return(output)
}

other_inputs_etc_bullet_1 <- function(data){
  working_year_total <- sum(data[3])
  previous_year_total <- sum(data[2])
  diff <- round(abs(working_year_total - previous_year_total), 0)
  diff_percent <- round((diff/previous_year_total)*100, digits = 0)
  up_down <- if_else(working_year_total - previous_year_total > 0, "an increase", "a decrease")
  
  bullet <- glue("Other inputs and costs in {region_name} totalled \u00A3{round(working_year_total, digits = 0)} million in 
                 {working_year}, {up_down} of \u00A3{diff} million ({diff_percent}%) from {previous_year}.")
  
  return(bullet)
}

other_inputs_etc_bullet_2 <- function(data){
  direct_payments <- data %>%
    mutate_if(is.numeric, round, digits = 0) %>% 
    filter(`Item Name` == "Compensation of employees")
  
  working_payments <- direct_payments[3]
  previous_payments <- direct_payments[2]
  change_payments <- abs(direct_payments[4])
  change_percent <- direct_payments[5]
  up_down <- direct_payments[6]
  
  bullet <- glue("Compensation of employees in {region_name} was \u00A3{working_payments} million in {working_year}, 
                 {up_down} of \u00A3{change_payments} million ({change_percent}%) from {previous_year}.")
  
  return(bullet)
}