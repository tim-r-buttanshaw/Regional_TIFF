###
# Project - tiff in the regions of England
# Title - Regions publishing functions
# Author - Tim Buttanshaw
# Description - Make functions for mass updating of tables etc. for publication 
# Date - 30/03/2022
###

# lookup functions -------------------------------------------------------------
itl1_lookup_names <- tibble(itl1_code = c("ukc","ukd", "uke", "ukf", "ukg", "ukh", "ukij", "ukk"), 
                            itl1_name = c('North East', 'North West','Yorkshire and The Humber',
                                          'East Midlands', 'West Midlands', 'East of England',
                                          'London and the South East', 'South West'))

itl2_lookup_names <- read_csv(here("TIFF - individual regions 2021", "data", "ITL2 codes from ONS website.csv")) %>% 
  select(-ObjectId) %>%
  clean_names %>% 
  mutate(nuts221cd = to_snake_case(str_replace_all(itl221cd, "TL", "UK")))

# replace ITL1 codes with their names
itl1_code_to_name <- function(x){
  i = 0
  while (i < nrow(x)){ 
    # assign the index of the nth code (in the original table) to the list of codes in the lookup table
    a <- match(x$nuts1_code[i+1], itl1_lookup_names$itl1_code)
    
    # replace the nth code (in the original tale) with it's corresponding name
    x$nuts1_code[i+1] = itl1_lookup_names$itl1_name[a]
    
    # go down 1 row in the original table
    i = i + 1
  }
  x <- x %>% 
    rename(itl1_name = nuts1_code)
  
  return(x)
}

# replace ITL2 codes with their names
itl2_code_to_name <- function(x){
  i = 0
  while (i < nrow(x)){ 
    a <- match(x$nuts2_code[i+1], itl2_lookup_names$nuts221cd)
    x$nuts2_code[i+1] = itl2_lookup_names$itl221nm[a]
    i = i + 1
  }
  x <- x %>% 
    rename(itl2_name = nuts2_code)
  
  return(x)
}

# replace final_account_item_codes with their names
item_code_to_name <- function(x){
  i = 0
  while (i < nrow(x)){ 
    a <- match(x$final_account_item_code[i+1], lookup_items$final_account_item_code)
    x$final_account_item_code[i+1] = lookup_items$item_name[a]
    i = i + 1
  }
  x <- x %>% 
    rename(item_name = final_account_item_code)
  
  return(x)
}

# generic lookup
general_lookup <- function(input_table, lookup_table, input_column, lookup_column, replacement_colum_name, output_column){
  i = 1
  while(i < nrow(input_table) + 1){
    a <- match(input_table$input_column[i], look_up_table$lookup_column)
    input_table$input_column[i] = look_table$replacement_column[a]
    i = i + 1
  }
  output <- input_table %>% 
    rename(output_column = input_column)
}

# main regions graphs ----------------------------------------------------------
# find the top 3 contributing itl1 regions in the working year
top3_finder <- function(item_code){
  top3 <- everything_cleaned %>% 
    filter(final_account_item_code == item_code) %>% # filter for the desired item
    filter(year == working_year) %>% # filter for the working year (set in the parameters)
    group_by(nuts1_code) %>% # group by itl1 region so the right values are summed
    summarise(total = sum(value)) %>% # sum the values to give itl1 totals
    arrange(desc(total)) %>% # sort by highest to lowest value
    head(3) # select the top 3
  
  top3_itl1_across <- everything_cleaned %>% 
    filter(final_account_item_code == item_code) %>% # filter for the desired item
    filter(nuts1_code %in% top3$nuts1_code) %>% # filter for the 3 regions which were top producers in the working year
    filter(year > working_year - 6) %>% # filter for only the past 6 years 
    group_by(nuts1_code, year) %>% # group by itl1 region and year so the correct values are summed 
    summarise(total = sum(value)) %>% # sum the item total for each of the past 6 years for each of the top 3 itl1 regions
    itl1_code_to_name() %>% # convert the itl1 codes to the region names
    pivot_wider(names_from = itl1_name, values_from = total) %>% # pivot the table wider so we can make this an interactive chart
    arrange(desc(year)) %>% #sort by year from smallest to largest
    rename(Year = year) %>% # rename year as Year
    mutate_if(is.numeric, label_number(accuracy = 1, big.mark = ",")) # format the values to have a comma and to be rounded to 0dp
  
  return(top3_itl1_across)
  
}

past_2_years <- function(item_code){
  output <- everything_cleaned %>%
    filter(final_account_item_code == item_code) %>% # filter for item
    filter(year >= previous_year) %>% # filter for the past 2 years
    group_by(nuts1_code, year) %>% # group by the itl1 code and year to sum the right groups of values
    summarise(total = sum(value)) %>% # suum the items from each year and itl1 region
    pivot_wider(names_from = year, values_from = total) %>% # pivot the table wider so we can make this an interactive chart
    arrange(desc(.[3])) %>% # sort by value from smallest to largest in the current year
    itl1_code_to_name() %>% # change the itl1 codes from their code to their name
    mutate_if(is.numeric, label_number(accuracy = 1, big.mark = ",")) # add the comma to numbers over 1,000
  
  return(output)
}

# other functions --------------------------------------------------------------
# pivot a data frame to years across the top and 
pivot_arrange_and_round <- function(x){
  y <- x %>%
    pivot_wider(names_from = 'year', values_from = 'value') %>% # pivot data frame so years are across the top
    arrange(desc(.[[3]])) %>% # arrange the items from largest value to smallest in the working year
    # mutate(value = if_else(value < 1, round(value, digits = 1), round(value, digits = 0)))
    # mutate(value = if_else(min(value) < 1, round(value, digits = 1), round(value, digits = 0)))
    mutate_if(is.numeric, label_number(accuracy = 1)) # round all the values to 0dp
  
  return(y)
}

# a set of functions which are performed on each data set with itl1 data in it to prepare it for publication
itl1_finisher <- function(x){
  y <- x %>% 
    group_by(final_account_item_code, year) %>% # use group by so we can sum everything together
    summarise(value = sum(value)) %>% # sum all the items with the same itl1 region code and year
    pivot_arrange_and_round() %>% # use this function (see above)
    item_code_to_name() %>% # replace all the item codes with their name
    clean_names(case = 'title', use_make_names = FALSE) %>% # make the column names title ready
    kable() # turn the tibble into a kable for md format
  
  return(y)
}

itl2_finisher <- function(x){
  y <- x %>% 
    group_by(nuts2_code, year) %>% # use group by so we can sum everything together
    summarise(value = sum(value)) %>% # sum all the items with the same itl2 region code and year
    pivot_arrange_and_round() %>% # use this function (see above)
    itl2_code_to_name() %>% # replace all the itl2 codes with their name
    clean_names(case = 'title', use_make_names = FALSE) %>% # make the column names title ready
    kable() # turn the tibble into a kable for md format
  
  return(y)
}

# kable formatting -------------------------------------------------------------
format_cells <- function(df, rows ,cols, value = c("italics", "bold", "strikethrough")){
  
  # select the correct markup
  # one * for italics, two ** for bold
  map <- setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[value]  
  
  for (r in rows){
    for(c in cols){
      
      # Make sure values are not factors
      df[[c]] <- as.character( df[[c]])
      
      # Update formatting
      df[r, c] <- paste0(markup, df[r, c], markup)
    }
  }
  
  return(df)
}

# tiff_years_accross <- everything_cleaned %>% 
#   filter(final_account_item_code == 99021) %>% 
#   group_by(nuts1_code, year) %>% 
#   summarise(tiff = sum(value)) %>%
#   ungroup() %>% 
#   pivot_wider(names_from = nuts1_code, values_from = tiff) %>% 
#   mutate_if(is.numeric, ~.x - lag(.x))
# 
# fbs_barplot(data = tiff_years_accross, aes(x = year, y = tiff, fill = nuts1_code))