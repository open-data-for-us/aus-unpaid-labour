# --------------------------------------- #
# Create modular functions to load census #
# --------------------------------------- #

### Libraries ----
library(tidyverse)
library(readxl)


### Table 03 ----
file_name <- "data/Unpaid work and care data summary - first and second release.xlsx"
table_census <- "Table 3"
range_census <- "A9:G44"
name_column <- 'relationship'

census_table_03 <- function(file_name, table_census, range_census, name_column){
  data <- read_excel(path = file_name,
                             range = paste(table_census, range_census, sep = "!")) %>%
    drop_na() %>%
    mutate(sex = ifelse(str_detect(`...1`, 'males'), 'Males', NA)) %>%
    mutate(sex = ifelse(str_detect(`...1`, 'females'), 'Females', sex)) %>%
    mutate(sex = ifelse(str_detect(`...1`, 'persons'), 'Persons', sex)) %>%
    fill(sex, .direction = c('up')) %>%
    filter(!(str_detect(`...1`, 'Total'))) 
  
  colnames(data)[1] <- name_column
  return(data)
}

# try to run the function
table_03 <- census_table_03(file_name, table_census, range_census, name_column)
# trial <- census_table_03(file_name, table_census="Table 3", range_census="A9:G44", name_column = 'relationship')

# table_03_transpose <- table_03 %>%
#   mutate(`Nil hours` = as.numeric(`Nil hours`)) %>%
#   pivot_wider(names_from = relationship,
#               values_from = c(sex,`Nil hours`:Total))

table_03_transpose <- t(table_03) %>%
  

### Table 07 ----
file_name <- "data/Unpaid work and care data summary - first and second release.xlsx"
table_census <- "Table 7"
range_census <- "A11:K46"
name_column <- 'unpaid_hours'

census_table_07 <- function(file_name, table_census, range_census, name_column){
  data <- read_excel(path = file_name,
                     range = paste(table_census, range_census, sep = "!")) %>%
    drop_na() %>%
    mutate(sex = ifelse(str_detect(`...1`, 'males'), 'Males', NA)) %>%
    mutate(sex = ifelse(str_detect(`...1`, 'females'), 'Females', sex)) %>%
    mutate(sex = ifelse(str_detect(`...1`, 'persons'), 'Persons', sex)) %>%
    fill(sex, .direction = c('up')) %>%
    filter(!(str_detect(`...1`, 'Total'))) 
  
  colnames(data)[1] <- name_column
  return(data)
}

# try to run the function
table_07 <- census_table_07(file_name, table_census, range_census, name_column)
# trial <- census_table_03(file_name, table_census="Table 3", range_census="A9:G44", name_column = 'relationship')


### Table 08 ----
file_name <- "data/Unpaid work and care data summary - first and second release.xlsx"
table_census <- "Table 8"
range_census <- "A11:K34"
name_column <- 'unpaid_assistance'

census_table_08 <- function(file_name, table_census, range_census, name_column){
  data <- read_excel(path = file_name,
                     range = paste(table_census, range_census, sep = "!")) %>%
    drop_na() %>%
    mutate(sex = ifelse(str_detect(`...1`, 'males'), 'Males', NA)) %>%
    mutate(sex = ifelse(str_detect(`...1`, 'females'), 'Females', sex)) %>%
    mutate(sex = ifelse(str_detect(`...1`, 'persons'), 'Persons', sex)) %>%
    fill(sex, .direction = c('up')) %>%
    filter(!(str_detect(`...1`, 'Total'))) 
  
  colnames(data)[1] <- name_column
  return(data)
}

# try to run the function
table_08 <- census_table_08(file_name, table_census, range_census, name_column)
# trial <- census_table_03(file_name, table_census="Table 3", range_census="A9:G44


### All tables ----
tables_3_7 <- full_join(table_03, table_07)

tables_7_8 <- full_join(table_07, table_08)

