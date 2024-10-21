# --------------------------------------- #
# Create modular functions to load census #
# --------------------------------------- #

### Libraries ----
library(tidyverse)
library(readxl)
library(janitor)


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


### Join tables ----

## Transpose table 3 to join with table 7
table_03_t <- as.data.frame(t(table_03)) %>%
  row_to_names(row_number = 1) 

table_03_male <-  table_03 %>% filter(sex %in% c('Males'))
table_03_male <- as.data.frame(t(table_03_male)) %>%
  row_to_names(row_number = 1) %>%
  filter(!`Husband, Wife or Partner` %in% c('Males')) %>%
  mutate(sex = as.character("Males")) %>%
  mutate(unpaid_hours = rownames(.))

table_03_female <-  table_03 %>% filter(sex %in% c('Females'))
table_03_female <- as.data.frame(t(table_03_female)) %>%
  row_to_names(row_number = 1) %>%
  filter(!`Husband, Wife or Partner` %in% c('Females')) %>%
  mutate(sex = as.character("Females")) %>%
  mutate(unpaid_hours = rownames(.))

rownames(table_03_male) <- NULL
rownames(table_03_female) <- NULL

table_03_final <- rbind(table_03_female, table_03_male) %>%
  relocate(unpaid_hours, sex, .before = `Husband, Wife or Partner`)

table_07_final <- table_07 %>% filter(!sex %in% c("Persons"))

table_3_7 <- full_join(table_03_final, table_07_final)

write.csv(table_3_7, file = "website/output/tables_03_07.csv")




