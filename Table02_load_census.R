### Libraries ----
library(tidyverse)
library(readxl)
#### Table 02 ----



# File path
file_name <- "data/Unpaid work and care data summary - first and second release.xlsx"

# Name and data from rows: table 03 = relationship
age_brackets <- read_excel(path = file_name,
                           range = "Table 2!A12:A68",
                           col_names = "age_brackets") %>%
  drop_na()

#unpaid_hours_cleaned <-unpaid_hours %>% 
#  filter(!unpaid_hours %in% "Number of hours of unpaid domestic work:")

# Name and data from other columns: table 03 = 
unpaid_hours <- read_excel(path = file_name,
                           range = "Table 2!B10:G68",
                           col_names = c("nil_hours",
                                         "Less than 5 hours"	,
                                         "5 to 14 hours",
                                         "15 to 29 hours",	
                                         "30 hours or more",
                                         "Total")) %>%
  
  filter(!is.na(`nil_hours`)) %>%  # Remove rows where "None(b)" is NA
  mutate(sex = ifelse(nil_hours %in% c('MALES', 'FEMALES', 'PERSONS'), nil_hours, NA)) %>%
  fill(sex, .direction = c("down")) %>%
  filter(!nil_hours %in% c('MALES', 'FEMALES', 'PERSONS'))


table_02 <- cbind(age_brackets, unpaid_hours) %>%
  relocate(sex, .before = age_brackets)


# Remove rows with "Total males", "Total females", and "Total persons" in 'unpaid_hours' column
table_02_cleaned <- table_02 %>%
  filter(!age_brackets %in% c("Total males", "Total females", "Total persons"))

table_02_cleaned <- table_02_cleaned %>%
  mutate(across(c("nil_hours", "Less than 5 hours", "5 to 14 hours", 
                  "15 to 29 hours", "30 hours or more", "Total"), 
                as.numeric))

# Step 2: Pivot longer to transform unpaid hours categories into a single column
table_02_long <- table_02_cleaned %>%
  pivot_longer(cols = c("nil_hours", "Less than 5 hours", "5 to 14 hours", 
                        "15 to 29 hours", "30 hours or more", "Total"),
               names_to = "unpaid_hours_category",  # Column to store the unpaid hours categories
               values_to = "unpaid_hours_value")    # Column to store the values of unpaid hours

# Pivot wider to transform 'age_brackets' into columns
table_02_wide <- table_02_long %>%
  pivot_wider(names_from = age_brackets,       # Convert 'age_brackets' to columns
              values_from = unpaid_hours_value) # Fill the new columns with 'unpaid_hours_value'

table_02_wide_cleaned <- table_02_wide %>%
  filter(sex != "PERSONS")

# View the cleaned table
head(table_02_wide_cleaned)

write_csv(table_02_wide_cleaned, "table_02_cleaned.csv")
