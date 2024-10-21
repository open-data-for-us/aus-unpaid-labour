# ----------------------------------- #
# Get the tables from the census data #
# ----------------------------------- #

### Libraries ----
library(tidyverse)
library(readxl)


### Census data ----

#### Table 03 ----
## Tables split by sex

## columns: 
# Nil hours	
# Less than 5 hours	
# 5 to 14 hours	
# 15 to 29 hours	
# 30 hours or more	
# Total

## rows: 
# Husband, Wife or Partner
# Lone parent
# Dependent student
# Non-dependent child
# Other related individual
# Non-family member
# Non-classifiable
# Total

# File path
file_name <- "data/Unpaid work and care data summary - first and second release.xlsx"


# Name and data from rows: table 03 = relationship
unpaid_hours <- read_excel(path = file_name,
                     range = "Table 7!A10:A46",
                     col_names = "unpaid_hours") %>%
  drop_na()

unpaid_hours_cleaned <-unpaid_hours %>% 
  filter(!unpaid_hours %in% "Number of hours of unpaid domestic work:")

# Name and data from other columns: table 03 = 
number_of_hours <- read_excel(path = file_name,
                        range = "Table 7!B10:K46",
  col_names = c("None(b)",
                "1-15 hours",
                "16 - 24 hours",
                "25-34 hours",
                "35-39 hours",
                "40 hours",
                "41-48 hours",
                "49 hours and over",
                "Not stated",
                "total"))  %>%
 
  filter(!is.na(`None(b)`)) %>%  # Remove rows where "None(b)" is NA
  mutate(sex = ifelse(`None(b)` %in% c('MALES', 'FEMALES', 'PERSONS'), `None(b)`, NA)) %>% 
  fill(sex, .direction = "down") %>%  # Fill down gender/sex information
  filter(!`None(b)` %in% c('MALES', 'FEMALES', 'PERSONS'))  # Remove rows with gender labels
  
number_of_hours_cleaned<- number_of_hours[-1,]

# Column bind the two subtables into main table 03
table_07 <- cbind(unpaid_hours_cleaned, number_of_hours_cleaned) %>%
  relocate(sex, .before = unpaid_hours)

## Next steps: 
# 1) Remove the total rows? the total column? 
# 2) Table 03 and 07 are similar, i.e. they both report on the number of unpaid work,
#    however, table 03 uses this data in columns, while table 07 uses it as rows; so 
#    so perhaps we should transpose and have the table 03 transposed?

# Remove rows with "Total males", "Total females", and "Total persons" in 'unpaid_hours' column
table_07_cleaned <- table_07 %>%
  filter(!unpaid_hours %in% c("Total males", "Total females", "Total persons"))

# View cleaned table
head(table_07_cleaned)


