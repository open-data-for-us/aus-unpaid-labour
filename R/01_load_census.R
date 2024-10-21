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
relationship <- read_excel(path = file_name,
                     range = "Table 3!A9:A44",
                     col_names = "relationship") %>%
  drop_na()


# Name and data from other columns: table 03 = 
unpaid_hours <- read_excel(path = file_name,
                        range = "Table 3!B10:G44",
  col_names = c("nil_hours",
                "less_5_hours",
                "5_to_14_hours",
                "15_to_29_hours",
                "more_30_hours",
                "total")) %>%
  filter(!is.na(nil_hours)) %>% # to keep the gender information
  mutate(sex = ifelse(nil_hours %in% c('MALES', 'FEMALES', 'PERSONS'), nil_hours, NA)) %>%
  fill(sex, .direction = c("down")) %>%
  filter(!nil_hours %in% c('MALES', 'FEMALES', 'PERSONS'))
  

# Column bind the two subtables into main table 03
table_03 <- cbind(census, unpaid_hours) %>%
  relocate(sex, .before = relationship)

## Next steps: 
# 1) Remove the total rows? the total column? 
# 2) Table 03 and 07 are similar, i.e. they both report on the number of unpaid work,
#    however, table 03 uses this data in columns, while table 07 uses it as rows; so 
#    so perhaps we should transpose and have the table 03 transposed?




