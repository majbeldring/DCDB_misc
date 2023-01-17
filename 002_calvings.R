### Loading, cleaning and describing Calving data 'kaelvninger'
## Danish Cattle Database (DCD) 2010-2020
## Maj Beldring, 2022


## Load Packages and settings:
library(tidyverse) 
#library(data.table)
Sys.setlocale("LC_ALL","English") # locale change for date formatting


## load calvings
calvings <- read_csv("M:/data/kaelvninger.csv") 


## change date format to only years
calvings$KAELVEDATO = format(as.Date(calvings$KAELVEDATO, format='%Y-%m-%d'),'%Y')

## calvings per year:
calvings_year <- subset(calvings, select = c(KAELVEDATO))
calvings_year <- stack(table(calvings_year))[2:1]

##  renaming columns
calvings_year <- calvings_year %>%
  rename(
    year = ind,
    calving = values
  )

