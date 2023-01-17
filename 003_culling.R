### Loading, cleaning and describing culling data 'slagtedata'
## Danish Cattle Database (DCD) 2010-2020
## Maj Beldring, 2022


## Load Packages and settings:
library(tidyverse) 
library(data.table)
Sys.setlocale("LC_ALL","English") # locale change for date formatting

## load cullings
cullings <- read_csv("M:/data/slagtedata.csv") 
glimpse(cullings)

## Cullings per year:
cullings$DATO = format(as.Date(cullings$DATO, format='%Y-%m-%d'),'%Y')

cullings_year <- subset(cullings, select = c(DATO))
cullings_year <- stack(table(cullings_year))[2:1]

## renaming columns
cullings_year <- cullings_year %>%
  rename(
    year = ind,
    calving = values
  )
