#---------------------------------------------------------------------
# Basic cleaning "yktr"
# Maj Beldring Henningsen, majbh@sund.ku.dk
# "yktr" , @years vs @KGMAELK (milk) , @CELLETAL (scc)

#--------------------------------------------------------------------
# Packages and settings
library(tidyverse)
library(reshape2)
library(data.table)
library(plotly)
library(GGally)
library(ggExtra)
library(ggalluvial)
Sys.setlocale("LC_ALL","English") # data formatting

#---------------------------------------------------------------------
# years (2010-2020) vs @scc and @milk: loading and cleaning raw data "yktr"

yktr        <- fread("M:/data/yktr.csv")
yktr_year <- subset(yktr, select = c(KONTROLDATO, KGMAELK, CELLETAL)) # Keep selected coloumns
yktr_year$KONTROLDATO = format(as.Date(yktr_year$KONTROLDATO, format='%Y-%m-%d'),'%Y') # keep years

# split data into "year vs scc" and "year vs milk"
year_scc <- subset(yktr_year, select = c(KONTROLDATO, CELLETAL))
year_milk <- subset(yktr_year, select = c(KONTROLDATO, KGMAELK))
#milk:
year_milk <- year_milk[!is.na(year_milk$KGMAELK), ]
range(year_milk$KGMAELK) # 0.0 99.5
#scc:
year_scc <- year_scc[!is.na(year_scc$CELLETAL), ] #remove NA from CELLETAL, overwriting yktr
range(year_scc$CELLETAL)
#year_scc <- subset(year_scc, CELLETAL >= 0 ) # removing NEG celletal, overwriting yktr

# calculate mean per year in both data set
y_scc <- aggregate(year_scc[, 2], list(year_scc$KONTROLDATO), mean)
y_milk <- aggregate(year_milk[, 2], list(year_milk$KONTROLDATO), mean)

# renaming columns with dplyr
y_scc <- y_scc %>%
  rename(
    year = Group.1,
    scc = x
  )
y_milk <- y_milk %>%
  rename(
    year = Group.1,
    milk = x
  )

# saving cleaned date
write.csv(y_scc,"C:\\Users\\majhe\\OneDrive - Københavns Universitet\\BDA_02936\\Project\\project_years\\year_scc.csv", row.names = FALSE)
write.csv(y_milk,"C:\\Users\\majhe\\OneDrive - Københavns Universitet\\BDA_02936\\Project\\project_years\\year_milk.csv", row.names = FALSE)

#--------------------------------------------------------------------
# All animals from yktr per year

#  per year 2010-2019 from "yktr" if needed for incidents per producing cow per year
yktr_animals <- subset(yktr, select = c(KONTROLDATO, DYR_ID)) # Keep selected coloumns

####
# Every dyr_ID should only appear once per year....
# use unique for this..

yktr_animals$KONTROLDATO = format(as.Date(yktr_animals$KONTROLDATO, format='%Y-%m-%d'),'%Y') # keep years
years_yktr <- subset(yktr_animals, select = c(KONTROLDATO))
animals_year_yktr <- stack(table(years_yktr))[2:1]
# renaming columns
animals_year_yktr <- animals_year_yktr %>%
  rename(
    year = ind,
    animals = values
  )
