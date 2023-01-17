#---------------------------------------------------------------------
# Maj Beldring Henningsen, majbh@sund.ku.dk

# for: BDA prediction curves fitted with stan -> "YearsPrediction.R"

# "yktr" : @years vs @KGMAELK (milk) , @CELLETAL (scc), @mastitis, 
# "vetresdyr" : @paratb

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

yktr <- read.table("yktr.csv",header=TRUE,sep=",")
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

y_scc <- y_scc %>%
  mutate_at(vars(scc), list(~ round(., 0)))
y_milk <- y_milk %>%
  mutate_at(vars(milk), list(~ round(., 0)))

# saving cleaned date
write.csv(y_scc,"C:\\Users\\majhe\\OneDrive - Københavns Universitet\\BDA_02936\\Project\\project_years\\year_scc.csv", row.names = FALSE)
write.csv(y_milk,"C:\\Users\\majhe\\OneDrive - Københavns Universitet\\BDA_02936\\Project\\project_years\\year_milk.csv", row.names = FALSE)

#--------------------------------------------------------------------
# years (2010-2019) vs @paraTB
# loading and cleaning raw data "vetresdyr"

#  per year 2010-2019 from "yktr" if needed for incidents per producing cow per year
yktr_animals <- subset(yktr, select = c(KONTROLDATO, DYR_ID)) # Keep selected coloumns
yktr_animals$KONTROLDATO = format(as.Date(yktr_animals$KONTROLDATO, format='%Y-%m-%d'),'%Y') # keep years
years_yktr <- subset(yktr_animals, select = c(KONTROLDATO))
animals_year_yktr <- stack(table(years_yktr))[2:1]
# renaming columns
animals_year_yktr <- animals_year_yktr %>%
  rename(
    year = ind,
    animals = values
  )

#---------------------------------------------------------------------
# import data: "vetresdyr"
vetresdyr <- read.csv2("C:/Users/majhe/OneDrive - Københavns Universitet/data/data_Maj/vetresdyr.csv",header=TRUE,sep=",")

# Cleaning "vetresdyr"
glimpse(vetresdyr)
paratb <- subset(vetresdyr, VETPRTYPE_ID == 7 ) #selecting paratb cases
paratb <- subset(paratb, ANALYSEDATA >= 0.3 ) #selecting paratb cases
paratb <- subset(paratb, select = c(DYR_ID, KONTROLDATO)) # keep only paratb cases and date
paratb$KONTROLDATO = format(as.Date(paratb$KONTROLDATO, format='%Y-%m-%d'),'%Y') # keep years
paratb <- subset(paratb, KONTROLDATO < 2020 ) #range to 2019
paratb <- subset(paratb, KONTROLDATO >= 2010 ) #range from 2010
years_prtb <- subset(paratb, select = c(KONTROLDATO))
prtb_year <- stack(table(years_prtb))[2:1] #calculating cases per year (times each years appear)

# renaming columns
prtb_year <- prtb_year %>%
  rename(
    year = ind,
    paratb = values
  )

# dataframe with paratb incidents per year per cow year 2010-2019 (animals from yktr)
# year_paratb_cow <- transform(prtb_year, prtb_cow = prtb / animals)

# save cleaned data
write.csv(prtb_year,"C:\\Users\\majhe\\OneDrive - Københavns Universitet\\BDA_02936\\Project\\project_years\\year_paratb.csv", row.names = FALSE)
