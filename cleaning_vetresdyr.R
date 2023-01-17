#--------------------------------------------------------------------
# Maj Beldring Henningsen, majbh@sund.ku.dk

# Paratb ; vetprtype_id=7 POS if: analysedata>=0.3

#-------------------------------------------------------------------
# Packages and settings
library(tidyverse) # includes dplyr and ggplot2
library(reshape2) # for melt commands
library(data.table) # for adding a row for occurences of each dyr_IDAN
Sys.setlocale("LC_ALL","English") # change locale to English, for data formatting

#---------------------------------------------------------------------
# import data: "vetresdyr"
vetresdyr <- read.csv2("vetresdyr.csv",header=TRUE,sep=",")

# Cleaning "vetresdyr" for paratb cases:
glimpse(vetresdyr)
paratb <- subset(vetresdyr, VETPRTYPE_ID == 7 ) #selecting paratb cases
# choosing POS tests for paratb
paratb_POS <- subset(paratb, ANALYSEDATA >= 0.3 ) #selecting paratb cases
paratb_POS <- subset(paratb_POS, select = c(DYR_ID, KONTROLDATO)) # keep only paratb cases and date
paratb_POS$KONTROLDATO = format(as.Date(paratb_POS$KONTROLDATO, format='%Y-%m-%d'),'%Y') # keep years
paratb_POS <- subset(paratb_POS, KONTROLDATO < 2020 ) #range to 2019
paratb_POS <- subset(paratb_POS, KONTROLDATO >= 2010 ) #range from 2010
paratb_POS <- subset(paratb_POS, select = c(KONTROLDATO))
paratb_POS <- stack(table(paratb_POS))[2:1] #calculating cases per year (times each years appear)
# renaming columns
paratb_POS <- paratb_POS %>%
  rename(
    year = ind,
    POS = values
  )

# dataframe with paratb incidents per year per cow year 2010-2019 (animals from yktr)
# animals <- c(727671,742897,742528,737013,734603,713808,733604,709572,719306,742897)
# year_paratb_cow <- transform(prtb_year, prtb_cow = prtb / animals)

# save cleaned data
write.csv(prtb_year,"C:\\Users\\majhe\\OneDrive - Københavns Universitet\\data\\year_paratb.csv", row.names = FALSE)

#-----------------------------------------------------------------------


