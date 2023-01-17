#----------------------------------------------------------------
# Maj Beldring Henningsen, majbh@sund.ku.dk
# location;
# "vetresdyr", "beskart"

# Packages and settings
library(tidyverse) # includes dplyr and ggplot2
library(reshape2) # for melt commands
library(data.table) # for adding a row for occurences of each dyr_IDAN
library(readxl)
setwd('C:/Users/majhe/Google Drive/PhD/R_data') # set location
Sys.setlocale("LC_ALL","English") # change locale to English, for data formatting

#------------------------------------------------------------------
# import data: "ptbprv", from data1, kv?gdatabasen
ptbprv <- read_excel("data1/ptbprv.xlsx")
beskart <- read.table("data1/beskart.csv",header=TRUE,sep=",")

# Cleaning data - ptbprv
head(ptbprv) #Look at header; noting
str(ptbprv) #Look at data structure
ptbprv = subset(ptbprv, select = c(BES_ID, DYR_ID, INFEKTIONSGRUPPE, KONTROLDATO)) # keep useful columns

range(ptbprv$INFEKTIONSGRUPPE) # Check for NAs
table(is.na(ptbprv$INFEKTIONSGRUPPE)) # how many of total values are NA
ptbprv <- ptbprv[!is.na(ptbprv$INFEKTIONSGRUPPE), ] #remove NA, overwriting ptbprv
range(ptbprv$INFEKTIONSGRUPPE) # from 0 to 9: correct
ptbprv <- subset(ptbprv, KONTROLDATO >= as.Date("2013-01-01"))
range(ptbprv$KONTROLDATO) # only 2013 dates

# Cleaning data - beskart
head(beskart) #Look at header; noting
str(beskart) #Look at data structure
beskart$DATO_OPHOER = as.Date(beskart$DATO_OPHOER, format='%d-%m-%Y')
beskart$DATO_OPHOER[is.na(beskart$DATO_OPHOER)] <- as.Date('2020-01-01') # replace NA's with 2020-01-01
beskart <- subset(beskart, DATO_OPHOER >= as.Date("2013-12-12")) # keep BES active after 2013
beskart <- subset(beskart, select = c(BES_ID, Xkoor, Ykoor)) # keep useful columns

table(is.na(beskart)) # how many of total values are NA
beskart <- beskart %>% drop_na() # drop rows with NA's

#---------------------------------------------------------------------
# merge ptbprv and beskart
location <- merge(ptbprv, beskart, by="BES_ID", sort=TRUE)

# GIS coordinates
# We want our longitude coordinate, this is the X coordinate
# we want to split in East and west
# we want to split at 11 degree EAST, thats appr 620133

# 1.rename infektionsgroup into 1 or 0

# 2.drop Y coordinates
# 3.Split location into east X<620133 and west X>620133
# 4.rename infektion to inf_eat and inf_west
# 5.merge
# 6.Piechart percentage paratb group 2 and 9 ind 2013
