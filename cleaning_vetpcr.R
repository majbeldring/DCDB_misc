#--------------------------------------------------------
# Maj Beldring Henningsen, majbh@sund.ku.dk
# create vetpcr data set from vetresdyr, vetrespcr and vetpcrkode
# file location: crypted container: choose M drive

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
library(bit64) # for correctly loading sunklinreg
library(data.table) # for using fread to load
Sys.setlocale("LC_ALL","English") # for date formats

#-------------------------------------------------------
# 1: Loading data:

# PCR recording
vetrespcr  <- fread("M:/data/vetrespcrNY.csv")
vetresdyr  <- fread("M:/data/vetresdyrNY.csv") 
vetpcrkode <- fread("M:/data/vetpcrkode.csv")

#-------------------------------------------------------
# 2: Creating vetpcr

# vetrespcr
vetrespcr <- subset(vetrespcr, select= c(VETPCRKD_ID, VETRESDYR_ID, ANALYSEDATAEJAFR))
vetrespcr <- vetrespcr[!is.na(vetrespcr$VETRESDYR_ID), ] #remove NA

# vetresdyr
vetresdyr <- subset(vetresdyr, VETPRTYPE_ID == 10 ) #keep only PCR
vetresdyr <- subset(vetresdyr, select=c(ID, DYR_ID, UDTAGDATO))
names(vetresdyr) <- c("VETRESDYR_ID","DYR_ID", "UDTAGDATO")
vetresdyr <- vetresdyr[, UDTAGDATO:=as.Date(UDTAGDATO)]
vetresdyr <- subset(vetresdyr, UDTAGDATO > as.Date("2009-12-31") ) # we only want data from 2010

# create vetpcr by merging vetrespcr and vetresdyr
vetpcr <- merge(vetresdyr, vetrespcr, by="VETRESDYR_ID", sort=TRUE, allow.cartesian=TRUE)
vetpcr <- subset(vetpcr, select=-c(VETRESDYR_ID))

#--------------------------------------------------------------
# 3. Mastitis specific

# The four major IMI pathogens for dry-off treatment test
vetpcr <- subset(vetpcr,VETPCRKD_ID == 1| VETPCRKD_ID ==6| VETPCRKD_ID == 8| 
                   VETPCRKD_ID == 9| VETPCRKD_ID == 20| VETPCRKD_ID == 21|VETPCRKD_ID == 23)
vetpcr$VETPCRKD_ID <- as.factor(vetpcr$VETPCRKD_ID)
levels(vetpcr$VETPCRKD_ID) <- c("s.aureus", "s.dys", "B.strep", 
                                 "s.uberis", "B.strep", "s.uberis", "s.dys")
names(vetpcr) <- c("DYR_ID", "PCR_DATE", "PATHOGEN", "PCR_VALUE")


# directly using vetpcrkode - dont keep the pathogen name, only test results
mastitis <-  vetpcrkode[grepl("aureus", NAVNKORT) | grepl("uberis", NAVNKORT) | grepl("dysgalactiae", NAVNKORT) |
                          grepl("agalactiae", NAVN) | grepl("strep", NAVNKORT), ID]
# PCR mastitis - keep only results for pathogens defined above
vetpcr_test <- vetpcr[VETPCRKD_ID %in% mastitis,
                      .(DYR_ID, ANALYSEDATAEJAFR, UDTAGDATO)]


#-------------------------------------------------------
# save cleaned data:

rm(vetpcrkode, vetresdyr, vetrespcr)
gc()

# save vetpcr in crypted folder:
# write.csv(vetpcr,"M:/vetpcr.csv", row.names = FALSE)

#--------------------------------------------------------


