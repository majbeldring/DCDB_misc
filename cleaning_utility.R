#---------------------------------------------------------------------
# Maj Beldring Henningsen, majbh@sund.ku.dk
# Data cleaning for BDA project
# "yktr" , @calvings vs @KGMAELK (milk) , @CELLETAL (scc)
# creating grouped data for further analysis (grouped by number of calvings)
# for: BDA utility analysis fitted with stan -> "utility_calvings.R"
# last updated: 2020-06-24

#--------------------------------------------------------------------
# Packages and settings
library(tidyverse) # includes dplyr and ggplot2
library(reshape2) # melt commands
library(data.table) # adding a row
library(plotly)
library(GGally)
library(ggExtra)
library(ggalluvial)
Sys.setlocale("LC_ALL","English") # data formatting

#---------------------------------------------------------------------
# @scc and @milk: loading and cleaning raw data "yktr" and "kaelvinger"

# "yktr"
yktr <- read.table("yktr.csv",header=TRUE,sep=",")
# cleaning yktr
str(yktr)
yktr <- yktr[!is.na(yktr$CELLETAL), ] #remove NA from CELLETAL, overwriting yktr
range(yktr$CELLETAL) # check range: 0 to 9999: All POS
yktr <- subset(yktr, select = c(DYR_ID, KONTROLDATO, KGMAELK, CELLETAL)) # Keep selected coloumns

# "kaelvinger"
calvings <- read.table("kaelvninger.csv",header=TRUE,sep=",")
# cleaning kaelv
summary(calvings) #Overview - No NA's in kaelvinger
table(is.na(calvings$KAELVNINGSNR)) # No NA'S
calv <- subset(calvings, select = c(DYR_ID, KAELVEDATO, KAELVNINGSNR)) # Keep selected coloumns
calv$KAELVEDATO = format(as.Date(calv$KAELVEDATO, format='%Y-%m-%d'))

#---------------------------------------------------------------------
# calvings 1: milk and scc 365 days after first calving

calv_1 <- subset(calv, KAELVNINGSNR == 1 ) # keep only calvning #1
length(unique(calv_1$DYR_ID)) #checking each animal only appears one
calv_1 <- merge(yktr, calv_1, by="DYR_ID", sort=TRUE) # merge YKTR and calv

# Keep only CELLETAL for up to 365 days after Kaelvning. Clean KONTROLDATO
# instead start-end goldning should be applied, and then only take yktr data in between...
calv_1 <- subset(calv_1, KONTROLDATO > KAELVEDATO & KONTROLDATO - 365 < KAELVEDATO)
# Adding a column with difference between kaelvedato and kontroldato
calv_1$days <- as.Date(as.character(calv_1$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(calv_1$KAELVEDATO), format="%Y-%m-%d")
range(calv_1$days)
calv_1 <- subset(calv_1, select = c(days, KGMAELK, CELLETAL))
names(calv_1) <- c("Days", "Milk_1", "SCC_1")
calv_1 <- calv_1[!is.na(calv_1$Milk_1), ] #remove NA
calv_1 <- aggregate(calv_1[, 2:3], list(calv_1$Days), mean)
names(calv_1) <- c("Days", "Milk_1", "SCC_1")

#-------------------------------------------------------------------
# Calving 2: milk and scc 365 days after second calving

calv_2 <- subset(calv, KAELVNINGSNR == 2 ) # keep only Kaelvning #2
length(unique(calv_2$dyr_IDAN)) #checking each animal only appears one
calv_2 <- calv_2[!duplicated(calv_2$dyr_IDAN),] # remove duplicates
calv_2 <- merge(yktr, calv_2, by="dyr_IDAN", sort=TRUE) # merge YKTR and Kaelv

# Keep only CELLETAL for up to 365 days after Kaelvning. Clean KONTROLDATO
calv_2 <- subset(calv_2, KONTROLDATO > KAELVEDATO & KONTROLDATO - 365 < KAELVEDATO)
# Adding a column with difference between kaelvedato and kontroldato
calv_2$days <- as.Date(as.character(calv_2$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(calv_2$KAELVEDATO), format="%Y-%m-%d")
calv_2 <- subset(calv_2, select = c(days, KGMAELK, CELLETAL))
names(calv_2) <- c("Days", "Milk_2", "SCC_2")
calv_2 <- aggregate(calv_2[, 2:3], list(calv_2$Days), mean)
names(calv_2) <- c("Days", "Milk_2", "SCC_2")

#----------------------------------------------------------------------
# Calvning 3: milk and scc 365 days after third calving

calv_3 <- subset(calv, KAELVNINGSNR == 3 ) # keep only Kaelvning #3
length(unique(calv_3$dyr_IDAN)) #checking each animal only appears one
calv_3 <- calv_3[!duplicated(calv_3$dyr_IDAN),] # remove duplicates (only 4)
calv_3 <- merge(yktr, calv_3, by="dyr_IDAN", sort=TRUE) # merge YKTR and Kaelv

# Keep only CELLETAL for up to 365 days after calving. Clean KONTROLDATO
calv_3 <- subset(calv_3, KONTROLDATO > KAELVEDATO & KONTROLDATO - 365 < KAELVEDATO)
# Adding a column with difference between kaelvedato and kontroldato
calv_3$days <- as.Date(as.character(calv_3$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(calv_3$KAELVEDATO), format="%Y-%m-%d")
calv_3 <- subset(calv_3, select = c(days, KGMAELK, CELLETAL))
names(calv_3) <- c("Days", "Milk_3", "SCC_3")
calv_3 <- aggregate(calv_3[, 2:3], list(calv_3$Days), mean) # mean milk and scc
names(calv_3) <- c("Days", "Milk_3", "SCC_3")

#--------------------------------------------------------------
# calving 4: milk and scc 365 days after fourth calving

calv_4 <- subset(calv, KAELVNINGSNR == 4 ) # keep only calving #4
length(unique(calv_4$dyr_IDAN)) #checking each animal only appears one
calv_4 <- calv_4[!duplicated(calv_4$dyr_IDAN),] # remove duplicates (only 4)
calv_4 <- merge(yktr, calv_4, by="dyr_IDAN", sort=TRUE) # merge YKTR and Kaelv

# Keep only CELLETAL for up to 365 days after Kaelvning. Clean KONTROLDATO
calv_4 <- subset(calv_4, KONTROLDATO > KAELVEDATO & KONTROLDATO - 365 < KAELVEDATO)
# Adding a column with difference between kaelvedato and kontroldato
calv_4$days <- as.Date(as.character(calv_4$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(calv_4$KAELVEDATO), format="%Y-%m-%d")
calv_4 <- subset(calv_4, select = c(days, KGMAELK, CELLETAL))
names(kaelv_4) <- c("Days", "Milk_4", "SCC_4")
calv_4 <- aggregate(calv_4[, 2:3], list(calv_4$Days), mean)
names(calv_4) <- c("Days", "Milk_4", "SCC_4")

#----------------------------------------------------------------------
# calving 5: milk and SCC 365 days after fifth calving

calv_5 <- subset(calv, KAELVNINGSNR == 5 ) # keep only Kaelvning #5
length(unique(calv_5$dyr_IDAN)) #checking each animal only appears one
calv_5 <- calv_5[!duplicated(calv_5$dyr_IDAN),] # remove duplicates
calv_5 <- merge(yktr, calv_5, by="dyr_IDAN", sort=TRUE) # merge YKTR and Kaelv

# Keep only CELLETAL for up to 365 days after Kaelvning. Clean KONTROLDATO
calv_5 <- subset(calv_5, KONTROLDATO > KAELVEDATO & KONTROLDATO - 365 < KAELVEDATO)
# Adding a column with difference between kaelvedato and kontroldato

calv_5$days <- as.Date(as.character(calv_5$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(calv_5$KAELVEDATO), format="%Y-%m-%d")
calv_5 = subset(calv_5, select = c(days, KGMAELK, CELLETAL))
names(calv_5) <- c("Days", "Milk_5", "SCC_5")
calv_5 <- calv_5[!is.na(calv_5$Milk_5), ] #remove NA
calv_5 <- aggregate(calv_5[, 2:3], list(calv_5$Days), mean)
names(calv_5) <- c("Days", "Milk_5", "SCC_5")

#--------------------------------------------------------------------
# calving 6: milk and scc 365 days after sixth calving

calv_6 <- subset(calv, KAELVNINGSNR == 6 ) # keep only Kaelvning #6
length(unique(calv_6$dyr_IDAN)) #checking each animal only appears one
calv_6 <- calv_6[!duplicated(calv_6$dyr_IDAN),] # remove duplicates
calv_6 <- merge(yktr, calv_6, by="dyr_IDAN", sort=TRUE) # merge YKTR and Kaelv

# Keep only CELLETAL for up to 365 days after Kaelvning. Clean KONTROLDATO
calv_6 <- subset(calv_6, KONTROLDATO > KAELVEDATO & KONTROLDATO - 365 < KAELVEDATO)
# Adding a column with difference between kaelvedato and kontroldato
calv_6$days <- as.Date(as.character(calv_6$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(calv_6$KAELVEDATO), format="%Y-%m-%d")
calv_6 <- subset(calv_6, select = c(days, KGMAELK, CELLETAL))
names(calv_6) <- c("Days", "Milk_6", "SCC_6")
calv_6 <- aggregate(calv_6[, 2:3], list(calv_6$Days), mean)
names(calv_6) <- c("Days", "Milk_6", "SCC_6")

#-------------------------------------------------------------------
# Merging all mean dataset to 'calv_all', step-by-step for testing

calv_12 <- merge(calv_1, calv_2, by="Days", sort=TRUE)
calv_123 <- merge(calv_12, calv_3, by="Days", sort=TRUE)
calv_1234 <- merge(calv_123, calv_4, by="Days", sort=TRUE)
calv_12345 <- merge(calv_1234, calv_5, by="Days", sort=TRUE)
calvings_all <- merge(calv_12345, calv_6, by="Days", sort=TRUE)

# Selecting days interval: Choosing days with interval of 7 days for firs 30 weeks
calv_all <- calv_all[c(7,14,21,28,35,42,49,56,63,70,77,84,91,98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210),]

calvings_milk <- subset(calv_all, select =
                          c(Milk_1,Milk_2,Milk_3,Milk_4, Milk_5, Milk_6))
calvings_scc <- subset(calv_all, select =
                          c(SCC_1,SCC_2,SCC_3,SCC_4, SCC_5, SCC_6))

# save cleaned data
write.csv(calvings_milk,"..\\calvings_milk.csv", row.names = FALSE)
write.csv(calvings_scc,"..\\calvings_scc.csv", row.names = FALSE)

