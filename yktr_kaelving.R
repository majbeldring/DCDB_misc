#---------------------------------------
# yield and SCC after calvings
# "yktr" and "kaelvninger"
# @KGMAELK , @CELLETAL, @days
# scc and lactation curves after calvings

#---------------------------------------------------
# Libraries and settings
library(tidyverse) # includes dplyr and ggplot2
library(reshape2) # for melt commands
library(data.table) # for adding a row for occurences of each dyr_IDAN
library(plotly)
library(GGally)
library(ggExtra)
library(ggalluvial)
#setwd('C:/Users/majhe/Google Drive/PhD/R_data') # set location
Sys.setlocale("LC_ALL","English") # change locale to English, for data formatting

#-----------------------------------------------------
# import and basic clean: "kaelvninger", "yktr" (maya)
kaelv <- read.table("data_maya/kaelvninger/kaelvninger.csv",header=TRUE,sep=",")
yktr <- read.table("data_maya/yktr/yktr.csv",header=TRUE,sep=",")

# cleaning yktr
head(yktr) #Look at header
table(is.na(yktr$dyr_IDAN)) # No NA in dyr_IDAN
table(is.na(yktr$KONTROLNR)) # No NA in KONTROLNR
table(is.na(yktr$CELLETAL)) # NA's found in CELLETAL
yktr <- yktr[!is.na(yktr$CELLETAL), ] #remove NA from CELLETAL, overwriting yktr
range(yktr$CELLETAL) # check range: -1029 to 9999: Celletal shouldn't be negative
yktr %>%  # identifying negative celletal
  filter(CELLETAL < 0) %>%  select(CELLETAL) #67 cases. Will remove these:
yktr <- subset(yktr, CELLETAL >= 0 ) # removing NEG celletal, overwriting yktr
yktr$KONTROLDATO = as.Date(yktr$KONTROLDATO, format='%d%b%Y:%H:%M:%S') #Change date format
# Removing unrelevant coloumns from YKTR (focus: kgmaelk and celletal):
yktr = subset(yktr, select = c(dyr_IDAN, KONTROLDATO, KGMAELK, CELLETAL)) # Keep selected coloumns

# cleaning kaelv
summary(kaelv) #Overview - No NA's in kaelvinger
table(is.na(kaelv$KAELVNINGSNR)) # No NA'S
kaelv$KAELVEDATO = as.Date(kaelv$KAELVEDATO, format='%d%b%Y:%H:%M:%S') #Change date format

#----------------------------------------------------------------------
# Kaelvning 1 - milk + SCC 365 days after first kaelvning

kaelv_1 <- subset(kaelv, KAELVNINGSNR == 1 ) # keep only Kaelvning #1
length(unique(kaelv_1$dyr_IDAN)) #checking each animal only appears one
# Removing unrelevant coloumns from kaelv:
kaelv_1 = subset(kaelv_1, select = c(dyr_IDAN, KAELVEDATO)) # Keep selected coloumns

kaelv_1 <- merge(yktr, kaelv_1, by="dyr_IDAN", sort=TRUE) # merge YKTR and Kaelv

# Keep only CELLETAL for up to 365 days after Kaelvning. Clean KONTROLDATO
kaelv_1 <- subset(kaelv_1, KONTROLDATO > KAELVEDATO & KONTROLDATO - 365 < KAELVEDATO)
# Adding a column with difference between kaelvedato and kontroldato
kaelv_1$days <- as.Date(as.character(kaelv_1$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(kaelv_1$KAELVEDATO), format="%Y-%m-%d")
range(kaelv_1$days)
head(kaelv_1)

kaelv_1 = subset(kaelv_1, select = c(days, KGMAELK, CELLETAL))
names(kaelv_1) <- c("Days", "Milk_1", "SCC_1")

# PLOT all kaelv_1:
p_1_SCC <- ggplot(kaelv_1, aes(x=as.numeric(Days), y=SCC_1)) +
  geom_smooth()
p_1_SCC # SCC
p_1_milk <- ggplot(kaelv_1, aes(x=as.numeric(Days), y=Milk_1)) +
  geom_smooth()
p_1_milk # Milk

#--------------------------------------------------------------
#MEAN of kaelv 1: calculating mean SCC and Milk per day

kaelv_1_mean <- aggregate(kaelv_1[, 2:3], list(kaelv_1$Days), mean)
str(kaelv_1_mean)
names(kaelv_1_mean) <- c("Days", "Milk_mean_1", "SCC_mean_1")

# ggplot CELLETAL MEAN kaelv_1:
p_1_SCC_mean <- ggplot(kaelv_1_mean, aes(x=as.numeric(Days), y=SCC_mean_1)) +
  geom_smooth()
p_1_SCC_mean
# ggplot KGMAELK MEAN all data
p_1_milk_mean <- ggplot(kaelv_1_mean, aes(x=as.numeric(Days), y=Milk_mean_1)) +
  geom_smooth()
p_1_milk_mean

#-----------------------------------------------------------------
# Kaelvning 2 - milk + SCC 365 days after first kaelvning

kaelv_2 <- subset(kaelv, KAELVNINGSNR == 2 ) # keep only Kaelvning #2
kaelv_2 = subset(kaelv_2, select = c(dyr_IDAN, KAELVEDATO)) # Keep selected coloumns
length(unique(kaelv_2$dyr_IDAN)) #checking each animal only appears one
kaelv_2 <- kaelv_2[!duplicated(kaelv_2$dyr_IDAN),] # remove duplicates (only 4)

kaelv_2 <- merge(yktr, kaelv_2, by="dyr_IDAN", sort=TRUE) # merge YKTR and Kaelv

# Keep only CELLETAL for up to 365 days after Kaelvning. Clean KONTROLDATO
kaelv_2 <- subset(kaelv_2, KONTROLDATO > KAELVEDATO & KONTROLDATO - 365 < KAELVEDATO)
# Adding a column with difference between kaelvedato and kontroldato
kaelv_2$days <- as.Date(as.character(kaelv_2$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(kaelv_2$KAELVEDATO), format="%Y-%m-%d")
range(kaelv_2$days)
head(kaelv_2)

kaelv_2 = subset(kaelv_2, select = c(days, KGMAELK, CELLETAL))
names(kaelv_2) <- c("Days", "Milk_2", "SCC_2")

# PLOT all kaelv_2:
p_2_SCC <- ggplot(kaelv_2, aes(x=as.numeric(Days), y=SCC_2)) +
  geom_smooth()
p_2_SCC # SCC
p_2_milk <- ggplot(kaelv_2, aes(x=as.numeric(Days), y=Milk_2)) +
  geom_smooth()
p_2_milk # Milk

#----------------------------------------------------------
#MEAN of kaelv 2: calculating mean SCC and Milk per day

kaelv_2_mean <- aggregate(kaelv_2[, 2:3], list(kaelv_2$Days), mean)
str(kaelv_2_mean)
names(kaelv_2_mean) <- c("Days", "Milk_mean_2", "SCC_mean_2")

# ggplot CELLETAL MEAN kaelv_2:
p_2_SCC_mean <- ggplot(kaelv_2_mean, aes(x=as.numeric(Days), y=SCC_mean_2)) +
  geom_smooth()
p_2_SCC_mean
# ggplot KGMAELK MEAN all data
p_2_milk_mean <- ggplot(kaelv_2_mean, aes(x=as.numeric(Days), y=Milk_mean_2)) +
  geom_smooth()
p_2_milk_mean

#------------------------------------------------------------------
# Kaelvning 3 - milk + SCC 365 days after first kaelvning

kaelv_3 <- subset(kaelv, KAELVNINGSNR == 3 ) # keep only Kaelvning #3
kaelv_3 = subset(kaelv_3, select = c(dyr_IDAN, KAELVEDATO)) # Keep selected coloumns
length(unique(kaelv_3$dyr_IDAN)) #checking each animal only appears one
kaelv_3 <- kaelv_3[!duplicated(kaelv_3$dyr_IDAN),] # remove duplicates (only 4)

kaelv_3 <- merge(yktr, kaelv_3, by="dyr_IDAN", sort=TRUE) # merge YKTR and Kaelv

# Keep only CELLETAL for up to 365 days after Kaelvning. Clean KONTROLDATO
kaelv_3 <- subset(kaelv_3, KONTROLDATO > KAELVEDATO & KONTROLDATO - 365 < KAELVEDATO)
# Adding a column with difference between kaelvedato and kontroldato
kaelv_3$days <- as.Date(as.character(kaelv_3$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(kaelv_3$KAELVEDATO), format="%Y-%m-%d")
range(kaelv_3$days)
head(kaelv_3)

kaelv_3 = subset(kaelv_3, select = c(days, KGMAELK, CELLETAL))
names(kaelv_3) <- c("Days", "Milk_3", "SCC_3")

# PLOT all kaelv_3:
p_3_SCC <- ggplot(kaelv_3, aes(x=as.numeric(Days), y=SCC_3)) +
  geom_smooth()
p_3_SCC # SCC
p_3_milk <- ggplot(kaelv_3, aes(x=as.numeric(Days), y=Milk_3)) +
  geom_smooth()
p_3_milk # Milk

#-------------------------------------------------------------
# MEAN of kaelv 3: calculating mean SCC and Milk per day

kaelv_3_mean <- aggregate(kaelv_3[, 2:3], list(kaelv_3$Days), mean)
str(kaelv_3_mean)
names(kaelv_3_mean) <- c("Days", "Milk_mean_3", "SCC_mean_3")

# ggplot CELLETAL MEAN kaelv_3:
p_3_SCC_mean <- ggplot(kaelv_3_mean, aes(x=as.numeric(Days), y=SCC_mean_3)) +
  geom_smooth()
p_3_SCC_mean
# ggplot KGMAELK MEAN all data
p_3_milk_mean <- ggplot(kaelv_3_mean, aes(x=as.numeric(Days), y=Milk_mean_3)) +
  geom_smooth()
p_3_milk_mean

#-------------------------------------------------------------
# Merge 1,2 and 3 calving

kaelv_12_mean <- merge(kaelv_1_mean, kaelv_2_mean, by="Days", sort=TRUE)
kaelv_123_mean <- merge(kaelv_12_mean, kaelv_3_mean, by="Days", sort=TRUE)

# Step 3: plot SCC and
# Plot SCC with 4 lines:
p_123_scc <- ggplot(kaelv_123_mean, aes(x=as.numeric(Days))) +
  geom_smooth(aes(y = SCC_mean_1), color = "darkred") +
  geom_smooth(aes(y = SCC_mean_2), color="cadetblue2") +
  geom_smooth(aes(y = SCC_mean_3), color = "darkgoldenrod1") +
  labs(y = " Mean Somatic Cell Count / cow", x = "Days after calving") +
  theme(legend.position = c(0.8, 0.9))
p_123_scc

# Plot SCC with 4 lines:
p_123_milk <- ggplot(kaelv_123_mean, aes(x=as.numeric(Days))) +
  geom_smooth(aes(y = Milk_mean_1), color = "darkred") +
  geom_smooth(aes(y = Milk_mean_2), color="cadetblue2") +
  geom_smooth(aes(y = Milk_mean_3), color = "darkgoldenrod1") +
  labs(y = "Mean Milk Kg / cow", x = "Days after calving") +
  theme(legend.position = c(0.8, 0.9))
p_123_milk


#-------------------------------------------------------------
# Plot SCC with 4 lines:
p_123 <- ggplot(kaelv_123_mean, aes(x=as.numeric(Days))) +
  geom_smooth(aes(y = Milk_mean_1), color = variable) +
  geom_smooth(aes(y = Milk_mean_2), color=variable) +
  geom_smooth(aes(y = Milk_mean_3), color = variable) +
  labs(y = "Mean Milk Kg / cow", x = "Days after calving") +
  scale_colour_manual(values=c("darkred","cadetblue2","darkgoldenrod1"))
  theme(legend.position = c(0.8, 0.9))
p_123

ggplot(dd) +
  geom_line(aes(x=fecha, y=value, colour=variable)) +
  scale_colour_manual(values=c("darkred","cadetblue2","darkgoldenrod1"))



