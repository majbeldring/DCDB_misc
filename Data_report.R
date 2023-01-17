
# Maj Beldring, majbh@sund.ku.dk
# UCPH, 2020

#------------------------------------------------------
# Background

# R for Data report with the purpose of creating an overview of received data
# from SEGES from the Danish Cattle Database for the ACROBAT project; 
# PhD project "Big Data".
# Data received May 2020

# Data requested for mainly dairy cattle with 11 controls per year (opposite 6 controls).
# Requested production data from 2010.
# data pre 2010 has been received in some of the data
# Data for other breeds than dairy cattle has been received in some of the data

# Total size of all received data: 18.6 GB
# Storage: Private secured crypted folder / P drive

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
Sys.setlocale("LC_ALL","English") # for date formats
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit
options(stringsAsFactors = FALSE) # prevent factorizing caracters

#------------------------------------------------------------
# load/overview of all data - except breeding values (in zipped folder):
# 49 files

# animal info
dyrinfo       <- read_csv("M:/data/dyrinfo.csv")      # 0.5 GB, breed, birthday, gender, parents ID
racekode      <- read_csv("M:/data/racekode.csv")     # breeds
oms           <- read_csv("M:/data/oms.csv")          # 1.8 GB; movement of animals. Issues with NAT_ID
omsaetkode    <- read_csv('M:/data/omsaetkode.csv')   # code for movement/culling/birth/export/import
aarsagskode   <- read_csv("M:/data/aarsagskode.csv")  # reason for movement
koenkode      <- read_csv("M:/data/koenkode.csv")     # gender code

# calvings and dryoff
kaelvninger   <- read_csv("M:/data/kaelvninger.csv")  # calvings, dates and outcome
goldninger    <- read_csv("M:/data/goldninger.csv")   # dryoff animals and dates

# insemination & reproduction (breeding values in seperate files)
ins          <- read_csv('M:/data/ins.csv')           # 0.8 GB, Insemination info
repro        <- read_csv("M:/data/repro.csv")         # 2.0 GB, reproduction information

# treatments
sundhed                 <- read_csv("M:/data/sundhed.csv")              # 2.0 GB, disease records for individual animals
lksygdomskode           <- read_csv("M:/data/lksygdomskode_fixed.csv")  # debugged prior loading
behandler               <- read_csv("M:/data/behandlerNY.csv")          # practitioners in sunklinreg
sunklinreg              <- read_csv("M:/data/sunklinreg.csv")           # 2.6 GB, clinical registrations
sunklinparameter        <- read_csv("M:/data/sunklinparameter.csv")     # codes for sunklinreg
sunmedenhedskode        <- read_csv("M:/data/sunmedenhedskode.csv")     # units in vetstat (only codes)

# vet
vetrespcr          <- read_csv("M:/data/vetrespcrNY.csv")          # pcr recordings for individual animals
vetresdyr          <- read_csv("M:/data/vetresdyrNY.csv")          # 0.7 GB, problems with virus data
vetresejd          <- read_csv("M:/data/vetresejd.csv")            # Vetres result on herd level (doubtful data...)
vetreskor          <- read_csv("M:/data/vetreskor.csv")            # correction for four different vet tests
vetpcrkode         <- read_csv("M:/data/vetpcrkode_fixed.csv")     # code for Pathogen names
vetproveart        <- read_csv("M:/data/vetproveart.csv")          # type of test (blood/milk test/... )
vetprovekode       <- read_csv("M:/data/vetprovekode.csv")         # code for result of test (including if and why a test was useless)
vetprovetype       <- read_csv("M:/data/vetprovetype.csv")         # code for 12 different antibodies
vetprovemateriale  <- read_csv("M:/data/vetprovemateriale.csv")    # code for type: 3 types: blood, milk, bacteriology

# medicin
medicindyr                  <- read_csv("M:/data/medicindyr.csv")             # 1.6 GB, Treatments NOTICE: ONLY FARMS WITH ADVISORY AGREEMENT. ISSUES with maengde variable
medicin                     <- read_csv("M:/data/medicin.csv")                # codes for drugs - Assigning AB drugs 
medicinanvendelseskode      <- read_csv("M:/data/medicin.csv")                # codes for drug usage, waste & ordinated included 
ordinationsgruppekode       <- read_csv("M:/data/ordinationsgruppekode.csv")  # codes for drug ordination (9 main groups)
# alternate medicindyr loading
library(data.table)
medicindyr                  <- fread("M:/data/medicindyr.csv")    # possible to load with fread

# herd info
brugsart      <- read_csv("M:/data/brugsart.csv")       # herd type
brugsartkode  <- read_csv("M:/data/brugsartkode.csv")   # code for hert type
dyrtilbes     <- read_csv("M:/data/dyrtilbes.csv")      # link between animal and herds
beskart       <- read_csv("M:/data/beskart.csv")        # location of herd
besstr        <- read_csv("M:/data/besstr.csv")         # herd size of various time
besyktr       <- read_csv("M:/data/besyktr.csv")        # Herds registered in yktr - and for which period

# production/ milk controls
yktr          <- read_csv("M:/data/yktr.csv")           # 3.2 GB, production, cattles with 11 controls per year

# milk
malksys           <- read_csv("M:/data/malksys.csv")             # milking systems on herd level - robot type
mlklevcellekim    <- read_csv("M:/data/mlklevcellekim.csv")      # cellekim on herd level (with CHRNR as ID)
mlklevindv        <- read_csv("M:/data/mlklevindv.csv")          # milk on herd level, week no (no dates), with CHRNR as ID

# cullings
slagtedata        <- read_csv('M:/data/slagtedata.csv')          # Slaughter weight, value and classification. Issues with weight variable
slagtefund        <- read_csv('M:/data/slagtefund.csv')          # findinds at cullings
slagtefundkode    <- read_csv('M:/data/slagtefundkode.csv')      # code for findinds at cullings
slagtefarvekode   <- read_csv('M:/data/slagtefarvekode.csv')     # code for colour at cullings
slagtefedmekode   <- read_csv('M:/data/slagtefedmekode.csv')     # code for tallow layer


# Misc
nationskode         <- read_csv('M:/data/nationskode.csv')        # country ID
tbfenhedkoedkode    <- read_csv('M:/data/tbfenhedkoedkode.csv')   # code for hours / meat. For ??
tbfenhedmaelkkode   <- read_csv('M:/data/tbfenhedmaelkkode.csv')  # code for hours / milk. For ??
kirtelkode          <- read_csv('M:/data/kirtelkode.csv')         # code for kirtel location, 1-6
ptbtilmeld          <- read_csv("M:/data/ptbtilmeld.csv")         # when a herd enter the ptb surveillance (by CHRNR)


#-------------------------------------------------------
# overview of the databases

# tools: ggpairs (ggally), n_distinct, summary, str
# misc
# dplyr::n_distinct(xx$DYR_ID)  # 
# dplyr::n_distinct(xx$BES_ID)  # 

# basic steps for each group:
## 1: basic cleaning: keep only 2010-2020 when relevant (yktr..)
## 2: n_distinct bes and dyr ; count individuals
## 3: join data; e.g. codings with main data
## 4: summary if relevant -> vaccines, AB usage, calvings, cullings, ...
## 5: yearly records: cullings, calvings, calving outcome, treatments, tests

#--------------------------------------------------------
# Quality controls: "yktr"
# 11 controls per year.

# basic cleaning: Keeping pos controls and only from 2010
production <- yktr %>%
  dplyr::select(DYR_ID, BES_ID, KONTROLDATO, CELLETAL, KGMAELK) %>%
  drop_na() %>%
  filter(CELLETAL > 0) %>%
  filter(KGMAELK > 0) %>%
  filter(KGMAELK < 100) %>%
  filter(KONTROLDATO > as.Date("2009-12-31"))

# descriptives:
summary(yktr)
dplyr::n_distinct(yktr$DYR_ID)  # 2.609.380 unique animals
dplyr::n_distinct(yktr$BES_ID)  # 3942 

#----------------------------------------------------------
# herd: "brugsbart", "brusarkode", "dyrtilbes"
# dyrinfo, herdtype, dyrtilbes, besstr, brugsart, brugsartkode

dplyr::n_distinct(dyrtilbes$BES_ID)  # 3942 unique herds

str(dyrinfo); str(racekode) # FOEDSELSDATO is date format

dyrinfo <- dyrinfo %>%
  dplyr::select(ID, RACE_ID, FOEDSELSDATO) %>%
  rename(BIRTH = FOEDSELSDATO, DYR_ID = ID)
racekode <- racekode %>%
  dplyr::select(ID, RACENAVN) %>%
  rename(RACE_ID = ID)

# join racekode and dyrinfo to create dyrinfo with breed names instead of codes:
breed <- left_join(dyrinfo, racekode , by = "RACE_ID") %>%
  dplyr::select(-RACE_ID) %>%
  drop_na()

# number of breed types
breed_types <- subset(breed, select = c(RACENAVN))
breed_types <- stack(table(breed_types))[2:1]
breed_year <- breed_types %>%
  rename(
    BREED = ind,
    COUNTS = values
  )

#rm(dyrinfo, racekode); gc()


#----------------------------------------------------------
# movements, cullings: "oms", "omsætningskode", #slagtedata"

# oms:
glimpse(oms)
dplyr::n_distinct(oms$DYR_ID)  # 6.612.455 unique animals
dplyr::n_distinct(oms$BES_ID)  # 23.084 unique herds

# slagtedata:
dplyr::n_distinct(slagtedata$DYR_ID)  # 3.876.655 unique animals
dplyr::n_distinct(slagtedata$BES_ID)

# culling data per year
slagtedata$DATO = format(as.Date(slagtedata$DATO, format='%Y-%m-%d'),'%Y') # years only
culling_year <- subset(slagtedata, select = c(DATO))
culling_year <- stack(table(culling_year))[2:1]
culling_year <- culling_year %>%
  rename(
    year = ind,
    culling = values)

#-------------------------------------------------------


#-------------------------------------------------------

#------------------------------------------------------
# vetrespcr, vetresdyr, vetpcrkode, create:vetpcr

str(vetrespcr); str(vetresdyr); str(vetpcrkode) # udtagdato format: Date

# PCR tests per year:
vetpcr_year <- subset(vetpcr, select = c(DATO))
vetpcr_year$DATO = format(as.Date(vetpcr_year$DATO, format='%Y-%m-%d'),'%Y')
vetpcr_year <- stack(table(vetpcr_year))[2:1]
# renaming columns
vetpcr_year <- vetpcr_year %>%
  rename(
    year = ind,
    PCR_tests = values)

# vetpcr now replace vetresdyr, vetrespcr and vetpcrkode
rm(vetresdyr, vetrespcr, vetpcrkode); gc()


#------------------------------------------------------------------
# calvings

str(kaelvninger) # kaelvedato format: POSIXc
dplyr::n_distinct(kaelvninger$DYR_ID)

# calvings per year:
calvings <- kaelvninger
calvings$KAELVEDATO = format(as.Date(calvings$KAELVEDATO, format='%Y-%m-%d'),'%Y') # only years
calvings_year <- subset(calvings, select = c(KAELVEDATO))
calvings_year <- stack(table(calvings_year))[2:1]
# renaming columns
calvings_year <- calvings_year %>%
  rename(
    year = ind,
    calving = values)

# calving outcome:

rm(calvings)
#rm(kaelvninger); gc()

#-------------------------------------------------------
# treatments (only keeping dry_off treatments with date and animal ID)

str(sundhed); str(lksygdomskode) # sygdomsdato format: Date

treatments <- sundhed %>%
  filter(SYGDOMSDATO > as.Date("2009-12-31")) %>%
  dplyr::select(DYR_ID, SYGDOMSDATO, LKSK_ID) %>%
  drop_na() %>%
  rename(ID = LKSK_ID)

treatments <- left_join(treatments, lksygdomskode , by = "ID")

# treatments per year from 2010:
treatments$SYGDOMSDATO = format(as.Date(treatments$SYGDOMSDATO, format='%Y-%m-%d'),'%Y') # change date format to only years
treatments_year <- subset(treatments, select = c(SYGDOMSDATO))
treatments_year <- stack(table(treatments_year))[2:1]

# renaming columns
treatments_year <- treatments_year %>%
  rename(
    year = ind,
    treatments = values
  )

glimpse(treatments)

#rm(sundhed, lksygdomskode); gc()

#------------------------------------------------------------
# brugsart -> herd, eco vs con

str(brugsart); str(brugsartkode)

brugsart <- brugsart %>%
  rename(ID = BRUGSART_ID)

# create herd dataset by joining code and brugsart:
herd <- left_join(brugsart, brugsartkode , by = "ID") %>%
  dplyr::select(-ID) %>%
  drop_na()

herd_types <- subset(herd, select = c(BRUGSARTTEKST))
herd_types <- stack(table(herd_types))[2:1]

herd_types <- herd_types %>%
  rename(
    herd = ind,
    COUNTS = values
  )


rm(brugsart, brugsartkode); gc()

#-----------------------------------------------------------
# ordinationsgroup vs diagnose

glimpse(lksygdomskode)
glimpse(ordinationsgruppekode)

ordinationsgruppekode <- ordinationsgruppekode %>%
  rename(ORDINATGRP_ID = ID)

ordinationsgrupper <- left_join(ordinationsgruppekode, lksygdomskode, by = "ORDINATGRP_ID") %>%
  dplyr::select(-ID)

write_excel_csv2(ordinationsgrupper, "ordination_diagnose.csv") #saving as write_excel prevents ?,?,? to be changed

#-----------------------------------------------------------
# check classes (and convert if needed) for each variable:

str(breed)
str(calvings)
str(dryoff)
str(herd)
str(production)
str(treatments)
str(vetpcr)

str(medicindyr)




