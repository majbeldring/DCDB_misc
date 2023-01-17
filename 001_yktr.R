### Loading, cleaning and describing YKTR milk control data
## Danish Cattle Database (DCD) 2010-2020
## Maj Beldring, 2022


## Load Packages and settings:
library(tidyverse)
#library(lubridate) # for date wrangling

## Settings if needed
Sys.setlocale("LC_ALL","English") # for date formats
options(stringsAsFactors = FALSE) # prevent factorizing caracters
memory.size()            # Checking your memory size
memory.limit(size=56000) # suggest for 64 bit


## Loading data, 'yktr' from DCD 
yktr          <- read_csv("M:/data/yktr.csv")

## Remove data pre 2010
yktr <- yktr %>% 
  filter(
         (KONTROLDATO > as.Date("2009-12-31"))) %>%
  identity()

str(yktr)


### SOMATIC CELL COUNT -----------------------------

## display max and min SCC values
yktr$CELLETAL %>% max(na.rm = TRUE)
yktr$CELLETAL %>% min(na.rm = TRUE)
# summary(yktr) # time consuming

## evaluate SCC entries in `yktr`:
scc_stats <- 
  yktr %>%
  select(SCC = CELLETAL) %>% 
  summarise(
    non_na_n = sum(!is.na(SCC)),
    na_n = n() - non_na_n,
    SCC0 = sum(SCC == 0, na.rm = TRUE),
    n_valid_scc = non_na_n - SCC0,
    SCC1 = sum(SCC == 1, na.rm = TRUE),
    SCC_2all = sum(SCC > 1, na.rm = TRUE),
    SCC_999 = sum(SCC == 9999, na.rm = TRUE)
  )

## count how often each SCC value appear:
yktr %>%
  count(CELLETAL) 


## Control dates (observations) and animals per year, 2010-2019:
year_yktr <- yktr %>%
  select(KONTROLDATO, DYR_ID, BES_ID)
year_yktr$KONTROLDATO = format(as.Date(year_yktr$KONTROLDATO, format='%Y-%m-%d'),'%Y') # keep years


## Overall Quality Controls per year for all animals:
year_yktr %>%
  group_by(KONTROLDATO) %>% 
  tally()


## Herds per year in 'yktr'
year_yktr %>% 
  select(KONTROLDATO, BES_ID) %>%
  mutate(BES_ID = as.factor(BES_ID)) %>% #don't change overall, it increases data size due to levels
  group_by(KONTROLDATO) %>%
  summarise(Herds = n_distinct(BES_ID))

## Animals per year in 'yktr'
year_yktr %>% 
  select(KONTROLDATO, DYR_ID) %>%
  mutate(DYR_ID = as.factor(DYR_ID)) %>% #don't change overall, it increases data size due to levels
  group_by(KONTROLDATO) %>%
  summarise(Animals = n_distinct(DYR_ID))

year_yktr %>% ungroup()


## SCC distribution



## SCC monthly average









## split data into "year vs scc" and "year vs milk"
year_scc <- subset(yktr_year, select = c(KONTROLDATO, CELLETAL))
year_milk <- subset(yktr_year, select = c(KONTROLDATO, KGMAELK))
##milk:
year_milk <- year_milk[!is.na(year_milk$KGMAELK), ]
range(year_milk$KGMAELK) # 0.0 99.5
## scc:
year_scc <- year_scc[!is.na(year_scc$CELLETAL), ] #remove NA from CELLETAL, overwriting yktr
range(year_scc$CELLETAL)

## calculate mean per year in both data set
y_scc <- aggregate(year_scc[, 2], list(year_scc$KONTROLDATO), mean)
y_milk <- aggregate(year_milk[, 2], list(year_milk$KONTROLDATO), mean)


## renaming columns with dplyr
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

