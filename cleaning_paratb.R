#--------------------------------------------------------------------
# Maj Beldring Henningsen, majbh@sund.ku.dk

# paratb cases 2008-2019:
# Cleaning data + visualizing paratb cases 
# posterior plot with stan for prediction

# "vetresdyr" @paratb (@prtb); vetprtype_id=7 POS if: analysedata>=0.3

#-------------------------------------------------------------------
# Packages and settings
library(tidyverse) # includes dplyr and ggplot2
library(reshape2) # for melt commands
library(data.table) # for adding a row for occurences of each dyr_IDAN
library(readxl)
Sys.setlocale("LC_ALL","English") # change locale to English, for date formatting

#---------------------------------------------------------------------
# import data: "vetresdyr"
vetresdyr <- read.csv2("P:/data_Maj/vetresdyr.csv",header=TRUE,sep=",")

# Cleaning "vetresdyr" for paratb cases:
glimpse(vetresdyr)
paratb <- subset(vetresdyr, VETPRTYPE_ID == 7 ) #selecting paratb cases

# POS cases: ANALYSEDATA >= 0.3
paratb_POS <- subset(paratb, ANALYSEDATA >= 0.3 ) #selecting POS paratb cases
paratb_POS <- subset(paratb_POS, select = c(DYR_ID, KONTROLDATO)) # keep only paratb cases and date
paratb_POS$KONTROLDATO = format(as.Date(paratb_POS$KONTROLDATO, format='%Y-%m-%d'),'%Y') # keep years
#paratb_NEG <- subset(paratb_NEG, KONTROLDATO < 2020, KONTROLDATO >=2008 )
paratb_POS <- subset(paratb_POS, KONTROLDATO < 2020 ) #range to 2019
paratb_POS <- subset(paratb_POS, KONTROLDATO >= 2008 ) #range from 2008 (pre 2008 tests not standardized)
paratb_POS <- subset(paratb_POS, select = c(KONTROLDATO))
paratb_POS <- stack(table(paratb_POS))[2:1] #calculating cases per year (times each year appear)

# renaming columns
paratb_POS <- paratb_POS %>%
  rename(
    year = ind,
    POS = values
  )

# NEG cases: ANALYSEDATA <0.3:
paratb_NEG <- subset(paratb, ANALYSEDATA < 0.3 ) #selecting NEG analysis (paratb not confirmed)
paratb_NEG <- subset(paratb, ANALYSEDATA >= 0 ) #sensuring no NEG values (confirm these should be deselected)
paratb_NEG <- subset(paratb_NEG, select = c(DYR_ID, KONTROLDATO)) # keep only paratb cases and date
paratb_NEG$KONTROLDATO = format(as.Date(paratb_NEG$KONTROLDATO, format='%Y-%m-%d'),'%Y') # keep years
#paratb_NEG <- subset(paratb_NEG, KONTROLDATO < 2020, KONTROLDATO >=2008 ) 
paratb_NEG <- subset(paratb_NEG, KONTROLDATO < 2020 ) #range to 2019
paratb_NEG <- subset(paratb_NEG, KONTROLDATO >= 2008 ) #range from 2008 (pre 2008 tests not standardized)
paratb_NEG <- subset(paratb_NEG, select = c(KONTROLDATO))
paratb_NEG <- stack(table(paratb_NEG))[2:1] #calculating cases per year (times each years appear)

# renaming columns
paratb_NEG <- paratb_NEG %>%
  rename(
    year = ind,
    NEG = values
  )

# Merging POS and NEG cases:
prtb <- merge(paratb_POS, paratb_NEG, by="year", sort=TRUE)

# adding a coloumn with total tests: (outliers such as NEG ANALYSEDATA are then not included in TOT)
#prtb <- aggregate(year_milk[, 2], list(year_milk$KONTROLDATO), sum)
prtb$total <- prtb$POS + prtb$NEG

# save cleaned data
write.csv(prtb_year,"year_paratb.csv", row.names = FALSE)

#------------------------------------------------------------------------
# including production animals per year from yktr, 2010-2019

prtb_animals <- subset(prtb, year >= 2010 ) #years from 2010

# merge again to one
# add coloumn with animals per year from yktr (add yktr script to find animals per year)
# add a coloumn with: calculate percentage of naimals tested. calculate percentage of pos cases of those tested..
# animals <- c(727671,742897,742528,737013,734603,713808,733604,709572,719306,742897)
# year_paratb_cow <- transform(prtb_year, prtb_cow = prtb / animals)


#-----------------------------------------------------------------------
# Visualizing ParaTB cases from 2008-2019 with ggplot Barplot

prtb <- read.csv("year_paratb.csv", header = TRUE)
prtb_2 <- melt(prtb, id.vars=1:1)

ggplot(data=prtb_2, aes(x=year, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  ylab("Paratb cases")
  theme_bw()

# plot 1 - visualizing data:
ggplot() + geom_point(aes(year, paratb), data = prtb, size = 1) +
  labs(y = 'Paratuberculosis cases', x= "Year") +
  guides(linetype = F)

ggplot(prtb, aes(fill=condition, y=total, x=year)) + 
  geom_bar(position="dodge", stat="identity")


#-----------------------------------------------------------------------
# posterior plot of paratb cases
# test which model is best with new data.
# create plot

### Redo so all analysedata is plotttet with a posterior distribution plot

paratb <- subset(vetresdyr, VETPRTYPE_ID == 7 ) #selecting paratb cases
paratb_pred <- subset(paratb, select = c(DYR_ID, KONTROLDATO)) # paratb contains only paratb data from teresdyr

# fit with linear stan model: # gaussian
fit_lin <- stan_glm(paratb ~ year, data = prtb, family = gaussian(),
                    refresh=1000, iter=1000, chains=4, seed=SEED)
summary(fit_lin, probs=c(0.1, 0.5, 0.9))
# prediction with linear gaussian model:
x_predict <- seq(2010,2025)
N_predict <- length(x_predict)
y_predict <- posterior_predict(fit_lin, newdata=data.frame(year=x_predict))
mu <- apply(t(y_predict), 1, quantile, c(0.05, 0.5, 0.95)) %>%
  t() %>% data.frame(x = x_predict, .) %>% gather(pct, y, -x)
pfit <- ggplot() +
  geom_point(aes(year, paratb), data = prtb, size = 1) +
  geom_line(aes(x, y, linetype = pct), data = mu, color = 'red') +
  scale_linetype_manual(values = c(2,1,2)) +
  labs(x = 'Year', y = 'Paratb cases') +
  guides(linetype = F)
(pfit)

# fit with non-linear stan model: # gaussian
fit_gam <- stan_gamm4(paratb ~ year + s(year), data=prtb,
                      family=gaussian(), adapt_delta=0.999,
                      refresh=1000, iter=2000, chain=4, seed=SEED)
summary(fit_gam, probs=c(0.1, 0.5, 0.9))

# prediction with non-linear gaussian model:
x_predict <- seq(2010,2025)
N_predict <- length(x_predict)
y_predict <- posterior_predict(fit_gam, newdata=data.frame(year=x_predict))
mu <- apply(t(y_predict), 1, quantile, c(0.05, 0.5, 0.95)) %>%
  t() %>% data.frame(x = x_predict, .) %>% gather(pct, y, -x)
pfit <- ggplot() +
  geom_point(aes(year, paratb), data = prtb, size = 1) +
  geom_line(aes(x, y, linetype = pct), data = mu, color = 'red') +
  scale_linetype_manual(values = c(2,1,2)) +
  labs(x = 'Year', y = 'paratb cases') +
  guides(linetype = F)
(pfit)

# compare
draws <- as.data.frame(fit_lin)
loo_lin <- loo(fit_lin)
draws <- as.data.frame(fit_gam)
loo_gam <- loo(fit_gam)
loo_compare(loo_lin, loo_gam)
plot(loo_lin, diagnostic = c("k", "n_eff"),
     label_points = FALSE, main = "PSIS diagnostic plot: Linear model")
plot(loo_gam, diagnostic = c("k", "n_eff"),
     label_points = FALSE, main = "PSIS diagnostic plot: Gam model")

# conclusion:
# Rhat:
# n_eff: we want the highest:




