# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #  Gender Equality Extension of SSPs  # # # # # # # # # # 
# # # # # # #           Data Processing           # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list = ls())

library(ggplot2)
library(tidyverse)
library(broom)
library(countrycode)
library(readstata13)
library(sandwich)
library(lmtest)
library(zoo)
library(plm)
library(broom)
library(readxl)
library(wrapr)
library(ggpmisc)
library(ggpubr)
library(gridExtra)

setwd('/Users/marinaandrijevic/Documents/GitHub/gender_inequality_SSPs/Input')


# 1. Gender Inequality Index (GII) - replication based on the UNDP methodology

custom.match <- c(`Eswatini (Kingdom of)` = 'SWZ') # Eswatini/Swaziland custom code for the countrycode package

# Load the UNDP data

gii.hist <- read.csv('gii_historical.csv', skip = 1) %>% 
  select(Country, X1995:X2018) %>% 
  mutate(countrycode = countrycode(Country, 'country.name', 'iso3c', custom_match = custom.match)) %>%
  gather(year, gii.hist, -countrycode, -Country) %>%
  mutate(year = year %>% str_replace("X","") %>% as.integer(),
         gii.hist = gii.hist %>% as.numeric()) %>% 
  filter(!year %in% c('.', 0)) %>% 
  select(-Country)

# Components: 

# Health (female reproductive health index): maternal mortality ratio, adolescent birth rate

custom.match <- c(`Eswatini (Kingdom of)` = 'SWZ') # Eswatini/Swaziland custom code for the countrycode package

mmr.undp <- read.csv('mmr.csv', skip = 1) %>% 
  select(Country, X1995:X2015) %>%
  mutate(countrycode = countrycode(Country, 'country.name', 'iso3c', custom_match = custom.match)) %>%
  gather(year, mmr, -countrycode, -Country) %>%
  mutate(year = year %>% str_replace("X","") %>% as.integer(),
         mmr = mmr %>% as.numeric()) %>% 
  filter(!year %in% c('.', 0) & year < 2000) %>% 
  drop_na(countrycode) %>% 
  select(-Country) %>% 
  filter(year < 2000)

mmr.wb <- read.csv('mmr_wb.csv', skip = 3) %>% 
  select(Country.Code, X2000:X2018) %>%
  rename(countrycode = Country.Code) %>% 
  gather(year, mmr, -countrycode) %>% 
  mutate(year = year %>% str_replace("X","") %>% as.integer())

mmr <- mmr.undp %>% 
  bind_rows(mmr.wb) 

abr <- read.csv('abr_wb.csv', skip = 3) %>% 
  select(Country.Code, X1990:X2018) %>%
  rename(countrycode = Country.Code) %>% 
  gather(year, abr, -countrycode) %>% 
  mutate(year = year %>% str_replace("X","") %>% as.integer())


# Empowerment: female and male population with at least secondary education, female and male share of parliamentary seats = female/male empowerment index

singleentry <- c('ATG', 'BRB', 'BRN', 'ERI', 'ESH', 'GRD', 'GUM', 'LBY', 'MRT', 'MYT', 'PNG', 'UZB', 'VIR', 'DJI', 'SYC') #will be deleted from the sample as the values are only for 2015 and this prevents interpolation

edu.prep <- read.csv('edu_mf.csv', skip = 8) %>% 
  rename_all(tolower) %>% 
  mutate(countrycode = countrycode(area, 'country.name', 'iso3c'),
         education = str_replace(str_to_lower(education), fixed(" "), ".")) %>% 
  filter(education %in% c('lower.secondary', 'upper.secondary', 'post.secondary')) %>% 
  drop_na(countrycode) %>% 
  group_by(scenario, countrycode, year, sex) %>% 
  mutate(agg.edu = sum(distribution, na.rm = T)/100) %>% 
  ungroup() %>% 
  filter(education == 'upper.secondary',
        !countrycode %in% singleentry)

dpinterp <- function(df) {
  with(df, approx(x=indx, y=value, xout=min(indx):max(indx))) %>% 
    bind_cols()
}

edu <- edu.prep %>% 
  select(countrycode, year, scenario, sex, agg.edu) %>% 
  group_by(scenario, countrycode, sex) %>% 
  rename(indx = year, value = agg.edu) %>% 
  filter(!indx == 2015) %>% 
  do(dpinterp(.)) %>% 
  rename(year = x, agg.edu = y) %>% 
  right_join(edu.prep %>% distinct(countrycode), by='countrycode') %>% 
  spread(sex, agg.edu) %>% 
  rename(edu.f = Female, edu.m = Male) %>% 
  filter(year < 2020 & scenario == 'SSP2')


parl1 <- read.csv('fem_parliament_seats.csv', skip = 1) %>%
  select(Country, X1995:X2018) %>%
  mutate(countrycode = countrycode(Country, 'country.name', 'iso3c', custom_match = custom.match)) %>%
  gather(year, parl.f, -countrycode, -Country) %>%
  mutate(year = year %>% str_replace("X","") %>% as.integer(),
         parl.f = (parl.f %>% as.numeric())/100) %>%
  filter(!year %in% c('.', 0)) %>%
  select(-Country) %>% 
  mutate(id = paste0(countrycode, year))


parl2 <- read.csv('seats-held-by-women-in-national-parliaments.csv', na.strings = "") %>% 
  rename(parl.f = 'Proportion.of.seats.held.by.women.in.national.parliaments........',
         countrycode = Code,
         year = Year) %>% 
  mutate(parl.f = parl.f/100) %>% 
  drop_na(countrycode) %>% 
  mutate(id = paste0(countrycode, year))


parl <- merge(parl1, parl2, by = "id", all = TRUE) %>% 
  mutate(parl.f = (parl.f.x %?% parl.f.y),  #fill the missing values from one dataset into the other
         year = year.x %?% year.y) %>% 
  select(id, parl.f) %>% 
  mutate(countrycode = substr(id,1,3),
         year = substr(id,4,8) %>% as.integer())


# Labour market: female and male LFPR; female/male labour market index

flfp <- read.csv('flfp_ilo_mod_est.csv', skip = 3) %>% 
  select(Country.Code, X1990:X2018) %>%
  rename(countrycode = Country.Code) %>% 
  gather(year, flfp, -countrycode) %>% 
  mutate(year = year %>% str_replace("X","") %>% as.integer(),
         flfp = flfp/100)

mlfp <- read.csv('mlfp_ilo_mod_est.csv', skip = 3) %>% 
  select(Country.Code, X1990:X2018) %>%
  rename(countrycode = Country.Code) %>% 
  gather(year, mlfp, -countrycode) %>% 
  mutate(year = year %>% str_replace("X","") %>% as.integer(),
         mlfp = mlfp/100)


# Put together to calculate the index 

gii.prep <- mmr %>% 
  left_join(abr, by = c('countrycode', 'year')) %>% 
  left_join(edu, by = c('countrycode', 'year')) %>% 
  left_join(parl, by = c('countrycode', 'year')) %>% 
  left_join(flfp, by = c('countrycode', 'year')) %>% 
  left_join(mlfp, by = c('countrycode', 'year')) %>% 
  mutate(mmr = ifelse(mmr >= 1000, 1000, mmr), 
         mmr = ifelse(mmr <= 10, 10, mmr),
         parl.f = ifelse(parl.f == 0, 0.01, parl.f),
         parl.m = 1 - parl.f)

# Add agegrated health, empowerment and LFPR dimensions

gii.master <- gii.prep %>% 
  mutate(health = ((10/mmr  * 1/abr)^1/2 + 1)/2,
         empower = ((parl.f*edu.f)^(1/2) + (parl.m*edu.m)^(1/2))/2,
         lfpr = (flfp + mlfp)/2,
         gf = ((10/mmr * 1/abr)^(1/2) * (parl.f*edu.f)^(1/2) * flfp)^(1/3),
         gm = ((parl.m*edu.m)^(1/2) * mlfp)^(1/3),
         gfm = (health * empower * lfpr)^(1/3),
         harm = 1/((1/gf + 1/gm)/2),
         gii = 1 - harm/gfm, 
         gii = ifelse(gii < 0, 0.01, gii)) %>% 
  drop_na(gii)

# Even though the UNDP gives data for 2018 as well, they use the maternal mortality ratio from 2015,
# and our analysis stops at 2017 because that's the latest for the MMR data

# Graphs for validation of the replicated index vs the original

validation <- gii.master %>% 
  left_join(gii.hist, by = c('countrycode', 'year')) %>% 
  arrange(countrycode, desc(countrycode))

s1 <- c(unique(validation$countrycode)[1:30])
s2 <- c(unique(validation$countrycode)[31:60])
s3 <- c(unique(validation$countrycode)[61:90])
s4 <- c(unique(validation$countrycode)[91:120])
s5 <- c(unique(validation$countrycode)[121:150])
s6 <- c(unique(validation$countrycode)[151:170])

ggplot(validation %>% filter(countrycode %in% s6)) + 
  geom_line(aes(year, gii.hist, color = 'UNDP')) +
  geom_line(aes(year, gii, color = 'Replication')) + 
  labs(title = 'Gender Inequality Index replication', color = 'Index version',  y = "GII", x = "Year") +
  facet_wrap(~countrycode) +
  theme(axis.text.x = element_text(angle = 90), legend.position = 'bottom') +
  ylim(0, 1) + 
  scale_color_manual(values=c("#4D9221", "#8E0152"))


#write.csv(gii.master, '/Users/marinaandrijevic/PhD/Gender equality projections/Data/Output/gii_replication.csv')


# Comparison with the CLIMI index and vulnerability index (Fig 1):

climi <- read.csv('climi.csv')

vuln <- read.csv('ndgain_vulnerability.csv') %>% 
  rename_all(tolower) %>% 
  gather(year, vulnerability, -name, -iso3) %>% 
  mutate(year = year %>% str_replace("x", "") %>% as.numeric) %>% 
  rename(countrycode = iso3)

comp <- gii.master %>%
  filter(year %in% 2007:2015) %>% # Gender inequality (2007 - 2015 average), to match the Climate Action Index
  group_by(countrycode) %>% 
  mutate(gii.avg = mean(gii)) %>% 
  ungroup() %>% 
  filter(year == 2010) %>% 
  select(-year) %>% 
  left_join(climi, by = 'countrycode') 

comp2 <- gii.master %>%
  left_join(vuln, by = c('countrycode', 'year')) 


f1 <- y ~ x

ggplot(comp, aes(x = gii.avg, y = climi) ) +
  geom_point() +
  geom_smooth(method = 'lm', level = 0.95, formula = f1) +
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~~")),
               parse = TRUE) +
  labs(y = "Climate action", x = "Gender inequality") + 
  theme_minimal() +
  theme(text = element_text(size=16),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  xlim(0, 0.7)


ggplot(comp2 %>% filter(year == 2017), aes(x = gii, y = vulnerability)) +  # Latest availbale years for the two indices: 2017
  geom_point() +
  geom_smooth(method = 'lm', level = 0.95, color = 'orange', formula = f1) +
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~~")),
               parse = TRUE) +
  labs(y = "Vulnerability", x = "Gender inequality") + 
  theme_minimal() +
  theme(text = element_text(size=16),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  xlim(0, 0.7)

cor(comp$climi, comp$gii.avg, use = 'complete.obs')

ggplot(comp) +
  geom_point(aes(climi, gii.avg)) +
  geom_smooth(aes(climi, gii.avg), method = lm, formula = y ~ splines::bs(x, 3)) +
  labs(y = "Gender Inequality Index (2007-2015 average)", x = "Index of Climate Action", title = 'Less gender inequality -> more climate action?' )


# Sensitivity analysis with other gender (in)equality indicators (Supplementary Material)

comp <- gii.hist %>% filter(year == 2016 | year == 2017) %>% select(countrycode, year, gii.hist)

# GDI Index

gdi <- read.csv('gender_development_index_obs.csv') %>% 
  left_join(gii.hist, by = c('countrycode', 'year')) %>% 
  #left_join(soceco, by = c('countrycode', 'year')) %>% 
  #mutate(mys.gap = ifelse(mys.gap < 1, 0, mys.gap)) %>% 
  #mutate(scenario = 'Observed') %>% 
  drop_na(gdi) %>% 
  mutate(gdi.log = log(gdi/(1-gdi))) %>% 
  filter(year == 2017 & gdi > 0.7) # Excludes two biggest ouliers


p1 <- ggscatter(gdi, x = 'gdi', y = 'gii.hist',
          add = "reg.line", 
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE,
          title = 'GDI vs GII (2017)', xlab = 'Gender Development Index', ylab = 'Gender Inequality Index') +
  stat_cor(method = "pearson", label.y = 0.9, label.x = 0.8) +
  scale_y_continuous(limits = c(0,1))

# WPS Index

wps16 <- read_excel('wps_2016_19.xlsx', sheet = '2016') %>% 
  mutate(year = 2016)

wps <- read_excel('wps_2016_19.xlsx', sheet = '2019') %>% 
  mutate(year = 2017) %>% 
  bind_rows(wps16) %>% 
  mutate(countrycode = countrycode(country, 'country.name', 'iso3c', custom_match = custom.match)) %>% 
  left_join(comp, by = c('countrycode', 'year')) %>% 
  #left_join(soceco, by = c('countrycode', 'year')) %>% 
  #mutate(mys.gap = ifelse(mys.gap < 1, 0, mys.gap)) %>% 
  #mutate(scenario = 'Observed') %>% 
  drop_na(wps) %>% 
  mutate(wps.log = log(wps/(1-wps))) %>% 
  filter(year == 2017)

p2 <- ggscatter(wps, x = 'wps', y = 'gii.hist',
          add = "reg.line", 
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE,
          title = 'WPS Index vs GII (2017)', xlab = 'Women, Peace and Security Index', ylab = 'Gender Inequality Index') +
  stat_cor(method = "pearson", label.y = 0.9, label.x = 0.6) +
  scale_y_continuous(limits = c(0,1))


# Global Gender Gap Index

gg.wef <- read_excel('gggi_wef.xlsx') %>%
  as.data.frame() %>% 
  gather(year, gg.wef, -Country, -Region) %>% 
  mutate(gg.wef = gg.wef %>% as.numeric(),
         year = year %>% as.integer(),
         countrycode = countrycode(Country, 'country.name', 'iso3c')) %>% 
  left_join(gii.hist, by = c('countrycode', 'year')) %>% 
  #left_join(soceco, by = c('countrycode', 'year')) %>% 
  #mutate(mys.gap = ifelse(mys.gap < 1, 0, mys.gap)) %>% 
  #mutate(scenario = 'Observed') %>% 
  drop_na(gg.wef) %>% 
  mutate(gg.wef.log = log(gg.wef/(1-gg.wef))) %>% 
  filter(year == 2017)

p3 <- ggscatter(gg.wef, x = 'gg.wef', y = 'gii.hist',
          add = "reg.line", 
          add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE,
          title = 'WEF Gender Gap vs GII (2017)', 
          xlab = 'Gender Gap (World Economic Forum)', 
          ylab = 'Gender Inequality Index') +
  stat_cor(method = "pearson", label.y = 0.9, label.x = 0.6) +
  scale_y_continuous(limits = c(0,1))

grid.arrange(p1, p2, p3, nrow = 1, top = 'Comparison with other indices of gender equality')


