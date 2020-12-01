# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # #  Gender Equality Extension of SSPs  # # # # # # # # # # 
# # # # # # #              Projections            # # # # # # # # # #
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
library(stargazer)
library(RColorBrewer)
library(colorspace)

setwd('/Users/marinaandrijevic/Documents/GitHub/gender_inequality_SSPs/')

# Load replicated GII

gii.master <- read.csv('/Users/marinaandrijevic/Documents/GitHub/gender_inequality_SSPs/Output/gii_replication.csv')

# Current range of the OECD countries (for cut-off values on figure legends)

gii.current <- gii.master %>% filter(year == 2017)

oecd <- gii.current %>% 
  filter(countrycode %in% c("AUS", "AUT", "BEL", "CAN", "CHL", "CZE","DNK","EST","FIN","FRA","DEU","GRC",
                            "HUN", "ISL", "IRL", "ITA", "JPN", "KOR", "LVA", "LUX", "MEX", "NLD", "NZL", 
                            "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR","USA")) 
summary(oecd)

# The range for OECD countries is GII 0.002 - 0.315; mean for all countries in the sample is 0.33, median 0.38

# # # Regression analysis

# Add covariates to the dataset: GDP, higher education, gap in mean years of schooling

gdp <- read.dta13('Input/pwt91.dta') %>% 
  select(countrycode:pop) %>% 
  mutate(lngdppc = log(rgdpe/pop), 
         lngdppcsq = lngdppc^2)

soceco <- read.csv('Input/educ_vars_ipol.csv') %>%  # Education and governance data (previously assembled)
  select(countrycode, year, post.secondary, mys.gap) %>% 
  left_join(gdp, by = c('countrycode', 'year')) %>% 
  filter(year %in% (1995:2018))

gii.reg <- gii.master %>% 
  left_join(soceco, by = c('countrycode', 'year')) %>% 
  mutate(mys.gap = ifelse(mys.gap < 1, 0, mys.gap)) %>% 
  mutate(scenario = 'Observed') %>% 
  select(countrycode, year, gii, lngdppc, post.secondary, mys.gap) %>% 
  na.omit() %>% 
  mutate(gii.log = log(gii/(1-gii)))

# Main regression specification (see Supplementary Material for more)

reg.mod <- lm(gii.log ~ lngdppc + mys.gap + post.secondary + factor(countrycode), data = gii.reg)
summary(reg.mod)


# Projections with logistic transformation 

# Out of sample data: 

newcntry <- c('BRB', 'BRN', 'CUB', 'DJI', 'ERI', 'GRD', 'GUY', 'HKG', 'LBY', 'MRT', 'PNG', 'PRI', 'SLB', 'SOM', 'TLS', 'TON', 'UZB', 'VUT', 'WSM')

df.prep <- data.frame(year = rep(2015,5), scenario = c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5'))

proj.prep <- read.csv('Input/gdp_edu_projections.csv') %>% 
  select(countrycode, year, scenario, gdppc, postsec, mys.gap) %>%
  filter(year == 2015 & scenario == 'SSP2') %>% 
  select(-year, -scenario) %>% 
  merge(df.prep)

proj.dat <- read.csv('Input/gdp_edu_projections.csv') %>% 
  filter(!year == 2015) %>% 
  select(countrycode, year, scenario, gdppc, postsec, mys.gap) %>% 
  bind_rows(proj.prep) %>% 
  filter(!countrycode %in% newcntry) %>% 
  mutate(mys.gap = ifelse(mys.gap < 1, 0, mys.gap),
       lngdppc = log(gdppc)) %>% 
  rename(post.secondary = postsec) 

gii.proj.log <- proj.dat %>% 
  mutate(gii.log.proj = predict(reg.mod, newdata = proj.dat), # Use predict function to derive out-of-sample projections
         gii.proj = exp(gii.log.proj) / (1 + exp(gii.log.proj))) %>% # Transform back from log scale to 0-1
  drop_na(countrycode) 

gii.complete <- gii.master %>% 
  filter(year <= 2017) %>% 
  select(countrycode, year, gii) %>% 
  mutate(scenario = 'Observed') %>% 
  drop_na(gii) %>% 
  bind_rows(gii.proj.log %>% filter(year >= 2015) %>% select(countrycode, scenario, year, gii.proj) %>% rename(gii = gii.proj))

#write.csv(gii.complete, 'Output/gii_obs_proj.csv')

sample <- sample(gii.complete$countrycode, size = 12)

sample1 <- c('NOR', 'SWE', 'DEU', 'GBR', 'ITA')

ggplot() + 
  geom_line(data = gii.complete %>% filter(countrycode %in% sample1 & year <= 2018), aes(year, gii, color = 'Observed'), size = 1) +
  geom_line(data = gii.complete %>% filter(countrycode %in% sample1 & year >= 2015), aes(year, gii, color = scenario)) +
  facet_wrap(~countrycode) + 
  labs(title = 'GII with log transformation') +
  ylim(0, 1)


# Average by region

regions.developing <- read.csv('Input/ccodes_regions.csv') %>% 
  mutate(region = recode(region, 'LDC' = 'LDCs'))

regions.developed <- read.csv('Input/regions.csv') %>% 
  filter(region %in% c('North America', 'Europe', 'East Asia & Pacific')) %>% 
  mutate(region = recode(region, 'North America' = "Europe & North America", "Europe" = "Europe & North America"))

regions <- regions.developing %>% 
  bind_rows(regions.developed)

gii.region <- gii.complete %>% 
  left_join(regions, by = 'countrycode') %>% 
  group_by(region, year, scenario) %>%
  mutate(gii.mean = mean(gii)) %>% 
  ungroup() %>% 
  drop_na(region)


# Main regional plot (Fig 3)

gii.region$scenario <- factor(gii.region$scenario, c("Observed", "SSP5", "SSP2", "SSP3", "SSP4", "SSP1"))

ggplot() +
  geom_point(data = gii.region %>%  filter(year <= 2015 & scenario == 'Observed' & !region == 'LDCs'), aes(year, gii.mean, color = scenario), size = 1) +
  geom_line(data = gii.region %>%  filter(year >= 2015 & !region == 'LDCs'), aes(year, gii.mean, color = scenario), size = 1) +
  #geom_ribbon(data = gii.region %>%  filter(year > 2015), aes(year, ymin= upp, ymax=low, color = scenario), alpha=0.2) +
  facet_wrap(~region) + 
  labs(y = 'Mean GII', x = 'Year', title = 'Gender Inequality Index (GII): regional projections', color = 'Scenario') +
  ylim(0, 0.70) +
  theme_minimal() +
  theme(legend.direction = 'horizontal', 
        legend.position = c(.725, .915),
        legend.background = element_rect(fill = '#f5f5f5', size = 0),
        legend.title = element_text(color = '#757575', size = 13),
        legend.text = element_text(color = '#757575', size = 10),
        #panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(colour = "grey", size=0.5),
        plot.title = element_text(color = '#757575'),
        axis.title.x = element_text(color = '#757575'),
        axis.title.y = element_text(color = '#757575'),
        text = element_text(size=15), 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous(breaks= c(2000, 2020, 2040, 2060, 2080, 2100)) +
  scale_color_manual(values=c("Observed" = "#05749E", "SSP1" = "#00BA38", "SSP2" = '#00BFC4', 
                              'SSP3' = '#FF8C00', 'SSP4' = "#F564E3", 'SSP5' = '#AED602'))



# LDCs

ldcs <- gii.region %>% filter(region == 'LDCs' & !countrycode %in% c('AGO', 'MMR'))

ggplot() +
  geom_line(data = ldcs %>%  filter(year <= 2015 & scenario == 'Observed'), aes(year, gii, color = scenario), size = 1) +
  geom_line(data = ldcs %>%  filter(year >= 2015), aes(year, gii, color = scenario)) +
  facet_wrap(~countrycode) + 
  labs(y = 'GII', x = 'Year', title = 'Gender Inequality Index (GII) projections for Least Developed Countries (LDCs)', color = 'Scenario') +
  ylim(0, 1) +
  theme(legend.position = 'bottom', text = element_text(size=12), axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values=c("Observed" = "#05749E", "SSP1" = "#00BA38", "SSP2" = '#00BFC4', 
                              'SSP3' = '#FF8C00', 'SSP4' = "#F564E3", 'SSP5' = '#AED602'))


# Stepwise regression to display in results (Supplementary Material)

library(sandwich)

m1 <- lm(gii.log ~ lngdppc + factor(countrycode), data = gii.reg)
cov1 <- vcovHC(m1, type = 'HC')
se1 <- sqrt(diag(cov1))

m2 <- lm(gii.log ~ lngdppc + mys.gap + factor(countrycode), data = gii.reg)
cov2 <- vcovHC(m2, type = 'HC')
se2 <- sqrt(diag(cov2))

m3 <- lm(gii.log ~ lngdppc + mys.gap + post.secondary + factor(countrycode), data = gii.reg)
cov3 <- vcovHC(m3, type = 'HC')
se3 <- sqrt(diag(cov3))

m4 <- lm(gii.log ~ lngdppc + mys.gap + post.secondary, data = gii.reg)
cov4 <- vcovHC(m4, type = 'HC')
se4 <- sqrt(diag(cov4))


stargazer(m1, m2, m3, m4, 
          se = list(se1, se2, se3, se4),
          title = 'Regression results: stepwise and fixed effects',
          column.labels = c('FE', 'FE', 'FE', 'OLS'),
          keep = c('lngdppc', 'mys.gap', 'post.secondary'),
          dep.var.labels = 'Gender Inequality Index',
          covariate.labels = c('GDP per capita', 'Gender gap in schooling', 'University education'),
          omit.stat = "f",
          align = TRUE)

# Maps (Figure 1)

library(viridis)
library(ggthemes)
library(maps)
library(rworldmap)

# Purple-green
brewer.pal(n = 10, name = "PRGn")
gii.palette2 <- c("#1B7837", "#5AAE61", "#E7D4E8", "#C2A5CF", "#9970AB", "#762A83", "#40004B")

# Select data 

ssp <- unique(gii.complete$scenario)

for(s in ssp){
dat <- gii.complete %>%
  filter(year == 2017) %>% # Choose the period and the scenario 
  rename(var = gii) # Choose the variable that is going to be shown on the map
map <- joinCountryData2Map(dat, joinCode = "ISO3", nameJoinColumn = "countrycode")
map_poly <-  fortify(map) %>% 
  merge(map@data, by.x="id", by.y="ADMIN", all.x=T) %>%
  arrange(id, order) %>% 
  mutate(var %>% as.numeric())
temp.map <- ggplot(map_poly, aes( x = long, y = lat, group = group)) +
  coord_map(projection = 'mollweide', xlim = c(-180, 180), ylim = c(-60, 75))  + # Remove antarctica
  geom_polygon(aes(fill = var)) +
  scale_fill_gradientn(colors = gii.palette2, breaks = c(0.3, 0.6, 0.9), limits = c(0,1)) +
  labs(fill = 'Value'
       ,title = dat$scenario   # Change the title of the map
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(family = 'Helvetica', color = 'gray40')
        ,plot.title = element_text(size = 18)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,axis.line = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = 'white')
        ,plot.background = element_rect(fill = 'white')
        ,legend.position = c(.08,.26)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = ''
           ,x = 18, y = -55
           ,size = 3
           ,family = 'Helvetica'
           ,color = 'gray50'
           ,hjust = 'left'
  )
ggsave(temp.map, file=paste0("map_", s ,".pdf"), width = 16, height = 10, units = "cm")
}


# Matching with population (SDG-related analysis for Fig 4)

gii.current <- gii.master %>% filter(year == 2017)

gii.relative <- gii.complete %>%
  filter(year %in% c(2017, 2020, 2030, 2050, 2100)) %>% 
  select(countrycode, year, scenario, gii) %>% 
  mutate(q = ifelse(gii <= 0.3, 'q1', 'q2'))

# Interpolate population size to get values for year 2017 (latest observed)

dpinterp <- function(df) {
  with(df, approx(x=indx, y=value, xout=min(indx):max(indx))) %>% 
    bind_cols()
}

pop.prep <- read.csv('Input/pop_size_fem_3.csv', skip = 8) %>% 
  rename_all(tolower) %>% 
  filter(scenario == 'SSP2' & year %in% c(2015:2020)) %>% 
  mutate(scenario = 'Observed')

pop.ipol <- pop.prep %>% 
  group_by(area, scenario, age, sex) %>% 
  rename(indx = year, value = population) %>% 
  do(dpinterp(.)) %>% 
  rename(year = x, population = y) %>% 
  right_join(pop.prep %>% distinct(area), by = 'area')

pop.size <-read.csv('Input/pop_size_fem_3.csv', skip = 8) %>% 
  rename_all(tolower) %>%
  filter(year == 2030) %>% 
  bind_rows(pop.ipol) %>% 
  mutate(countrycode = countrycode(area, 'country.name', 'iso3c')) %>% 
  drop_na(countrycode) %>% 
  filter(sex == 'Female') %>% 
  left_join(gii.relative, by = c('countrycode', 'year', 'scenario')) %>%
  drop_na(gii) %>% 
  group_by(year, scenario, age) %>% 
  mutate(girls.total = sum(population)) %>% 
  ungroup() %>% 
  group_by(year, scenario, age, q) %>% 
  mutate(girls.qs = sum(population)) %>% 
  ungroup() %>% 
  mutate(fract = girls.qs/girls.total * 100) %>% 
  group_by(scenario, year) %>% 
  arrange(desc(q)) %>%
  mutate(id = paste0(scenario, year, age, q)) %>% 
  ungroup() %>% 
  filter(!duplicated(id)) %>% 
  mutate(q = q %>% as.factor()) %>% 
  group_by(year, scenario, age) %>% 
  mutate(lab.ypos = cumsum(fract) - 0.5*fract) %>% 
  ungroup() %>% 
  mutate(scenpair = paste0(scenario, '_', year))


# For 0-14
ggplot(pop.size %>% filter(age == '0--14')) +
  geom_bar(aes(x = 2, y = fract, fill = q), stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(x = 2, y = lab.ypos, label = round(fract)), color = "white") +
  scale_fill_manual(values = c(q1 = '#deb7d3', q2 = '#b465a8')) +
  theme_void() + 
  xlim(0.5, 2.5) + 
  facet_wrap(~scenpair, nrow = 1)

# For 15+ 
ggplot(pop.size %>% filter(age == '15+')) +
  geom_bar(aes(x = 2, y = fract, fill = q), stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(x = 2, y = lab.ypos, label = round(fract)), color = "white") +
  scale_fill_manual(values = c(q1 = '#f5979f', q2 = '#a81e24')) +
  theme_void() + 
  xlim(0.5, 2.5) + 
  facet_wrap(~scenpair, nrow = 1) 
