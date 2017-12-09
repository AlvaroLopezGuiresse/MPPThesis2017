
library(tidyr)
library(dplyr)
library(xlsx)
library(countrycode)
library(WDI)
library(zoo)



# input: output_panel_pfr_index.csv from script 3_PFRI_index_calculation.R.
# input_eclac_public_expenditure.csv, input_wef_judicial_independence.xlsx,
# output: output_regressors_database.csv




rm(list = ls())
cat("\014")


# years and countries.
years <- c(2006:2015)

countries <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "GTM", 
               "HND", "MEX", "NIC", "PAN", "PER", "PRY", "SLV", "URY", "VEN")


# WEF JUDICIAL INDEPENDENCE.
############################
setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis2017/2) DATABASE")
wefji <- read.xlsx("input_wef_judicial_independence.xlsx", 1)

# gather to long format.
t_wefji <- gather(wefji, key = country, value = wefji, Albania:Zimbabwe)

# iso3c.
t_wefji$iso3c <- countrycode(t_wefji$country, 'country.name', 
                                'iso3c', warn = TRUE)

wefji <- t_wefji %>% 
  filter(iso3c %in% countries) 





# WORLD BANK.
#############
wb <- WDI(country = 'all', start = '1996', end = '2016', 
                 indicator = c('SI.POV.GINI', 'SP.DYN.LE00.IN', 'NY.GDP.MKTP.CD', 
                               'ny.gdp.totl.rt.zs', 'SP.RUR.TOTL.ZS', 'SE.TER.ENRR'), extra =TRUE)

# select data for the sample
wb <- wb %>% filter(year %in% years) %>% 
  filter(iso3c %in% countries)

colnames(wb)[4] <- "gini"
colnames(wb)[5] <- "lifeexpectancy"
colnames(wb)[6] <- "gdp"
colnames(wb)[7] <- "natres"
colnames(wb)[8] <- "ruralpop"
colnames(wb)[9] <- "tertiaryschool"
colnames(wb)[2] <- "country.wb"

wb <- wb %>% 
  dplyr::select(iso3c, year, gdp, gini, lifeexpectancy, natres, ruralpop, tertiaryschool)



# ECLAC
########
p_expenditure <- read.csv("input_eclac_public_expenditure.csv")
colnames(p_expenditure)[8:27] <- c(1996:2015)
p_expenditure <- p_expenditure[,-(2:7)] 
p_expenditure <- gather(p_expenditure, year, value, -country)

p_expenditure$iso3c <- countrycode(p_expenditure$country, 'country.name', 'iso3c', warn = TRUE)
p_expenditure$iso3c[p_expenditure$country=="REP. DOMINICANA"] <- "DOM"

p_expenditure <- p_expenditure %>% 
  filter(year %in% years) %>% 
  filter(iso3c %in% countries) %>% 
  dplyr::select(iso3c, year, value) %>% 
  rename(p_expenditure = value)

p_expenditure$year <- p_expenditure$year %>% 
  as.numeric()



# merge datasets.
#################

df <- full_join(wefji, wb, by = c("iso3c", "year"))
df <- full_join(df, p_expenditure, by = c("iso3c", "year"))
df <- df %>% 
  rename(year_panel = year)


# import PFR Index.
pfr_index_1925_2016 <- read.csv("output_panel_pfr_index.csv")

# arrange dataset.
pfr_index <- pfr_index_1925_2016 %>% 
  filter(year_panel %in% years)

# merge datasets.
df_01 <- full_join(df, pfr_index, by = c("iso3c", "year_panel"))

df_01 <- df_01 %>% 
  group_by(iso3c) %>% 
  mutate_at(vars(OS, PF, PI, RS, PFR_index), funs(zoo::na.locf(., na.rm = F))) 


setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis2017/3) FIGURES & ANALYSiS")
write.csv(df_01, "database.csv")





