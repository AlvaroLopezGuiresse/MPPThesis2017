

library(dplyr)
library(plm)
library(stargazer)



# time series plots


setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis/3) FIGURES & ANALYSiS")
rm(list = ls())


idea_excel <- read.csv("database_old_excel.csv")
idea_R <- read.csv("database_new_R.csv")


# latin american sample
countries <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "GTM", 
               "HND", "MEX", "NIC", "PAN", "PER", "PRY", "SLV", "URY", "VEN")


idea_excel <- idea_excel %>% 
  rename(iso3c = Ticker, year_panel = Year) %>% 
  select(iso3c, year_panel, blpi_pct, pf_pct, rs_pct, os_pct, idea_pct, CoC, un_region_name)

idea_excel <- idea_excel %>% 
  group_by(iso3c) %>% 
  mutate(CoC_01 = na.locf(CoC, na.rm = F))

idea_excel <- idea_excel %>%  
  filter(!iso3c == "TWN")
  
# sample latin america
idea_excel <- idea_excel %>% 
  filter(iso3c %in% countries) %>% 
  filter(year_panel >= 2006 & year_panel <= 2015)
  
df <- full_join(idea_excel, idea_R, by = c("iso3c", "year_panel"))



# linear regressions
m1 <- lm(CoC ~ idea_pct, data = df)
summary(m1)

m2 <- lm(CoC ~ wefji, data = df)
summary(m2)

m3 <- plm(CoC ~ p_expenditure, data = df)
summary(m3)

m4 <- plm(CoC ~ idea_pct, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m4)

m5 <- plm(CoC ~ wefji, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m5)

m6 <- plm(CoC ~ p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m6)

m7 <- plm(CoC ~ idea_pct, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m7)

m8 <- plm(CoC ~ wefji, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m8)

m9 <- plm(CoC ~ p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m9)



m10 <- plm(CoC ~ idea_pct + wefji + p_expenditure + lifeexpectancy + ruralpop +
            wefji:idea_pct + wefji:p_expenditure, data = df, 
          index= c("iso3c", "year_panel"), model = "within")
summary(m10)

m11 <- plm(CoC ~ idea_pct + wefji + p_expenditure + lifeexpectancy + ruralpop +
            wefji:idea_pct + wefji:p_expenditure, data = df, 
          index= c("iso3c", "year_panel"), model = "random")
summary(m11)











