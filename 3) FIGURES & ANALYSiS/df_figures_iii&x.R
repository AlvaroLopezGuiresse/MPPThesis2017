
library(dplyr)
library(tidyr)
library(ggthemes)
library(scales)
library(zoo)




setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis2017/3) FIGURES & ANALYSiS")
rm(list = ls())

# import Excel database
idea_excel <- read.csv("database_old_excel.csv")

idea_excel <- idea_excel %>% 
  rename(iso3c = Ticker, year_panel = Year) %>% 
  select(iso3c, year_panel, blpi_pct, pf_pct, rs_pct, os_pct, idea_pct, CoC, un_region_name)

idea_excel <- idea_excel %>% 
  group_by(iso3c) %>% 
  mutate(CoC_01 = na.locf(CoC, na.rm = F))
 
idea_excel <- idea_excel %>%  
  filter(!iso3c == "TWN")

# import idea R
idea_R <- read.csv("database.csv")

# latin american sample
countries <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "GTM", 
               "HND", "MEX", "NIC", "PAN", "PER", "PRY", "SLV", "URY", "VEN")










# Figure III. PFR Index Time Series by Regions, 1996-2015: America, Europe, Arica, Oceania, and Asia
df <- idea_excel %>% 
  group_by(un_region_name, year_panel) %>% 
  summarise(pfr = sum(idea_pct) / n())

write.csv(df, "figure_iii.csv")





# Figure IV. PFR Index and CoC Indicator time series, 1996-2015
df <- idea_excel %>% 
  group_by(year_panel) %>% 
  summarise(pfr = sum(idea_pct)/n(), coc = sum(CoC_01, na.rm = T)/n()) %>% 
  mutate(coc = coc * 2 + 5)

write.csv(df, "figure_iv.csv")





# Figure V. PFR Sub-Index Level by region, 1996 and 2015. 
# BLPI: Bans and limits on private income, 
# PF: Public funding, 
# RS: Regulation on spending, OS: Oversight and sanctions.
df <- idea_excel %>% 
  gather(index_type, index, blpi_pct:os_pct) %>% 
  filter(year_panel == 1996|year_panel == 2015)
  
df <- df %>% 
  group_by(year_panel, un_region_name, index_type) %>% 
  summarise(pfr = mean(index), coc = mean(CoC_01, na.rm = T))

df$year_panel <- df$year_panel %>% as.factor()

write.csv(df, "figure_v.csv")





# Figure VII. PFR Sub-index Time Series in Latin America, 1996 and 2015. 
# BLPI: Bans and limits on private income, PF: Public funding, RS: Regulation on spending, 
# OS: Oversight and sanctions

df <- idea_excel %>% 
  gather(index_type, index, blpi_pct:idea_pct) %>% 
  filter(iso3c %in% countries) %>% 
  group_by(year_panel, index_type) %>% 
  summarise(pfr = mean(index))

write.csv(df, "figure_vii.csv")





# Figure VIII. PFR Index Level in Latin America, 2006 and 2015
df <- idea_excel %>% 
  gather(index_type, index, blpi_pct:idea_pct) %>% 
  filter(iso3c %in% countries) %>% 
  group_by(year_panel,iso3c, index_type) %>% 
  filter(year_panel == 2006 | year_panel == 2015) %>% 
  summarise(pfr = mean(index)) %>% 
  filter(index_type == "idea_pct")

df$year_panel <- df$year_panel %>% 
  as.factor()

write.csv(df, "figure_viii.csv")





# Figure IX. Mean PFR Index and CoC Indicator series in Latin America, 2006-2015
df <- idea_excel %>% 
  filter(iso3c %in% countries) %>% 
  group_by(year_panel) %>% 
  summarise(pfr = mean(idea_pct), coc = mean(CoC_01, na.rm = T)) %>% 
  mutate(coc = coc * 2 + 5)

write.csv(df, "figure_ix.csv")





# Figure X. PFR Index and World Bank Control of Corruption Change in Latin America, 2006-2015
# como hacer para las dos en simultaneo...
df_06 <- idea_excel %>% 
  filter(iso3c %in% countries) %>% 
  filter(year_panel == 2006 | year_panel == 2015) %>% 
  group_by(iso3c, year_panel) %>% 
  select(iso3c, year_panel, idea_pct) %>% 
  spread(year_panel, idea_pct) %>% 
  mutate(idea_diff = `2015` - `2006`) %>% 
  gather(year_panel, idea_pct, `2006`, `2015`) %>% 
  select(iso3c, year_panel, idea_diff) %>% 
  filter(year_panel == 2015) %>% 
  select(-year_panel)
 

df_07 <- idea_excel %>% 
  filter(iso3c %in% countries) %>% 
  filter(year_panel == 2006 | year_panel == 2015) %>% 
  group_by(iso3c, year_panel) %>% 
  select(iso3c, year_panel, CoC_01) %>% 
  spread(year_panel, CoC_01) %>% 
  mutate(coc_diff = `2015` - `2006`) %>% 
  gather(year_panel, CoC_01, `2006`, `2015`) %>% 
  select(iso3c, year_panel, coc_diff) %>% 
  filter(year_panel == 2015) %>% 
  select(-year_panel)


df <- full_join(df_06, df_07, by = c("iso3c"))
df <- df %>% 
  gather(key, value, -(iso3c))

write.csv(df, "figure_x.csv")

#
















