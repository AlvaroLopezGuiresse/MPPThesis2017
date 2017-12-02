
library(dplyr)
library(ggthemes)
library(scales)

# time series plots


setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis2017/3) FIGURES & ANALYSiS")
rm(list = ls())

# import IDEA Excel
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
df_01 <- idea_excel %>% 
  group_by(un_region_name, year_panel) %>% 
  summarise(pfr = sum(idea_pct) / n())

p <- ggplot(df_01, aes(x = year_panel, y = pfr, color = un_region_name)) +
  geom_line() +
  theme_hc() +
  scale_colour_hc()
p



# Figure IV. PFR Index and CoC Indicator time series, 1996-2015
df_02 <- idea_excel %>% 
  group_by(year_panel) %>% 
  summarise(pfr = sum(idea_pct)/n(), coc = sum(CoC_01, na.rm = T)/n())

p <- ggplot(df_02, aes(x = year_panel, y = pfr, color = un_region_name)) +
  geom_line()
p

# Figure V. PFR Sub-Index Level by region, 1996 and 2015. 
# BLPI: Bans and limits on private income, PF: Public funding, 
# RS: Regulation on spending, OS: Oversight and sanctions.
df_03 <- idea_excel %>% 
  gather(index_type, index, blpi_pct:idea_pct) %>% 
  filter(year_panel == 1996|year_panel == 2015)

df_03 <- df_03 %>% 
  group_by(year_panel, un_region_name) %>% 
  summarise(pfr = mean(index), coc = mean(CoC_01, na.rm = T))

p <- ggplot(df_03, aes(x = year_panel, y = pfr, color = un_region_name)) +
  geom_bar()
p



# Figure VII. PFR Sub-index Time Series in Latin America, 1996 and 2015. 
# BLPI: Bans and limits on private income, PF: Public funding, RS: Regulation on spending, 
# OS: Oversight and sanctions

df_04 <- idea_excel %>% 
  gather(index_type, index, blpi_pct:idea_pct) %>% 
  filter(iso3c %in% countries) %>% 
  group_by(year_panel, index_type) %>% 
  summarise(pfr = mean(index))

p <- ggplot(df_04, aes(x = year_panel, y = pfr, color = index_type )) +
  geom_line()
p


# Figure VIII. PFR Index Level in Latin America, 2006 and 2015
df_04 <- idea_excel %>% 
  gather(index_type, index, blpi_pct:idea_pct) %>% 
  #filter(iso3c %in% countries) %>% 
  group_by(year_panel, un_region_name, index_type) %>% 
  filter(year_panel == 2006 | year_panel == 2015) %>% 
  summarise(pfr = mean(index)) %>% 
  filter(!index_type == "idea_pct")


# Figure IX. Mean PFR Index and CoC Indicator series in Latin America, 2006-2015
df_05 <- idea_excel %>% 
  filter(iso3c %in% countries) %>% 
  group_by(year_panel) %>% 
  summarise(pfr = mean(idea_pct), coc = mean(CoC_01, na.rm = T))

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


df_08 <- full_join(df_06, df_07, by = c("iso3c"))

 



#
















