

library(plotly)
library("RColorBrewer")



setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis2017/3) FIGURES & ANALYSiS")
rm(list = ls())




# SCATTER

# latin american sample
countries <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "GTM", 
               "HND", "MEX", "NIC", "PAN", "PER", "PRY", "SLV", "URY", "VEN")

# import database excel
idea_excel <- read.csv("database_old_excel.csv")

# select variables
idea_excel <- idea_excel %>% 
  rename(iso3c = Ticker, year_panel = Year) %>% 
  select(iso3c, year_panel, blpi_pct, pf_pct, rs_pct, os_pct, idea_pct, CoC, un_region_name)

# replace CoC NAs
idea_excel <- idea_excel %>% 
  group_by(iso3c) %>% 
  mutate(CoC_01 = na.locf(CoC, na.rm = F))

# filter
idea_excel <- idea_excel %>% 
  filter(iso3c %in% countries) %>% 
  filter(year_panel >= 2006 & year_panel <= 2015)

# import dataframes latin america new database (includes controls)
idea_R <- read.csv("database.csv")

# merge datasets latin america
df <- full_join(idea_excel, idea_R, by = c("iso3c", "year_panel"))

# CoC is recoded
df$CoC_recoded <- df$CoC + 2.5
df$CoC_recoded <- df$CoC_recoded * 2

# Index is recoded idea_pct
df$idea_pct_recoded <- df$idea_pct * 100




# Scatterplot 2015
df2015 <- df[(df$year_panel==2015),]

write.csv(df2015, "figure_xii.csv")

# Scatterplot 2006
df2006 <- df[(df$year_panel== 2006),]

write.csv(df2006, "figure_xi.csv")

