

library(rworldmap)


setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis2017/3) FIGURES & ANALYSiS")

rm(list = ls())

# heatmaps

idea_global <- read.csv("database_old_excel.csv")

idea_global <- idea_global %>% 
  rename(iso3c = Ticker, year_panel = Year) %>% 
  select(iso3c, year_panel, blpi_pct, pf_pct, rs_pct, os_pct, idea_pct, CoC, un_region_name)

idea_global$iso3c <- idea_global$iso3c %>%
  as.character()

idea_global <- idea_global %>% 
  group_by(iso3c) %>% 
  mutate(CoC_01 = na.locf(CoC, na.rm = F))

idea_global$idea_pct <- idea_global$idea_pct * 100

idea_global_2015 <- idea_global %>% 
  filter(year_panel == 2015) %>% 
  select(iso3c, idea_pct) %>% 
  as.data.frame()

write.csv(idea_global_2015, "figure_ii_vi.csv")







