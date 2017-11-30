

library(rworldmap)

rm(list = ls())

# heatmaps

idea_global <- read.csv("database_full.csv")

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

n1 <- joinCountryData2Map(idea_global_2015, joinCode="ISO3", nameJoinColumn= "iso3c")
map1 <- mapCountryData(n1, nameColumnToPlot = "idea_pct", mapTitle = "PFR Index 2015", colourPalette = "heat", catMethod = "pretty")


# heatmap latinoamerica
n2 <- joinCountryData2Map(idea_global_2015, joinCode="ISO3", nameJoinColumn="iso3c")
mapCountryData(n2, nameColumnToPlot="idea_pct", mapTitle="Political Finance Regulation Index 2015 (Latin America)", mapRegion="latin america", colourPalette = "heat")










