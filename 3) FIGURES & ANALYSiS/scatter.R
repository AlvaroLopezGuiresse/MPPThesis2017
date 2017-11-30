

setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis/3) FIGURES & ANALYSiS")
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
idea_R <- read.csv("database_new_R.csv")

# merge datasets latin america
df <- full_join(idea_excel, idea_R, by = c("iso3c", "year_panel"))

# CoC is recoded
df$CoC_recoded <- df$CoC + 2.5
df$CoC_recoded <- df$CoC_recoded * 2

# idea_pct
df$idea_pct_recoded <- df$idea_pct * 100




# Scatterplot 2015
# Plotly
df2015 <- df[(df$year_panel==2015),]

p <- plot_ly(df2015, x = ~idea_pct, y = ~CoC_recoded, color = ~wefji, marker = list(size = 30),
             text= ~iso3c, type='scatter', mode= 'markers', 
             title="Effect of Political Finance Regulation on Control of Corruption 2006") %>% 
  add_annotations(x = df2015$idea_pct,
                  y = df2015$CoC_recoded,
                  text = rownames(df2015$iso3c),
                  font = list(color = 'white'),
                  xref = "x",
                  yref = "y",
                  showarrow = F,
                  ax = 20,
                  ay = -40) %>%
  layout(title ='Party Finance Reform and Control of Corruption 2015', 
         xaxis = list(title='Political Finance Regulation', range = c(0, 1)), 
         yaxis = list(title='Control of Corruption', range = c(0, 10)), autosize = T)
p



# Crear Scatterplot con datos de 2006
df2006 <- df[(df$year_panel== 2006),]

AnalisisAL2006 <- AnalisisAL2006[!(AnalisisAL2006$country=="Haiti"),]

p2006 <- plot_ly(AnalisisAL2006, x = ~idea_pct, y = ~CoCRecoded, color = ~wefji, marker = list(size = 30),
                 text= ~iso3c, type='scatter', mode= 'markers', title="Effect of Political Finance Regulation on Control of Corruption 2006") %>% 
  add_annotations(x = AnalisisAL2006$idea_pct,
                  y = AnalisisAL2006$CoCRecoded,
                  text = rownames(AnalisisAL2006$iso3c),
                  xref = "x",
                  yref = "y",
                  showarrow = F,
                  font = list(color = 'white'),
                  arrowhead = 4,
                  arrowsize = .5,
                  ax = 20,
                  ay = -40) %>% 
  layout(title ='Party Finance Reform and Control of Corruption 2006', 
         xaxis = list(title='Political Finance Regulation', range = c(0, 1)), 
         yaxis = list(title='Control of Corruption', range = c(0, 10)), autosize = T)

p2006


# scatter con ggplot 2015
p <- ggplot(df2015, aes(x = idea_pct_recoded, y = CoC_recoded, colour = wefji)) +
  geom_point(size = 10)
