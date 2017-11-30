library(zoo)


library(WDI)
library(countrycode)
library(xlsx)
library(repmis)
library(tidyr)
library(WDI)
library(XML)
library(plyr)
library(plm)
library(stargazer)
library(ggplot2)
library(plotly)
library(plm)
library(googleVis)
library(reshape2)
library(foreign)
library(calibrate)
library(rworldmap)
library(magrittr)



setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis/3) FIGURES & ANALYSiS")
rm(list = ls())

# import dataframes global
idea_global <- read.csv("database_full.csv")

idea_global <- idea_global %>% 
  rename(iso3c = Ticker, year_panel = Year) %>% 
  select(iso3c, year_panel, blpi_pct, pf_pct, rs_pct, os_pct, idea_pct, CoC, un_region_name)

#... no corre el grupo
idea_global <- idea_global %>% 
  group_by(iso3c) %>% 
mutate(CoC_01 = na.locf(CoC, na.rm = F))

# import dataframes latin america
idea <- read.csv("PFR_Index_excel.csv")
database <- read.csv("database.csv")

# merge datasets latin america
df <- full_join(database, idea, by = c("iso3c", "year_panel"))

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



##### Heatmap global

AnalisisRCompleta <- read.csv("AnalisisRCompleta.csv")

AnalisisRCompleta <- AnalisisRCompleta[,-(1)] 

AnalisisRCompleta$IDEARecoded <- AnalisisRCompleta$idea_pct * 100

AnalisisR2015 <- AnalisisRCompleta[(AnalisisRCompleta$year=="2015"),]

n <- joinCountryData2Map(AnalisisR2015, joinCode="ISO3", nameJoinColumn="iso3c")

mapCountryData(n, nameColumnToPlot="idea_pct", mapTitle="PFR Index 2015", colourPalette = 'heat', catMethod = "pretty", mapResolution = 'coarse')

##### Heatmap latinoamerica


n2 <- joinCountryData2Map(AnalisisAL2015, joinCode="ISO3", nameJoinColumn="iso3c")

mapCountryData(n2, nameColumnToPlot="idea_pct", mapTitle="Political Finance Regulation Index 2015 (Latin America)", mapRegion="latin america", colourPalette = "heat")


######Transformar gastos de cepal a numeros absolutos

AnalisisAL$CepalGastosAbs <- AnalisisAL$CepalGastos * AnalisisAL$GDP

AnalisisAL$CepalGastosAbs <- AnalisisAL$CepalGastosAbs / 100

AnalisisAL$CepalGastosAbs <- AnalisisAL$CepalGastosAbs / 1000000000

######Subset de Freedom House

AnalisisRCompletaWEF_fh <- AnalisisRCompletaWEF


####Regresion en r

AnalisisRCompletaWEF <- read.csv('AnalisisRCompletaWEF.csv')

m1 <- plm(CoC ~ idea_pct + natres + lifeexpectancy + wefji + wefji:natres + wefji:idea_pct, data=AnalisisRCompletaWEF, index= c('iso3c', 'year'), model = 'within')
summary(m1)

m2 <- plm(CoC ~ idea_pct + lifeexpectancy + natres + wefji:natres + wefji:idea_pct, data=AnalisisRCompletaWEF, index= c('iso3c', 'year'), model = 'random')
summary(m2)

m3 <- plm(CoC ~ idea_pct*wefji+ lifeexpectancy + CepalGastosAbs*wefji + ruralpop, data=AnalisisAL, index= c('iso3c', 'year'), model = 'within')
summary(m3)

stargazer(m3)

m4 <- plm(CoC ~ idea_pct*wefji+ lifeexpectancy + CepalGastosAbs*wefji + ruralpop, data=AnalisisAL, index= c('iso3c', 'year'), model = 'within')
summary(m4)


wefji <- read.csv('WEFJudicialIndependence.csv')
