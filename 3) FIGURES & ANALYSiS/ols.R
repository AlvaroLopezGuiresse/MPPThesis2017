

library(dplyr)
library(plm)
library(stargazer)



# time series plots


setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis2017/3) FIGURES & ANALYSiS")

idea_excel <- read.csv("database_old_excel.csv")
idea_R <- read.csv("database.csv")




# latin american sample
countries <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "GTM", 
               "HND", "MEX", "NIC", "PAN", "PER", "PRY", "SLV", "URY", "VEN")

idea_excel <- idea_excel %>% 
  rename(iso3c = Ticker, year_panel = Year) %>% 
  dplyr::select(iso3c, year_panel, blpi_pct, pf_pct, rs_pct, os_pct, idea_pct, CoC, un_region_name)

idea_excel <- idea_excel %>% 
  group_by(iso3c) %>% 
  mutate(CoC_01 = na.locf(CoC, na.rm = F))

idea_excel <- idea_excel %>%  
  filter(!iso3c == "TWN")
  
# sample latin america
idea_excel <- idea_excel %>% 
  filter(iso3c %in% countries) %>% 
  filter(year_panel >= 2006 & year_panel <= 2015)
  
data <- full_join(idea_excel, idea_R, by = c("iso3c", "year_panel"))




# results thesis
df <- data %>% 
  dplyr::select(iso3c, year_panel, CoC, CoC_01, idea_pct, wefji, p_expenditure, ruralpop, lifeexpectancy)

# linear regressions

# Table I.

# PFR Index
m1 <- lm(CoC ~ idea_pct, data = df)
summary(m1)

m2 <- plm(CoC ~ idea_pct, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m2)

m3 <- plm(CoC ~ idea_pct, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m3)

# WEF
m4 <- lm(CoC ~ wefji, data = df)
summary(m4)

m5 <- plm(CoC ~ wefji, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m5)

m6 <- plm(CoC ~ wefji, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m6)

# PI
m7 <- plm(CoC ~ p_expenditure, data = df)
summary(m7)

m8 <- plm(CoC ~ p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m8)

m9 <- plm(CoC ~ p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m9)






# Table II.
m10 <- plm(CoC ~ idea_pct + wefji + p_expenditure, data = df)
summary(m10)

m11 <- plm(CoC ~ idea_pct + wefji + p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m11)

m12 <- plm(CoC ~ idea_pct + wefji + p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m12)

m13 <- plm(CoC ~ idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure, data = df)
summary(m13)

m14 <- plm(CoC ~ idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m14)

m15 <- plm(CoC ~ idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m15)





# Table III.
m16 <- plm(CoC ~ idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure + lifeexpectancy + ruralpop, data = df)
summary(m16)

m17 <- plm(CoC ~ idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure + lifeexpectancy + ruralpop, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m17)

m18 <- plm(CoC ~ idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure + lifeexpectancy + ruralpop, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m18)











# results recalculated
df <- data %>% 
  dplyr::select(iso3c, year_panel, CoC, CoC_01, PFR_index, wefji, p_expenditure, ruralpop, lifeexpectancy)

df <- df %>% 
  rename(idea_pct = PFR_index)

# linear regressions

# Table I.

# PFR Index
m1 <- lm(CoC ~ idea_pct, data = df)
summary(m1)

m2 <- plm(CoC ~ idea_pct, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m2)

m3 <- plm(CoC ~ idea_pct, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m3)

# WEF
m4 <- lm(CoC ~ wefji, data = df)
summary(m4)

m5 <- plm(CoC ~ wefji, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m5)

m6 <- plm(CoC ~ wefji, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m6)

# PI
m7 <- plm(CoC ~ p_expenditure, data = df)
summary(m7)

m8 <- plm(CoC ~ p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m8)

m9 <- plm(CoC ~ p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m9)






# Table II.
m10 <- plm(CoC ~ idea_pct + wefji + p_expenditure, data = df)
summary(m10)

m11 <- plm(CoC ~ idea_pct + wefji + p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m11)

m12 <- plm(CoC ~ idea_pct + wefji + p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m12)

m13 <- plm(CoC ~ idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure, data = df)
summary(m13)

m14 <- plm(CoC ~ idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m14)

m15 <- plm(CoC ~ idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m15)





# Table III.
m16 <- lm(CoC ~ iso3c + idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure + lifeexpectancy + ruralpop, data = df)
summary(m16)
x16 <- margins.plm(m16)

m17 <- plm(CoC ~ idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure + lifeexpectancy + ruralpop, data = df, index= c("iso3c", "year_panel"), model = "within")
summary(m17)

m18 <- plm(CoC ~ idea_pct + wefji + p_expenditure + wefji:idea_pct + wefji:p_expenditure + lifeexpectancy + ruralpop, data = df, index= c("iso3c", "year_panel"), model = "random")
summary(m18)





interplot(m = m16, var1 = "idea_pct", var2 = "wefji", hist = TRUE) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  aes(color = "pink") + theme(legend.position="none")

interplot(m = m16, var1 = "p_expenditure", var2 = "wefji", hist = TRUE) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  aes(color = "pink") + theme(legend.position="none")









