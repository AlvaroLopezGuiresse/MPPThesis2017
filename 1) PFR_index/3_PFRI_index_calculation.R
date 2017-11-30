
library(tidyr)
library(dplyr)

rm(list = ls())
cat("\014")


# Data management to calculate PFR Index.
##########################################


setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis/1) PFR_index")
df <- read.csv("pfri_database.csv")


# index calculation pfri_level: before year enaction == 0, after year enaction ==1.
# ie. for ARG in Ques_1 the enaction year is 2009 thus before 
df <- df %>% 
  mutate(pfri_level = ifelse(yes_no == 1, ifelse(year_panel >= year_enforcement, 1, 0), 0))


# number of different questions per country.
sum_01 <- df %>%  
  group_by(iso3c) %>% 
  summarise(n_qtotal = n_distinct(question)) %>% 
  ungroup()

df <- df %>%  
  group_by(iso3c) %>% 
  mutate(n_question = n_distinct(question)) %>% 
  ungroup()


# number of questions enforced by country for every year
sum_02 <- df %>%  
  group_by(iso3c, year_panel) %>% 
  summarise(n_enforced =sum(pfri_level)) %>% 
  ungroup()

df <- df %>%  
  group_by(iso3c, year_panel) %>% 
  mutate(n_enforced = sum(pfri_level)) %>% 
  ungroup()


# pfri is calculated :)
df <- df %>%  
  mutate(pfri_pct = n_enforced/n_question)

sum_03 <- df %>%  
  group_by(iso3c, year_panel) %>% 
  summarise(pfri_pct = sum(pfri_pct)/n())

pfr_index <- sum_03





# PFR_subindex
# subcategories are included.
PI <- paste0("Ques_", c(1:13, 15, 17))
PF <- paste0("Ques_", c(19, 23, 25, 26, 28))
RS <-  paste0("Ques_", c(29:31, 33))
OS <- paste0("Ques_", c(35:39, 41, 43))

df <- df %>% 
  mutate(pfr_subindex= ifelse(question %in% PI, "PI", NA)) %>% 
  mutate(pfr_subindex= ifelse(question %in% PF, "PF", pfr_subindex)) %>% 
  mutate(pfr_subindex= ifelse(question %in% RS, "RS", pfr_subindex)) %>% 
  mutate(pfr_subindex= ifelse(question %in% OS, "OS", pfr_subindex))


# subindex is calculated
# subindex number of different questions per country.
sum_01 <- df %>%  
  group_by(iso3c, pfr_subindex) %>% 
  summarise(n_qtotal_sub = n_distinct(question)) %>% 
  ungroup()

df <- df %>%  
  group_by(iso3c, pfr_subindex) %>% 
  mutate(n_question_sub = n_distinct(question)) %>% 
  ungroup()


# subindex number of questions enforced by country for every year
sum_02 <- df %>%  
  group_by(iso3c, pfr_subindex, year_panel) %>% 
  summarise(n_enforced_sub =sum(pfri_level)) %>% 
  ungroup()

df <- df %>%  
  group_by(iso3c, pfr_subindex, year_panel) %>% 
  mutate(n_enforced_sub = sum(pfri_level)) %>% 
  ungroup()  


# pfr_subindex is calculated :):)
df <- df %>%  
  mutate(pfri_pct_sub = n_enforced_sub/n_question_sub)

sum_03 <- df %>%  
  group_by(iso3c, pfr_subindex, year_panel) %>% 
  summarise(pfri_pct_sub = sum(pfri_pct_sub)/n())

sum_03 <- sum_03 %>% 
  spread(pfr_subindex, pfri_pct_sub)

sum_03 <- sum_03 %>% 
  select(iso3c, year_panel, PI, PF, RS, OS)

# POLITICAL FINANCE SUB-INDEX:
# PI: Bans and Limits on Private Income.
# PF: Public Funding.
# RS: Regulation on Spending.
# OS: Oversigth and Sanctions
pfr_subindex <- sum_03


# merge datasets
panel_pfr <- full_join(pfr_subindex, pfr_index, by = c("iso3c", "year_panel"))

panel_pfr <- panel_pfr %>% 
  rename(PFR_index = pfri_pct)

setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis/2) DATABASE")
write.csv(panel_pfr, "panel_pfr_index.csv")



rm(list = ls())
cat("\014")








