

library(tidyr)
library(dplyr)
library(stringr)

rm(list = ls())
cat("\014") 


# input: input_IDEA_survey, output_IDEA_appendix (from 1_appendix)
# output: output_PFR_database


# Data management to build PFR Index panel.
###########################################


# import datasets IDEA survey & appendix
setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis2017/1) PFR_index")
idea_survey <- read.csv("input_IDEA_survey.csv")
idea_survey$ISO <- idea_survey$ISO %>% 
  as.character()

idea_appendix <- read.csv("output_IDEA_appendix.csv")
idea_appendix$iso3c <- idea_appendix$iso3c %>% 
  as.character()

# sample for the regression model:
# 18 latin american countries
countries <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "GTM", 
               "HND", "MEX", "NIC", "PAN", "PER", "PRY", "SLV", "URY", "VEN")

# questions dissmissed for coding from IDEA survey for
# our thesis work. This is an assumption, thus subject to revision.
idea_survey <- idea_survey %>% 
  filter(ISO %in% countries) %>% 
  dplyr::select(-c(X14, X16, X18, X20, X21, X22, X24, X27, X28, X32, X34, X40, X42)) %>% 
  dplyr::rename(iso3c = ISO)

# NA are included in survey.
t_survey <- idea_survey %>% 
  gather(question, yes_no, X1:X43) %>% 
  mutate(yes_no = ifelse(yes_no=="No data", NA, yes_no))

# rename variables as Ques_n.
t_survey <- t_survey %>% 
  dplyr::mutate(number = str_extract(question, "\\-*\\d+\\.*\\d*")) %>% 
  dplyr::mutate(question = paste0("Ques_", number)) %>% 
  dplyr::select(-number)

# select the minimun enforcement year per question in appendix.
t_appendix <- idea_appendix %>% 
  gather(question, enaction, Ques_1:Ques_43) %>% 
  dplyr::select(-Type) %>% 
  dplyr::filter(enaction == 1) %>% 
  dplyr::group_by(iso3c, question) %>% 
  dplyr::filter(year_enforcement == min(year_enforcement))

# Unique observations: iso3c -> Ques_n -> year_enforcement 
t_appendix <- t_appendix %>% 
  dplyr::group_by(iso3c, year_enforcement, question) %>% 
  dplyr::mutate(drop = seq(1:n())) %>% 
  dplyr::filter(drop != 2) %>% 
  dplyr::select(-drop)

# merge datasets by "iso3c" & "question"
t_survey_appendix <- full_join(t_survey, t_appendix, c("iso3c", "question"))

# delete NA for year_enforcement & yes_no
t_survey_appendix <- t_survey_appendix %>% 
  dplyr::select(Country.x, iso3c, question, yes_no, year_enforcement) %>% 
  dplyr::filter(!is.na(year_enforcement)) %>% 
  dplyr::filter(!is.na(yes_no))

t_survey_appendix <- t_survey_appendix %>% 
  dplyr::mutate(t_year_enforcement = year_enforcement, t_na = NA) %>% 
  spread(t_year_enforcement, t_na) %>% 
  gather(year_panel, t_na, ... = `1925`:`2016`) %>% 
  dplyr::select(-t_na)

t_survey_appendix <- t_survey_appendix %>% 
  dplyr::select(-Country.x) %>% 
  dplyr::arrange(iso3c, question) 

write.csv(t_survey_appendix, "output_PFR_Index_database.csv")

rm(list = ls())
cat("\014") 
