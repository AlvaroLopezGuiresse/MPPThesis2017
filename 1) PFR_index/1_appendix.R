
# Load packages
library(rio)
library(dplyr)
library(stringi)
library(stringr)
library(tidyr)
library(zoo)
library(countrycode)
library(data.table)

rm(list = ls())
cat("\014") 

# input: input_IDEA_appendix.xlsx
# output: output_IDEA_appendix.csv


# Data management IDEA appendix.
################################

# sample: 18 latin american countries
countries <- c("ARG", "BOL", "BRA", "CHL", "COL", "CRI", "DOM", "ECU", "GTM", 
               "HND", "MEX", "NIC", "PAN", "PER", "PRY", "SLV", "URY", "VEN")


# function to extrct the year form attribution variable (legal source quoted
# for every question) from the appendix in the IDEA survey.
clean_text <- function(whyisthis) {
  
  # Detect numbers and sub them
  years <-  stri_extract_all_regex(whyisthis, "\\d{4}") %>% unlist() %>%
    as.numeric()
  
  years <- years[years <= 2016 & years >= 1900]
  if (length(years) == 0) return(NA)
  if (is.na(years)) return(NA)
  
  # Return output
  return(max(years, na.rm = T))
}


# import IDEA appendix
setwd("/Users/alvarolopezguiresse/OneDrive/Documents/[0.2] Data Management in R/MPPThesis2017/1) PFR_index")
idea_appendix <- import("input_IDEA_appendix.xlsx")[, 1:4]
idea_appendix <- idea_appendix %>% fill(Country)
idea_appendix_0 <- idea_appendix

# filter latin american countries
idea_appendix$iso3c <- countrycode(idea_appendix$Country, 'country.name', 'iso3c', warn = FALSE)
idea_appendix <- idea_appendix %>% 
  filter(iso3c %in% countries)


# extract year of enforcement from Attribution
temp_idea_appendix <- rowwise(idea_appendix)
temp_idea_appendix <- mutate(temp_idea_appendix, 
                             year_enforcement = clean_text(Attribution))


# create several variables... with separate...
temp_idea_appendix <- separate(data = temp_idea_appendix, 
                                  col = `Used in Question`, 
                                  fill = "right", 
                                  into = paste0("temp_", 1:43))


# gather temporal variables to extract the Questions which were enacted...
temp_idea_appendix_01 <- gather(data = temp_idea_appendix, 
                                key = Temp, 
                                value = Question, 
                                ... = starts_with("temp_"))


# temp_idea_appendix_01 <- arrange(temp_idea_appendix_01, Country, Attribution, Dummies)
temp_idea_appendix_01 <- filter(temp_idea_appendix_01, !is.na(Question))
temp_idea_appendix_01 <- select(temp_idea_appendix_01, -Temp)


# After every year-question are togheter for every country... 
# variables for every Question are created... and a value of 1 will be assigned,
# to identify the years along with the question variables (when later spread as columns)
temp_idea_appendix_02 <- mutate(temp_idea_appendix_01, 
                                Question = paste0("Ques_", Question), 
                                temp = 1)


# group by ... to delete duplicates... n() the frequency of every grouped single observation...
# every question should be unique... by year by legal source and by country...
# later dupicates across time (i.e. question legislated in several years - Q2 was legislated in 2009 and 2012)
# will be taken the oldest one...
temp_idea_appendix_02 <- group_by(temp_idea_appendix_02, Country, Attribution, Question)
temp_idea_appendix_02 <- mutate(temp_idea_appendix_02, drop = seq(1:n()))
temp_idea_appendix_02 <- filter(temp_idea_appendix_02, drop != 2)
temp_idea_appendix_02 <- select(temp_idea_appendix_02, -drop)


# variables for every Question are spread into columns... the value of 1 represents a question (law)
# being enacted in a specific year...
temp_idea_appendix_03 <- spread(data = temp_idea_appendix_02, 
                                key = Question, 
                                value = temp, 
                                fill = 0)


# variables are re-ordered...
temp_idea_appendix_03 <- select(temp_idea_appendix_03, Country, Attribution, Type, 
                                year_enforcement, paste0("Ques_", 1:43))


# final dataset
idea_appendix <- temp_idea_appendix_03


# iso3c is added...
idea_appendix$iso3c <- countrycode(idea_appendix$Country, 'country.name', 'iso3c', warn = FALSE)
idea_appendix <- idea_appendix %>% select(Country, iso3c, everything())


# LATAM sample is selected... questions which couldn't be coded are 
# droped, missing values ie. years that couldn't be traced are also dropped...
# Q_28...
drop <-  paste0("Ques_", c(14, 16, 18, 20, 21, 22, 24, 27, 28, 32, 34, 40, 42))

idea_appendix <- idea_appendix %>% 
  filter(iso3c %in% countries) %>% 
  filter(!is.na(year_enforcement))


idea_appendix <- idea_appendix[ , -which(names(idea_appendix) %in% drop)] %>% 
  as.data.frame()

write.csv(idea_appendix, "output_IDEA_appendix.csv")


rm(list = ls()) 
cat("\014") 
