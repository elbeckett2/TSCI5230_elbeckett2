#'---
#' title: "TSCI 5050: Introduction to Data Science"
#' author: 'Author One ^1^, Author Two ^1^'
#' abstract: |
#'  | Provide a summary of objectives, study design, setting, participants,
#'  | sample size, predictors, outcome, statistical analysis, results,
#'  | and conclusions.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output:
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0, class.output="scroll-20", attr.output='style="max-height: 150px; overflow-y: auto;"');

library(rio);# simple command for importing and exporting
library(pander); # format tables
#library(printr); # set limit on number of lines printed
library(dplyr); #add dplyr library
library(lubridate) #date manipulation
library(stringr) #string manipulation

options(max.print=500);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
whatisthis <- function(xx){
  list(class=class(xx),info=c(mode=mode(xx),storage.mode=storage.mode(xx)
                              ,typeof=typeof(xx)))};

data_location <- "~/Documents/UTHSCSA/GS2/TSCI5230/kaggle_dataset1/" #specify location of dataset
list.files(data_location,full.names = T) #identify files within dataset by PATH
#test <- import("/Users/beckette/Documents/UTHSCSA/GS2/TSCI5230/kaggle_dataset1//allergies.csv")
dat <- sapply(list.files(data_location,full.names = T),import) #create data set for the course
#data ingestion ----

##Preliminary Data
#set up tools
library(lubridate)

#determine age (copilot -> reviewed and edited by Eva)
patients <- dat$`/Users/beckette/Documents/UTHSCSA/GS2/TSCI5230/kaggle_dataset1//patients.csv`
patients$age_2025 <- 2025 - year(patients$BIRTHDATE) #age of paitents as of Dec 31 2025

#determine age at time of death (copilot -> reviewed and edited by Eva)
age_at_death <- function(birthdate, deathdate) {
  floor(interval(birthdate, deathdate) / years(1))
} #creates the function
patients$age_at_time_death <- age_at_death(patients$BIRTHDATE,patients$DEATHDATE) #calculate the age at time of death

#determine general demographics and conditions (copilot -> reviewed and edited by Eva)
conditions <- dat$`/Users/beckette/Documents/UTHSCSA/GS2/TSCI5230/kaggle_dataset1//conditions.csv`
conditions_description_types <- table(conditions$DESCRIPTION)

##Elena's code 08/26/2026
# data ingestion ----

# Your two data frames
conditions <- dat$`/Users/beckette/Documents/UTHSCSA/GS2/TSCI5230/kaggle_dataset1//conditions.csv`
patients <- dat$`/Users/beckette/Documents/UTHSCSA/GS2/TSCI5230/kaggle_dataset1//patients.csv`

# First, subset to acute/viral conditions
acute_viral <- conditions %>%
  filter(grepl("acute|viral", DESCRIPTION, ignore.case = TRUE))

# Match patients and calculate age at encounter, mutate is used to redefine a column
acute_viral_age <- acute_viral %>%
  left_join(
    patients %>% select(Id, BIRTHDATE),
    by = c("PATIENT" = "Id")
  ) %>%
  mutate(
    START = as.Date(START),
    BIRTHDATE = as.Date(BIRTHDATE),
    age_at_encounter = time_length(
      interval(BIRTHDATE, START),
      unit = "years"
    )
  )

