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

#Load tools ----
library(rio);# simple command for importing and exporting
library(pander); # format tables
library(printr); # set limit on number of lines printed
library(dplyr); #add dplyr library
library(lubridate) #date manipulation
library(stringr) #string manipulation
library(tidyr) #to use pivot_wider function

options(max.print=500);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
whatisthis <- function(xx){
  list(class=class(xx),info=c(mode=mode(xx),storage.mode=storage.mode(xx)
                              ,typeof=typeof(xx)))};

data_location <- "~/Documents/UTHSCSA/GS2/TSCI5230/kaggle_dataset1/" #specify location of dataset
list.files(data_location,full.names = T) #identify files within dataset by PATH
#test <- import("/Users/beckette/Documents/UTHSCSA/GS2/TSCI5230/kaggle_dataset1//allergies.csv")
dat <- sapply(list.files(data_location, full.names = TRUE), import,simplify = FALSE) %>% 
  setNames(.,basename(names(.))) #create data set for the course

#Condition Frequency ----

#set up tools
library(lubridate)

#determine age (copilot -> reviewed and edited by Eva)
patients <- dat$patients.csv
patients$age_2025 <- 2025 - year(patients$BIRTHDATE) #age of paitents as of Dec 31 2025

#determine age at time of death (copilot -> reviewed and edited by Eva)
age_at_death <- function(birthdate, deathdate) {
  floor(interval(birthdate, deathdate) / years(1))
} #creates the function
patients$age_at_time_death <- age_at_death(patients$BIRTHDATE,patients$DEATHDATE) #calculate the age at time of death

#determine general demographics and conditions (copilot -> reviewed and edited by Eva)
conditions <- dat$conditions.csv
conditions_description_types <- table(conditions$DESCRIPTION)

# PRELIM: Acute pharyngitis ----
#Elena's code 08/26/2026

# First, subset to acute/viral conditions
acute_viral <- conditions %>%
  filter(grepl("acute|viral", DESCRIPTION, ignore.case = TRUE)) %>%
# Match patients and calculate age at encounter, mutate is used to redefine a column
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


#Regression between conditions and time  ----
  
#Example Acute Viral Pharyngitis
temp <- filter(dat$conditions.csv,(DESCRIPTION =="Acute viral pharyngitis (disorder)")) %>%
  mutate(month=floor_date(START, unit = "month")) %>% 
  group_by(month) %>% summarize(count=n())
lm(count~month,temp)

#Regressions for all conditions over time
condition_slopes <- mutate(dat$conditions.csv, month=floor_date(START, unit = "month")) %>% #create data frame
  group_by(month, CODE, DESCRIPTION) %>% summarise(count=n()) %>% #summarise lists the information you specify
  group_by(CODE, DESCRIPTION) %>% filter(year(month)>=2023 & length(unique(month))>10) %>% 
  summarise(events=lm(count~month)$coefficients[2]) %>% arrange(desc(events))

plot(condition_slopes$events, type="l")
abline(v=25,col="blue") #25 reasonable cut-off for conditions

top_condition_slopes <- head(condition_slopes, 25)$CODE #empty space before comma means all the rows and empty space after comma means all the columns

#DESCRIPTION and CODE  mapping
code_map<-dat$conditions.csv[c("CODE","DESCRIPTION")] %>% unique() %>% #removes duplicate rows
  {setNames(.$DESCRIPTION,.$CODE)} #no longer dataframe but now a vector with names, curly brakets

#search within df code_map without having to call the dataframe
code_map<-dat$conditions.csv[c("CODE","DESCRIPTION")] %>% unique() %>% #removes duplicate rows
  with(setNames(DESCRIPTION, CODE))#turns 1st argument into an environment to just include variable, ie columns, in the dataframe

#code to co-occurence, pulls all the conditions a patient has that falls in the top conditions
patient_codes <- filter(dat$conditions.csv, CODE %in% top_condition_slopes)[c("PATIENT","CODE")] %>% #%in% filters for value within vector
  unique() %>% #keeps the unique rows, tells you number of unique values to de-duplicate
  mutate(present=1) %>% #define 1 as present
  pivot_wider(names_from = CODE, values_from = present, values_fill = 0) %>% #each patient is a row, and each column a condition, replaces NA with 0
  select(-PATIENT) # select() behaves more predictably than .[] or .$ callouts

#determine number of unique patient IDs and construct list of pairwaise combinations of codes
n_patients<-nrow(dat$patients.csv) #7946 total number of patients
code_combos <- combn(top_condition_slopes, 2, simplify = FALSE) #all possible pairwise conditions in list form

#
fn_lift <- function(xx){
  counta <- sum(patient_codes[[ xx[1] ]])
  countb <- sum(patient_codes[[ xx[2] ]])
  browser()
}

#[[]] pull item by position, [] pulls item within list
