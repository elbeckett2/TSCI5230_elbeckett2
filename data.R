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
library(purrr)

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

#DESCRIPTION and CODE  mapping ----
code_map<-dat$conditions.csv[c("CODE","DESCRIPTION")] %>% unique() %>% #removes duplicate rows
  {setNames(.$DESCRIPTION,.$CODE)} #no longer dataframe but now a vector with names, curly brakets

#search within df code_map without having to call the dataframe
code_map<-dat$conditions.csv[c("CODE","DESCRIPTION")] %>% unique() %>% #removes duplicate rows
  with(setNames(DESCRIPTION, CODE))#turns 1st argument into an environment to just include variable, ie columns, in the dataframe

#code to co-occurence, pulls all the conditions a patient has that falls in the top conditions
paitent_codes <- filter(dat$conditions.csv,CODE %in% top_condition_slopes)%>% #%in% filters for a value within a vector, unique keeps all different rows,
  distinct(PATIENT, CODE) %>% #drops columns not referencing and condensing to columns of interest
  mutate(present=1) %>% 
  pivot_wider(names_from = CODE, values_from= present, values_fill = 0 )
encounter_codes <- filter(dat$conditions.csv,CODE %in% top_condition_slopes)%>% #%in% filters for a value within a vector, unique keeps all different rows,
  distinct(ENCOUNTER, CODE) %>% 
  mutate(present=1) %>% 
  pivot_wider(names_from = CODE, values_from= present, values_fill = 0 )

#determine number of unique patient IDs and construct list of pairwaise combinations of codes
n_patients<-nrow(dat$patients.csv) #7946 total number of patients
n_encounters<-nrow(dat$encounters.csv) #number of encounters
code_combos <- combn(top_condition_slopes, 2, simplify = FALSE) #all possible pairwise conditions in list form

#Create function to quantify number of conditions, when conditions overlap by a pair basis, output as square matrix
fn_lift <- function(xx, code_source=patient_codes,denom=n_patients){
  counta <- sum(code_source[[ xx[1] ]]) #[[]] pull item by position, [] pulls item within list
  countb <- sum(code_source[[ xx[2] ]])
  expected <- counta*countb/denom
  observed <- sum(code_source[[ xx[1] ]]*code_source[[ xx[2] ]])
  out <- if(expected == 0){1} else{observed/expected}
  data.frame(cnd_a=xx[],cnd_b=rev(xx[]),lift=out) #create square matrix
}

#Create lift matrix for co-occurrence of conditions and heat map
patient_lift_matrix <- map(code_combos, fn_lift) %>% list_rbind() %>%
  mutate(cnd_a=code_map[cnd_a]) %>% 
  mutate(cnd_b=code_map[cnd_b]) %>% 
  xtabs(lift~cnd_a+cnd_b,data=.)
heatmap(log1p(patient_lift_matrix), symm=T,scale="none",col=hcl.colors(50, "RdBu", rev=TRUE))

rownames(patient_lift_matrix)
colnames(patient_lift_matrix)
code_map[rownames(patient_lift_matrix)]

#Co-occurence of codes in same patient at same visit
encounter_lift_matrix <- map(code_combos, fn_lift, code_source=encounter_codes, denom=n_encounters) %>% list_rbind() %>%
  mutate(cnd_a=code_map[cnd_a]) %>% 
  mutate(cnd_b=code_map[cnd_b]) %>% 
  xtabs(lift~cnd_a+cnd_b,data=.)
e_map<-heatmap(log1p(encounter_lift_matrix), symm=T,scale="none",col=hcl.colors(50, "RdBu", rev=TRUE))
colnames(encounter_lift_matrix)[e_map$colInd]

#xtabs cross tabulates a data frame



