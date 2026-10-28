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
debug <- 0; # debug flag: controls how much output/warnings/messages knitr shows below
# knitr chunk options: show code when debug > -1 (i.e. always, since debug=0),
# show warnings/messages only when debug > 0 (i.e. currently suppressed);
# also caps output box height/scrolling for long printed output
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0, class.output="scroll-20", attr.output='style="max-height: 150px; overflow-y: auto;"');

#Load tools ----
library(rio);# simple command for importing and exporting
library(pander); # format tables
library(printr); # set limit on number of lines printed
library(dplyr); #add dplyr library
library(lubridate) #date manipulation
library(stringr) #string manipulation
library(tidyr) #to use pivot_wider function
library(purrr) # provides map()/list_rbind() used later for the lift-matrix calculations

options(max.print=500); # limit console print output to 500 items
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf); # don't wrap/split pander tables regardless of width

# helper/debug function: reports the class, mode, storage.mode, and typeof of an object
# (useful for inspecting an unfamiliar variable's underlying data type)
whatisthis <- function(xx){
  list(class=class(xx),info=c(mode=mode(xx),storage.mode=storage.mode(xx)
                              ,typeof=typeof(xx)))};

data_location <- "~/Documents/UTHSCSA/GS2/TSCI5230/kaggle_dataset1/" #specify location of dataset
list.files(data_location,full.names = T) #identify files within dataset by PATH

# import every file found in data_location into a named list of data frames,
# then rename each list element to just the file's base name (drops full path)
dat <- sapply(list.files(data_location, full.names = TRUE), import,simplify = FALSE) %>% 
  setNames(.,basename(names(.))) #create data set for the course

#Condition Frequency ----

#determine age (copilot -> reviewed and edited by Eva)
#account for whether the patient's birthday has occurred yet in 2025 (can be off by 1 year)
patients <- dat$patients.csv
patients$age_2025 <- 2025 - year(patients$BIRTHDATE) #age of patients as of Dec 31 2025

#determine age at time of death (copilot -> reviewed and edited by Eva)
age_at_death <- function(birthdate, deathdate) {
  floor(interval(birthdate, deathdate) / years(1))
} #creates the function

# NOTE: for patients who haven't died, DEATHDATE will be NA, so age_at_time_death
# will correctly return NA for those rows (not a bug, just worth knowing)
patients$age_at_time_death <- age_at_death(patients$BIRTHDATE,patients$DEATHDATE) #calculate the age at time of death

#determine general demographics and conditions (copilot -> reviewed and edited by Eva)
conditions <- dat$conditions.csv
conditions_description_types <- table(conditions$DESCRIPTION)

# Example: Acute pharyngitis ----
#Elena's code 08/26/2026

# First, subset to acute/viral conditions
acute_viral <- conditions %>%
  filter(grepl("acute|viral", DESCRIPTION, ignore.case = TRUE)) %>% # keep rows where DESCRIPTION contains "acute" or "viral" (case-insensitive)
# Match patients and calculate age at encounter, mutate is used to redefine a column
  left_join(
    patients %>% select(Id, BIRTHDATE), # bring in each patient's birthdate
    by = c("PATIENT" = "Id") # join condition rows to patients on PATIENT == Id
  ) %>%
  mutate(
    START = as.Date(START),
    BIRTHDATE = as.Date(BIRTHDATE),
    age_at_encounter = time_length(
      interval(BIRTHDATE, START),
      unit = "years"
    ) # compute patient's exact (fractional) age at the time of the encounter
  )

#Example Acute Viral Pharyngitis
# filter conditions down to only "Acute viral pharyngitis (disorder)" rows,
# bucket each encounter's START date into its calendar month, then count
# how many such encounters occurred per month
temp <- filter(dat$conditions.csv,(DESCRIPTION =="Acute viral pharyngitis (disorder)")) %>%
  mutate(month=floor_date(START, unit = "month")) %>% 
  group_by(month) %>% summarize(count=n())
lm(count~month,temp) # fit a linear trend of monthly count over time
# NOTE: the lm() result is not assigned to a variable, so it is only printed to
# console/report and is not reused - if the intent was to save/inspect it further,
# this needs to be assigned, e.g. `fit <- lm(count~month,temp)`

#Regressions for all conditions over time ----
condition_slopes <- mutate(dat$conditions.csv, month=floor_date(START, unit = "month")) %>% #convert encounter dates to months
  group_by(month, CODE, DESCRIPTION) %>% summarise(count=n()) %>% #counts how many times each condition occurs each month
  group_by(CODE, DESCRIPTION) %>% filter(year(month)>=2023 & length(unique(month))>10) %>%
  # restricts to months from 2023 onward and to condition codes with more than 10
  # distinct months of data (i.e. enough data points for a later regression to be meaningful).
  summarise(events=lm(count~month)$coefficients[2]) %>% arrange(desc(events)) # extracts slope of that line and ranks in descending order

plot(condition_slopes$events, type="l") # plot the ranked slopes to visually spot a cutoff/elbow
abline(v=25,col="blue") #25 reasonable cut-off for conditions

top_condition_slopes <- head(condition_slopes, 25)$CODE #empty space before comma means all the rows and empty space after comma means all the columns
# takes the first 25 rows (already ranked by descending slope) and pulls out their CODE column

#DESCRIPTION and CODE  mapping ----
code_map<-dat$conditions.csv[c("CODE","DESCRIPTION")] %>% unique() %>% #removes duplicate rows
  {setNames(.$DESCRIPTION,.$CODE)} #no longer dataframe but now a vector with names, curly brakets
# the period represents the entire data frame coing from previous step
# setNames(object, nm), first argument objects = values, second argument nm = vector names

#search within df code_map without having to call the dataframe
code_map<-dat$conditions.csv[c("CODE","DESCRIPTION")] %>% unique() %>% #removes duplicate rows
  with(setNames(DESCRIPTION, CODE))#turns 1st argument into an environment to just include variable, ie columns, in the dataframe

#code to co-occurence ----
#pulls all the conditions a patient has that falls in the top conditions
patient_codes <- filter(dat$conditions.csv,CODE %in% top_condition_slopes) %>%
        #%in% filters for a value within a vector, unique keeps all different rows,
        #filters to only keep rows whose CODE is in the top conditions
  distinct(PATIENT, CODE) %>%
    #drops columns not referencing and condensing to columns of interest
    #keep only unique patient-condition pairs
    #answers whether patient had the condition, not how many times
  mutate(present=1) %>% #adds new column called present, where 1 indicates patient has condition
  pivot_wider(names_from = CODE, values_from= present, values_fill = 0 )
    #converts from long format to wide format
    #each condition code is a made a column, where values to fill each column are 0 or 1 to indicate if a patient has the condition

encounter_codes <- filter(dat$conditions.csv,CODE %in% top_condition_slopes)%>% #%in% filters for a value within a vector, unique keeps all different rows,
  distinct(ENCOUNTER, CODE) %>% # keep only unique encounter-condition pairs (did this encounter include this condition)
  mutate(present=1) %>% # flag column: 1 = condition present at this encounter
  pivot_wider(names_from = CODE, values_from= present, values_fill = 0 ) # long -> wide: one column per condition code, 0/1 indicator

#determine number of unique patient IDs and construct list of pairwaise combinations of codes
n_patients<-nrow(dat$patients.csv) #7946 total number of patients
n_encounters<-nrow(dat$encounters.csv) #number of encounters
code_combos <- combn(top_condition_slopes, 2, simplify = FALSE)
  #all possible pairwise conditions in list form
    #combn(x, m), where x = vector of items, m = number of items per combination
    #from conditions with top greatest slopes, generate every possible combination pairs
    #simplify=FALSE makes the output a list instead of a matrix

#Create function to quantify number of conditions ----
# when conditions overlap by a pair basis, output as square matrix
# computes "lift" for a pair of condition codes xx=c(codeA, codeB): how much more/less often
# the two conditions co-occur than would be expected if they were independent
# (lift = observed co-occurrence rate / expected co-occurrence rate under independence)
fn_lift <- function(xx, code_source=patient_codes,denom=n_patients){
  # () defines arguments/the inputs, {} define body of the function/what to do with inputs, "xx" are argument names defined by programmer
  counta <- sum(code_source[[ xx[1] ]])
  #[[]] pull element stored inside container, [] give subset of container
  ## [[]] necessary to give function the inputs which are a vector, vs subset
  countb <- sum(code_source[[ xx[2] ]])
  expected <- counta*countb/denom
  observed <- sum(code_source[[ xx[1] ]]*code_source[[ xx[2] ]]) #multiply bc only co-occurences will give 1
  out <- if(expected == 0){1} else{observed/expected}
    #prevent division by zero
    #output ratio of observed to expected to determine if observed is more than expected
  data.frame(cnd_a=xx[],cnd_b=rev(xx[]),lift=out) # this last line is the output of the function
  # arguments in data.frame are each a column with the defined values that fill it
  # produces 2 rows per pair (codeA,codeB) and (codeB,codeA) with the same lift value,
  # so the resulting long table can be pivoted into a symmetric square matrix later
}

#Create lift matrix for co-occurrence of conditions and heat map ----
patient_lift_matrix <- map(code_combos, fn_lift) %>% list_rbind() %>%
  # map(object, function) is a purr pckg that applies the same function to every element of a vector or list
  # takes each element of code_combos and runs fn_lift() on it
  # map function returns list of dataframes, where fn_lift returns a data frame
  # list_rbind takes each dataframe in the list and row-binds them together to create a single df
  mutate(cnd_a=code_map[cnd_a]) %>%
  mutate(cnd_b=code_map[cnd_b]) %>%
  # replace condition CODE with its human-readable DESCRIPTION
    # code_map[cnd_a] uses the values in cnd_a as names to look up in code_map, which is named vector
    # cnd_a= will replace the codes in cnd_a with the corresponding descriptions identified using code_map
  xtabs(lift~cnd_a+cnd_b,data=.)
  # reshape long (cnd_a, cnd_b, lift) table into a square matrix
    # cnd_a is first and is placed on rows
    # cnd_b is second and placed on columns
    # fill cells using lift
    # . means to use the obct that came from the previous pipe step
heatmap(log1p(patient_lift_matrix), symm=T,scale="none",col=hcl.colors(50, "RdBu", rev=TRUE))
  # visualize co-occurrence strength
  # log1p compresses skew

rownames(patient_lift_matrix)
colnames(patient_lift_matrix)
code_map[rownames(patient_lift_matrix)]
# NOTE: the three lines above are not assigned to anything - they just print to console/report,
# presumably for inspection of the matrix's row/column labels

#Co-occurence of codes in same patient at same visit ----
encounter_lift_matrix <- map(code_combos, fn_lift, code_source=encounter_codes, denom=n_encounters) %>% list_rbind() %>%
  mutate(cnd_a=code_map[cnd_a]) %>% 
  mutate(cnd_b=code_map[cnd_b]) %>% 
  xtabs(lift~cnd_a+cnd_b,data=.)
e_map<-heatmap(log1p(encounter_lift_matrix), symm=T,scale="none",col=hcl.colors(50, "RdBu", rev=TRUE)) # heatmap() also returns row/col dendrogram ordering, captured here in e_map

colnames(encounter_lift_matrix)[e_map$colInd] # get condition names in the order the heatmap's clustering placed them

# manually pick out conditions that cluster together at the "systemic" end of the
systemic_conditions<-colnames(encounter_lift_matrix)[e_map$colInd][18:25]
  # heatmap's dendrogram ordering (positions 18-25 in the clustered column order)
    # [e_map$colInd] reorders the column names called from encounter_lift_matrix
  # NOTE: 18:25 is a hard-coded index range based on visually inspecting where the
    # systemic-looking cluster falls in this particular heatmap - if code_combos,
    # top_condition_slopes, or the clustering order change, this range will silently
    # point at the wrong conditions instead of erroring

# manually pick out conditions that are not systemic or or do not overlap with systemic
local_conditions<-colnames(encounter_lift_matrix)[e_map$colInd][-(3:5)] %>% setdiff(systemic_conditions)
  # take the clustered column order, drop positions 3-5 (presumably a small
    # outlier/unclustered group not wanted in either bucket), then remove anything
    # already classified as systemic above - what's left is treated as "local" conditions
      # setdiff(x, y) returns elements in x that are not in y, piped object becomes 1st argument
  # NOTE: same fragility as above - -(3:5) is also a hard-coded index range tied to
    # this specific heatmap's clustering order, so it needs re-checking any time the
    # underlying condition list or clustering changes

#Summarize conditions by encounter ----
encounter_summary<-group_by(dat$conditions.csv, ENCOUNTER, PATIENT) %>%
  # group_by slices up conditions df into groups that share the same encounter and patient
  # pass it to summarize which runs arguments for each group
  summarize(local=length(intersect(local_conditions,DESCRIPTION))
            # create column called local
            # intersect (y,x) return values appearing in both vectors
            , systemic=length(intersect(systemic_conditions,DESCRIPTION))
            , first_condition_start=min(START,na.rm = TRUE)
            # earliest condition start recorded in 
            #na.rm means remove entries with missing values
            , last_condition_start=max(START,na.rm = TRUE)
            # most recent condition start recorded in encounter
            , condition_type = case_when(
              local > 0 & systemic >0 ~ "both",
              local > 0 ~ "local",
              systemic > 0 ~ "systemic",
              TRUE ~ "neither"
            )
                                                      )

#first_condition_start is a good proxy for encounter date 
# find encounters where the conditions did not all start on the same date
subset(encounter_summary,first_condition_start!=last_condition_start & condition_type!="neither")
# != means not equal to
# looking at scenarios where the start date for the earliest and most recent conditions are different
# confirm that the dates are not that different and so using the first condition start date is acceptable

# look at how many local/systemic conditions tend to occur in an encounter
#Most encounters have more or at least 4 different systemic conditions
subset(encounter_summary,systemic > 0)$systemic %>% hist()
# create histogram of systemic conditions

# Most encounters have 1 local condition, none have more than 3
subset(encounter_summary,local > 0)$local %>% hist()
# create a histogram of local conditioins

# count encounters containing systemic conditions, local conditions, or both
sum(encounter_summary$systemic > 0) #no. encounters with systemic conditions
sum(encounter_summary$local > 0) #no. encounters with local conditions
sum((encounter_summary$systemic > 0)*(encounter_summary$local > 0)) #no encounters have both local and systemic conditions 

# examine the distributions and overlap
#contingency table for systemic and local conditions, do all patients have local conditions
with(encounter_summary,table(local))
with(encounter_summary,table(systemic))
with(encounter_summary,table(local,systemic))
encounter_summary$condition_type %>% table() %>% prop.table()
# with(data, expression), used to not repeatedly type encounter_summary$
# prop. table changes from raw data to percentage

# Create Patient summary ----
patient_summary <- group_by(encounter_summary,PATIENT) %>% # group_by(dataframe, column)
  summarize(n_local = sum(local > 0)
            ,n_systemic = sum(systemic > 0)
            ,date_systemic = if(n_systemic > 0){min(first_condition_start[systemic>0])} else {NA}
            ,date_local = if(n_local > 0){min(first_condition_start[local>0])} else {NA}
            ,condition_order = case_when(
              n_local > 0 & n_systemic == 0 ~ "local only",
              n_systemic > 0 & n_local == 0 ~ "systemic only",
              n_local > 0 & n_systemic > 0 & date_local < date_systemic ~ "local first",
              n_local > 0 & n_systemic > 0 & date_systemic < date_local ~ "systemic first",
              n_local > 0 & n_systemic > 0 & date_local == date_systemic ~ "same date",
              TRUE ~ "neither"
            )
            
            ,interval = as.numeric(difftime(date_systemic, date_local, units = "days")) #convert to numeric value
            #,debug = {if(n_local+n_systemic > 0){browser()}
            #  0
            # }
) %>%
  left_join(dat$patients.csv,by=c(PATIENT='Id'))

# summarize patient-level patterns
hist(patient_summary$n_local
     ,breaks=30
     ,main="Number of Local Conditions per Patient"
     ,xlab="Number of Local Conditions")

hist(patient_summary$n_systemic
     ,breaks=30
     ,main="Number of Systemic Conditions per Patient"
     ,xlab="Number of Systemic Conditions")

#period between local and systemic encounters most frequently between 0-20 days
hist(patient_summary$interval
     ,breaks=100
     ,main="Interval Between Systemic and Local Conditions"
     ,xlab="Interval (days)")

barplot(table(patient_summary$condition_order)
        ,main="Condition Order per Patient"
        ,ylab="Number of Patients"
        ,las=2)

#History of disease spread ----

# earliest local and systemic dates in the dataset
patient_summary %>%
  summarize(date_local=min(date_local,na.rm=T)
            ,date_systemic=min(date_systemic,na.rm=T))

# earliest local and systemic dates by state
group_by(patient_summary,STATE) %>%
  summarize(date_local=min(date_local,na.rm=T)
            ,date_systemic=min(date_systemic,na.rm=T)) %>%
  arrange(date_local,date_systemic)

# earliest local and systemic dates by Texas city
subset(patient_summary,STATE=="Texas" & !is.na(date_local)) %>%
  group_by(CITY) %>%
  summarize(date_local=min(date_local,na.rm=T) # have warning message
            ,date_systemic=min(date_systemic,na.rm=T)) %>%
  arrange(date_local,date_systemic)


#Create local/systemic patient and encounter lists ----
local_encounters <- subset(encounter_summary,local>0)$ENCOUNTER
systemic_encounters <- subset(encounter_summary,systemic>0)$ENCOUNTER

local_patients <- subset(encounter_summary,local>0)$PATIENT %>% unique()
systemic_patients <- subset(encounter_summary,systemic>0)$PATIENT %>% unique()


#Procedures ----
subset(dat$procedures.csv,ENCOUNTER %in% local_encounters)$DESCRIPTION %>% table() %>% sort()
subset(dat$procedures.csv,ENCOUNTER %in% systemic_encounters)$DESCRIPTION %>% table() %>% sort()

# Observations ----
subset(dat$observations.csv,ENCOUNTER %in% local_encounters)$DESCRIPTION %>% table() %>% sort()
subset(dat$observations.csv,ENCOUNTER %in% systemic_encounters)$DESCRIPTION %>% table() %>% sort()

# Number of records within each encounter for local and systemic conditions
subset(dat$observations.csv,ENCOUNTER %in% local_encounters) %>% nrow()

# Creating a function to number the rows for encounter types ----
fn_rows_encounter <- function(xx)
{summarise(
  dat[[xx]],
  source = xx,
  rows = n(),
  rows_local = sum(ENCOUNTER %in% local_encounters, na.rm = TRUE),
  rows_systemic = sum(ENCOUNTER %in% systemic_encounters, na.rm = TRUE),
  encounters = length(unique(ENCOUNTER)),
  encounters_local = length(intersect(ENCOUNTER,local_encounters)),
  encounters_systemic = length(intersect(ENCOUNTER,systemic_encounters)),
  rows_per_encounter = rows/encounters,
  local_rows_per_encounter = rows_local/encounters_local,
  systemic_rows_per_encounter = rows_systemic/encounters_systemic
)
}

# Counts of condition types by encounter for all datasets in dat
names(dat) %>% setdiff(c("patients.csv","encounters.csv")) %>% sapply(fn_rows_encounter, simplify = FALSE) %>% bind_rows()
# sapply do not turn into matrix

# Creating a function to number the rows for clinical phenotypes of patients ----
fn_rows_patient <- function(xx)
{summarise(
  dat[[xx]],
  source = xx,
  rows = n(),
  rows_local = sum(PATIENT %in% local_patients, na.rm = TRUE),
  rows_systemic = sum(PATIENT %in% systemic_patients, na.rm = TRUE),
  patients = length(unique(PATIENT)),
  patient_local = length(intersect(PATIENT,local_patients)),
  patient_systemic = length(intersect(PATIENT,systemic_patients)),
  rows_per_patient = rows/patients,
  local_rows_per_patient = rows_local/patient_local,
  systemic_rows_per_patient = rows_systemic/patient_systemic
)
}

# Counts of condition types by patient for all datasets in dat
names(dat) %>% setdiff(c("patients.csv","encounters.csv")) %>% sapply(fn_rows_patient, simplify = FALSE) %>% bind_rows()
# sapply do not turn into matrix

# Standardize dataframes (except pateints.csv lacks same column naming system) ----
transmute(dat$medications.csv, PATIENT, ENCOUNTER, CODE, DESCRIPTION, source = "medications") %>% filter(!is.na(CODE)) %>% View() # only see non-missing entries
transmute(dat$medications.csv, PATIENT, ENCOUNTER, CODE=REASONCODE, DESCRIPTION=REASONDESCRIPTION, source = "medications_reasons") %>% filter(!is.na(CODE)) %>% View()

fn_standardize <- function(xx){
  if("SSN" %in% names(xx)) {return(NULL)}
  if("Id" %in% names(xx) & !"ENCOUNTER" %in% names(xx)) {xx <- mutate(xx, ENCOUNTER=Id)}
  out <- transmute(xx, PATIENT, ENCOUNTER, CODE=as.character(CODE), DESCRIPTION, subtype="CODE") %>% filter(!is.na(CODE))
  if(all(c("REASONCODE","REASONDESCRIPTION") %in% names(xx))){
    out <- bind_rows(out, transmute(xx, PATIENT, ENCOUNTER, CODE=as.character(REASONCODE), DESCRIPTION=REASONDESCRIPTION,
                                    subtype = "reason") %>% filter(!is.na(CODE)))
  }
  # alternative if(length(intersect(colnames(xx),c("REASONCODE","REASONDESCRIPTION")))==2){}
  #browser()
  return(out)
}

# Create a fact table of standardized codes and descriptions ----
fact_table <- sapply(dat, fn_standardize, simplify=FALSE) %>% bind_rows(.id = "type") %>% filter(ENCOUNTER %in% dat$encounters.csv$Id)

# Determine the number of encounters with a specified code "agitation"
summarise(fact_table, encounter_no = length(unique(ENCOUNTER)),
          encounter_w_code = length(unique(ENCOUNTER[CODE == "24199005"])),
          systemic = length(systemic_encounters),
          systemic_w_code = length(intersect(ENCOUNTER[CODE == "24199005"],systemic_encounters)))

# Create a function to determine number of encounters for each code (medication, allergy, care plan, etc.)
fn_encounter_codes <- function(xx){
  encounter_w_code <- length(unique(fact_table$ENCOUNTER[fact_table$CODE == xx]))
  systemic_w_code <- length(intersect(fact_table$ENCOUNTER[fact_table$CODE == xx],systemic_encounters))
  out <- c(systemic_w_code = systemic_w_code, encounter_w_code = encounter_w_code)
  return(out)
}

# test if fn_encounter_codes works on a test code (agitation)
fn_encounter_codes("24199005")

# Apply fn_encounter_codes to all unique codes from fact_table
encounter_code_tallies <- sapply(unique(fact_table$CODE), fn_encounter_codes, simplify=FALSE) %>% bind_rows

# Create a table of number of encounters for each code (Generated by Microsoft Copilot Basic)
encounter_code_tallies_2 <- fact_table %>%
  group_by(CODE, DESCRIPTION, type, subtype) %>%
  summarise(encounter_w_code = n_distinct(ENCOUNTER),
            systemic_w_code = sum(unique(ENCOUNTER) %in% systemic_encounters),
            local_w_code = sum(unique(ENCOUNTER) %in% local_encounters)
  ) %>% 
  mutate(total_encounters = nrow(dat$encounters.csv),
         total_systemic_encounters = length(systemic_encounters),
         total_local_encounters = length(local_encounters),
         fraction_total_w_code = encounter_w_code/total_encounters,
         fraction_systemic_w_code = systemic_w_code/total_systemic_encounters,
         fraction_local_w_code = local_w_code/total_local_encounters,
         local_no_code = total_local_encounters - local_w_code,
         nonlocal_w_code = encounter_w_code - local_w_code,
         nonlocal_no_code = total_encounters - encounter_w_code - total_local_encounters + local_w_code,
         #Haldane corrections
         haldane_local_w_code = local_w_code + 0.5,
         haldane_local_no_code = local_no_code + 0.5,
         haldane_nonlocal_w_code = nonlocal_w_code + 0.5,
         haldane_nonlocal_no_code = nonlocal_no_code + 0.5,
         #Proportions
         local_code_prevalence = haldane_local_w_code/(haldane_local_w_code +haldane_local_no_code),
         nonlocal_code_absence = haldane_nonlocal_no_code/(haldane_nonlocal_w_code+haldane_nonlocal_no_code),
         local_likelihood_ratio = local_code_prevalence/(1-nonlocal_code_absence),
         local_OR = haldane_local_w_code*haldane_nonlocal_no_code/(haldane_local_no_code*haldane_nonlocal_w_code)
         ) %>% arrange(desc(local_likelihood_ratio))

# |           | Has Code                                          | No Code                                                                                        |
# |-----------|---------------------------------------------------|------------------------------------------------------------------------------------------------|
# | Is Local  | local_w_code                                      | local_no_code = total_local_encounters - local_w_code                                          |
# | Not Local | nonlocal_w_code = encounter_w_code - local_w_code | nonlocal_no_code = total_encounters - encounter_w_code - total_local_encounters + local_w_code |

filter(fact_table, CODE == "59408-5")
filter(dat$observations.csv, CODE == "59408-5") %>% with(browser())

# Validating total encounters between dataframes ----
n_distinct(fact_table$ENCOUNTER) # number of encounters from fact_table
n_distinct(dat$encounters.csv$Id) # number of encounters from encounters.csv, where Id is ENCOUNTER
discreprant_encounters <- setdiff(fact_table$ENCOUNTER,c(dat$encounters.csv$Id,""))

## HW 1 - do tallies for systemic vs local
## HW 2 - group tallies by patient vs encounter

