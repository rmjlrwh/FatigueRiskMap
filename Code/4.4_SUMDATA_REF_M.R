# -------------------------------------------------------------------
###### Combine patients, diagnoses - reference group (registered patietns) FEMALES #####
# Males & females separated for ref group because of large files
# -------------------------------------------------------------------

rm(list=ls())
gc()

# -------------------------------------------------------------------
# Prep
# -------------------------------------------------------------------

# Install pacman so future packages can be installed and loaded at the same time
install.packages("pacman")
library(pacman)

# Install & load packages at same time
# If it asks "install from sources that need compilation?" Hit "no"
pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, data.table, dplyr,beepr, RMySQL)

# Set working directory
setwd("S:/ECHO_IHI_CPRD/Data/Becky/fatigue other diseases")

# Load other data
load("ref_cohort.RData")


# -------------------------------------------------------------------
###### Create patient level template file for disease counts #####
# -------------------------------------------------------------------

# List of unique patids
n_diags_ref <-  ref_cohort %>%
  filter(gender == 1) %>% 
  select("e_patid")

# Expand - repeat 240 epatids for each disease num
n_diags_ref <- n_diags_ref %>%
  slice(rep(1:n(), each = 239))

# Label each repeated row with disease num
n_diags_ref <- n_diags_ref %>%
  group_by(e_patid) %>%
  mutate(disease_number_new = row_number())

# Drop two 'skipped' numbers from a previous merging of diseases
# *IBD (merge UC and crohns)
# Portal hypertension (portal hypertension and oesophageal varices)
n_diags_ref <- n_diags_ref %>%
  filter(disease_number_new != 72 & disease_number_new != 80)

# Drop dfs not needed
rm(ref_cohort)



# 
###### Add prev and sub diagnoses flags to template file #####
# -------------------------------------------------------------------

# Load other data
load("diag_events_prev_ref.RData")

# Merge previous disease flags for reference (default = ever)
n_diags_ref = merge(x = n_diags_ref, y = diag_events_prev_ref [
  , c("e_patid","disease_number_new","eventdate")]
  , by =c("e_patid", "disease_number_new"), all.x = TRUE) 

# Drop dfs not needed
rm(diag_events_prev_ref)

# Rename eventdate
n_diags_ref <- n_diags_ref %>%
  rename(prev_diag_date = eventdate)

# Load next merge data
load("diag_events_sub_ref.RData")

# Merge subsequent disease flags for reference (default = < 12 months after)
n_diags_ref = merge(x = n_diags_ref, y = diag_events_sub_ref [
  , c("e_patid","disease_number_new", "eventdate")]
  , by =c("e_patid", "disease_number_new"), all.x = TRUE) 

# Drop dfs not needed
rm(diag_events_sub_ref)

# Rename eventdate
n_diags_ref <- n_diags_ref %>%
  rename(sub_diag_date = eventdate)

# -------------------------------------------------------------------
###### Patient level file #####
# -------------------------------------------------------------------

load("ref_cohort.RData")

# Age at index
ref_cohort <- ref_cohort %>%
  mutate(age_idate = as.integer((indexdate-yob_approx)/ 365.24)) %>%
  ungroup()

# Replace 29 year olds as 30 due to only having year of birth (not exact date)
ref_cohort <- ref_cohort %>%
  mutate(age_idate = ifelse(age_idate == 29,30,age_idate))

# Age group
ref_cohort <- ref_cohort |> 
  mutate(age_cat_10 = case_when(
    ref_cohort$age_idate < 40 ~ "30-39",
    ref_cohort$age_idate < 50 ~ "40-49",
    ref_cohort$age_idate < 60 ~ "50-59",
    ref_cohort$age_idate < 70 ~ "60-69",
    ref_cohort$age_idate < 80 ~ "70-79",
    ref_cohort$age_idate < 90 ~ "80-89",
    ref_cohort$age_idate >= 90 ~ "90+"
  ),
  age_cat_10 = factor(age_cat_10))

# Save results
save(ref_cohort, file = "ref_cohort.RData")

# Add patient info
n_diags_ref = merge(x = n_diags_ref, y = ref_cohort [
  , c("e_patid", "gender","age_idate", "age_cat_10", "indexdate")]
  , by =c("e_patid"), all.x = TRUE) 

# Drop dfs not needed
rm(ref_cohort)

# -------------------------------------------------------------------

# Merge broad disease num

load("lookup_diseases_meta.RData")

# Merge
n_diags_ref = merge(x = n_diags_ref, y = lookup_diseases_meta [
  , c("disease_number_new", "broad_disease_group_num", "prevdiag_period", "male_only", "female_only")]
  , by =c("disease_number_new"), all.x = TRUE) 


# -------------------------------------------------------------------
# Create disease flags


#  Prev diag flag (ever)
n_diags_ref <- n_diags_ref %>%
  mutate(n_diags_ref, prev_diag = ifelse (
    is.na(prev_diag_date)
    ,
    0, 1
  ))

# 2 year previous disease flags
n_diags_ref <- n_diags_ref %>%
  mutate(n_diags_ref, prev_diag_2yr = ifelse (
    prev_diag_date >= (indexdate %m-% years(2))
    ,
    1, 0
  ))

# Replace missing with 0
n_diags_ref <- n_diags_ref %>%
  mutate(prev_diag_2yr = ifelse (
    is.na(prev_diag_2yr)
    ,
    0, prev_diag_2yr
  ))

# Main prev disease flag (non infectious disease + chronic infections = ever, infections = 2 years)
n_diags_ref <- n_diags_ref %>%
  mutate(prev_diag_main = prev_diag) |>
  mutate(prev_diag_main = ifelse (
    prevdiag_period == "2yr"
    ,
    prev_diag_2yr, prev_diag_main
  ))

# Sub diag flag (default = 12 months)
n_diags_ref <- n_diags_ref %>%
  mutate(n_diags_ref, sub_diag = ifelse (
    is.na(sub_diag_date)
    ,
    0, 1
  ))


# -------------------------------------------------------------------
# Replace flags for sex-impossible diseases
# -------------------------------------------------------------------


n_diags_ref <- n_diags_ref %>%
  mutate(
    
    prev_diag_main = ifelse (
      (male_only == 1 & gender == 2) | (female_only == 1 & gender == 1),
      0, prev_diag_main
    ),
    
    prev_diag_2yr = ifelse (
      (male_only == 1 & gender == 2) | (female_only == 1 & gender == 1),
      0, prev_diag_2yr
    ),
    
    prev_diag = ifelse (
      (male_only == 1 & gender == 2) | (female_only == 1 & gender == 1),
      0, prev_diag
    ),
    
    sub_diag = ifelse (
      (male_only == 1 & gender == 2) | (female_only == 1 & gender == 1),
      0, sub_diag
    ),
    
    prev_diag_date = case_when (
      (male_only == 1 & gender == 2) | (female_only == 1 & gender == 1)
      ~ NA_Date_, TRUE ~ prev_diag_date
    ),
    
    sub_diag_date = case_when (
      (male_only == 1 & gender == 2) | (female_only == 1 & gender == 1)
      ~ NA_Date_, TRUE ~ sub_diag_date
    )
    
  )


# Months between indexdate and subsequent diagnosi
n_diags_ref <- n_diags_ref %>%
  mutate(n_diags_ref, months_diff = (interval(indexdate, sub_diag_date)) %/%
           months(1)) %>%
  mutate(n_diags_ref, months_diff = months_diff + 1)

# Remove vars
n_diags_ref <- n_diags_ref %>%
  select(-c(prev_diag, prev_diag_date, sub_diag_date, broad_disease_group_num, indexdate))

# Add cohort tag
n_diags_ref_m <- n_diags_ref %>%
  mutate(cohort = 2)

# Save results
save(n_diags_ref_m, file = "n_diags_ref_m.RData")

# -------------------------------------------------------------------
# Broad disease group file
# -------------------------------------------------------------------

# Add functions from functions file
source("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Code/2023_03_10//0.1_FUNCTIONS.R")

load("n_diags_ref_m.RData")
load("lookup_diseases_meta.RData")

lookup_diseases_meta <- lookup_diseases_meta |>
  select(c("disease_number_new", "broad_disease_group_num"))

# Drop unnecessary vars
n_diags_ref_m_broad <- n_diags_ref_m |>
  select(!c("prev_diag_2yr", "prevdiag_period", "months_diff"))

rm("n_diags_ref_m")

# Merge broad disease group num
n_diags_ref_m_broad = merge(x = n_diags_ref_m_broad, y = lookup_diseases_meta [
  , c("disease_number_new", "broad_disease_group_num")]
  , by =c("disease_number_new"), all.x = TRUE)

n_diags_ref_m_broad <- n_diags_ref_m_broad |>
  select(-c(disease_number_new)) |>
  group_by(cohort, broad_disease_group_num, e_patid) |>
  dplyr::summarize(
    prev_diag_main = prev_diag_main[which(prev_diag_main == 1)[1]],
    sub_diag = sub_diag[which(sub_diag == 1)[1]],
    gender = gender[[1]],
    age_idate = age_idate[[1]],
    age_cat_10 = age_cat_10[[1]]
  ) 

# FUN!
# Replace missing
n_diags_ref_m_broad <- n_diags_ref_m_broad |>
  replace_missing(prev_diag_main) |>
  replace_missing(sub_diag)

save(n_diags_ref_m_broad, file = "n_diags_ref_m_broad.RData")

rm(n_diags_ref_m_broad)

# -------------------------------------------------------------------
###### Tidy up #####
# -------------------------------------------------------------------

# Drop dfs not needed
rm(list=ls())
gc()
