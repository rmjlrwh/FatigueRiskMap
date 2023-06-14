# -------------------------------------------------------------------
###### Combine patients, diagnoses - non fatigue presenters  #####
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
setwd("S:/ECHO_IHI_CPRD/Data/Becky/Fatigue other diseases")

# Load other data
load("nfp_cohort.RData")
load("lookup_diseases_meta.RData")


# -------------------------------------------------------------------
###### Patient level file #####
# -------------------------------------------------------------------

# Age at index
nfp_cohort <- nfp_cohort %>%
  mutate(age_idate = as.integer((indexdate-yob_approx)/ 365.24)) %>%
  ungroup()

# Replace 29 year olds as 30 due to only having year of birth (not exact date)
nfp_cohort <- nfp_cohort %>%
  mutate(age_idate = ifelse(age_idate == 29,30,age_idate))

# Age group
nfp_cohort <- nfp_cohort |> 
  mutate(age_cat_10 = case_when(
    nfp_cohort$age_idate < 40 ~ "30-39",
    nfp_cohort$age_idate < 50 ~ "40-49",
    nfp_cohort$age_idate < 60 ~ "50-59",
    nfp_cohort$age_idate < 70 ~ "60-69",
    nfp_cohort$age_idate < 80 ~ "70-79",
    nfp_cohort$age_idate < 90 ~ "80-89",
    nfp_cohort$age_idate >= 90 ~ "90+"
  ),
  age_cat_10 = factor(age_cat_10))

# Save results
save(nfp_cohort, file = "nfp_cohort.RData")

# -------------------------------------------------------------------
###### Create patient level template file for disease counts #####
# -------------------------------------------------------------------

# List of unique patids
n_diags_nfp <-  nfp_cohort %>%
  select("e_patid", "gender", "age_idate", "age_cat_10", "indexdate")

# Expand - repeat  epatids for each disease num
n_diags_nfp <- n_diags_nfp %>%
  slice(rep(1:n(), each = 239))

# Label each repeated row with disease num
n_diags_nfp <- n_diags_nfp %>%
  group_by(e_patid) %>%
  mutate(disease_number_new = row_number())

# Drop two 'skipped' numbers from a previous merging of diseases
# *IBD (merge UC and crohns)
# Portal hypertension (portal hypertension and oesophageal varices)
n_diags_nfp <- n_diags_nfp %>%
  filter(disease_number_new != 72 & disease_number_new != 80)

# 
###### Add prev and sub diagnoses flags to template file #####
# -------------------------------------------------------------------

# Load other data
load("diag_events_sub_nfp.RData")
load("diag_events_prev_nfp.RData")

# Merge previous disease flags for fatigue (default = ever)
n_diags_nfp = merge(x = n_diags_nfp, y = diag_events_prev_nfp [
  , c("e_patid","disease_number_new", "prev_diag", "eventdate")]
  , by =c("e_patid", "disease_number_new"), all.x = TRUE)  %>%
  rename(prev_diag_date = eventdate) %>%
  rename(prev_diag = prev_diag)

# Drop dfs not needed
rm(diag_events_prev_nfp)

# Merge subsequent disease flags for fatigue (default = < 12 months after)
n_diags_nfp = merge(x = n_diags_nfp, y = diag_events_sub_nfp [
  , c("e_patid","disease_number_new", "sub_diag", "eventdate")]
  , by =c("e_patid", "disease_number_new"), all.x = TRUE)  %>%
  rename(sub_diag_date = eventdate)

# Drop dfs not needed
rm(diag_events_sub_nfp)



# -------------------------------------------------------------------
# Merge broad disease num

load("lookup_diseases_meta.RData")

# Merge
n_diags_nfp = merge(x = n_diags_nfp, y = lookup_diseases_meta [
  , c("disease_number_new", "broad_disease_group_num", "prevdiag_period",  "male_only", "female_only")]
  , by =c("disease_number_new"), all.x = TRUE) 



# -------------------------------------------------------------------
# Create disease flags

# Replace missing with 0
n_diags_nfp <- n_diags_nfp %>%
  mutate(n_diags_nfp, prev_diag = ifelse (
    is.na(prev_diag)
    ,
    0, prev_diag
  ))

# Replace missing with 0
n_diags_nfp <- n_diags_nfp %>%
  mutate(n_diags_nfp, sub_diag = ifelse (
    is.na(sub_diag)
    ,
    0, sub_diag
  ))

# 2 year previous disease flags
n_diags_nfp <- n_diags_nfp %>%
  mutate(n_diags_nfp, prev_diag_2yr = ifelse (
    prev_diag_date >= (indexdate %m-% years(2))
    ,
    1, 0
  ))

# Replace missing with 0
n_diags_nfp <- n_diags_nfp %>%
  mutate(prev_diag_2yr = ifelse (
    is.na(prev_diag_2yr)
    ,
    0, prev_diag_2yr
  ))

# Main prev disease flag (non infectious disease + chronic infections = ever, infections = 2 years)
n_diags_nfp <- n_diags_nfp %>%
  mutate(prev_diag_main = prev_diag) |>
  mutate(prev_diag_main = ifelse (
    prevdiag_period == "2yr"
    ,
    prev_diag_2yr, prev_diag_main
  ))



# -------------------------------------------------------------------
# Replace flags for sex-impossible diseases
# -------------------------------------------------------------------


n_diags_nfp <- n_diags_nfp %>%
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
n_diags_nfp <- n_diags_nfp %>%
  mutate(n_diags_nfp, months_diff = (interval(indexdate, sub_diag_date)) %/%
           months(1)) %>%
  mutate(n_diags_nfp, months_diff = months_diff + 1)

# Add cohort tag
n_diags_nfp <- n_diags_nfp %>%
  mutate(cohort = 1)

# Remove vars
n_diags_nfp <- n_diags_nfp %>%
  select(-c(prev_diag, prev_diag_date, sub_diag_date, broad_disease_group_num, indexdate))

# Save results
save(n_diags_nfp, file = "n_diags_nfp.RData")


# -------------------------------------------------------------------
# Broad disease group file
# -------------------------------------------------------------------

# Add functions from functions file
source("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Code/2023_03_10//0.1_FUNCTIONS.R")

load("n_diags_nfp.RData")
load("lookup_diseases_meta.RData")

lookup_diseases_meta <- lookup_diseases_meta |>
  select(c("disease_number_new", "broad_disease_group_num"))

# Drop unnecessary vars
n_diags_nfp_broad <- n_diags_nfp |>
  select(!c("prev_diag_2yr", "prevdiag_period", "months_diff"))

rm("n_diags_nfp")

# Merge broad disease group num
n_diags_nfp_broad = merge(x = n_diags_nfp_broad, y = lookup_diseases_meta [
  , c("disease_number_new", "broad_disease_group_num")]
  , by =c("disease_number_new"), all.x = TRUE)

n_diags_nfp_broad <- n_diags_nfp_broad |>
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
n_diags_nfp_broad <- n_diags_nfp_broad |>
  replace_missing(prev_diag_main) |>
  replace_missing(sub_diag)

save(n_diags_nfp_broad, file = "n_diags_nfp_broad.RData")

rm(n_diags_nfp_broad)

# -------------------------------------------------------------------
###### Tidy up #####
# -------------------------------------------------------------------

# Drop dfs not needed
rm(list=ls())
gc()
