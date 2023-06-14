# -------------------------------------------------------------------
###### Combine patients, diagnoses -  fatigue presenters  #####
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
load("fat_cohort.RData")
load("lookup_diseases_meta.RData")


# -------------------------------------------------------------------
###### Patient level file #####
# -------------------------------------------------------------------

# Age at index
fat_cohort <- fat_cohort %>%
  mutate(age_idate = as.integer((indexdate-yob_approx)/ 365.24)) %>%
  ungroup()

# Replace 29 year olds as 30 due to only having year of birth (not exact date)
fat_cohort <- fat_cohort %>%
  mutate(age_idate = ifelse(age_idate == 29,30,age_idate))

# Age group
fat_cohort <- fat_cohort |> 
  mutate(age_cat_10 = case_when(
    fat_cohort$age_idate < 40 ~ "30-39",
    fat_cohort$age_idate < 50 ~ "40-49",
    fat_cohort$age_idate < 60 ~ "50-59",
    fat_cohort$age_idate < 70 ~ "60-69",
    fat_cohort$age_idate < 80 ~ "70-79",
    fat_cohort$age_idate < 90 ~ "80-89",
    fat_cohort$age_idate >= 90 ~ "90+"
  ),
  age_cat_10 = factor(age_cat_10))

# Save results
save(fat_cohort, file = "fat_cohort.RData")

# -------------------------------------------------------------------
###### Create patient level template file for disease counts #####
# -------------------------------------------------------------------

# List of unique patids
n_diags_fat <-  fat_cohort %>%
  select("e_patid", "gender", "age_idate", "age_cat_10", "indexdate", "pvf")

# Expand - repeat epatids for each disease num
n_diags_fat <- n_diags_fat %>%
  slice(rep(1:n(), each = 239))

# Label each repeated row with disease num
n_diags_fat <- n_diags_fat %>%
  group_by(e_patid) %>%
  mutate(disease_number_new = row_number())

# Drop two 'skipped' numbers from a previous merging of diseases
  # *IBD (merge UC and crohns)
  # Portal hypertension (portal hypertension and oesophageal varices)
n_diags_fat <- n_diags_fat %>%
  filter(disease_number_new != 72 & disease_number_new != 80)


###### Add prev and sub diagnoses flags to template file #####
# -------------------------------------------------------------------

# Load other data
load("diag_events_sub_fat.RData")
load("diag_events_prev_fat.RData")

# Merge previous disease flags for fatigue (default = ever)
n_diags_fat = merge(x = n_diags_fat, y = diag_events_prev_fat [
  , c("e_patid","disease_number_new", "prev_diag", "eventdate")]
  , by =c("e_patid", "disease_number_new"), all.x = TRUE)  %>%
  rename(prev_diag_date = eventdate) %>%
  rename(prev_diag = prev_diag)

# Drop dfs not needed
rm(diag_events_prev_fat)

# Merge subsequent disease flags for fatigue (default = < 12 months after)
n_diags_fat = merge(x = n_diags_fat, y = diag_events_sub_fat [
  , c("e_patid","disease_number_new", "sub_diag", "eventdate")]
  , by =c("e_patid", "disease_number_new"), all.x = TRUE)  %>%
  rename(sub_diag_date = eventdate)

# Drop dfs not needed
rm(diag_events_sub_fat)


# -------------------------------------------------------------------
# Merge broad disease num and prev diag period

load("lookup_diseases_meta.RData")

# Merge
n_diags_fat = merge(x = n_diags_fat, y = lookup_diseases_meta [
  , c("disease_number_new", "broad_disease_group_num", "prevdiag_period", "male_only", "female_only")]
  , by =c("disease_number_new"), all.x = TRUE) 

# -------------------------------------------------------------------




# -------------------------------------------------------------------
# Create disease flags

# Replace missing with 0
n_diags_fat <- n_diags_fat %>%
  mutate(n_diags_fat, prev_diag = ifelse (
    is.na(prev_diag)
    ,
    0, prev_diag
  ))

# Replace missing with 0
n_diags_fat <- n_diags_fat %>%
  mutate(n_diags_fat, sub_diag = ifelse (
    is.na(sub_diag)
    ,
    0, sub_diag
  ))

# 2 year previous disease flags
n_diags_fat <- n_diags_fat %>%
  mutate(prev_diag_2yr = ifelse (
    prev_diag_date >= (indexdate %m-% years(2))
    ,
    1, 0
  ))

# Replace missing with 0
n_diags_fat <- n_diags_fat %>%
  mutate(prev_diag_2yr = ifelse (
    is.na(prev_diag_2yr)
    ,
    0, prev_diag_2yr
  ))


# Main prev disease flag (non infectious disease + chronic infections = ever, infections = 2 years)
n_diags_fat <- n_diags_fat %>%
  mutate(prev_diag_main = prev_diag) |>
  mutate(prev_diag_main = ifelse (
    prevdiag_period == "2yr"
    ,
    prev_diag_2yr, prev_diag_main
  ))



# -------------------------------------------------------------------
# Replace flags for sex-impossible diseases
# -------------------------------------------------------------------

gc() 

libary(lubridate)
n_diags_fat <- n_diags_fat %>%
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
n_diags_fat <- n_diags_fat %>%
  mutate(n_diags_fat, months_diff = (interval(indexdate, sub_diag_date)) %/%
           months(1)) %>%
  mutate(n_diags_fat, months_diff = months_diff + 1)


# -------------------------------------------------------------------
# Format
# -------------------------------------------------------------------


# Add cohort tag
n_diags_fat <- n_diags_fat %>%
  mutate(cohort = 0)

# Remove vars
n_diags_fat <- n_diags_fat %>%
  select(-c(prev_diag, prev_diag_date, sub_diag_date, broad_disease_group_num, indexdate))

# Save results
save(n_diags_fat, file = "n_diags_fat.RData")



# -------------------------------------------------------------------
# Broad disease group file
# -------------------------------------------------------------------

# Add functions from functions file
source("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Code/2023_03_10//0.1_FUNCTIONS.R")

load("n_diags_fat.RData")
load("lookup_diseases_meta.RData")

lookup_diseases_meta <- lookup_diseases_meta |>
  select(c("disease_number_new", "broad_disease_group_num"))

# For PVF/CFS, exclude patients with CFS/ PVF from cohort
n_diags_fat_broad <- n_diags_fat %>%
  filter(
    (is.na(pvf) & disease_number_new == 226)
    | disease_number_new != 226)

rm("n_diags_fat")

# Drop unnecessary vars
n_diags_fat_broad <- n_diags_fat_broad |>
  select(!c("prev_diag_2yr", "prevdiag_period", "pvf","months_diff"))

# Merge broad disease group num
n_diags_fat_broad = merge(x = n_diags_fat_broad, y = lookup_diseases_meta [
  , c("disease_number_new", "broad_disease_group_num")]
  , by =c("disease_number_new"), all.x = TRUE)

n_diags_fat_broad <- n_diags_fat_broad |>
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
n_diags_fat_broad <- n_diags_fat_broad |>
  replace_missing(prev_diag_main) |>
  replace_missing(sub_diag)

save(n_diags_fat_broad, file = "n_diags_fat_broad.RData")

rm(n_diags_fat_broad)


# -------------------------------------------------------------------
###### Tidy up #####
# -------------------------------------------------------------------

# Drop dfs not needed
rm(list=ls())
gc()
