## -------------------------------------------------------------------
###### Combine aggregate counts from fat, nfp, ref m and ref f #####
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


# -------------------------------------------------------------------
# Patient level file - for counts & models: Individual diseases
# -------------------------------------------------------------------

load("n_diags_fat.RData")
load("n_diags_nfp.RData")
load("n_diags_ref_f.RData")
load("n_diags_ref_m.RData")

# Remove unnecessary vars
#Also remove fatigued patients with cfs/ pvf 
n_diags_fat <- n_diags_fat |>
  filter(
    (is.na(pvf) & disease_number_new == 226)
    | disease_number_new != 226) |>
  select(!c("months_diff", "pvf", "male_only", "female_only"))

n_diags_nfp <- n_diags_nfp |>
  select(!c("months_diff", "male_only", "female_only"))

n_diags_ref_f <- n_diags_ref_f |>
  select(!c("months_diff", "male_only", "female_only"))

n_diags_ref_m <- n_diags_ref_m |>
  select(!c("months_diff", "male_only", "female_only"))

gc()

# Combine 
n_diags <- rbind(n_diags_fat,n_diags_nfp, n_diags_ref_f, n_diags_ref_m)

# Save results
save(n_diags, file = "n_diags.RData")

rm("n_diags_fat","n_diags_nfp", "n_diags_ref_f", "n_diags_ref_m")

rm("n_diags")
gc()
#

# -------------------------------------------------------------------
# Patient level file - for counts & models: Broad disease groups
# -------------------------------------------------------------------

load("n_diags_fat_broad.RData")
load("n_diags_nfp_broad.RData")
load("n_diags_ref_f_broad.RData")
load("n_diags_ref_m_broad.RData")

gc()

# Combine 
n_diags_broad <- bind_rows(n_diags_fat_broad,n_diags_nfp_broad, n_diags_ref_f_broad, n_diags_ref_m_broad)

# Save results
save(n_diags_broad, file = "n_diags_broad.RData")

rm("n_diags_fat_broad","n_diags_nfp_broad", "n_diags_ref_f_broad", "n_diags_ref_m_broad")

rm("n_diags_broad")
gc()
#

# -------------------------------------------------------------------
# Cohorts
# -------------------------------------------------------------------

load("fat_cohort.RData")
load("nfp_cohort.RData")
load("ref_cohort.RData")

fat_cohort <- fat_cohort |>
  select(!c("yob","readcode","readcode_desc")) |>
  mutate(cohort = 0)

nfp_cohort <- nfp_cohort |>
  select(!c("eventtype")) |>
  mutate(cohort = 1)

ref_cohort <- ref_cohort |>
  select(!c("yob","dob","n_days")) |>
  mutate(cohort = 2)

cohort <- bind_rows(fat_cohort,nfp_cohort,ref_cohort)

# Save results
save(cohort, file = "cohort.RData")


# -------------------------------------------------------------------
###### Tidy up #####
# -------------------------------------------------------------------

# Drop dfs not needed
 rm(list=ls())
gc()


