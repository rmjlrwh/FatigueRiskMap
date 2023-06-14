# -------------------------------------------------------------------
###### Format diagnoses file #####
# -------------------------------------------------------------------

# Drop dfs not needed
rm(list=ls())
gc()

# -------------------------------------------------------------------
# Prep
# -------------------------------------------------------------------

# Set seed for reproducibility
set.seed(320345)

# Install pacman so future packages can be installed and loaded at the same time
install.packages("pacman")
library(pacman)

# Install & load packages at same time
# If it asks "install from sources that need compilation?" Hit "no"
pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, data.table, dplyr,beepr, RMySQL)

# Set working directory
setwd("S:/ECHO_IHI_CPRD/Data/Becky/Fatigue other diseases")

# set up MySQL connection ---- 
con <- dbConnect(
  MySQL(),
  dbname = "becky",
  user = "rmjlrwh",
  password = rstudioapi::askForPassword("Database password"),
  host = "dsh-00872msq01.idhs.ucl.ac.uk",
  port = 3306
)

# -------------------------------------------------------------------
###### Load data #####
# -------------------------------------------------------------------


# Send query for diagnosis counts
query = 'SELECT * FROM fat3_ref_diagnoses_events_all;'
rs = dbSendQuery(con, query)

# Convert results
diag_events_all_ref = fetch(rs, n=-1)

# Send query for lookups
query = 'SELECT * FROM fat3_lookup_diseases_meta;'
rs = dbSendQuery(con, query)

# Convert results
lookup_diseases_meta = fetch(rs, n=-1)

# Save results
save(lookup_diseases_meta, file = "lookup_diseases_meta.RData")

# Load other data
load("ref_cohort.RData")


# -------------------------------------------------------------------
###### identify previous/ subsequent diagnoses #####
# -------------------------------------------------------------------


# Check column types
str(diag_events_all_ref)
str(ref_cohort)

# Format dates
diag_events_all_ref <- diag_events_all_ref %>%
  mutate(eventdate = as.Date(eventdate))

ref_cohort <- ref_cohort %>%
  mutate(tod = as.Date(tod)) %>%
  mutate(deathdate = as.Date(deathdate)) %>%
  mutate(lcd = as.Date(lcd))


# Join eligibility dates
diag_events_join_ref = merge(x = diag_events_all_ref, y = ref_cohort [
  , c("e_patid","crd","uts","lcd","tod","deathdate", "indexdate", "gender", "yob_approx")]
  , by = "e_patid", all = FALSE)


# Flag eligible diagnoses
# If after crd & uts date, and before lcd, tod, or death date
diag_events_join_ref <- diag_events_join_ref |>
  mutate(diag_events_join_ref, elig = ifelse (
    
    eventdate >= crd &
      eventdate >= uts &
      
      eventdate <= lcd &
      (eventdate <= tod |  is.na(tod)) &
      (eventdate <= deathdate |  is.na(deathdate)),
    1, 0
  ))

# Flag ncras/ hes diagnoses as eligible
diag_events_join_ref <- diag_events_join_ref |>
  mutate(diag_events_join_ref, elig = ifelse (
    (data_source == "cr_icd"
     | data_source == "hes_icd"
     | data_source == "hes_opcs")
    & (eventdate <= deathdate |  is.na(deathdate)),
    1, elig
  ))

# Keep eligible diagnoses
diag_events_join_ref <- diag_events_join_ref |>
  filter(elig == 1)

# Sort chronologically by patient
diag_events_sort_ref <- diag_events_join_ref[
  with(diag_events_join_ref, order(e_patid, eventdate)),
]


# Flag 'previous diagnoses' if eligible and occurring anytime BEFORE indexdate
diag_events_sort_ref <- diag_events_sort_ref %>%
  mutate(diag_events_sort_ref, prev_diag = ifelse (
    elig == 1 &
      eventdate < indexdate,
    1, 0
  ))

# Flag 'subsequent diagnoses' if eligible and occurring < 12 months AFTER indexdate (or on same day as index)
diag_events_sort_ref <- diag_events_sort_ref %>%
  mutate(diag_events_sort_ref, sub_diag = ifelse (
    elig == 1 &
      eventdate < (indexdate %m+% months(12)) &
      eventdate >= indexdate,
    1, 0
  ))



# -------------------------------------------------------------------
###### Collapse to last previous diagnosis of each disease #####
# -------------------------------------------------------------------

# Identify latest previous diagnosis
diag_events_prev_ref <- diag_events_sort_ref %>%
  filter(elig == 1 & prev_diag == 1) %>%
  group_by(e_patid, disease_number_new) %>%
  slice_max(order_by = eventdate, n = 1, with_ties = FALSE)

# V2: Create composite variable. No duplicates found
diag_events_dups_ref <- diag_events_prev_ref %>%
  unite('pat_disease',e_patid,disease_number_new, remove= FALSE)

diag_events_dups_ref |> group_by(pat_disease) |> 
  summarise(N = n()) |>
  filter(N > 1)

# Save results
save(diag_events_prev_ref, file = "diag_events_prev_ref.RData")


# -------------------------------------------------------------------
###### Collapse to first subsequent diagnosis for each disease #####
# -------------------------------------------------------------------

# Identify first subsequent diagnosis
diag_events_sub_ref <- diag_events_sort_ref %>%
  filter(elig == 1 & sub_diag == 1) %>%
  group_by(e_patid, disease_number_new) %>%
  slice_min(order_by = eventdate, n = 1, with_ties = FALSE)

# V2: Create composite variable. No duplicates found
diag_events_dups_ref <- diag_events_sub_ref %>%
  unite('pat_disease',e_patid,disease_number_new, remove= FALSE)

diag_events_dups_ref |> group_by(pat_disease) |> 
  summarise(N = n()) |>
  filter(N > 1)

# Save results
save(diag_events_sub_ref, file = "diag_events_sub_ref.RData")

# Disconnect
dbDisconnect(db_becky)

# Drop dfs not needed
rm(list=ls())




