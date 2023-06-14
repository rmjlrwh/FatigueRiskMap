## -------------------------------------------------------------------
###### Create fatigue cohort #######
# -------------------------------------------------------------------


# Clear data frames first
rm(list=ls())
gc()

# Install pacman so future packages can be installed and loaded at the same time
install.packages("pacman")
library(pacman)

# Install & load packages at same time
# If it asks "install from sources that need compilation?" Hit "no"
pacman::p_load(rio,tidyverse,here,skimr,binom, tidyr,lubridate, 
               data.table, dplyr, RMySQL, DBI)

# Set working directory
setwd("S:/ECHO_IHI_CPRD/Data/Becky/Fatigue other diseases")

# -------------------------------------------------------------------
# ## Load data
# -------------------------------------------------------------------

# Set seed for reproducibility
set.seed(439575)

# set up MySQL connection ---- 
con <- dbConnect(
  MySQL(),
  dbname = "becky",
  user = "rmjlrwh",
  password = rstudioapi::askForPassword("Database password"),
  host = "dsh-00872msq01.idhs.ucl.ac.uk",
  port = 3306
)

# -- -----------------------------------------------------------------------
#   -- A) Extract fatigue events
# -- -----------------------------------------------------------------------
#   
#   -- Select fatigue events 
# -- Only if occurring between study start (1st Jan 2007) and study end (31st Oct 2021)
# -- Also select variables to define further eligiblity criteria later
# -- Only select events for patients in the 'cohort' not 'random sample'

query <- dbSendQuery(
  conn = con,
  statement = "
select d.d_cprdclin_key, d.e_patid, d.eventdate, d.medcode, d.symptom_desc, d.symptom_number, 
l4.readcode, l4.descc as readcode_desc
, l.crd,  l.tod, l.deathdate, l.yob, l.gender
,l3.uts, l3.lcd

from 18_299_Lyratzopoulos_e2.query_cohort_clinical d
LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_patient l ON  d.e_patid = l.e_patid
LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_linkage_eligibility_gold l2 ON  d.e_patid = l2.e_patid
LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_practice l3 ON  l2.e_pracid = l3.e_pracid
LEFT JOIN lookup_tables.lookup_medical l4 ON  d.medcode = l4.medcode
left join 18_299_Lyratzopoulos_e2.cprd_cohort_file l5 on d.e_patid = l5.e_patid

where symptom_number = 8 -- fatigue
AND d.eventdate >= makedate(2007, 1)
AND d.eventdate <= makedate(2021, 304)
AND l5.e_patid is not null -- cohort sample
;
"
)

# n = -1 => take all records
fat_events <- dbFetch(query, n = -1) 
dbClearResult(query)

# -------------------------------------------------------------------
###### Define potentially eligible fatigue events #####
# -------------------------------------------------------------------

# Year of birth as date variable
fat_events <- fat_events %>%
  mutate(yob_approx = as.Date(ISOdate(fat_events$yob,8,1)))

# Year turned 30
fat_events <- fat_events %>%
  mutate(age30date = fat_events$yob_approx %m+% years(30))

# Year turned 100
fat_events <- fat_events %>%
  mutate(age100date = fat_events$yob_approx %m+% years(100))

# Format dates as date
fat_events <- fat_events %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  mutate(crd = as.Date(crd)) %>%
  mutate(uts = as.Date(uts)) %>%
  mutate(lcd = as.Date(lcd)) %>%
  mutate(tod = as.Date(tod)) %>%
  mutate(deathdate = as.Date(deathdate)) |>
  mutate(age30date = as.Date(age30date, format = "%Y-%m-%d")) |>
  mutate(age100date = as.Date(age100date, format = "%Y-%m-%d"))

# Study start & end values

# Study_start: Cohort from CPRD is from start 2007
study_start <- as.Date("2007-01-01")

# Study end: NCRAS data will be until end of 2018, 
# so I should include fatigue up to end of 2017
study_end <- as.Date("2017-12-31")

# Follow up start
# After: study start, 2 years After UTS and CRD date, after age 30
fat_events <- fat_events |> 
  mutate(fu_start = pmax((crd %m+% years(2)),
                         (uts %m+% years(2)),
                         age30date,
                         study_start
                         , na.rm = TRUE))

# Follow up end
# Before: study end, before lcd or tod, death date, age 100, or study end
fat_events <- fat_events |> 
  mutate(fu_end = pmin(lcd,
                       tod,
                       deathdate,
                       age100date,
                       study_end
                       , na.rm = TRUE))

# Potentially eligible flag
fat_events <- fat_events |>
  mutate(fat_events, pot_elig = ifelse (
    eventdate >= (crd %m+% years(2)) &
      eventdate >= (uts %m+% years(2))  &
      eventdate >= age30date &
      eventdate >= study_start &
      
      eventdate <= lcd & 
       (eventdate <= tod | is.na(tod))   & 
      # (eventdate <= (tod %m-% years(1)) | is.na(tod))   & 
  
      (eventdate <= deathdate |  is.na(deathdate)) &
      eventdate <= age100date &
      eventdate <= study_end,
    1, 0
  ))



# -------------------------------------------------------------------
###### Identify fatigue with fatigue < 2 years before #####
# -------------------------------------------------------------------

# Sort chronologically per patient

# Method 1
fat_events_sort1 <- fat_events[
  with(fat_events, order(e_patid, eventdate)),
]

# v2
fat_events_sort1 <- fat_events_sort1 %>%
  group_by(e_patid) %>%
  mutate(eventdate_lag1 = lag(eventdate)) %>%
  fill(eventdate_lag1) %>%
  ungroup()

# Date 2 year before fatigue
fat_events_sort1 <- fat_events_sort1 %>%
  mutate(eventdate = as.Date(eventdate)) %>%
  mutate(eventdate_pre_2yr = fat_events_sort1$eventdate %m-% years(2))

# Flag 'eligible fatigue events' if potentially eligible and no prev fatigue event < 2 year before
fat_events_sort1 <- fat_events_sort1 %>%
  mutate(fat_events_sort1, elig = ifelse (
    pot_elig == 1 &
      (eventdate_lag1 < eventdate_pre_2yr | is.na(eventdate_lag1))
    ,
    1, 0
  ))


# -------------------------------------------------------------------
###### Identify first (index) fatigue presentation #####
# -------------------------------------------------------------------

# Identify first eligible fatigue presentation
fat_cohort <- fat_events_sort1 %>%
  filter(elig == 1) %>%
  group_by(e_patid) %>%
  slice_min(order_by = eventdate, n = 1, with_ties = FALSE) %>%
  ungroup()

# Check one row per patient
length(unique(fat_cohort$e_patid))
length(fat_cohort$e_patid)

## Drop columns that aren't needed in fat cohort
fat_cohort <- fat_cohort |>
  select(-c(d_cprdclin_key, medcode, symptom_desc, symptom_number
            , pot_elig, eventdate_lag1, eventdate_pre_2yr, elig ))


# -------------------------------------------------------------------
###### Identify patients with CFS or PVF on same day as index fatigue date #####
# -------------------------------------------------------------------

# Merge index date into fatigue events
fat_cohort_sameday = merge(x = fat_events_sort1, y = fat_cohort [
  , c("e_patid","eventdate")]
  , by = c("e_patid", "eventdate") , all = FALSE)

# Keep fatigue events that are CFS or PVF, then pick just one row per patient
pvf <- fat_cohort_sameday |>
  mutate(fat_cohort_sameday, pvf = ifelse (
    readcode == "R007400" | readcode == "F286.12" |
      readcode == "F286.14" | readcode == "R007411" |
      readcode == "F286.13" |
      readcode == "F286.00" | readcode == "F286.11" |
      readcode == "8Q1..00" | readcode == "F286100" |
      readcode == "F286000" | readcode == "F286200" |
      readcode == "8HkW.00",
        1, 0)) |>
  filter(pvf == 1) |>
  group_by(e_patid) |>
  slice_min(order_by = eventdate, n = 1, with_ties = FALSE) 


# Merge flag for CFS and PVF into fatigue cohort
fat_cohort = merge(x = fat_cohort, y = pvf [
  , c("e_patid", "pvf")]
  , by = c("e_patid") , all.x = TRUE)


# rename eventdate 
fat_cohort <- fat_cohort |>
  rename(indexdate = eventdate)

# Save results
save(fat_cohort, file = "fat_cohort.RData")


# -------------------------------------------------------------------
###### Send back to SQL #####
# -------------------------------------------------------------------

# Send query for fatigue events
query = 'DROP TABLE IF EXISTS fat3_fat_cohort;'
rs = dbSendQuery(con, query)

# Send data to SQL
dbWriteTable(con, "fat3_fat_cohort",fat_cohort)

# Create indexes/ format - do in SQL at diagnoses stage


# -------------------------------------------------------------------
###### Inclusion/ exclusion counts for Fig 1 #####
# -------------------------------------------------------------------

# Step 1. Patients with any symptom with UTS follow up 2007 - Oct 2021 while aged 30-99 years

# Select info about cohort group patients for count 1
query <- dbSendQuery(
  conn = con,
  statement = "

select d.e_patid
, l.crd,  l.tod, l.deathdate, l.yob, l.gender
,l3.uts, l3.lcd

, case when yob is not null
then makedate(yob,182)
else null
end as dob

from 18_299_Lyratzopoulos_e2.cprd_cohort_file d
LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_patient l ON  d.e_patid = l.e_patid
LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_linkage_eligibility_gold l2 ON  d.e_patid = l2.e_patid
LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_practice l3 ON  l2.e_pracid = l3.e_pracid

;
"
)

# n = -1 => take all records
fat_count1 <- dbFetch(query, n = -1) 
dbClearResult(query)

fat_count1 <- fat_count1 |>
  mutate(step = 1) |>
  group_by(step) |>
  dplyr::summarize(P_fat = n_distinct(e_patid)) 


# Select info about cohort group no. records in clinical for count 1
query <- dbSendQuery(
  conn = con,
  statement = "
select count(e_patid) as R_fat
from
(select l.e_patid, d.eventdate
  
  from 18_299_Lyratzopoulos_e2.cprd_consultation d
  right join 18_299_Lyratzopoulos_e2.cprd_cohort_file l on d.e_patid = l.e_patid
) as tbl1
;
"
)

# n = -1 => take all records
fat_count1_r <- dbFetch(query, n = -1) 
dbClearResult(query)

# Combine patinet and record counts
fat_count1 = cbind(fat_count1, fat_count1_r)
  

# Step 2. 1+ fatigue record 2007-2017
fat_step2 <- fat_events |>
  filter(eventdate >= study_start & 
           eventdate <= study_end
    ) |>
  mutate(step = 2)

fat_count2 <- fat_step2 |>
  group_by(step) |>
  dplyr::summarize(P_fat = n_distinct(e_patid),
                   R_fat = n()) 


# Step 3. 1+ fatigue record aged 30-99 years with UTS follow up > 2 years before
fat_step3 <- fat_step2 |>
  filter(      eventdate >= age30date &
               eventdate <= age100date &
             (eventdate <= deathdate |  is.na(deathdate))
  ) |>
  mutate(step = 3)

fat_count3 <- fat_step3 |>
  group_by(step) |>
  dplyr::summarize(P_fat = n_distinct(e_patid),
                   R_fat = n()) 

# Step 4. 1+ fatigue record within UTS
fat_step4 <- fat_step3 |>
  filter(      eventdate >= (crd %m+% years(2)) &
                 eventdate >= (uts %m+% years(2))  &
                 eventdate <= lcd & 
               (eventdate <= tod | is.na(tod))
               # (eventdate <= (tod %m-% years(1)) | is.na(tod))
               
  ) |>
  mutate(step = 4)

fat_count4 <- fat_step4 |>
  group_by(step) |>
  dplyr::summarize(P_fat = n_distinct(e_patid),
                   R_fat = n()) 

# Step 5. 1+ fatigue record without a fatigue record < 2 years before ("new onset fatigue")
fat_count5 <- fat_events_sort1 |>
  filter(elig == 1) |>
  mutate(step = 5) |>
  group_by(step) |>
  dplyr::summarize(P_fat = n_distinct(e_patid),
                   R_fat = n()) 

# Append counts
fat_count <- rbind(fat_count1, fat_count2, fat_count3, fat_count4, fat_count5)


# Save results
save(fat_count, file = "fat_count.RData")

# -------------------------------------------------------------------
###### Disconnect #####
# -------------------------------------------------------------------


# Kill all connections
killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)
    + dbDisconnect(con)
}

# Remove redundant data frames
rm("data", "db_becky", "db_lyrat_e2", "mydb", "rs")

