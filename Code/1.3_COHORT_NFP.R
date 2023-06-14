## -------------------------------------------------------------------
###### Create non fatigue presenter (NFP) cohort #######
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
               data.table, dplyr, RMySQL, DBI, writexl)

# Set working directory
setwd("S:/ECHO_IHI_CPRD/Data/Becky/Fatigue other diseases")

# Set seed for reproducibility
set.seed(439575)

# -------------------------------------------------------------------
# ## SQL connection
# -------------------------------------------------------------------

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
#   -- Extract all reference group patients
# -- -----------------------------------------------------------------------

query <- dbSendQuery(
  conn = con,
  statement = "
Select * from becky.fat3_ref_patients;
"
)
# n = -1 => take all records
nfp_cohort_prov <- dbFetch(query, n = -1) 
dbClearResult(query)


# -- -----------------------------------------------------------------------
#   -- Select consutlations in reference patients
# -- -----------------------------------------------------------------------
# 

# # TAKES A DAY! RERUN ONLY WHEN NEEDED AND SECTION BY SECTION


# -- Select any consultations, if:
#   -- After: nfp study start (Jul 2011),
# -- Before: nfp study end (Jul 2012)

query <- dbSendQuery(
  conn = con,
  statement = "
drop table if exists  becky.fat3_nfp_consultations;
"
)

query <- dbSendQuery(
  conn = con,
  statement = "
create table becky.fat3_nfp_consultations
(select d.e_patid, d.eventdate

  from 18_299_Lyratzopoulos_e2.cprd_consultation d
  inner join becky.fat3_ref_patients l on d.e_patid = l.e_patid

  where d.eventdate >= makedate(2011, 182)

  AND d.eventdate <= makedate(2012, 182)
)
;
"
)

query <- dbSendQuery(
  conn = con,
  statement = "
CREATE INDEX `e_patid` ON becky.fat3_nfp_consultations (`e_patid`);
"
)

query <- dbSendQuery(
  conn = con,
  statement = "
CREATE INDEX `eventdate` ON becky.fat3_nfp_consultations (`eventdate`);
"
)


# -- -----------------------------------------------------------------------
#   -- Extract fatigue events for reference group
# -- -----------------------------------------------------------------------


#  RERUN ONLY WHEN NEEDED AND MAY NEED TO BE SECTION BY SECTION

query <- dbSendQuery(
  conn = con,
  statement = "
drop table if exists  becky.fat3_ref_fatigue_events;
"
)

query <- dbSendQuery(
  conn = con,
  statement = "
create table becky.fat3_ref_fatigue_events
(select d.e_patid, d.eventdate
  from 18_299_Lyratzopoulos_e2.query_cohort_clinical d
  inner join becky.fat3_ref_patients l on d.e_patid = l.e_patid

  where symptom_number = 8 -- fatigue

  AND l.e_patid is not null 
)
;
"
)

query <- dbSendQuery(
  conn = con,
  statement = "
CREATE INDEX `e_patid` ON becky.fat3_ref_fatigue_events (`e_patid`);
"
)


# -- -----------------------------------------------------------------------
#   -- Combine consultations and fatigue events
# -- -----------------------------------------------------------------------
  
query <- dbSendQuery(
  conn = con,
  statement = "
Select *
  from

(
  select e_patid, eventdate, 
  case when e_patid is not null then 'c' else null end as eventtype
  from becky.fat3_nfp_consultations d 
  
  UNION ALL
  
  select e_patid, eventdate,
  case when e_patid is not null then 'f' else null end as eventtype
  from becky.fat3_ref_fatigue_events d
  
) as tablename
;
"
)
# n = -1 => take all records
nfp_cons_fat <- dbFetch(query, n = -1) 
dbClearResult(query)


# -------------------------------------------------------------------
###### Define potentially eligible consultations #####
# -------------------------------------------------------------------

# Year of birth as date variable
nfp_cohort_prov <- nfp_cohort_prov %>%
  mutate(yob_approx = as.Date(ISOdate(nfp_cohort_prov$yob,8,1)))

# Year turned 30
nfp_cohort_prov <- nfp_cohort_prov %>%
  mutate(age30date = nfp_cohort_prov$yob_approx %m+% years(30))

# Year turned 100
nfp_cohort_prov <- nfp_cohort_prov %>%
  mutate(age100date = nfp_cohort_prov$yob_approx %m+% years(100))

#Format eventdate
nfp_cohort_prov <- nfp_cohort_prov %>%
  mutate(
    crd = as.Date(crd, format = "%Y-%m-%d"),
    uts = as.Date(uts, format = "%Y-%m-%d"),
    lcd = as.Date(lcd, format = "%Y-%m-%d"),
    tod = as.Date(tod, format = "%Y-%m-%d"),
    deathdate = as.Date(deathdate, format = "%Y-%m-%d"),
    age30date = as.Date(age30date, format = "%Y-%m-%d"),
    age100date = as.Date(age100date, format = "%Y-%m-%d")
  )

nfp_cons_fat <- nfp_cons_fat %>%
  mutate(
    eventdate = as.Date(eventdate, format = "%Y-%m-%d")
  )
    

# Study start & end values

# Study_start: July 2011
study_start <- as.Date("2011-07-01")

# Study end: July 2012 
study_end <- as.Date("2012-07-01")

# Follow up start
# After: study start, 2 years After UTS and CRD date, after age 30
nfp_cohort_prov <- nfp_cohort_prov |> 
  mutate(fu_start = pmax((crd %m+% years(2)),
                         (uts %m+% years(2)),
                         age30date,
                         study_start
                         , na.rm = TRUE))

# Follow up end
# Before: study end, 1 year before lcd or tod, death date, age 100, or study end
nfp_cohort_prov <- nfp_cohort_prov |> 
  mutate(fu_end = pmin(lcd,
                       tod,
                       deathdate,
                       age100date,
                       study_end
                       , na.rm = TRUE))

# Merge start/ end dates
nfp_cons_fat = merge(x = nfp_cons_fat, y = nfp_cohort_prov [
  , c("e_patid","fu_start","fu_end","crd","uts","age30date","lcd","tod","deathdate","age100date")]
  , by = "e_patid", all = FALSE)


# Potentially eligible flag
nfp_cons_fat <- nfp_cons_fat |>
  mutate(nfp_cons_fat, pot_elig = ifelse (
    eventdate >= fu_start &
      eventdate <= fu_end,
    1, 0
  ))


# Counts for fig 1
# 1+ consultation with UTS follow up while aged 30-99 years
nfp_step3 <- nfp_cons_fat |>
  filter( eventtype == "c" &
            eventdate >= age30date &
            eventdate <= age100date &
           (eventdate <= deathdate | is.na(deathdate))
          
  ) 


nfp_count3 <- nfp_step3 |>
  mutate(step = 3) |>
  group_by(step) |>
  dplyr::summarize(P_nfp = n_distinct(e_patid),
                   R_nfp = n()) 

# 1+ consultation within UTS follow up
nfp_step4 <- nfp_cons_fat |>
  filter( eventtype == "c" 
          & eventdate >= age30date 
          & eventdate <= age100date 
          & (eventdate <= deathdate | is.na(deathdate)) 
           & eventdate >= study_start 
            & eventdate <= study_end 
            & eventdate >= (crd %m+% years(2)) 
            & eventdate >= (uts %m+% years(2))  
          &  eventdate <= lcd
           & (eventdate <= tod | is.na(tod))
             ) 


nfp_count4 <- nfp_step4 |>
  mutate(step = 4) |>
  group_by(step) |>
  dplyr::summarize(P_nfp = n_distinct(e_patid),
                   R_nfp = n()) 


# -------------------------------------------------------------------
###### Identify consultations with fatigue < 2 years before #####
# -------------------------------------------------------------------

# Sort chronologically per patient

# Method 1
nfp_cons_fat_sort1 <- nfp_cons_fat[
  with(nfp_cons_fat, order(e_patid, eventdate)),
]

# Add prev fatigue event - must specify if fatigue

# v1
nfp_cons_fat_sort1 <- nfp_cons_fat_sort1 %>%
  group_by(e_patid) %>%
  mutate(eventdate_lag1 = if_else(lag(eventtype) == "f",
                                  lag(eventdate), as.Date(NA))) %>%
  fill(eventdate_lag1) %>%
  ungroup()

# Date 1 year before consultation
nfp_cons_fat_sort1 <- nfp_cons_fat_sort1 %>%
  mutate(eventdate_pre_2yr = nfp_cons_fat_sort1$eventdate %m-% years(2))

# Flag 'eligible consultations' if potentially eligible and no prev fatigue event < 1 year before
nfp_cons_fat_sort1 <- nfp_cons_fat_sort1 %>%
  mutate(nfp_cons_fat_sort1, elig = ifelse (
    pot_elig == 1 & eventtype == "c" &
      (eventdate_lag1 < eventdate_pre_2yr | is.na(eventdate_lag1))
    ,
    1, 0
  ))

# Flag 'eligible consutlations' if no fatigue on same day
nfp_fat <- nfp_cons_fat_sort1 |>
  filter(eventtype == "f") |>
  select(e_patid, eventdate) |>
  mutate(fat_sameday = 1)

nfp_cons_fat_sort1 <- nfp_cons_fat_sort1 |>
  left_join(nfp_fat, by=c('e_patid','eventdate'))
  
nfp_cons_fat_sort1 <- nfp_cons_fat_sort1 |>
  mutate(elig = 
           ifelse(fat_sameday %in% "1"
                  ,
                  0, elig
  ))


# -------------------------------------------------------------------
###### Identify RANDOM (index) consultation #####
# -------------------------------------------------------------------

# Identify random eligible consultation
nfp_cohort <- nfp_cons_fat_sort1 %>%
  filter(elig == 1) %>%
  group_by(e_patid) %>%
  slice_sample(n = 1) %>%
  rename(indexdate = eventdate) %>%
  ungroup()

# Check one row per patient
length(unique(nfp_cohort$e_patid))
length(nfp_cohort$e_patid)

# # Add extra patient info from ref cohort
nfp_cohort = merge(x = nfp_cohort, y = nfp_cohort_prov [
  , c("e_patid", "gender", "yob_approx")]
  , by = "e_patid", all = FALSE)

## Drop columns not needed
nfp_cohort <- nfp_cohort |>
  select(-c(pot_elig, eventdate_lag1, eventdate_pre_2yr, elig, fat_sameday ))

# Save results
save(nfp_cohort, file = "nfp_cohort.RData")


# -------------------------------------------------------------------
###### Send back to SQL to get diagnoses #####
# -------------------------------------------------------------------


# Send query for fatigue events
query = 'DROP TABLE IF EXISTS fat3_nfp_cohort;'
rs = dbSendQuery(con, query)

# Send data to SQL
dbWriteTable(con, "fat3_nfp_cohort", nfp_cohort)


# -------------------------------------------------------------------
###### Inclusion/ exclusion counts for Fig 1 #####
# -------------------------------------------------------------------

# Step 1. 1 m random sample
# Load
load("ref_count1.RData")

nfp_count1 <- ref_count1 |>
  mutate(step = 1) |>
  rename(P_nfp = P_ref,
         R_nfp = R_ref)

# Step 2. 1+ consultation July 2011 - July 2012
# Load
  query <- dbSendQuery(
    conn = con,
    statement = "
select count(distinct e_patid) as P_nfp, count(e_patid) as R_nfp
from becky.fat3_nfp_consultations
;
"
  )
# n = -1 => take all records
nfp_count2 <- dbFetch(query, n = -1) 
dbClearResult(query)

nfp_count2 <- nfp_count2 |>
  mutate(step = 2)

# count 3 & 4 done above


# Step 5. 1+ non fatigue record without a fatigue record < 2 years before or on same day
nfp_count5 <- nfp_cons_fat_sort1 |>
  filter(eventtype == "c" & elig == 1) |>
  mutate(step = 5) |>
  group_by(step) |>
  dplyr::summarize(P_nfp = n_distinct(e_patid),
                   R_nfp = n()) 

# Append counts
nfp_count <- rbind(nfp_count1, nfp_count2, nfp_count3, nfp_count4, nfp_count5)

# Save results
save(nfp_count, file = "nfp_count.RData")



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
