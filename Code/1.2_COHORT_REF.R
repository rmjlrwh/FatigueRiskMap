## -------------------------------------------------------------------
###### Create reference (registered patients) cohort #######
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
drop table if exists  becky.fat3_ref_patients;
"
)

query <- dbSendQuery(
  conn = con,
  statement = "
create table becky.fat3_ref_patients 
(select d.e_patid
  , l.crd,  l.tod, l.deathdate, l.yob, l.gender
  ,l3.uts, l3.lcd
  
  , case when yob is not null
  then makedate(yob,182)
  else null
  end as dob
  
  from 18_299_Lyratzopoulos_e2.cprd_random_sample d
  LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_patient l ON  d.e_patid = l.e_patid
  LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_linkage_eligibility_gold l2 ON  d.e_patid = l2.e_patid
  LEFT JOIN 18_299_Lyratzopoulos_e2.cprd_practice l3 ON  l2.e_pracid = l3.e_pracid
  
)
;
"
)

query <- dbSendQuery(
  conn = con,
  statement = "
CREATE INDEX `e_patid` ON becky.fat3_ref_patients (`e_patid`);
"
)

query <- dbSendQuery(
  conn = con,
  statement = "
Select * from becky.fat3_ref_patients;
"
)
# n = -1 => take all records
ref_cohort <- dbFetch(query, n = -1) 
dbClearResult(query)


# -------------------------------------------------------------------
###### Define potentially eligible dates #####
# -------------------------------------------------------------------

# Year of birth as date variable
ref_cohort <- ref_cohort %>%
  mutate(yob_approx = as.Date(ISOdate(ref_cohort$yob,8,1)))

# Year turned 30
ref_cohort <- ref_cohort %>%
  mutate(age30date = ref_cohort$yob_approx %m+% years(30))

# Year turned 100
ref_cohort <- ref_cohort %>%
  mutate(age100date = ref_cohort$yob_approx %m+% years(100))

# Format dates as date
ref_cohort <- ref_cohort %>%
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
# so include fatigue up to end of 2017
study_end <- as.Date("2017-12-31")


# Follow up start & end dates for count3 in Fig 1
ref_count3 <- ref_cohort |> 
  mutate(fu_start = pmax(age30date,
                         study_start
                         , na.rm = TRUE))

# Follow up end for count 3 in Fig 1
# Before: study end, death date, age 100
ref_count3 <- ref_count3 |> 
  mutate(fu_end = pmin(deathdate,
                       age100date,
                       study_end
                       , na.rm = TRUE))

# minus fu_start from fu_end
ref_count3 <- ref_count3 %>%
  mutate(n_days = lubridate::interval(ref_count3$fu_start,ref_count3$fu_end)/days(1))

# Drop patients who do not have 2 years follow up i.e. no gap between start and end dates
ref_count3 <- ref_count3 %>%
  filter(n_days >= 1)


# Actual follow up start and end dates for each patient

# Follow up start
# After: study start, 2 years After UTS and CRD date, after age 30
ref_cohort <- ref_cohort |> 
  mutate(fu_start = pmax((crd %m+% years(2)),
                         (uts %m+% years(2)),
                         age30date,
                         study_start
                         , na.rm = TRUE))

# Follow up end
# Before: study end, before lcd or tod, death date, age 100, or study end
ref_cohort <- ref_cohort |> 
  mutate(fu_end = pmin(lcd,
                       tod,
                       deathdate,
                       age100date,
                       study_end
                      , na.rm = TRUE))

# minus fu_start from fu_end
ref_cohort <- ref_cohort %>%
  mutate(n_days = lubridate::interval(ref_cohort$fu_start,ref_cohort$fu_end)/days(1))

# Drop patients who do not have 3 years follow up i.e. no gap between start and end dates
ref_cohort <- ref_cohort %>%
  filter(n_days >= 1)

# Pick random date in between fu start and end

# Convert to data table
setDT(ref_cohort)

# Select random date between follow up start and end
ref_cohort[, indexdate := sample(
  seq.Date(
    fu_start,
    fu_end,
    by = 1
  ),
  size = 1
),
by = e_patid]

# COnvert back to data frame
class(ref_cohort) <- class(as.data.frame(ref_cohort))

# Check it worked
head(ref_cohort, 10)

# Save results
save(ref_cohort, file = "ref_cohort.RData")

# -------------------------------------------------------------------
###### Send back to SQL to get diagnoses #####
# -------------------------------------------------------------------


# Send query for fatigue events
query = 'DROP TABLE IF EXISTS fat3_ref_cohort;'
rs = dbSendQuery(con, query)

# Send data to SQL
dbWriteTable(con, "fat3_ref_cohort", ref_cohort)


# -------------------------------------------------------------------
###### Inclusion/ exclusion counts for Fig 1 #####
# -------------------------------------------------------------------


# Step 1. 1 m random sample

# Load
query <- dbSendQuery(
  conn = con,
  statement = "
select count(distinct e_patid) as P_ref, count(e_patid) as R_ref
from
(select l.e_patid, d.eventdate
  
  from 18_299_Lyratzopoulos_e2.cprd_consultation d
  right join becky.fat3_ref_patients l on d.e_patid = l.e_patid
) as tbl1
;
"
)

# n = -1 => take all records
ref_count1 <- dbFetch(query, n = -1) 
dbClearResult(query)

# Save results
save(ref_count1, file = "ref_count1.RData")

ref_count1 <- ref_count1 |>
  mutate(step = 1) |>
  mutate(R_ref = NA)


# Step 2. Same as above
ref_count2 <- ref_count1 |>
  mutate(step = 2) 

# STep 3. 1+ day with UTS follow up > 2 years before while aged 30-99 years
ref_count3 <- ref_count3 |>
  mutate(step = 3) |>
  group_by(step) |>
  dplyr::summarize(P_ref = n_distinct(e_patid)) |>
  mutate(R_ref = NA)

# Step 4. 1+ day within UTS follow up
ref_count4 <- ref_cohort |>
  mutate(step = 4) |>
  group_by(step) |>
  dplyr::summarize(P_ref = n_distinct(e_patid)) |>
  mutate(R_ref = NA)

# Step 5. Can have prev fatigue, so same number
ref_count5 <- ref_count4 |>
  mutate(step = 5)

# Append counts
ref_count <- rbind(ref_count1, ref_count2, ref_count3, ref_count4, ref_count5)

# Save results
save(ref_count, file = "ref_count.RData")

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

