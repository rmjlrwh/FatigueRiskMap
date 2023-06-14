# -------------------------------------------------------------------
# Fig 1 & Appenendix 6a. Descriptive analysis - specific diseases
# -------------------------------------------------------------------

# Prev diag counts/ unadjusted %s for sensitivity analysis
# ID diseases with < 100 diagnoses in fatigue


# -------------------------------------------------------------------
# Prep
# -------------------------------------------------------------------

# Drop dfs not needed
rm(list=ls())
gc()

# Add functions from functions file
source("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Code/2023_03_10//0.1_FUNCTIONS.R")

# Packages listed in NCmisc
pacman::p_load(writexl,Hmisc, binom)

# Load other data
load("n_diags.RData")
load("lookup_diseases_meta.RData")


# -------------------------------------------------------------------
# Fig 1
# -------------------------------------------------------------------

# Add up cohort counts

load("fat_count.RData")
load("nfp_count.RData")
load("ref_count.RData")

count <- fat_count |>
  left_join(nfp_count) |>
  left_join(ref_count)

# Add exclusions
count <- count |>
  mutate(P_fat_excl = lag(P_fat) - P_fat,
         R_fat_excl = lag(R_fat) - R_fat, 
         P_nfp_excl = lag(P_nfp) - P_nfp,
         R_nfp_excl = lag(R_nfp) - R_nfp,
         P_ref_excl = lag(P_ref) - P_ref,
         R_ref_excl = lag(R_ref) - R_ref
  ) 


# Order vars
count <- count |>
  select(c(
    "step",
    contains("fat"),
    contains("nfp"),
    contains("ref"),
  )
  )


# Export results
write_xlsx(count, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//Fig1_counts.xlsx",
           format_headers = TRUE)

rm("count","fat_count","nfp_count", "ref_count")
gc()

# # -------------------------------------------------------------------
# # Total, by cohort & gender
# # -------------------------------------------------------------------

# Shorten file
n_diags <- n_diags |>
  select(prev_diag_main, cohort, gender, disease_number_new, sub_diag)

gc() 

# n = Count of patients total with each sub diag of each disease,
# Fatigued patients with cfs/ pvf on index date can't be counted as having cfs/ pvf, 
# But they stay in denominator of all fatigued patients
count_allage_n <- n_diags |>
  allage_n_fun(sub_diag, cohort, gender, disease_number_new)


# With no prev diag of each diseases
# FUN!
# N = Count of patients total with/ without prev diag of each disease,
# CFS/ PVF fatigued patients on index date removed from denominator
count_allage_nop_N <- n_diags |>
  allage_noprev_N_fun(prev_diag_main, cohort, gender, disease_number_new)

# n = Count of patients total with each sub diag of each disease with/ without  prev diag,
# CFS/ PVF fatigued patients on index date removed from numerator
count_allage_nop_n <- n_diags |>
  allage_noprev_n_fun(prev_diag_main, sub_diag, cohort, gender, disease_number_new)

rm("n_diags")
gc() 

# All patients, incl with prev diag

load("cohort.RData") # Info about total patients in cohorts, after exclusiosn (before disease free)

# FUN!
# N = Count of patients total,
count_allage_N <- cohort |>
  allage_N_fun(cohort, gender)

rm("cohort")
gc()

# # -------------------------------------------------------------------
# # Merge
# # -------------------------------------------------------------------

# Join N and n for 'all'into one table 
count_allage = merge(x = count_allage_N, y = count_allage_n [
  , c("cohort","gender", "disease_number_new", "n")]
  , by =c("cohort","gender"), all.x = TRUE)

# FUN!
# obs % with diagnosis & CI
count_allage <- count_allage |>
  replace_missing(n) |>
  obs_prop() |>
  obs_ci() |>
  rename(all_N = N,
       all_n = n,
       all_obs_est = obs_est,
       all_obs_lb = obs_lb,
       all_obs_ub = obs_ub)

# Join N and n for'no prev' into a second table
count_allage_nop = merge(x = count_allage_nop_N, y = count_allage_nop_n [
  , c("cohort","gender", "disease_number_new","n")]
  , by =c("cohort","gender", "disease_number_new"), all.x = TRUE)

# FUN!
# obs % with diagnosis & CI
count_allage_nop <- count_allage_nop |>
  replace_missing(n) |>
  obs_prop() |>
  obs_ci() |>
  rename(noprev_N = N,
         noprev_n = n,
         noprev_obs_est = obs_est,
         noprev_obs_lb = obs_lb,
         noprev_obs_ub = obs_ub)

# Bind 'all' and 'no prev' tables so they're wide
# Combine to lookup meta diseases
count = merge(x = count_allage, y = count_allage_nop [
  , c("cohort","gender", "disease_number_new", 
      "noprev_N", "noprev_n", "noprev_obs_est","noprev_obs_lb", "noprev_obs_ub")]
  , by =c("cohort","gender", "disease_number_new"), all.x = TRUE)


# # -------------------------------------------------------------------
# # Format
# # -------------------------------------------------------------------

# Round results  
count <- count |>
  mutate(across(contains(c("_est","_lb","_ub")),
                round, 3))

# FUN!
# Merge disease name
count = disease_name(count)

# FUN!
# Labels
count <- count |>
  label_gender() |>
  label_cohort()

# # -------------------------------------------------------------------
# # Stats
# # -------------------------------------------------------------------

# Diff between prev diags vs all patients
count <- count |>
  mutate(diff = all_obs_est - noprev_obs_est)

# Flag if without prev diags is higher/ lower than all patients
count <- count |>
  sigdiff(all_obs_lb, noprev_obs_est, all_obs_est, noprev_obs_lb, all_obs_ub, noprev_obs_ub)

# Save
save(count, file = "count.RData")

# # -------------------------------------------------------------------
# # Export excel for prev diag appendix
# # -------------------------------------------------------------------

# Select vars
count <- count |>
  select(!c("cohort","gender"))

# Suppres counts < 6
count <- count |>
  suppress(all_n) |>
  suppress(all_N) |>
  suppress(noprev_n) |>
  suppress(noprev_N)
  

# Replace missing if count suppressed
count <- count |>
  mutate_at(
    vars(c("all_obs_est","all_obs_lb","all_obs_ub","diff")),
    ~ifelse(
      all_n == "<6", NA, .x)
  ) |>
  mutate_at(
    vars(c("noprev_obs_est","noprev_obs_lb","noprev_obs_ub","diff")),
    ~ifelse(
      noprev_n == "<6", NA, .x)
  ) 

# Order vars
count <- count |>
  select(c(
    "gender_lb",
    "cohort_lb",
    "broad_disease_group_num",
    "broad_disease_group_new",
    "disease_number_new",
    "disease_new",
    contains("noprev"),
    contains("all")),
    "diff",
    "sig"
  )

# Summary stats of sig diff, by gender, in fatigue
summary_sig_prev <- count |>
  filter(cohort_lb == "Fatigue presenters") |>
  select(gender_lb,sig) |>
  group_by(gender_lb,sig) |>
  dplyr::summarize(n = n())

# Export results
write_xlsx(summary_sig_prev, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App6a_supp1. sig diff without prev dx.xlsx",
           format_headers = TRUE)

summary_sig_prev

# no. diseases more than 1% lower after removing prev dx, by gender, in fatigue
summary_sig_prev_1p <- count |>
  filter(cohort_lb == "Fatigue presenters" & diff > 1) |>
  select(gender_lb) |>
  group_by(gender_lb) |>
  dplyr::summarize(n = n())

# Export results
write_xlsx(summary_sig_prev_1p, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App6a_supp2. 1 perc lower without prev dx.xlsx",
           format_headers = TRUE)

summary_sig_prev_1p

# Larges/ smallest no prev diag cohort, by gender, cohort
N_noprev_max <- count |>
  select(cohort_lb, gender_lb,disease_number_new, disease_new, noprev_N) |>
  group_by(cohort_lb, gender_lb) |>
  dplyr::slice_max(noprev_N)

N_noprev_min <- count |>
  select(cohort_lb, gender_lb,disease_number_new, disease_new, noprev_N) |>
  group_by(cohort_lb, gender_lb) |>
  dplyr::slice_min(noprev_N)


# Export results
write_xlsx(N_noprev_max, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App6a_supp3. Max cohort size.xlsx",
           format_headers = TRUE)

# Export results
write_xlsx(N_noprev_min, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App6a_supp4. Min cohort size.xlsx",
           format_headers = TRUE)


# REname diff
count <- count |>
  rename(diff2 = diff)

# Var labels
label(count) = as.list(
  var.labels.prev[match(names(count),
                   names(var.labels.prev))])

names(count) <- label(count)


# # Export results
write_xlsx(count, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App6a.xlsx")


rm("count_allage","count_allage_nop","count_allage_nop_N", "count_allage_nop_n",
   "count_allage_n", "count_allage_N", "n_diags","cohort", "count","N_noprev_max", "N_noprev_min")
gc()
