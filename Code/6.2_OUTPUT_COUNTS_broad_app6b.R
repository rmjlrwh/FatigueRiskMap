# -------------------------------------------------------------------
# Appendix 6b. Descriptive analysis - for broad groups so all cancers combined can be added
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Prep
# -------------------------------------------------------------------

# Add functions from functions file
source("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Code/2023_03_10//0.1_FUNCTIONS.R")

# Install & load packages at same time
# If it asks "install from sources that need compilation?" Hit "no"

# Packages listed in NCmisc
pacman::p_load(tidyverse,ggplot2, binom, marginaleffects, purrr, splines, writexl, Hmisc)

# Load other data
load("n_diags_broad.RData")
load("lookup_diseases_meta_broad.RData")
load("cohort.RData") # Info about total patients in cohorts, after exclusiosn (before 'disease free'/ cfs removed)
load("count.RData") # Overall cohort coutns after with exclusion Ns for fig 1 (before 'disease free'/ cfs removed)

# # -------------------------------------------------------------------
# # Total, by cohort & gender
# # -------------------------------------------------------------------

# All patients, incl with prev diag
# FUN!
# N = Count of patients total with no prev diag of each disease,
count_allage_N_b <- cohort |>
  allage_N_fun(cohort, gender)

# n = Count of patients total with each sub diag of each disease if no prev diag,
count_allage_n_b <- n_diags_broad |>
  allage_n_fun(sub_diag, cohort, gender, broad_disease_group_num)

# With no prev diag of each diseases
# FUN!
# N = Count of patients total with no prev diag of each disease,
count_allage_nop_N_b <- n_diags_broad |>
  allage_noprev_N_fun(prev_diag_main, cohort, gender, broad_disease_group_num)


# N = Count of patients total with no prev diag of each disease,
# by gender and by cohort
allage_noprev_N_fun <- function(data, varf, var1, var2, var3) {
  data |>
    filter({{varf}} == 0) %>%
    select({{var1}},{{var2}}, {{var3}})  %>%
    group_by({{var1}},{{var2}}, {{var3}}) %>%
    dplyr::summarize(N = n())
}


# n = Count of patients total with each sub diag of each disease if no prev diag,
count_allage_nop_n_b <- n_diags_broad |>
  allage_noprev_n_fun(prev_diag_main, sub_diag, cohort, gender, broad_disease_group_num)


# # -------------------------------------------------------------------
# # Merge
# # -------------------------------------------------------------------

# Join N and n for 'all'into one table 
count_allage_b = merge(x = count_allage_N_b, y = count_allage_n_b [
  , c("cohort","gender", "broad_disease_group_num", "n")]
  , by =c("cohort","gender"), all.x = TRUE)

# FUN!
# obs % with diagnosis & CI
count_allage_b <- count_allage_b |>
  replace_missing(n) |>
  obs_prop() |>
  obs_ci() |>
  rename(all_N = N,
       all_n = n,
       all_obs_est = obs_est,
       all_obs_lb = obs_lb,
       all_obs_ub = obs_ub)

# Join N and n for'no prev' into a second table
count_allage_nop_b = merge(x = count_allage_nop_N_b, y = count_allage_nop_n_b [
  , c("cohort","gender", "broad_disease_group_num","n")]
  , by =c("cohort","gender", "broad_disease_group_num"), all.x = TRUE)

# FUN!
# obs % with diagnosis & CI
count_allage_nop_b <- count_allage_nop_b |>
  replace_missing(n) |>
  obs_prop() |>
  obs_ci() |>
  rename(noprev_N = N,
         noprev_n = n,
         noprev_obs_est = obs_est,
         noprev_obs_lb = obs_lb,
         noprev_obs_ub = obs_ub)

# Bind 'all' and 'no prev' tables so they're wide
count_b = merge(x = count_allage_b, y = count_allage_nop_b [
  , c("cohort","gender", "broad_disease_group_num", 
      "noprev_N", "noprev_n", "noprev_obs_est","noprev_obs_lb", "noprev_obs_ub")]
  , by =c("cohort","gender", "broad_disease_group_num"), all.x = TRUE)


# # -------------------------------------------------------------------
# # Format
# # -------------------------------------------------------------------

# Round results  
count_b <- count_b |>
  mutate(across(contains(c("_est","_lb","_ub")),
                round, 3))

# FUN!
# Merge disease name
count_b = disease_name_b(count_b)

# FUN!
# Labels
count_b <- count_b |>
  label_gender() |>
  label_cohort()

# # -------------------------------------------------------------------
# # Stats
# # -------------------------------------------------------------------

# Diff between prev diags vs all patients
count_b <- count_b |>
  mutate(diff = all_obs_est - noprev_obs_est)

# Flag if without prev diags is higher/ lower than all patients
count_b <- count_b |>
  sigdiff(all_obs_lb, noprev_obs_est, all_obs_est, noprev_obs_lb, all_obs_ub, noprev_obs_ub)

# Save
save(count_b, file = "count_b.RData")

# Save just all cancers combined to add into other tables
count_b_cancers <- count_b |>
  filter(broad_disease_group_new == "Cancers") |>
  mutate(
    disease_number_new = 1001,
    disease_new = "All cancers combined"
  )


# Bind with main count file
count <- rbind(count, count_b_cancers)

# Keep just fatigue
count <- count |>
  filter(cohort == 0)

# Save
save(count, file = "count.RData")



# # -------------------------------------------------------------------
# # Export excel for prev diag appendix
# # -------------------------------------------------------------------

# Select vars
count_b <- count_b |>
  select(!c("cohort","gender"))

# Suppres counts < 6
count_b <- count_b |>
  suppress(all_n) |>
  suppress(all_N) |>
  suppress(noprev_n) |>
  suppress(noprev_N)


# Replace missing if count suppressed
count_b <- count_b |>
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
count_b <- count_b |>
  select(c(
    "gender_lb",
    "cohort_lb",
    "broad_disease_group_num",
    "broad_disease_group_new",
    contains("noprev"),
    contains("all")),
    "diff",
    "sig"
  )

# REname diff
count_b <- count_b |>
  rename(diff2 = diff)

# Var labels
label(count_b) = as.list(
  var.labels.prev[match(names(count_b),
                   names(var.labels.prev))])

names(count_b) <- label(count_b)


# # Export results
write_xlsx(count_b, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App6b.xlsx")

  
rm("count_allage_b","count_allage_nop_b","count_allage_nop_N_b", "count_allage_nop_n_b",
   "count_allage_n_b", "count_allage_N_b", "n_diags_broad","count_b","cohort","count_b_cancers",
   "count", "summary_sig_prev", "summary_sig_prev_1p", "summary_sig_prev_b", "summary_sig_prev_b_1p")
gc()