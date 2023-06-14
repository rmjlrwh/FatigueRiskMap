# -------------------------------------------------------------------
###### Cubic spline age models, by individual disease #####
# -------------------------------------------------------------------

# Create modelled age-specific estimates for every possible cohort/ gender/ age/ disease combination

# -------------------------------------------------------------------
# Prep
# -------------------------------------------------------------------

# Install pacman so future packages can be installed and loaded at the same time
install.packages("pacman")
library(pacman)


# Install & load packages at same time
# Packages listed in NCmisc
pacman::p_load(tidyverse,ggplot2, binom, marginaleffects, purrr, splines, sandwich)

# Set working directory
setwd("S:/ECHO_IHI_CPRD/Data/Becky/Fatigue other diseases")

# Load other data
load("n_diags.RData")
load("lookup_diseases_meta.RData")


# -------------------------------------------------------------------
# Prep models
# -------------------------------------------------------------------

# Drop some impossible sex-disease combos (e.g. women with erectile dysfunction/ hyperplasia of prostate),
# as this will break the models
model_data <- n_diags |>
  filter(gender == 1 | (gender== 2 & disease_number_new != 107)) 

rm(n_diags)
gc()

model_data <- model_data |>
  filter(gender == 1 | (gender== 2 & disease_number_new != 112))

gc()

# Model data - only patients without prev diag
model_data <- model_data %>%
  filter(
    prev_diag_main == 0) |>
    select(c("gender","disease_number_new", "age_idate", "sub_diag", "cohort"))

gc()

# Re-scale age ------------------------------------------------------------
model_data <- model_data |>
  mutate(age_idate = (age_idate-60)/10)

# # Nest model data -------------------------------------------------------

# Sort the data so results can be replicated
model_data <- model_data |> 
 # unnest(data) |>
  arrange(cohort, gender, disease_number_new, age_idate, sub_diag) |>
  group_by(cohort, gender, disease_number_new) |>
  nest()

# Save, clear, quit and reload to help with memory
save(model_data, file = "model_data.RData")
gc()

install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse,ggplot2, binom, marginaleffects, purrr, splines, sandwich)

# Set working directory
setwd("S:/ECHO_IHI_CPRD/Data/Becky/Fatigue other diseases")
load("model_data.RData")

# only required libraries for first modelling step
library(dplyr); library(tidyr); library(purrr); library(splines)

# Fit models --------------------------------------------------------------
set.seed(4345)

# Run poisson model with 4 knots, centred on age 60, rescaled (divided by 10)
models_disclosive <- model_data |>
  mutate(
    model = map(
      data,  ~glm(sub_diag ~ ns(age_idate, knots = c(-2, 0, 1, 2)),
                  family = poisson, 
                  data = .x)
    )
  ) |>
  select(cohort,gender, disease_number_new, model) |>
  ungroup()

rm(model_data)
gc()


# make prediction data
age_idate <- (seq(30,99)-60)/10
predictions_modelvars_only <- data.frame(age_idate)

# do predictions
model_preds <- models_disclosive |>
  mutate(
    model_preds = map(
      .x = model, 
      ~ predictions(., predictions_modelvars_only, vcov = "HC3") |>
        select(age_idate, pr_est = predicted, pr_lb = conf.low, pr_ub = conf.high)
    )
  )

rm("models_disclosive")
rm("predictions_modelvars_only")
gc()

# Unnest predictions
model_preds <- model_preds |> 
  select(cohort,gender, disease_number_new, model_preds) |> 
  unnest(model_preds) |> 
  mutate(age = (age_idate*10)+60)

save(model_preds, file = "model_results_unnest.RData")

rm(model_preds)
gc()

# -------------------------------------------------------------------
# Counts to add in later
# -------------------------------------------------------------------

# Add functions from functions file
source("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Code/2023_03_10//0.1_FUNCTIONS.R")

load("n_diags.RData")
load("lookup_diseases_meta.RData")

# N = Count of patients total with no prev diag of each disease,
# by gender and 10 year age, by cohort
count_N <- n_diags |>
  count_N_fun(prev_diag_main, cohort, gender, disease_number_new, age_cat_10)

# n = Count of patients total with each sub diag of each disease if no prev diag,
# by gender and 10 year age, by cohort
count_n <- n_diags |>
  count_n_fun(prev_diag_main, sub_diag, cohort, gender, disease_number_new, age_cat_10)

rm(n_diags)
gc()

# -------------------------------------------------------------------
###### Add observed %s #####
# -------------------------------------------------------------------

load("model_results_unnest.RData")

# 
# By 10 year age band
# Add age cat into models data for join with observed counts
model_preds <- model_preds |> 
  age_band() 

# Add observed counts by 10 year age band
model_preds = merge(x = model_preds, y = count_n [
  , c("cohort","gender", "age_cat_10", "disease_number_new", "n")]
  , by =c("cohort","gender", "age_cat_10", "disease_number_new"), all.x = TRUE)

model_preds = merge(x = model_preds, y = count_N [
  , c("cohort", "gender", "age_cat_10", "disease_number_new", "N")]
  , by =c("cohort", "gender", "age_cat_10", "disease_number_new"), all.x = TRUE)

# drop unnecessary dfs
rm("count_n","count_N")
gc()

# Replace N/a with 0 in ages with 0 cases for that disease
model_preds <- model_preds |>
  mutate(n = ifelse(is.na(n),0,n))


# -------------------------------------------------------------------
###### % Format #####
# -------------------------------------------------------------------

# 
# Predictions %s 1-100
model_preds <- model_preds |>
  prop100(pr_est) |>
  prop100(pr_lb) |>
  prop100(pr_ub)

# 
# obs % with diagnosis & CI
model_preds <- model_preds |>
  obs_prop() |>
  obs_ci()

# Round results  
model_preds <- model_preds |>
  mutate(across(contains(c("_est","_lb","_ub")),
                round, 4))

# Labels
model_preds <- model_preds |>
  label_gender() |>
  label_cohort() |>
  mutate(age_cat_10_n = as.numeric(age_cat_10))

# Merge disease name
model_preds = disease_name(model_preds)

# Save results
save(model_preds, file = "model_results_byage.RData")



# -------------------------------------------------------------------
###### Tidy up #####
# -------------------------------------------------------------------

# Drop dfs not needed
rm(list=ls())
gc()


