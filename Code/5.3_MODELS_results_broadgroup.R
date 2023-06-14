
# -------------------------------------------------------------------
###### Repeat models, for broad disease group #####
# -------------------------------------------------------------------

# Clear data
rm(list=ls())
gc()

# -------------------------------------------------------------------
# Prep
# -------------------------------------------------------------------

# Add functions from functions file
source("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Code/2023_03_10//0.1_FUNCTIONS.R")

# Packages listed in NCmisc
pacman::p_load(tidyverse,ggplot2, binom, marginaleffects, purrr, splines)

# Set working directory
setwd("S:/ECHO_IHI_CPRD/Data/Becky/Fatigue other diseases")

# Load other data
load("n_diags_broad.RData")

# 
# -------------------------------------------------------------------

# Model data - only patients without prev diag
model_data <- n_diags_broad %>%
  filter(
    prev_diag_main == 0) %>%
  select(c("gender","broad_disease_group_num", "age_idate", "sub_diag", "cohort"))

rm(n_diags_broad)

# Re-scale age ------------------------------------------------------------
model_data <- model_data |>
  mutate(age_idate = (age_idate-60)/10)

# # Nest model data -------------------------------------------------------
# model_data <- model_data |>
#   group_by(cohort,gender, broad_disease_group_num) |>
#   nest()

# Sort the data so results can be replicated
model_data <- model_data |> 
  # unnest(data) |>
  arrange(cohort, gender, broad_disease_group_num, age_idate, sub_diag) |>
  group_by(cohort, gender, broad_disease_group_num) |>
  nest()


# Fit models --------------------------------------------------------------

set.seed(48945)

# Run poisson model with 4 knots, centred on age 60, rescaled (divided by 10)
models_disclosive <- model_data |>
  mutate(
    model = map(
      data,  ~glm(sub_diag ~ ns(age_idate, knots = c(-2, 0, 1, 2)),
                  family = poisson, 
                  data = .x)
    )
  ) |>
  select(cohort,gender, broad_disease_group_num, model) |> # Not sure if this is needed
  ungroup() # Or this

rm(model_data)
gc()

# make prediction data
age_idate <- (seq(30,99)-60)/10
predictions_modelvars_only <- data.frame(age_idate)

# do predictions
model_preds_broad <- models_disclosive |>
  mutate(
    model_preds_broad = map(
      .x = model, 
      ~ predictions(., predictions_modelvars_only, vcov = "HC3") |>
        select(age_idate, pr_est = predicted, pr_lb = conf.low, pr_ub = conf.high)
    )
  )

rm(models_disclosive)
rm(predictions_modelvars_only)
gc()

# Unnest predictions
model_preds_broad <- model_preds_broad |> 
  select(cohort,gender, broad_disease_group_num, model_preds_broad) |> 
  unnest(model_preds_broad) |> 
  mutate(age = (age_idate*10)+60)

save(model_preds_broad, file = "model_results_unnest_broad.RData")


# -------------------------------------------------------------------
###### Add observed %s #####
# -------------------------------------------------------------------

load("n_diags_broad.RData")

# N = Count of patients total with no prev diag of each disease,
# by gender and 10 year age, by cohort
count_N <- n_diags_broad |>
  count_N_fun(prev_diag_main, cohort, gender, broad_disease_group_num, age_cat_10)

# n = Count of patients total with each sub diag of each disease if no prev diag,
# by gender and 10 year age, by cohort
count_n <- n_diags_broad |>
  count_n_fun(prev_diag_main, sub_diag, cohort, gender, broad_disease_group_num, age_cat_10)

rm(n_diags_broad)

load("model_results_unnest_broad.RData")

# By 10 year age band
# Add age cat into models data for join with observed counts
model_preds_broad <- model_preds_broad |> 
  age_band()

# Add observed counts by 10 year age band
model_preds_broad = merge(x = model_preds_broad, y = count_n [
  , c("cohort","gender", "age_cat_10", "broad_disease_group_num", "n")]
  , by =c("cohort","gender", "age_cat_10", "broad_disease_group_num"), all.x = TRUE)

model_preds_broad = merge(x = model_preds_broad, y = count_N [
  , c("cohort", "gender", "age_cat_10", "broad_disease_group_num", "N")]
  , by =c("cohort", "gender", "age_cat_10", "broad_disease_group_num"), all.x = TRUE)

# drop unnecessary dfs
rm("count_n","count_N")
gc()

# Replace N/a with 0 in ages with 0 cases for that disease
model_preds_broad <- model_preds_broad |>
  replace_missing(n)
  

# -------------------------------------------------------------------
###### % Format #####
# -------------------------------------------------------------------

# Predictions %s 1-100
model_preds_broad <- model_preds_broad |>
  prop100(pr_est) |>
  prop100(pr_lb) |>
  prop100(pr_ub)
  
# obs % with diagnosis & CI
model_preds_broad <- model_preds_broad |>
  obs_prop() |>
  obs_ci()
  
# Round results  
model_preds_broad <- model_preds_broad |>
  mutate(across(contains(c("_est","_lb","_ub")),
                round, 4))

# Labels
model_preds_broad <- model_preds_broad |>
  label_gender() |>
  label_cohort() |>
  mutate(age_cat_10_n = as.numeric(age_cat_10))

# Adapt lookup to keep just broad disease groups
load("lookup_diseases_meta.RData")

lookup_diseases_meta_broad <- lookup_diseases_meta |>
  select(broad_disease_group_num, broad_disease_group_new) |>
  distinct(broad_disease_group_num, broad_disease_group_new, .keep_all = TRUE)

save(lookup_diseases_meta_broad, file = "lookup_diseases_meta_broad.RData")

# Merge disease name
model_preds_broad = disease_name_b(model_preds_broad)

# Save results
save(model_preds_broad, file = "model_results_byage_broad.RData")

load("model_results_byage_broad.RData")

# M/ F data frames
g1_m <- model_preds_broad |>
  filter(gender == 1
  )

g1_f <- model_preds_broad |>
  filter(gender == 2
  )


# Estimates + CIs for each cohort, by year of age

# Men
ggplot() +
  geom_ribbon(data = g1_m, 
              aes(x = age, y = pr_est, ymin = pr_lb, ymax = pr_ub, fill = cohort_lb)) +
  geom_line(data = g1_m, aes(x = age, y = pr_est, colour = cohort_lb)) +
  geom_line(data = g1_m, aes(x = age, y = obs_est, colour= cohort_lb)) +
  facet_wrap(vars(broad_disease_group_new), scales = "free") +
  theme(legend.position="bottom") +
  labs(x = "Age in years", y = "Risk (%)", colour = "Observed (10 year bands):", fill = "Modelled:") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_continuous(limits=c(30,100),
                     breaks=c(30,40,50,60,70,80,90,100),
                     labels=c(30,40,50,60,70,80,90,100)) +
  scale_fill_manual(values=alpha(c("red","green","blue"),.3))

# WOmen
ggplot() +
  geom_ribbon(data = g1_f, 
              aes(x = age, y = pr_est, ymin = pr_lb, ymax = pr_ub, fill = cohort_lb)) +
  geom_line(data = g1_f, aes(x = age, y = pr_est, colour = cohort_lb)) +
  geom_line(data = g1_f, aes(x = age, y = obs_est, colour= cohort_lb)) +
  facet_wrap(vars(broad_disease_group_new), scales = "free") +
  theme(legend.position="bottom") +
  labs(x = "Age in years", y = "Risk (%)", colour = "Observed (10 year bands):", fill = "Modelled:") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_continuous(limits=c(30,100),
                     breaks=c(30,40,50,60,70,80,90,100),
                     labels=c(30,40,50,60,70,80,90,100)) +
  scale_fill_manual(values=alpha(c("red","green","blue"),.3))



# -------------------------------------------------------------------
###### Tidy up #####
# -------------------------------------------------------------------

# Drop dfs not needed
rm(list=ls())
gc()



