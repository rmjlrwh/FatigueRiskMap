
# -------------------------------------------------------------------
###### Appendix 9. Cumulative risk by month #####
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Prep
# -------------------------------------------------------------------

# Add functions from functions file
source("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Code/2023_03_10//0.1_FUNCTIONS.R")

# Packages listed in NCmisc
pacman::p_load(tidyverse,ggplot2, binom, kableExtra, patchwork, stringr,
               tidytext, writexl, htmltools, Hmisc)

# Load data
load("n_diags_fat.RData")
load("n_diags_nfp.RData")
load("lookup_diseases_meta.RData")


# -------------------------------------------------------------------
# Format files
# -------------------------------------------------------------------

# Remove prev diags
n_diags_fat <- n_diags_fat |>
  filter(prev_diag_main == 0) |>
  select(cohort, gender, e_patid, disease_number_new, sub_diag, months_diff)

n_diags_nfp <- n_diags_nfp |>
  filter(prev_diag_main == 0) |>
  select(cohort, gender, e_patid, disease_number_new, sub_diag, months_diff)

# Append diags files
n_diags_month <- rbind(n_diags_fat, n_diags_nfp)

rm("n_diags_fat","n_diags_nfp")

# Count no. patients
count_month <- n_diags_month |>
  group_by(gender, cohort, disease_number_new) |>
  mutate(N = n()) |>
  ungroup()

# Count number of first diagnoses per month
count_month <- count_month |>
  filter(sub_diag == 1) |>
  select(gender, cohort, disease_number_new, months_diff, N) |>
  group_by(gender, cohort, disease_number_new, months_diff, N) |>
  dplyr::summarize(n_permonth = n()) |>
  ungroup() |>
  group_by(gender, cohort,disease_number_new) |>
  mutate(n = cumsum(n_permonth))

# Run functions
count_month <- count_month |>
  obs_prop() |>
  obs_ci() |>
  label_cohort()

# Merge disease name
count_month = disease_name(count_month)

# -------------------------------------------------------------------
# Graphs
# -------------------------------------------------------------------


# Example diseases - sample of diseases with high and low excess risk at 12 months plus cancer

# Men
data <- count_month |>
  filter(gender == 1) |>
  filter(              disease_new == "Lower Respiratory Tract Infections" | 
                         disease_new == "Hypertension" | 
                         disease_new ==  "Ear and Upper Respiratory Tract Infections" | 
                         disease_new == "Urinary Tract Infections" | 
                         
                         disease_new == "Depression" | 
                         disease_new ==  "Connective & soft tissue disorders" |
                         disease_new == "Lung cancer" |
                         disease_new == "Lower GI cancer" |
                         
                         
                         disease_new == "Other or unspecified infectious organisms" |
                         disease_new == "Insomnia & sleep disturbances" |
                         disease_new == "Erectile dysfunction" |
                         disease_new == "Chronic kidney disease" |            
                         
                         disease_new == "Bacterial Diseases (excl TB)" |
                         disease_new == "Dermatitis (atopc/contact/other/unspecified)" |            
                         disease_new == "Coronary heart disease not otherwise specified" |
                         disease_new == "Pneumonitis" 
                       
  ) |>
  disease_labels()



# Graph
p_count_month_m <- ggplot() +
  geom_ribbon(data = data, 
              aes(x = months_diff, y = obs_est, ymin = obs_lb, ymax = obs_ub, fill = cohort_lb)) +
  geom_line(data = data, aes(x = months_diff, y= obs_est, colour = cohort_lb)) +
  facet_wrap(vars(disease_new), scales = "free") +
  theme(legend.position="bottom") +
  labs(x = "Months after index", y = "Cumulative incidence (%)", fill = "Cohort", colour = "Cohort") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_continuous(limits=c(0,12),
                     breaks=c(0,2,4,6,8,10,12),
                     labels=c(0,2,4,6,8,10,12)) +
  scale_fill_manual(values=alpha(c("red","green","blue"),.3)) + 
  theme_bw()

p_count_month_m


# Save graph
ggsave("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App 9a. Months_male.png",plot=p_count_month_m)



# Women
data <- count_month |>
  filter(gender == 2) |>
  filter(
    disease_new == "Urinary Tract Infections" | 
    disease_new ==  "Ear and Upper Respiratory Tract Infections" | 
    disease_new == "Lower Respiratory Tract Infections" | 
     disease_new == "Depression" | 
      
     disease_new == "Hypertension" | 
      disease_new ==  "Connective & soft tissue disorders" | 
      disease_new == "Hypo or hyperthyroidism" |
      disease_new == "Insomnia & sleep disturbances" |
      
      disease_new == "Dermatitis (atopc/contact/other/unspecified)" |            
      disease_new == "Anxiety disorders" |
      disease_new == "Chronic kidney disease" |
      disease_new == "Menorrhagia and polymenorrhoea" |
      
      disease_new == "Bacterial Diseases (excl TB)" |
      disease_new == "Osteoarthritis (excl spine)" | 
      disease_new == "Lung cancer" |
      disease_new == "Lower GI cancer"
    ) |>
  disease_labels()


# Graph
p_count_month_f <- ggplot() +
  geom_ribbon(data = data, 
              aes(x = months_diff, y = obs_est, ymin = obs_lb, ymax = obs_ub, fill = cohort_lb)) +
  geom_line(data = data, aes(x = months_diff, y= obs_est, colour = cohort_lb)) +
  facet_wrap(vars(disease_new), scales = "free") +
  theme(legend.position="bottom") +
  labs(x = "Months after index", y = "Cumulative incidence (%)", fill = "Cohort", colour = "Cohort") +
  theme(axis.text.x = element_text(size=8)) +
  scale_x_continuous(limits=c(0,12),
                     breaks=c(0,2,4,6,8,10,12),
                     labels=c(0,2,4,6,8,10,12)) +
  scale_fill_manual(values=alpha(c("red","green","blue"),.3)) + 
  theme_bw()

p_count_month_f


# Save graph
ggsave("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App 9b. Months_female.png",plot=p_count_month_f)

