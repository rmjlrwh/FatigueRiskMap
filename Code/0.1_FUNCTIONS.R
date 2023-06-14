## -------------------------------------------------------------------
###### Create functions to call in analysis #######
# -------------------------------------------------------------------

# Install pacman so future packages can be installed and loaded at the same time
install.packages("pacman")
library(pacman)


# -------------------------------------------------------------------
###### Generic functions across all files #####
# -------------------------------------------------------------------

rm(list=ls())
gc()

setwd("S:/ECHO_IHI_CPRD/Data/Becky/Fatigue other diseases")


# -------------------------------------------------------------------
## Gen vars ##

# Replace missing with 0
replace_missing <- function(data, var) {
  data |>
    mutate(
      "{{var}}" := ifelse(
        is.na({{var}}),
        0,
        {{var}} 
      ))
}


# 10 year age band
age_band <- function(data){
  data |>
  mutate(age_cat_10 = case_when(
    age < 40 ~ "30-39",
    age < 50 ~ "40-49",
    age < 60 ~ "50-59",
    age < 70 ~ "60-69",
    age < 80 ~ "70-79",
    age < 90 ~ "80-89",
    age >= 90 ~ "90+"
  ),
  age_cat_10 = factor(age_cat_10))
  }

# Labels
label_gender <- function(data) {
  data |>
    mutate(gender_lb = case_when (
      gender == 1 ~ "Men",
      gender == 2 ~ "Women"
    ))
  }

label_cohort <- function(data) {
  data |>
    mutate(
      cohort_lb = case_when (
        cohort == 0 ~ "Fatigue presenters", 
        cohort == 1 ~ "Non-fatigue presenters",
        cohort == 2 ~ "Registered patients")
    )
}


# Label vars
var.labels = c(gender_lb = "Sex",
               gender = "Sex",
               age = "Age in years",
               age_cat_10 = "10 year age band",
               
               broad_disease_group_num = "Disease group no.",
               broad_disease_group_new = "Disease group",
               disease_number_new = "Disease no.",
               disease_new = "Disease",
               
               c0_n = "Fatigue presenters: cases (n)",
               c0_est = "Fatigue presenters: modelled risk (%)",
               c0_lb = "Fatigue presenters: 95% lower bound (%)",
               c0_ub = "Fatigue presenters: 95% upper bound (%)",
               c0_rank = "Fatigue presenters: rank",

               c1_n = "Non-fatigue presenters: cases (n)",
               c1_est = "Non-fatigue presenters: modelled risk (%)",
               c1_lb = "Non-fatigue presenters: 95% lower bound (%)",
               c1_ub = "Non-fatigue presenters: 95% upper bound (%)",
               c1_rank = "Non-fatigue presenters: rank",
               
               c2_n = "Registered patients: cases (n)",
               c2_est = "Registered patients: modelled risk (%)",
               c2_lb = "Registered patients: 95% lower bound (%)",
               c2_ub = "Registered patients: 95% upper bound (%)",
               c2_rank = "Registered patients: rank",
               
               pr_est_0 = "Fatigue presenters: modelled risk (%)",
               pr_lb_0 = "Fatigue presenters: 95% lower bound (%)",
               pr_ub_0 = "Fatigue presenters: 95% upper bound (%)",

               pr_est_1 = "Non-fatigue presenters: modelled risk (%)",
               pr_lb_1 = "Non-fatigue presenters: 95% lower bound (%)",
               pr_ub_1 = "Non-fatigue presenters: 95% upper bound (%)",
               
               pr_est_2 = "Registered patients: modelled risk (%)",
               pr_lb_2 = "Registered patients: 95% lower bound (%)",
               pr_ub_2 = "Registered patients: 95% upper bound (%)",
               
               diff = "Absolute excess risk (%) in fatigue vs non-fatigue presenters",
               rank = "Disease rank",

               sig = "95% CI comparison between fatigue & non-fatigue presenters",
               sig_f_vs_nfp = "95% CI comparison between fatigue & non-fatigue presenters",
               sig_f_vs_ref = "95% CI comparison between fatigue presenters & registered patients",
               sig_nfp_vs_ref = "95% CI comparison between non-fatigue presenters & registered patients"
               )


# Label vars
var.labels.prev = c(gender_lb = "Sex",
               gender = "Sex",
               cohort_lb = "Cohort",
               age = "Age in years",
               age_cat_10 = "10 year age band",
               
               broad_disease_group_num = "Disease group no.",
               broad_disease_group_new = "Disease group",
               disease_number_new = "Disease no.",
               disease_new = "Disease",
               
               noprev_N = "No previous disease: cohort (N)",
               noprev_n = "No previous disease: cases (n)",
               noprev_obs_est = "No previous disease: modelled risk (%)",
               noprev_obs_lb = "No previous disease: 95% lower bound (%)",
               noprev_obs_ub = "No previous disease: 95% upper bound (%)",

               all_N = "All patients: cohort (N)",
               all_n = "All patients: cases (n)",
               all_obs_est = "All patients: modelled risk (%)",
               all_obs_lb = "All patients: 95% lower bound (%)",
               all_obs_ub = "All patients: 95% upper bound (%)",
               
               sig = "95% CI comparison between patients with no previous diagnosis vs all patients",
               diff2 = "Absolute difference in risk (%) if including all patients"
)





# Shorten disease labels
disease_labels <- function(data) {
  
  data |>
    mutate(
      disease_new = str_replace(
        disease_new, "Coronary heart disease not otherwise specified", "CHD NOS"),
      disease_new = str_replace(
        disease_new, "Gastro-oesophageal reflux disease", "Gastro-oes. reflux"),
      disease_new = str_replace(
        disease_new, "Other or unspecified infectious organisms", "Other infect. orgs."),
      disease_new = str_replace(
        disease_new, "\\(atopc/contact/other/unspecified\\)", ""),
      disease_new = str_replace(
        disease_new, "Connective & soft tissue disorders", "Connec. tissue & joint"),
      disease_new = str_replace(
        disease_new, "Lower Respiratory Tract Infections", "Lower RTIs"),
      disease_new = str_replace(
        disease_new, "Hypo or hyperthyroidism", "Hypo/hyperthyroid."),
      disease_new = str_replace(
        disease_new, "Urinary Tract Infections", "UTIs"),
      disease_new = str_replace(
        disease_new, "Menorrhagia and polymenorrhoea", "Menorrhag./polymenor."),
      disease_new = str_replace(
        disease_new, "Allergic and chronic rhinitis", "Aller./chron. rhinitis"),
      disease_new = str_replace(
        disease_new, "Osteoarthritis (excl spine)", "Osteoarth. exc. spine"),

      disease_new = str_replace(
        disease_new, "Postviral fatigue syndrome, neurasthenia and fibromyalgia", "PVS/ neurasth./ fibromyal."),
      
      disease_new = str_replace(
        disease_new, "Chronic kidney disease", "CKD"),
      
      # Added 25/04/2023
      
     # Bacterial diseases (excl TB)
      #Oesophagitis and oesophageal ulcer
      #Ear and Upper Respiratory Tract Infections
      #Insomnia & sleep disturbances
      #Soft tissue
      
      disease_new = str_replace(
        disease_new, "Bacterial Diseases \\(excl TB\\)", "Bact. diseases"),
      disease_new = str_replace(
        disease_new, "Oesophagitis and oesophageal ulcer", "Oesophagitis/ ulcer"),
      disease_new = str_replace(
        disease_new, "Ear and Upper Respiratory Tract Infections", "Ear & upper RTIs"),
      disease_new = str_replace(
        disease_new, "Insomnia & sleep disturbances", "Insomn./ sleep disturb."),
     disease_new = str_replace(
       disease_new, "Peripheral neuropathies \\(excluding cranial nerve and carpal tunnel syndromes\\)", 
       "Periph. neuropathies"),
     disease_new = str_replace(
            disease_new, "Vitamin B12 deficiency anaemia", "B12 defic. anaemia"),
     disease_new = str_replace(
       disease_new, "Diverticular disease of intestine \\(acute and chronic\\)", "Diverticular disease"),
     
     disease_new = str_replace(
       disease_new, "Hypo or hyperthyroidism", "Thyroid disorders")
     
  )

  
}



# Shorten broad disease labels
broad_disease_labels <- function(data) {
  
  data |>
    mutate(broad_disease_group_new = str_replace(
    broad_disease_group_new, "Diseases of the ", "")) %>%
  mutate(broad_disease_group_new = str_replace(
    broad_disease_group_new, " conditions", "")) %>%
  mutate(broad_disease_group_new = str_replace(
    broad_disease_group_new, "Musculoskeletal", "MSK")) %>%
  mutate(broad_disease_group_new = str_replace(
    broad_disease_group_new, "Mental Health Disorders", "Mental H.")) %>%
  mutate(broad_disease_group_new = str_replace(
    broad_disease_group_new, " System", "")) %>%
  mutate(broad_disease_group_new = str_replace(
    broad_disease_group_new, "Haematological/Immunological", "Haem./Imm.")) |>
    
    mutate(broad_disease_group_new = str_replace(
      broad_disease_group_new, "Cardiovascular system", "Cardiovascular")) |>        
    mutate(broad_disease_group_new = str_replace(
      broad_disease_group_new, "Infectious diseases", "Infections")) |>
    mutate(broad_disease_group_new = str_replace(
      broad_disease_group_new, "Genitourinary system", "Genitourinary"))
}


    
# -------------------------------------------------------------------
## Counts ##

# ALL AGES COMBINED

# N = Count of patients total with no prev diag of each disease,
# by gender and by cohort
allage_N_fun <- function(data, var1, var2) {
  data |>
    select({{var1}},{{var2}})  %>%
    group_by({{var1}},{{var2}}) %>%
    dplyr::summarize(N = n())
}

# n = Count of patients total with each sub diag of each disease if no prev diag,
# by gender and cohort
allage_n_fun <- function(data, varf1, var1, var2, var3) {
  data |>
    filter({{varf1}} == 1) %>%
    select({{var1}},{{var2}}, {{var3}})  %>%
    group_by({{var1}},{{var2}}, {{var3}}) %>%
    dplyr::summarize(n = n())
}

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
# by gender and by cohort
allage_noprev_n_fun <- function(data, varf1, varf2, var1, var2, var3) {
  data |>
    filter({{varf1}} == 0 & {{varf2}} == 1) %>%
    select({{var1}},{{var2}}, {{var3}})  %>%
    group_by({{var1}},{{var2}}, {{var3}}) %>%
    dplyr::summarize(n = n())
}



# BY AGE
# N = Count of patients total with no prev diag of each disease,
# by gender and 10 year age, by cohort
count_N_fun <- function(data, varf, var1, var2, var3, var4) {
  data |>
    filter({{varf}} == 0) %>%
    select({{var1}},{{var2}}, {{var3}}, {{var4}})  %>%
    group_by({{var1}},{{var2}}, {{var3}}, {{var4}}) %>%
    dplyr::summarize(N = n())
}

# n = Count of patients total with each sub diag of each disease if no prev diag,
# by gender and 10 year age, by cohort
count_n_fun <- function(data, varf1, varf2, var1, var2, var3, var4) {
  data |>
    filter({{varf1}} == 0 & {{varf2}} == 1) %>%
    select({{var1}},{{var2}}, {{var3}}, {{var4}})  %>%
    group_by({{var1}},{{var2}}, {{var3}}, {{var4}}) %>%
    dplyr::summarize(n = n())
}


# Suppress counts <6
suppress <- function(data, var) {
  data |>
  mutate("{{var}}" := ifelse(
    {{var}} < 6,"<6",{{var}}
    )
    )
}


# -------------------------------------------------------------------
## Stats ##

# Transform predictions to 1-100 %
prop100 <- function(data, var) {
  data |>
    mutate("{{var}}" := {{var}}*100)
}

# Observed proportions
obs_prop <- function(data) {
  data |>
  mutate(obs_est = n / N * 100)
}

# Observed CIs
obs_ci <- function(data) {
  data |>
  mutate(
         obs_lb = (binom.confint(n, N, conf.level=0.95, method="wilson")$lower)*100,
         obs_ub = (binom.confint(n, N, conf.level=0.95, method="wilson")$upper)*100
         )
}
  

# Similar/ lower/ higher
sigdiff <- function(data, group_1_lb, group_2_ub, 
                    group_1_ub, group_2_lb, 
                    group_1_est, group_2_est) {
  data |>
  mutate(sig = case_when(
    {{group_1_lb}} > {{group_2_ub}} ~ "Higher (significant)",
    {{group_1_ub}} < {{group_2_lb}} ~ "Lower (significant)")
  ) |>
  mutate(sig = ifelse(
      is.na(sig) & {{group_1_est}} > {{group_2_est}},"Higher (non-significant)",sig)) |>
  mutate(sig = ifelse(
      is.na(sig) & {{group_1_est}} < {{group_2_est}},"Lower (non-signifnicant)",sig)) |>
  mutate(sig = ifelse(
      is.na(sig) & !is.na({{group_1_est}}) & !is.na({{group_2_est}}),"Similar",sig))
}



# -------------------------------------------------------------------
## Merges ##

# Merge  disease name
disease_name <- function(data) {
  merge(x = data, y = lookup_diseases_meta [
    , c("disease_number_new", "disease_new", "broad_disease_group_num", "broad_disease_group_new")]
    , by =c("disease_number_new"), all.x = TRUE)
}

# Merge broad disease name
disease_name_b <- function(data) {
  merge(x = data, y = lookup_diseases_meta_broad [
    , c("broad_disease_group_num", "broad_disease_group_new")]
    , by =c("broad_disease_group_num"), all.x = TRUE)
}




# -------------------------------------------------------------------
## Graphs ##



load("lookup_diseases_meta_broad.RData")

# Overall colour df
broad_disease_group_new <- lookup_diseases_meta_broad |>
  select(broad_disease_group_new)

hex_code <-c("#FF0000", "#FF8000", "#CCCC00", "#80FF00", "#00CC00",
             "#00FF80", "#00CCCC", "#0080FF", "#7F00FF", "#FF00FF",
             "#FF007F", "#808080", "#994C00", "#999900", "#006633",
             "#660033")

colour_df <-data.frame(broad_disease_group_new,hex_code)



# Overall colour df
broad_disease_group_new <- lookup_diseases_meta_broad |>
  select(broad_disease_group_new)


hex_code <-c("#330000", "#FF8000", "#330000", "#330000","#330000",
             "#330000","#330000","#330000","#330000","#330000",
             "#330000","#330000","#330000","#330000","#330000",
             "#330000")
             
colour_cancer <-data.frame(broad_disease_group_new,hex_code)


rm("lookup_diseases_meta_broad")
rm("broad_disease_group_new")
