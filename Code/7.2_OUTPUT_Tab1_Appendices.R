
# -------------------------------------------------------------------
###### Table 1 & collect/ output appendices 7-8 #####
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Prep
# -------------------------------------------------------------------

# Add functions from functions file
source("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Code/2023_03_10//0.1_FUNCTIONS.R")

# Packages listed in NCmisc
pacman::p_load(tidyverse,ggplot2, binom, kableExtra, patchwork, stringr,
               tidytext, writexl, htmltools, Hmisc)


#####################################################################
# -------------------------------------------------------------------
###### % Table 1: Demogs #####
# -------------------------------------------------------------------
#####################################################################

load("cohort.RData")
load("n_diags_broad.RData")
load("lookup_diseases_meta_broad.RData")
load("lookup_diseases_meta.RData")

# -------------------------------------------------------------------
###### Combine aggregated tables #####
# -------------------------------------------------------------------

# Add patients with any diagnosis
# Flag for each patient if they had prev or sub diag of any disease 
n_diags_any <- n_diags_broad |>
  select(-c(broad_disease_group_num)) |>
  group_by(cohort, e_patid) |>
  dplyr::summarize(
    prev_diag_main = prev_diag_main[which(prev_diag_main == 1)[1]],
    sub_diag = sub_diag[which(sub_diag == 1)[1]],
    gender = gender[[1]],
    age_idate = age_idate[[1]],
    age_cat_10 = age_cat_10[[1]]
  ) 


  # -------------------------------------------------------------------


# Counts

# Count of patients in each age cat, by gender
age_med <- cohort %>%
  select(cohort, gender, age_idate)  %>%
  group_by(cohort, gender) %>%
  dplyr::summarize(n = as.numeric(median(age_idate))) |>
  mutate(var = "Median age") |>
  mutate(varlevel = "Years")

# Count of patients in each age cat, by gender
age <- cohort %>%
  select(cohort, gender, age_cat_10)  %>%
  group_by(cohort, gender, age_cat_10) %>%
  dplyr::summarize(n = n()) |>
  mutate(var = "Age group") |>
  rename(varlevel = age_cat_10)

# Count of n_diags_broad with prev diags of any disease, by gender
prev_diag_any <- n_diags_any %>%
  filter(prev_diag_main == 1) |>
  select(cohort, gender, prev_diag_main)  %>%
  group_by(cohort, gender) %>%
  dplyr::summarize(n = n()) |>
  mutate(var = "Previous diagnoses")

# Count of n_diags_broad with prev diags of broad groups, by gender
prev_diag <- n_diags_broad %>%
  filter(prev_diag_main == 1) |>
  select(cohort, gender, broad_disease_group_num, prev_diag_main)  %>%
  group_by(cohort, gender, broad_disease_group_num) %>%
  dplyr::summarize(n = n()) |>
  mutate(var = "Previous diagnoses")


# Count patients with sub diag of any disease, by gender
sub_diag_any <- n_diags_any %>%
  filter(sub_diag == 1) |>
  select(cohort, gender, sub_diag)  %>%
  group_by(cohort, gender) %>%
  dplyr::summarize(n = n()) |>
  mutate(var = "Subsequent diagnoses")

# Count of patients with sub diags of broad groups, by gender
sub_diag <- n_diags_broad %>%
  filter(sub_diag == 1) |>
  select(cohort, gender, broad_disease_group_num, sub_diag)  %>%
  group_by(cohort, gender, broad_disease_group_num) %>%
  dplyr::summarize(n = n()) |>
  mutate(var = "Subsequent diagnoses")

# Add broad disease names
prev_diag = merge(x = prev_diag, y = lookup_diseases_meta_broad [
  , c("broad_disease_group_num", "broad_disease_group_new")]
  , by =c("broad_disease_group_num"), all.x = TRUE)

sub_diag = merge(x = sub_diag, y = lookup_diseases_meta_broad [
  , c("broad_disease_group_num", "broad_disease_group_new")]
  , by =c("broad_disease_group_num"), all.x = TRUE)

# Format 'varlevel' so counts can all be added into one table
prev_diag_any <- prev_diag_any |>
  mutate(varlevel = "Any disease studied")

prev_diag <- prev_diag |>
  select(-c("broad_disease_group_num")) |>
  rename(varlevel = broad_disease_group_new)

sub_diag_any <- sub_diag_any |>
  mutate(varlevel = "Any disease studied")

sub_diag <- sub_diag |>
  select(-c("broad_disease_group_num")) |>
  rename(varlevel = broad_disease_group_new)

# Bind vars
demogs <- rbind(age,age_med,sub_diag_any,sub_diag,prev_diag_any,prev_diag)

# Count of patients total, by cohort & gender
n_total <- cohort |>
  select(cohort, gender)  %>%
  group_by(cohort, gender) %>%
  dplyr::summarize(N = n())

# Merge totals together
demogs = merge(x = demogs, y = n_total [
  , c("cohort","gender", "N")]
  , by =c("cohort","gender"), all.x = TRUE)

# Add totals row
n_total <- n_total |>
  mutate(n = N) |>
  mutate(varlevel = "N") |>
  mutate(var = "Total")

demogs <- rbind(demogs, n_total)


# -------------------------------------------------------------------
###### Format tables #####
# -------------------------------------------------------------------

# # % with diagnosis & CI
demogs <- demogs %>%
  mutate(prop = ifelse(
    varlevel != "N" & varlevel != "Years",
    round(n / N * 100,2),
    "-") 
  ) |>
  select(-c("N"))

# Reshape wider
demogs_wide <- demogs |>
  arrange(gender,cohort, var, varlevel) |>
  pivot_wider(
    id_cols = c("var", "varlevel"),
    names_from = (c(gender,cohort)),
    values_from = (c(n,prop)),
    names_vary = 'slowest'
  ) |>
  arrange(var, varlevel) |>
  select(-c(var))


# Age group by gender and cohort
table1 <- kbl(demogs_wide,
              col.names = c(
                "",
                "n","%","n","%","n","%",
                "n","%","n","%","n","%"),
              format.args = list(big.mark = ",", scientific = FALSE)) |>
  
  kable_classic() |>
  
  add_header_above(
    c(" " = 1, 
      "Fatigue presenters" = 2, "Non-fatigue presenters" = 2, "Registered patients" = 2,
      "Fatigue presenters" = 2, "Non-fatigue presenters" = 2, "Registered patients" = 2)
  ) |>
  
  add_header_above(
    c(" " = 1, "Men" = 6, "Women" = 6)
  ) |>

  
  column_spec(7, border_right=T) |>
    
  pack_rows("Age group", 1, 7) |>
  pack_rows("Median age",8,8) |>
  pack_rows("Previous diagnoses", 9, 25) |>
  pack_rows("Subsequent diagnoses", 26, 43) |>
  pack_rows("Total", 43, 43) |>
  
  save_kable(file = "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//table1.html", self_contained = T)

table1

rm("age","cohort","n_diags_broad","n_total","prev_diag","sub_diag")



# -------------------------------------------------------------------
###### Appendix 7 for fig 2 #####
# -------------------------------------------------------------------

load("counts_ageadj.RData")

# Round results
counts_ageadj <- counts_ageadj %>%
  mutate_at(
    vars(contains(c("diff","_est","_lb","_ub"))), ~round(.,2)
  )


# Similar/ lower/ higher
counts_ageadj <- counts_ageadj |>
  sigdiff(c0_lb, c1_ub, c0_ub, c1_lb, c0_est, c1_est) |>
  rename(sig_f_vs_nfp = sig) |>
  sigdiff(c0_lb, c2_ub, c0_ub, c2_lb, c0_est, c2_est) |>
  rename(sig_f_vs_ref = sig) |>
  sigdiff(c1_lb, c2_ub, c1_ub, c2_lb, c1_est, c2_est) |>
  rename(sig_nfp_vs_ref = sig)

# Add labelled values
counts_ageadj <- counts_ageadj |>
  label_gender()

# Summary stats of sig diff fp vs nfp, by gender
summary_sig <- counts_ageadj |>
  filter(disease_number_new != 1001) |>
  select(gender_lb,sig_f_vs_nfp) |>
  group_by(gender_lb,sig_f_vs_nfp) |>
  dplyr::summarize(n = n())


# Export results
write_xlsx(summary_sig, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App7_supp1. Sigdiff_fvsnfp.xlsx",
           format_headers = TRUE)



# How many diseases were higher in both men and women?
# either sig or non sig higher
summary_sig <- counts_ageadj |>
  filter(disease_number_new != 1001 
         & (sig_f_vs_nfp == "Higher (significant)" | sig_f_vs_nfp == "Higher (non-significant)" )) |>
  group_by(disease_number_new) |>
  dplyr::summarize(n = n()) |>
  filter(n == 2) |>
  group_by(n) |>
  dplyr::summarize(disease_count = n())

# Export results
write_xlsx(summary_sig, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App7_supp2. Sigdiff_fvsnfp_mandw.xlsx",
           format_headers = TRUE)


# How many diseases were higher in both men and women?
# Sig only
summary_sig <- counts_ageadj |>
  filter(disease_number_new != 1001 & sig_f_vs_nfp == "Higher (significant)") |>
  group_by(disease_number_new) |>
  dplyr::summarize(n = n()) |>
  filter(n == 2) |>
  group_by(n) |>
  dplyr::summarize(disease_count = n())

# Export results
write_xlsx(summary_sig, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App7_supp3. Sigdiff_fvsnfp_mandw_sig.xlsx",
           format_headers = TRUE)


# Summary stats of sig diff nfp vs reg pats, by gender
summary_sig <- counts_ageadj |>
  filter(disease_number_new != 1001) |>
  select(gender_lb,sig_nfp_vs_ref) |>
  group_by(gender_lb,sig_nfp_vs_ref) |>
  dplyr::summarize(n = n())

# Export results
write_xlsx(summary_sig, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App7_supp4. Sigdiff_nfpvsref.xlsx",
           format_headers = TRUE)


# Suppres counts < 6
counts_ageadj <- counts_ageadj |>
  suppress(c0_n) |>
  suppress(c1_n) |>
  suppress(c2_n) |>
  suppress(noprev_n) 

# Order vars
counts_ageadj <- counts_ageadj |>
  select(c(
    "gender_lb",
    "broad_disease_group_num",
    "broad_disease_group_new",
    "disease_number_new",
    "disease_new",
    contains("c0"),
    contains("c1"),
    contains("c2"),
    diff,
    "sig_f_vs_nfp",
    "sig_f_vs_ref",
    "sig_nfp_vs_ref"
  )
  )

# Var labels
label(counts_ageadj) = as.list(
  var.labels[match(names(counts_ageadj),
                   names(var.labels))])

# Export results - main appendix 7
app6 <- counts_ageadj
names(app6) <- label(app6)
write_xlsx(app6, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App7. Age-adjusted risk.xlsx",
           format_headers = TRUE)


# Top diseases in any sex
counts_ageadj_top <- counts_ageadj |>
  filter(diff > 1) |>
  select(broad_disease_group_num, broad_disease_group_new, disease_number_new, disease_new, c0_est, diff) |>
  group_by(disease_number_new, disease_new) |>
  dplyr::slice_max(diff) |>
  dplyr::slice_max(c0_est) |>
  rename(max_diff = diff) |>
  ungroup()

# Merge into sex specific estimates
counts_ageadj = merge(x = counts_ageadj, y = counts_ageadj_top [
  , c("disease_number_new", "max_diff")]
  , by =c("disease_number_new"), all.x = TRUE)

# Top diseases in any sex
counts_ageadj_top <- counts_ageadj |>
  filter(c0_est > 1) |>
  select(broad_disease_group_num, broad_disease_group_new, disease_number_new, disease_new, c0_est, diff) |>
  group_by(disease_number_new, disease_new) |>
  dplyr::slice_max(diff) |>
  dplyr::slice_max(c0_est) |>
  rename(max_c0_est = c0_est) |>
  ungroup()

# Merge into sex specific estimates
counts_ageadj = merge(x = counts_ageadj, y = counts_ageadj_top [
  , c("disease_number_new", "max_c0_est")]
  , by =c("disease_number_new"), all.x = TRUE)


# % for top diseases (> 1% actual risk) in each sex
counts_ageadj_top_aer <- counts_ageadj |>
  filter(!is.na(max_diff)) |>
  select(gender_lb, broad_disease_group_num, broad_disease_group_new, disease_number_new, disease_new, c0_est, diff, max_diff)

# Reshape wider
counts_ageadj_top_aer <- counts_ageadj_top_aer |>
  pivot_wider(
    id_cols = c("disease_number_new", "disease_new"),
    names_from = (c(gender_lb)),
    values_from = (c(c0_est,diff,max_diff)),
    names_vary = 'slowest'
  ) |>
  select(!c(max_diff_Women)) |>
  rename(max_diff = max_diff_Men) |>
  arrange(disease_number_new, disease_new, c0_est_Men, diff_Men, c0_est_Women, diff_Women)


# % for top diseases (> 1% actual risk) in each sex
counts_ageadj_top_r <- counts_ageadj |>
  filter(!is.na(max_c0_est)) |>
  select(gender_lb, broad_disease_group_num, broad_disease_group_new, disease_number_new, disease_new, c0_est, diff, max_c0_est)

# Reshape wider
counts_ageadj_top_r <- counts_ageadj_top_r |>
  pivot_wider(
    id_cols = c("disease_number_new", "disease_new"),
    names_from = (c(gender_lb)),
    values_from = (c(c0_est,diff,max_c0_est)),
    names_vary = 'slowest'
  ) |>
  select(!c(max_c0_est_Women)) |>
  rename(max_c0_est = max_c0_est_Men) |>
  arrange(disease_number_new, disease_new, c0_est_Men, diff_Men, c0_est_Women, diff_Women)

# Var labels
top_lb <- c(
  disease_number_new = "Disease no.",
  disease_new = "Disease",
  c0_est_Women = "Female fatigue presenters: modelled risk (%)",
  c0_est_Men = "Male fatigue presenters: modelled risk (%)",
  diff_Women = "Absolute excess risk (%) in female fatigue vs non-fatigue presenters",
  diff_Men = "Absolute excess risk (%) in male fatigue vs non-fatigue presenters",
  max_c0_est = "Max risk in men or women",
  max_diff = "Max diff in men or women"
  ) 

label(counts_ageadj_top_aer) = as.list(
  top_lb[match(names(counts_ageadj_top_aer),
              names(top_lb))])

label(counts_ageadj_top_r) = as.list(
  top_lb[match(names(counts_ageadj_top_r),
              names(top_lb))])


# Short list of top diseases for clinician categorisation of incident diseases
# Either >1% actual risk or >1% absolute excess risk
counts_ageadj_top <- counts_ageadj |>
  filter(c0_est > 1 | diff > 1) |>
  select(broad_disease_group_num, broad_disease_group_new, disease_number_new, disease_new, c0_est) |>
  group_by(disease_number_new, disease_new) |>
  dplyr::slice_max(c0_est)

# Export results
names(counts_ageadj_top_aer) <- label(counts_ageadj_top_aer)
write_xlsx(counts_ageadj_top_aer, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//Box1. Age-adjusted risk_top_1paer.xlsx",
           format_headers = TRUE)

names(counts_ageadj_top_r) <- label(counts_ageadj_top_r)
write_xlsx(counts_ageadj_top_r, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//Box2. Age-adjusted risk_top_1prisk.xlsx",
           format_headers = TRUE)

names(counts_ageadj_top) <- label(counts_ageadj_top)
write_xlsx(counts_ageadj_top, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App7_supp1. Age-adjusted risk_top.xlsx",
           format_headers = TRUE)


# -------------------------------------------------------------------
###### Appendix 8 for Fig 3-4 #####
# -------------------------------------------------------------------

load("model_preds_wide.RData")

# Only keep diseases with > 100 cases in men/ women with fatigue
model_preds_wide <- model_preds_wide |>
  mutate_at(
    vars(contains(c("diff","pr_","obs_", "sig"))),
    ~ifelse(
      noprev_n < 100, NA, .x)
  )

# Round results
model_preds_wide <- model_preds_wide %>%
  mutate_at(
    vars(contains(c("diff","pr_","obs_"))), funs(round(.,2))
  )

# Suppres counts < 6
model_preds_wide <- model_preds_wide |>
  suppress(noprev_n) 

# Keep vars needed
model_preds_wide <- model_preds_wide |>
  select(!contains(c("obs_")))

# Add labelled values
model_preds_wide <- model_preds_wide |>
  label_gender()

# Order vars
model_preds_wide <- model_preds_wide |>
  select(c(
    "gender_lb",
    "age",
    "broad_disease_group_num",
    "broad_disease_group_new",
    "disease_number_new",
    "disease_new",
    contains("_0"),
    contains("_1"),
    contains("_2"),
    "diff",
    "rank",
    "sig"
  )
  )

# Label vars
label(model_preds_wide) = as.list(
  var.labels[match(names(model_preds_wide),
                   names(var.labels))])

# Keep only 5 year increments of age
five_age <- c(30,35,40,45,50,55,60,65,70,75,80,85,90)
model_preds_wide <- model_preds_wide |>
  filter(
    age %in% five_age
  )

# Top diseases (> 1% actual risk) in each age/ sex combo
agespec_top <- model_preds_wide |>
  filter(pr_est_0 > 1 & (age == 40 | age == 60 | age == 80)) |>
  select(gender_lb, age, broad_disease_group_num, broad_disease_group_new, disease_number_new, disease_new, pr_est_0, diff) |>
  group_by(gender_lb, age, disease_number_new, disease_new) |>
  dplyr::summarize(
    pr_est_0 = pr_est_0[[1]],
    diff = diff[[1]]
  ) |>
  ungroup()

# Label vars
label(agespec_top) = as.list(
  var.labels[match(names(agespec_top),
                   names(var.labels))])

# Top diseases - deduplicated (>1% risk in any age/ sex combo)
agespec_top_dedup <- agespec_top |>
  select(disease_number_new, disease_new, pr_est_0) |>
  group_by(disease_number_new, disease_new) |>
  dplyr::slice_max(pr_est_0)

# Export results
names(model_preds_wide) <- label(model_preds_wide)
write_xlsx(model_preds_wide, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App8. Age-specific risk.xlsx",
           format_headers = TRUE)

# Export results
names(agespec_top) <- label(agespec_top)
write_xlsx(agespec_top, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//Box3. Age-specific risk_top.xlsx",
           format_headers = TRUE)

# Export results
names(agespec_top_dedup) <- label(agespec_top_dedup)
write_xlsx(agespec_top_dedup, 
           "S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//App8_supp1. Age-specific risk_topdedup.xlsx",
           format_headers = TRUE)

rm("age_med","c0_f","c1_f","c2_f","c1_m","c2_m","c0_m",
   "count","g1_f","g1_m","g2_f_colour_df","g2_m_colour_df","g2f","g2m",
   "mtest_f","mtest_m","p_group","rank_60","result","summary_rank_1","summary_rank_2",
   "summary_rank_3","summary_sig","test",
   "counts_ageadj","counts_ageadj_long","counts_fat","demogs","demogs_wide","diseases_incl",
   "model_preds","model_preds_wide")

gc()