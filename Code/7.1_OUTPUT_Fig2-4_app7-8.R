# -------------------------------------------------------------------
###### Create Fig 2-4 and appendix 7-8 from models #####
# -------------------------------------------------------------------

# Drop dfs not needed
rm(list=ls())
gc()

# Add functions from functions file
source("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Code/2023_03_10//0.1_FUNCTIONS.R")

# -------------------------------------------------------------------
# Prep
# -------------------------------------------------------------------

# Packages listed in NCmisc
pacman::p_load(tidyverse,ggplot2, binom, kableExtra, patchwork, stringr,
               tidytext, writexl, htmltools, Hmisc)


# -------------------------------------------------------------------
###### % Graph 2: Increase in risk, by selected age #####
# -------------------------------------------------------------------


# -------------------------------------------------------------------
###### % Add in cancers broad group #####
# -------------------------------------------------------------------

load("model_results_byage.RData")
load("model_results_byage_broad.RData")
load("count.RData") # Cohort sizes in patients with fatigue
load("lookup_diseases_meta_broad.RData")


# Rename disease name for cancers combined
model_preds_broad <- model_preds_broad |>
  filter(broad_disease_group_new == "Cancers") |>
  mutate(
    disease_number_new = 1001,
    disease_new = "All cancers combined"
    )

# Append cancers broad group
model_preds <- rbind(model_preds,model_preds_broad)

# Change infinite or > 100% ubs to 100
model_preds <- model_preds |>
  mutate(pr_ub =
    ifelse(
      pr_ub > 100, 100, pr_ub)
  )

save(model_preds, file = "model_results_byage_combo.RData")

# -------------------------------------------------------------------
###### % Calc diff between fat vs non fat presenters #####
# -------------------------------------------------------------------

# Reshape wider
model_preds_wide <- model_preds |>
  pivot_wider(
    id_cols = c("gender", "age", "disease_number_new", "disease_new", 
                "broad_disease_group_new", "broad_disease_group_num"),
    names_from = cohort,
    values_from = c(pr_est, pr_lb, pr_ub, obs_est, obs_lb, obs_ub)
  )

# Diff between fat vs non fat presenters
model_preds_wide <- model_preds_wide %>%
  mutate(diff = pr_est_0 - pr_est_1)

# Flag if without prev diags is higher/ lower than all patients
model_preds_wide <- model_preds_wide |>
  sigdiff(pr_lb_0, pr_ub_1, pr_ub_0, pr_lb_1, pr_est_0, pr_est_1)

# Rank diseases with greatest diff, by year of age
model_preds_wide <- model_preds_wide |>
  group_by(gender, age) |>
  mutate(
    rank = order(order(diff, decreasing = TRUE))
    )

# Merge no. cases in fatigue
model_preds_wide = merge(x = model_preds_wide, y = count [
  , c("gender", "disease_number_new", "noprev_n")]
  , by =c("gender", "disease_number_new"), all.x = TRUE)

# Replace missing with 0
model_preds_wide <- model_preds_wide |>
  replace_missing(noprev_n)

save(model_preds_wide, file = "model_preds_wide.RData")


# -------------------------------------------------------------------
######  Fig 2 & 3: Age specific predictions #####
# -------------------------------------------------------------------

# Merge in diff between fat vs ref and rank
model_preds_topdiff = merge(x = model_preds, y = model_preds_wide [
  , c("gender", "age", "disease_number_new", "diff", "rank", "sig")]
  , by.x =c("gender", "age", "disease_number_new")
  , by.y =c("gender", "age", "disease_number_new")
) 

# Labels
model_preds_topdiff <- model_preds_topdiff |>
  label_gender() |>
  label_cohort() 

# Merge no. cases in fatigue
model_preds_topdiff = merge(x = model_preds_topdiff, y = count [
  , c("gender", "disease_number_new", "noprev_n")]
  , by =c("gender", "disease_number_new"), all.x = TRUE)

# Replace missing with 0
model_preds_topdiff <- model_preds_topdiff |>
  replace_missing(noprev_n)

# Only keep diseases with > 100 cases in men/ women with fatigue
model_preds_topdiff <- model_preds_topdiff |>
  mutate_at(
    vars(contains(c("_est","_lb","_ub"))),
    ~ ifelse(
      noprev_n < 100, NA, .x)
  )

# keep ages of interest
model_preds_topdiff <- model_preds_topdiff %>%
  filter(age == 40 | age == 60 | age == 80)

# Select diseases to include in graph for each age (40, 60, 80)
# Must be in top 30 diseases with greatest excess risk in fatigue presenters for that age (by gender)
diseases_incl <- model_preds_topdiff |>
  filter(grepl("Higher", sig)) |>
  filter(noprev_n >= 100) |>
  filter(cohort == 0) |>
  filter(rank <= 30) |>
  select(gender, disease_number_new, age, diff, rank) |>
  mutate(incl = 1)
  
# merge disease inclusion flag into data with %s for all three cohorts (fp, nfp, ref)
  model_preds_topdiff = merge(x = model_preds_topdiff, y = diseases_incl [
    , c("gender", "age", "disease_number_new", "incl")]
    , by =c("gender", "age", "disease_number_new"), all.x = TRUE)

# Shorten disease names for graph
  model_preds_topdiff <- model_preds_topdiff |>
    disease_labels()
 

# Men graph
g3_data <- model_preds_topdiff %>%
  filter(gender == 1 & incl == 1) |>
  group_by(cohort_lb) |>
  dplyr::mutate(
    age = as.factor(age),
    disease_new = reorder_within(disease_new,desc(rank),age)
  )


# Scatter plot v1 - 1 graph per age group, ordering disease by frequency
# Men
g3 <- ggplot2::ggplot() +
  geom_point(
    data=g3_data, aes(y=disease_new, x=pr_est, color = cohort_lb)
  ) +
  geom_errorbarh(
    data=g3_data, height=.1, aes(y=disease_new, xmin=pr_lb, xmax=pr_ub, color = cohort_lb)
  ) +
  scale_x_continuous(
    name = "Risk (%)"
  ) +
  scale_y_discrete(
    name = "") +
  theme(axis.text.y = element_text(size=9, colour = "#FF8000"), axis.text.x = element_text(size=9)) +
  labs(color = "Cohort") +
  facet_wrap(vars(age),  scales = "free") +
  scale_y_reordered(name = "") +
  theme_bw()

g3

# Save graph
ggsave("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//Fig3.png",plot=g3)



# Women
# Men graph
g4_data <- model_preds_topdiff %>%
  filter(gender == 2 & incl == 1) |>
  group_by(cohort_lb) |>
  dplyr::mutate(
    age = as.factor(age),
    disease_new = reorder_within(disease_new,desc(rank),age)
  )

# Scatter plot v1 - 1 graph per age group, ordering disease by frequency
# Women
g4 <- ggplot2::ggplot() +
  geom_point(
    data=g4_data, aes(y=disease_new, x=pr_est, color = cohort_lb)
  ) +
  geom_errorbarh(
    data=g4_data, height=.1, aes(y=disease_new, xmin=pr_lb, xmax=pr_ub, color = cohort_lb)
  ) +
  scale_x_continuous(
    name = "Risk (%)"
  ) +
  scale_y_discrete(
    name = "") +
  theme(axis.text.y = element_text(size=9, colour = "#FF8000"), axis.text.x = element_text(size=9)) +
  labs(color = "Cohort") +
  facet_wrap(vars(age),  scales = "free") +
  scale_y_reordered(name = "") +
  theme_bw()

g4

# Save graph
ggsave("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//Fig4.png",plot=g4)



#####################################################################
# -------------------------------------------------------------------
###### Fig 2: Age adjusted predictions #####
# -------------------------------------------------------------------
#####################################################################


# -------------------------------------------------------------------
###### Add observed counts by year of age #####
# -------------------------------------------------------------------


rm("model_preds","model_preds_topdiff", "g1_m","g1_f","g3","g4", "diseases_topdiff", "age_idate")
gc()
load("n_diags.RData")

# N = Count of patients total with no prev diag of each disease,
# by gender and year of age, by cohort
count_N_yr <- n_diags |>
  count_N_fun(prev_diag_main, cohort, gender, disease_number_new, age_idate)

# n = Count of patients total with each sub diag of each disease if no prev diag,
# by gender and 10 year age, by cohort
count_n_yr <- n_diags |>
  count_n_fun(prev_diag_main, sub_diag, cohort, gender, disease_number_new, age_idate)

# Merge count data
counts = merge(x = count_N_yr, y = count_n_yr [
  , c("cohort","gender", "age_idate", "disease_number_new", "n")]
  , by =c("cohort","gender", "age_idate", "disease_number_new"), all.x = TRUE)


rm("n_diags")
gc()

load("n_diags_broad.RData")

# Repeat for cancers combined
n_diags_broad <- n_diags_broad |>
  filter(broad_disease_group_num == 2) |>
  mutate(
    disease_number_new = 1001
    ) 

# N = Count of patients total with no prev diag of each disease,
# by gender and year of age, by cohort
count_N_yr_broad <- n_diags_broad |>
  count_N_fun(prev_diag_main, cohort, gender, disease_number_new, age_idate)

# n = Count of patients total with each sub diag of each disease if no prev diag,
# by gender and 10 year age, by cohort
count_n_yr_broad <- n_diags_broad |>
  count_n_fun(prev_diag_main, sub_diag, cohort, gender, disease_number_new, age_idate)

# Merge count data
counts_broad = merge(x = count_N_yr_broad, y = count_n_yr_broad [
  , c("cohort","gender", "age_idate", "disease_number_new", "n")]
  , by =c("cohort","gender", "age_idate", "disease_number_new"), all.x = TRUE)

# Append cancers broad group
counts <- rbind(counts,counts_broad)

# Replace N/a with 0 in ages with 0 cases for that disease
counts <- counts |>
  replace_missing(n)
  
# Keep counts for fatigue only
counts_fat <- counts |>
  filter(cohort == 0) |>
  rename(age = age_idate)

# Merge rates into fatigue count data (by age and sex)
counts_fat = merge(x = counts_fat, y = model_preds_wide [
  , c("gender", "age", "disease_number_new", "disease_new", 
      "broad_disease_group_new", "broad_disease_group_num",
      "pr_est_0", "pr_est_1", "pr_est_2")]
  , by.x =c("gender", "age", "disease_number_new")
  , by.y =c("gender", "age", "disease_number_new")
) 


rm("count_n_yr","count_N_yr","counts","n_diags", 
   "n_diags_broad", "count_n_yr_broad", "count_N_yr_broad", "counts_broad",
   "model_preds_broad", "model_preds_wide")
gc()


# -------------------------------------------------------------------
###### Calc age adjusted %s #####
# -------------------------------------------------------------------

# Estimate number of patients in each age & sex group with each disease
counts_fat <- counts_fat |>
  mutate(c0_n = n,
         c1_n = N * (pr_est_1/100),
         c2_n = N * (pr_est_2/100))

# Add up all men and women with each disease
counts_ageadj <- counts_fat |>
  group_by(gender, disease_number_new, disease_new, broad_disease_group_new, broad_disease_group_num) |>
  dplyr::summarize(N = sum(N),
            c0_n = sum(c0_n),
            c1_n = sum(c1_n),
            c2_n = sum(c2_n))

counts_ageadj <- counts_ageadj %>%
  mutate(N = round(N),
         c0_n = round(c0_n,0),
         c1_n = round(c1_n,0),
         c2_n = round(c2_n,0)
  )


# CIs

# Function
ci_function <- function(cases) {
  counts_ageadj <- counts_ageadj |>
    group_by({{cases}}) |>
    
    mutate(
      "{{cases}}_est" := ifelse(is.na({{cases}}), 
                                NA,
                                ({{cases}}/ N)*100
      )
    ) |>
    
    mutate(
      "{{cases}}_lb" := ifelse(is.na({{cases}}), 
                               NA,
                               (binom.confint({{cases}}, N, conf.level=0.95, method="wilson")$lower)*100
      )
    ) |>
    
    mutate(
      "{{cases}}_ub" := ifelse(is.na({{cases}}), 
                               NA,
                               (binom.confint({{cases}}, N, conf.level=0.95, method="wilson")$upper)*100
      )
    ) |>
    ungroup()
}

# Run functions
counts_ageadj <- ci_function(c0_n)
counts_ageadj <- ci_function(c1_n)
counts_ageadj <- ci_function(c2_n)

# Adjust names - remove "_n"
names(counts_ageadj) <- sub("_n_", "_", names(counts_ageadj))

# Round results  
counts_ageadj <- counts_ageadj |>
  mutate(across(contains(c("_est","_lb","_ub")),
                round, 3))

# Rank diseases for each cohort
counts_ageadj <- counts_ageadj %>%
  group_by(gender) |>
  mutate(c0_rank = order(order(c0_est, decreasing = TRUE)),
         c1_rank = order(order(c1_est, decreasing = TRUE)),
         c2_rank = order(order(c2_est, decreasing = TRUE))
  )


# Merge no. cases in fatigue
counts_ageadj = merge(x = counts_ageadj, y = count [
  , c("gender", "disease_number_new", "noprev_n")]
  , by =c("gender", "disease_number_new"), all.x = TRUE)

# Replace missing with 0
counts_ageadj <- counts_ageadj |>
  replace_missing(noprev_n) |>
  replace_missing(c0_n) |>
  replace_missing(c1_n) |>
  replace_missing(c2_n)

# Diff between fat vs non fat presenters
counts_ageadj <- counts_ageadj %>%
  mutate(diff = c0_est - c1_est)

# Only keep diseases with > 100 cases in men/ women with fatigue
counts_ageadj <- counts_ageadj |>
  mutate_at(
    vars(contains(c("diff","_est","_lb","_ub"))),
    ~ ifelse(
      noprev_n < 100, NA, .x)
  )

  save(counts_ageadj, file = "counts_ageadj.RData")

    
# -------------------------------------------------------------------
###### Create fig 2 graph #####
# -------------------------------------------------------------------

load("counts_ageadj.RData")
  
  # Make into long dataset for graphs
  counts_ageadj_long <- counts_ageadj |>
    select(!c("noprev_n")) |>
    pivot_longer(-c("gender", "disease_number_new", "disease_new", "broad_disease_group_new", "broad_disease_group_num","N"),
                 names_to = c("cohort",".value"),
                 names_sep="_")
  
  # Remove _est from cohort column
  counts_ageadj_long$cohort <- gsub("c","",as.character(counts_ageadj_long$cohort))
  
  # Create 6 dfs for gender/ cohort combos
  
  # Data frame function
  df_function <- function(gender_val, cohort_val) {
    df <- counts_ageadj_long |>
      filter(
        gender == {{gender_val}} & cohort == {{cohort_val}} # If x gender and x cohort
        & (rank <= 20 | disease_new =="All cancers combined") ## If cancer or top 20
      )
  }
  
  # Run function        
  c0_m <- df_function(1, 0)
  c1_m <- df_function(1, 1)
  c2_m <- df_function(1, 2)
  c0_f <- df_function(2, 0)
  c1_f <- df_function(2, 1)
  c2_f <- df_function(2, 2)
  
  
# Add colour scheme  
  
  # Shorten broad disease labels
  colour_df <- colour_df %>%
    broad_disease_labels() 
  
  # Format disease name as factor variable for 'nlevels' to work
  colour_df <- colour_df %>%
    mutate(broad_disease_group_new = as.factor(broad_disease_group_new))
  
  # Order by broad disease group
  colour_df <- colour_df[order(colour_df$broad_disease_group_new, na.last=TRUE),]
  
  
  
# Create graph function
graph_function <-function(df) {
  # Change to generic data frame name
  data <- df # To check/ run
  
  # Labels
  data <- data %>%
    mutate(gender_lb = case_when (
      gender == 1 ~ "Men",
      gender == 2 ~ "Women"
    )) %>%
    mutate(cohort_lb = case_when (
      cohort == 0 ~ "Fatigue presenters",
      cohort == 1 ~ "Non-fatigue presenters",
      cohort == 2 ~ "Registered patients"
    ))
  
  # Shorten disease labels
  data <- data %>%
    disease_labels() |>
    mutate(disease_new = str_trunc(disease_new,20,"right"))
  
  
  # Shorten broad disease labels
  data <- data %>%
    broad_disease_labels() 
    
  # Format disease name as factor variable for 'nlevels' to work
  data <- data %>%
    mutate(disease_new = as.factor(disease_new)) %>%
    mutate(disease_number_new = as.factor(disease_number_new)) %>%
    mutate(broad_disease_group_new = as.factor(broad_disease_group_new))
  
  # Colour df just for this data frame
  temp_colour_df <- data |>
    left_join(colour_df) |>
    select(broad_disease_group_new, hex_code) |>
    group_by(broad_disease_group_new, hex_code)|>
    slice_max(order_by = hex_code, n = 1, with_ties = FALSE)
  
  # Pull out Colour scheme values just for this df
  temp_colour <- temp_colour_df |>
    pull(hex_code)
  
  
  # x10 so it plots better
  data <- data |>
    mutate(est = est * 10)
  
  # Add a gap between each main group
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 1
  nlevels <- nlevels(data$broad_disease_group_new)
  to_add <- data.frame(
    matrix(NA, empty_bar*nlevels, ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$broad_disease_group_new <- rep(levels(data$broad_disease_group_new), each=empty_bar)
  data <- rbind(data, to_add)
  
  # Order by disease group and then freq of disease
  data <- data[order(data$broad_disease_group_new, data$est, na.last=TRUE),]
  
  # ID for each row
  data$id <- seq(1, nrow(data))
  
  # Delete last observation so circle joins up
  data <- head(data,-1)
  
  # ----- This section prepare a dataframe for labels ---- #
  # Get the name and the y position of each label
  label_data <- data
  
  # calculate the ANGLE of the labels
  number_of_bar <- nrow(label_data)
  angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  
  # flip angle BY to make them readable
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  
  # ----- Prep a data frame for base lines ---- #
  
  base_data <- data %>%
    group_by(broad_disease_group_new) %>%
    dplyr::summarize(start=min(id), end=max(id) - empty_bar ) %>%
    rowwise() %>%
    mutate(title = mean(c(start,end)))
  
  # Add 1 to final 'end' (11 rather than 10)
  base_data$end[nlevels] = number_of_bar
  
  
  # ----- Prep a data frame for grid ---- #
  
  # make grid lines go all the way round
  grid_data <- base_data
  grid_data$start[1] = number_of_bar+1
  grid_data$end[1] = 0
  grid_data <- grid_data %>%
    filter(end == 0)
  
  
  # ----- ------------------------------------------- ---- #
  # Make the plot
  p <- ggplot(data, aes(x=as.factor(id), y=est, fill=broad_disease_group_new)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(stat="identity", alpha=0.5) +
    
    # Set broad disease group colour so it stays the same in each gender/ cohort graph - doesn't work
    scale_fill_manual(
      values=alpha(temp_colour)) +

    # Add grid lines val = 100, 75, 50, 25 %
    geom_segment(data = grid_data, aes(x = end, y= 0, xend = start, yend = 0),
                 colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
    geom_segment(data = grid_data, aes(x = end, y= 20, xend = start, yend = 20),
                 colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
    geom_segment(data = grid_data, aes(x = end, y= 40, xend = start, yend = 40),
                 colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
    geom_segment(data = grid_data, aes(x = end, y= 60, xend = start, yend = 60),
                 colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
    
    # Add bar again so it layers on top of grid lines
    geom_bar(stat="identity", alpha=0.5) +
    
    # Add grid line values 100, 75, 50, 25
    annotate("text", x = rep(number_of_bar+1,4),y = c(0,20,40,60), label = c("0","2","4","6"), 
             color = "grey", size = 3, angle = 0, fontface = "bold", hjust = 0.5) +
    
    ylim(-100,120) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    coord_polar() + 
    geom_text(data=label_data, 
              aes(x=id, y=est+10, label=disease_new, hjust=hjust), 
              color="black", fontface="bold",alpha=0.6, size=3.2, 
              angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add group label
    geom_text(data=base_data, aes(x = title, y = -25, label = broad_disease_group_new), 
              #hjust=c(1,0.5,0), 
              colour = "black", apha = 0.8, size = 2.5, 
              fontface = "bold", inherit.aes=FALSE) +
    
    ggtitle(paste0(data$gender_lb,sep=", ",data$cohort_lb)) # Title for each graph +
  
}

# Run the function for each df
result <- list(c0_m, c1_m, c2_m, c0_f,  c1_f, c2_f) |>
  lapply(graph_function)

# View
# Combine graphs
g2 <- 
  result[[1]] + result[[2]] + result[[3]] + 
  result[[4]] + result[[5]] + result[[6]] + plot_layout(ncol = 3) 
g2

# Save graph
ggsave("S:/ECHO_IHI_CPRD/Becky/Fatigue other disease/Table exports//Fig2.png",plot=g2)

