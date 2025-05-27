#install.packages("languageserver")
#install.packages("tidyverse")
# install.packages("splines")
# install.packages("ggeffects")
# install.packages("patchwork")
# install.packages("gridExtra")

#######################################################################################
# IMPORT LIBRARIES
#######################################################################################
library(splines)
library(ggeffects)
library(patchwork)
library(tidyverse)
library(gridExtra)
library(scales)

#######################################################################################
# Set matched or whole group
#######################################################################################
whole_group_or_matched = 'matched'
truncate_threshold=10
#######################################################################################
# Load data
#######################################################################################
# Construct paths to the CSV files
grouped_mortality_outcomes_path_UMCU <- "C:\\motten\\projects\\hemat\\data\\processed\\survival\\UMCU_matched_grouped_mortality_outcomes.csv"
grouped_support_windows_path_UMCU <- "C:\\motten\\projects\\hemat\\data\\processed\\survival\\UMCU_grouped_support_windows.csv"
grouped_support_path_UMCU <- "C:\\motten\\projects\\hemat\\data\\processed\\survival\\UMCU_grouped_support.csv"

# Load tables
grouped_mortality_outcomes_UMCU <- read_csv(grouped_mortality_outcomes_path_UMCU)
grouped_support_windows_UMCU <- read_csv(grouped_support_windows_path_UMCU)
grouped_support_UMCU <- read_csv(grouped_support_path_UMCU)


# Construct paths to the CSV files
grouped_mortality_outcomes_path_AUMC <- "C:\\motten\\projects\\hemat\\data\\processed\\survival\\matched_grouped_mortality_outcomes.csv"
grouped_support_windows_path_AUMC <- "C:\\motten\\projects\\hemat\\data\\processed\\survival\\matched_grouped_support_windows.csv"
grouped_support_path_AUMC <- "C:\\motten\\projects\\hemat\\data\\processed\\survival\\matched_grouped_support.csv"

# Load tables
grouped_mortality_outcomes_AUMC <- read_csv(grouped_mortality_outcomes_path_AUMC)
grouped_support_windows_AUMC <- read_csv(grouped_support_windows_path_AUMC)
grouped_support_AUMC <- read_csv(grouped_support_path_AUMC)

#######################################################################################
# Merge data
#######################################################################################
# Merge grouped_mortality_outcomes onto grouped_support_windows based on 'Hospital_Admission_TK'
grouped_support_windows_UMCU <- left_join(grouped_support_windows_UMCU, grouped_mortality_outcomes_UMCU, by = "Hospital_Admission_TK")
grouped_support_UMCU <- left_join(grouped_support_UMCU, grouped_mortality_outcomes_UMCU, by = "Hospital_Admission_TK")

# Merge grouped_mortality_outcomes onto grouped_support_windows based on 'PatientContactId'
grouped_support_windows_AUMC <- left_join(grouped_support_windows_AUMC, grouped_mortality_outcomes_AUMC, by = "PatientContactId")
grouped_support_AUMC <- left_join(grouped_support_AUMC, grouped_mortality_outcomes_AUMC, by = "PatientContactId")

#######################################################################################
# Prepare concat of data
#######################################################################################

# rename PatientContactId to Hospital_Admission_TK for the AUMC data
grouped_support_windows_AUMC <- grouped_support_windows_AUMC %>%
  rename(Hospital_Admission_TK = PatientContactId)

grouped_support_AUMC <- grouped_support_AUMC %>%
  rename(Hospital_Admission_TK = PatientContactId)


# Select columns from grouped_support_windows_UMCU to match AUMC
aligned_umcu <- grouped_support_windows_UMCU %>%
  select(
    Hospital_Admission_TK,             # Match directly
    ICMCOpnameDatumTijd_td,            # Match directly
    ICMCVertrekDatumTijd_td,           # Match directly
    day_of_admission_counter,          # Match directly
    mech_vent,                         # Match directly
    rrt,                               # Match directly
    vasopr_inotr,                      # Match directly
    transfusion,                       # Match directly
    organ_support_minimal_two_incl_trans,  # Match directly
    organ_support_minimal_three_incl_trans, # Match directly
    organ_support_minimal_four_incl_trans,  # Match directly
    organ_support_minimal_two_excl_trans,   # Match directly
    organ_support_minimal_three_excl_trans, # Match directly
    mech_vent_counter,                 # Match directly
    rrt_counter,                       # Match directly
    vasopr_inotr_counter,              # Match directly
    transfusion_counter,               # Match directly
    organ_support_minimal_two_incl_trans_counter, # Match directly
    organ_support_minimal_three_incl_trans_counter, # Match directly
    organ_support_minimal_four_incl_trans_counter, # Match directly
    organ_support_minimal_two_excl_trans_counter, # Match directly
    organ_support_minimal_three_excl_trans_counter, # Match directly
    malignant_hemat,                   # Match directly
    mort_30d,                          # Match directly
    mort_90d,                          # Match directly
    mort_1y,                           # Match directly
    mort_5y,                           # Match directly
    Status,                            # Match directly
    Survival_in_days,                  # Match directly
    ICMCtrajectLigduurDays_binned      # Match directly
  )

# Identify the extra columns to drop
extra_columns <- setdiff(colnames(grouped_support_windows_AUMC), colnames(aligned_umcu))

# Drop the extra columns
grouped_support_windows_AUMC <- grouped_support_windows_AUMC[, !(colnames(grouped_support_windows_AUMC) %in% extra_columns)]

# Check if the columns in aligned_umcu now match AUMC
setdiff(colnames(grouped_support_windows_AUMC), colnames(aligned_umcu))
setdiff(colnames(aligned_umcu), colnames(grouped_support_windows_AUMC))

#######################################################################################
# Rbind data
#######################################################################################
# Combine the two datasets
grouped_support_windows <- rbind(grouped_support_windows_AUMC, aligned_umcu)

# Verify the structure of the combined dataset
str(grouped_support_windows)

####
preproces_func<-function(truncate_boundry){
multiplerows <- multiplerows %>%
  filter(organ_support < truncate_boundry) 

multiplerows = multiplerows %>% 
    # Retain only days for which data is available (i.e. dont interpolate to last patient)
    filter(organ_support %in% multiplerows$organ_support) %>% 
    group_by(organ_support) %>% 
    mutate(outcome_mean = mean(outcome)) %>% 
    ungroup()


# survsum = multiplerows %>% 
#     group_by(los, malignant_hemat) %>% 
#     summarize(n = n(),
#               mort90d = mean(mort90d))
survsum <- multiplerows %>%
  group_by(organ_support, malignant_hemat) %>%
  summarize(n = n(), outcome = mean(outcome), .groups = 'drop')


hemat_filtered_survsum <- survsum %>%
  filter(malignant_hemat == 1)

non_hemat_filtered_survsum <- survsum %>%
  filter(malignant_hemat == 0)

hemat_filtered_multiplerows <- multiplerows %>%
  filter(malignant_hemat == 1)

non_hemat_filtered_multiplerows <- multiplerows %>%
  filter(malignant_hemat == 0)


hemat_nplot <- ggplot(data = hemat_filtered_survsum, aes(x = organ_support, y = n, fill = factor(malignant_hemat))) +
  geom_col() +
  scale_fill_manual(values = c("1" = "blue")) +  
  theme(aspect.ratio = 0.2)
  

non_hemat_nplot = ggplot(data = non_hemat_filtered_survsum, aes(x = organ_support, y = n, fill = factor(malignant_hemat))) +
  geom_col() +
  scale_fill_manual(values = c("0" = "red")) +  
  theme(aspect.ratio = 0.2)

(ggplot(data= survsum, aes(x=organ_support, color = factor(malignant_hemat))) +
    geom_point(aes(y=outcome)) +  
    expand_limits(y=c(0,1)) + 
    ggtitle("Observed mortality rate")) /
    hemat_nplot +
    non_hemat_nplot +
    scale_color_manual(values = c("blue", "red")) |

(ggplot(data= survsum, aes(x=organ_support, color = factor(malignant_hemat))) +
    geom_point(aes(y=qlogis(outcome))) + 
    ggtitle("Logit of Observed mortality rate")) /
    hemat_nplot +
    non_hemat_nplot +
    scale_color_manual(values = c("blue", "red"))  # Define colors for 0 and 1
}

####
# initialize dfs for bootstrap fits
#####

# Prepare data frames for storing bootstrap results
bootpred = tibble(iter = integer(),
                        organ_support = numeric(),
                        outcome = numeric())
bootpred_csv = tibble(iter = integer(),
                        organ_support = numeric(),
                        outcome = numeric())

    ######################################################################################
    # BOOTSTRAP TO FIND OPTIMAL NUMBER OF SPLINES IN TOTAL POPULATION
    ######################################################################################

bootstrap_aic <- function(data, iterations, truncate_boundry) {
  unique_ids <- unique(data$id)
  num_unique_ids <- length(unique_ids)
  
  # Preallocate a vector to store AIC values
  aic_values_fit1 <- numeric(iterations)
  aic_values_fit2k <- numeric(iterations)
  aic_values_fit3k <- numeric(iterations)
  aic_values_fit4k <- numeric(iterations)
  aic_values_fit5k <- numeric(iterations)
  aic_values_fit6k <- numeric(iterations)
  aic_values_fit7k <- numeric(iterations)
  
  # Create knots
  for (k in 1:iterations) {
    # Step 1: Sample IDs with replacement
    id_b <- sample(unique_ids, size = num_unique_ids, replace = TRUE)
    
    # Step 2: Create a new dataframe where each ID in id_b is represented appropriately
    id_count <- table(id_b)
    multiplerows_b <- data[data$id %in% id_b, ]
    multiplerows_b <- multiplerows_b[rep(1:nrow(multiplerows_b), times = id_count[as.character(multiplerows_b$id)]), ]
    
    # Calculate quantile knots
    knots2 <- quantile(multiplerows_b$organ_support, c(0.33, 0.67))
    knots3 <- quantile(multiplerows_b$organ_support, c(0.10, 0.50, 0.90))
    knots4 <- quantile(multiplerows_b$organ_support, c(0.05, 0.35, 0.65, 0.95))
    knots5 <- quantile(multiplerows_b$organ_support, c(0.05, 0.275, 0.50, 0.725, 0.95))
    knots6 <- quantile(multiplerows_b$organ_support, c(0.05, 0.23, 0.41, 0.59, 0.77, 0.95))
    knots7 <- quantile(multiplerows_b$organ_support, c(0.025, 0.1833, 0.3417, 0.50, 0.6583, 0.8167, 0.975))
    
    # Filter the knots to ensure they have unique values
    knots2 <- knots2[!duplicated(knots2)]
    knots3 <- knots3[!duplicated(knots3)]
    knots4 <- knots4[!duplicated(knots4)]
    knots5 <- knots5[!duplicated(knots5)]
    knots6 <- knots6[!duplicated(knots6)]
    knots7 <- knots7[!duplicated(knots7)]
       
    # Fit models with valid knots
    fits <- list()
    
    # Fit model with linear term (no spline)
    fit1 <- glm(outcome ~ organ_support + organ_support == 0, data = multiplerows_b, family = binomial())
    
    aic_values_fit1[k] <- AIC(fit1)
    fits[[1]] <- fit1
    
    # Fit models with splines only if knots are valid (the number of unique knots equal to number of knots)
    if (length(knots2) == 2) {
      fit2k <- glm(outcome ~ ns(organ_support, knots = knots2)+ (organ_support == 0), data = multiplerows_b, family = binomial())
      if (!is.null(fit2k)) {
        aic_values_fit2k[k] <- AIC(fit2k)
        fits[[2]] <- fit2k
    }
    }
    
    if (length(knots3) == 3) {
      fit3k <- glm(outcome ~ ns(organ_support, knots = knots3)+ (organ_support == 0), data = multiplerows_b, family = binomial())
      if (!is.null(fit3k)) {
        aic_values_fit3k[k] <- AIC(fit3k)
        fits[[3]] <- fit3k
    }
    }
    
    if (length(knots4) == 4) {
      fit4k <- glm(outcome ~ ns(organ_support, knots = knots4)+ (organ_support == 0), data = multiplerows_b, family = binomial())
      if (!is.null(fit4k)) {
        aic_values_fit4k[k] <- AIC(fit4k)
        fits[[4]] <- fit4k
    }
    }
    
    if (length(knots5) == 5) {
      fit5k <- glm(outcome ~ ns(organ_support, knots = knots5)+ (organ_support == 0), data = multiplerows_b, family = binomial())
      if (!is.null(fit5k)) {
        aic_values_fit5k[k] <- AIC(fit5k)
        fits[[5]] <- fit5k
    }
  }
  #   if (length(knots6) == 6) {
  #     fit6k <- glm(outcome ~ ns(organ_support, knots = knots6) + (organ_support == 0), data = multiplerows_b, family = binomial())
  #     if (!is.null(fit6k)) {
  #       aic_values_fit6k[k] <- AIC(fit6k)
  #       fits[[6]] <- fit6k
  # }
  #   }
    
  #   if (length(knots7) == 7) {
  #     fit7k <- glm(outcome ~ ns(organ_support, knots = knots7)+ (organ_support == 0), data = multiplerows_b, family = binomial())
  #       if (!is.null(fit7k)) {
  #       aic_values_fit7k[k] <- AIC(fit7k)
  #       fits[[7]] <- fit7k
  # }
  #   }
    
    # Remove NULL models from the list
fits <- fits[!sapply(fits, is.null)]

    if (length(fits) > 0) {
      # Find the best model based on AIC
      fit_best = fits[[which.min(sapply(fits, AIC))]]
      # Generate bootstrap predictions
      bootpred_b = tibble(iter = k, organ_support = c(0, seq(1, min(truncate_boundry,truncate_boundry_axis), length.out = 100)))
      bootpred_b$outcome = predict(fit_best, newdata = bootpred_b, type = "response")
      bootpred = bind_rows(bootpred, bootpred_b)

      bootpred_b_csv = tibble(iter = k, organ_support = seq(0, truncate_boundry, by = 1))
      bootpred_b_csv$outcome = predict(fit_best, newdata = bootpred_b_csv, type = "response")
      bootpred_csv = bind_rows(bootpred_csv, bootpred_b_csv)
    }
  }
    return(list(bootpred = bootpred, bootpred_csv = bootpred_csv))
}


make_spline_plot_func <- function(iterations, truncate_boundry, truncate_boundry_axis=28){

  survsum <- multiplerows %>%
    group_by(organ_support, malignant_hemat) %>%
    summarize(n = n(), outcome = mean(outcome), .groups = 'drop')
  
  survsum <- survsum %>%
    filter(organ_support < truncate_boundry)

  multiplerows <- multiplerows %>%
    filter(organ_support < truncate_boundry)

  hemat_filtered_survsum <- survsum %>%
    filter(malignant_hemat == 1)

  non_hemat_filtered_survsum <- survsum %>%
    filter(malignant_hemat == 0)

  hemat_filtered_multiplerows <- multiplerows %>%
    filter(malignant_hemat == 1)

  non_hemat_filtered_multiplerows <- multiplerows %>%
    filter(malignant_hemat == 0)

    bootpred_hemat = bootstrap_aic(data=filter(multiplerows, malignant_hemat == 1), 
                                  iterations=iterations, truncate_boundry=truncate_boundry)

    bootpred_hemat_csv = bootpred_hemat$bootpred_csv
    bootpred_hemat = bootpred_hemat$bootpred

    bootpred_nonhemat = bootstrap_aic(data=filter(multiplerows, malignant_hemat == 0), 
                                      iterations=iterations,truncate_boundry=truncate_boundry) 
    
    bootpred_nonhemat_csv = bootpred_nonhemat$bootpred_csv
    bootpred_nonhemat = bootpred_nonhemat$bootpred

    bootpred = bind_rows(mutate(bootpred_hemat, type = "hemat"), 
                        mutate(bootpred_nonhemat, type = "nonhemat"))
    
    bootpred_csv = bind_rows(mutate(bootpred_hemat_csv, type = "hemat"), 
                        mutate(bootpred_nonhemat_csv, type = "nonhemat"))

    ggplot(data=bootpred, aes(x=organ_support, y=outcome, 
                              group=interaction(iter, type), color=type)) +
      geom_line() 
  # Summarize the bootstrap results
  bootpred_summary <- bootpred %>%
      group_by(organ_support, type) %>%
      summarise(q025 = quantile(outcome, .025),
                q10 = quantile(outcome, .10),
                q50 = quantile(outcome, .50),
                q90 = quantile(outcome, .90),
                q975 = quantile(outcome, .975))
    # Add organ_support = -0.5 = 0.0 for aesthetic purposes
    bootpred_summary <- bootpred_summary %>%
    bind_rows(
      bootpred_summary %>%
        filter(organ_support == 0) %>%
        mutate(organ_support = -0.5)
  )
if(nrow(filter(multiplerows, organ_support == 0)) == 0){
  bootpred_summary = filter(bootpred_summary, organ_support >= 0.75)
}

  bootpred_csv_summary <- bootpred_csv %>%
      group_by(organ_support, type) %>%
      summarise(q025 = quantile(outcome, .025),
                q10 = quantile(outcome, .10),
                q50 = quantile(outcome, .50),
                q90 = quantile(outcome, .90),
                q975 = quantile(outcome, .975))

  write.csv(bootpred_csv_summary, file_path_csv, row.names = FALSE)
  ggplot(data=bootpred_summary, aes(x=organ_support, 
                            fill=type)) +
    geom_ribbon(aes(ymin=q025, ymax=q975)) 

  # Combine the summaries into one plot
  theme_plots = theme(aspect.ratio = 0.8, panel.border = element_blank(),
      axis.line.x.bottom = element_line(), axis.line.y.left = element_line(),
      panel.grid.minor = element_blank(), 
      axis.title.y = element_text(size = 30),
      axis.title.x = element_text(size = 25),
      axis.text.y = element_text(size = 20),
      axis.text.x = element_text(size = 20))

  combined_plot <- ggplot(data=bootpred_summary) +
      geom_ribbon(aes(fill=type, x=organ_support, ymin=q025, ymax=q975), alpha=0.2) +
      geom_ribbon(aes(fill=type, x=organ_support, ymin=q10, ymax=q90), alpha=0.4) +
      geom_point(data=survsum, aes(x=organ_support, y=outcome, group=malignant_hemat, color=as.factor(malignant_hemat)),
      size=0.6) +
      expand_limits(y = c(0,1)) +
      scale_y_continuous(name=outcome_str_long, labels = percent, expand = c(0, 0), limits=c(0, 1)) +
      scale_x_continuous(name=NULL, breaks=seq(0, truncate_boundry_axis, by = 7), expand = c(0, 0), limits=c(-1, truncate_boundry_axis)) +
      scale_fill_manual(values = c("blue", "red"), name = "Legend", labels = c("Malignant heamatology", "Matched controls"),guide = "none") +
      scale_color_manual(values = c("red", "blue"), guide = "none") + 
      theme_bw() + 
      theme_plots #+
      #ggtitle(paste("Fit of landmarking with splined analysis for\nHematologic and Non-Hematologic Patients for\n",organ_support_str_long," and ",outcome_str_long))
  combined_plot


  # hemat_nplot <- ggplot(data = hemat_filtered_survsum, aes(x = organ_support, y = n, fill = factor(malignant_hemat))) +
  #   geom_col(alpha=0.6, width=0.6) +
  #   scale_fill_manual(values = c("1" = "blue"), guide="none") +  
  #   scale_x_continuous(name=NULL, breaks=seq(0, truncate_boundry_axis, by = 7), limits=c(-0.3, truncate_boundry_axis)) +
  #   theme_bw() + theme_plots + 
  #   theme(aspect.ratio = 0.2)

  hemat_nplot <- ggplot(data = hemat_filtered_survsum, aes(x = organ_support, y = n, fill = factor(malignant_hemat))) +
    geom_col(alpha=0.6, width=0.6) +
    scale_fill_manual(values = c("1" = "blue"), guide="none") +  
    scale_x_continuous(name=NULL, breaks=seq(0, truncate_boundry_axis, by = 7), expand = c(0, 0), limits=c(-1, truncate_boundry_axis)) +
    scale_y_continuous(limits=c(0, 600), name=NULL) + # Set y-axis limits to 0 and 600
    theme_bw() + theme_plots + 
    theme(aspect.ratio = 0.2)

    

  non_hemat_nplot = ggplot(data = non_hemat_filtered_survsum, aes(x = organ_support, y = n, fill = factor(malignant_hemat))) +
    geom_col(alpha=0.6, width=0.6) +
    scale_fill_manual(values = c("0" = "red"), guide="none") +  
    scale_x_continuous(name=organ_support_str_long, breaks=seq(0, truncate_boundry_axis, by = 7), expand = c(0, 0),limits=c(-1, truncate_boundry_axis)) +
    scale_y_continuous(limits=c(0, 2500), name=NULL) + # Set y-axis limits to 0 and 600
    theme_bw() + theme_plots + 
    theme(aspect.ratio = 0.2)

  # Arrange plots using patchwork
  combined_plot <- combined_plot / 
    (hemat_nplot) /
    (non_hemat_nplot)
      ggsave(file_path_png, combined_plot, width = 12, height = 12, dpi = 300)
      ggsave(file_path_pdf, combined_plot, width = 160, units="mm")

  # Display the combined plot
   return(combined_plot)
}

#####################################################################################
# Define numer of iterations
#####################################################################################

#####################################################################################
# Define organ support variables
#####################################################################################
multiplerows<-grouped_support_windows
iterations<-500
truncate_threshold=10
truncate_boundry_axis= 28
#####################################################################################
# 1. LENGHT OF STAY
#####################################################################################
# make only distinct rows for the organ support duration to avoid too much weight for repetative value 
# e.g. 5 rows with 0 days organ support from 1 patient
# rename the organ support type to organ_support
multiplerows <- multiplerows %>%
  rename(organ_support = day_of_admission_counter)
# create the strings that are used for namint the 
organ_support_str_short="los"
organ_support_str_long='Length of stay (days)'

multiplerows <- multiplerows %>%
  rename(outcome = mort_1y)
outcome_str_short="mort_1y"
outcome_str_long='Mortality at 1 year (%)'

multiplerows <- multiplerows %>%
  rename(id = Hospital_Admission_TK)

# create survsum to find the threshold to truncate
survsum <- multiplerows %>%
  group_by(organ_support, malignant_hemat) %>%
  summarize(n = n(), outcome = mean(outcome), .groups = 'drop')

# Filter rows where malignant_hemat == 1 and n < truncate_threshold
lowest_organ_support <- survsum %>%
  filter(malignant_hemat == 1, n < truncate_threshold) %>%
  summarise(min_organ_support = min(organ_support, na.rm = TRUE)) %>%
  pull(min_organ_support)


truncate_boundry= lowest_organ_support-1

# Create file paths
file_path_pdf<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".pdf", sep="")
file_path_png<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".png", sep="")
file_path_csv<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".csv", sep="")

#### preproces function
preproces_func(truncate_boundry)

#### run spline function
make_spline_plot_func(iterations,truncate_boundry)

#####################################################################################
# Define organ support variables
#####################################################################################
multiplerows<-grouped_support_windows
#####################################################################################
# 2. MECHANICAL VENTILATION
#####################################################################################

# make only distinct rows for the organ support duration to avoid too much weight for repetative value 
# e.g. 5 rows with 0 days organ support from 1 patient
multiplerows <- multiplerows %>%
  distinct(Hospital_Admission_TK, mech_vent_counter, .keep_all = TRUE)
# rename the organ support type to organ_support
multiplerows <- multiplerows %>%
  rename(organ_support = mech_vent_counter)
# create the strings that are used for namint the 
organ_support_str_short="mech_vent"
organ_support_str_long='Days of invasive mechanical ventilation'

multiplerows <- multiplerows %>%
  rename(outcome = mort_1y)
outcome_str_short="mort_1y"
outcome_str_long='Mortality at 1 year (%)'

multiplerows <- multiplerows %>%
  rename(id = Hospital_Admission_TK)

# Create file paths
file_path_pdf<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".pdf", sep="")
file_path_png<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".png", sep="")
file_path_csv<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".csv", sep="")

# create survsum to find the threshold to truncate
survsum <- multiplerows %>%
  group_by(organ_support, malignant_hemat) %>%
  summarize(n = n(), outcome = mean(outcome), .groups = 'drop')

# Filter rows where malignant_hemat == 1 and n < truncate_threshold
lowest_organ_support <- survsum %>%
  filter(malignant_hemat == 1, n < truncate_threshold) %>%
  summarise(min_organ_support = min(organ_support, na.rm = TRUE)) %>%
  pull(min_organ_support)


truncate_boundry= lowest_organ_support-1

#### preproces function
preproces_func(truncate_boundry)

#### make spline function
make_spline_plot_func(iterations,truncate_boundry)


#####################################################################################
#####################################################################################
# Define organ support variables
#####################################################################################
#####################################################################################
multiplerows<-grouped_support_windows

#####################################################################################
# 3. VASOPRESSION OR INOTROPY
#####################################################################################

# make only distinct rows for the organ support duration to avoid too much weight for repetative value 
# e.g. 5 rows with 0 days organ support from 1 patient
multiplerows <- multiplerows %>%
  distinct(Hospital_Admission_TK, vasopr_inotr_counter, .keep_all = TRUE)
# rename the organ support type to organ_support
multiplerows <- multiplerows %>%
  rename(organ_support = vasopr_inotr_counter)
# create the strings that are used for namint the 
organ_support_str_short="vaso"
organ_support_str_long='Days of vasopressor or inotropic support'

multiplerows <- multiplerows %>%
  rename(outcome = mort_1y)
outcome_str_short="mort_1y"
outcome_str_long='Mortality at 1 year (%)'

multiplerows <- multiplerows %>%
  rename(id = Hospital_Admission_TK)

# Create file paths
file_path_pdf<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".pdf", sep="")
file_path_png<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".png", sep="")
file_path_csv<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".csv", sep="")

# create survsum to find the threshold to truncate
survsum <- multiplerows %>%
  group_by(organ_support, malignant_hemat) %>%
  summarize(n = n(), outcome = mean(outcome), .groups = 'drop')

# Filter rows where malignant_hemat == 1 and n < truncate_threshold
lowest_organ_support <- survsum %>%
  filter(malignant_hemat == 1, n < truncate_threshold) %>%
  summarise(min_organ_support = min(organ_support, na.rm = TRUE)) %>%
  pull(min_organ_support)


truncate_boundry= lowest_organ_support-1
#### preproces function
preproces_func(truncate_boundry)

#### make spline function
make_spline_plot_func(iterations,truncate_boundry)


#####################################################################################
# 4. RENAL REPLACEMENT THERAPY
#####################################################################################
#####################################################################################
multiplerows<-grouped_support_windows

# make only distinct rows for the organ support duration to avoid too much weight for repetative value 
# e.g. 5 rows with 0 days organ support from 1 patient
multiplerows <- multiplerows %>%
  distinct(Hospital_Admission_TK, rrt_counter, .keep_all = TRUE)
# rename the organ support type to organ_support
multiplerows <- multiplerows %>%
  rename(organ_support = rrt_counter)
# create the strings that are used for namint the 
organ_support_str_short="rrt"
organ_support_str_long='Days of renal replacement therapy'

multiplerows <- multiplerows %>%
  rename(outcome = mort_1y)
outcome_str_short="mort_1y"
outcome_str_long='Mortality at 1 year (%)'

multiplerows <- multiplerows %>%
  rename(id = Hospital_Admission_TK)

# Create file paths
file_path_pdf<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".pdf", sep="")
file_path_png<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".png", sep="")
file_path_csv<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".csv", sep="")

# create survsum to find the threshold to truncate
survsum <- multiplerows %>%
  group_by(organ_support, malignant_hemat) %>%
  summarize(n = n(), outcome = mean(outcome), .groups = 'drop')

# Filter rows where malignant_hemat == 1 and n < truncate_threshold
lowest_organ_support <- survsum %>%
  filter(malignant_hemat == 1, n < truncate_threshold) %>%
  summarise(min_organ_support = min(organ_support, na.rm = TRUE)) %>%
  pull(min_organ_support)


truncate_boundry= lowest_organ_support-1
#### preproces function
preproces_func(truncate_boundry)

#### make spline function
make_spline_plot_func(iterations,truncate_boundry)


#####################################################################################
# 5. TRANSFUSION
#####################################################################################
#####################################################################################
multiplerows<-grouped_support_windows

# make only distinct rows for the organ support duration to avoid too much weight for repetative value 
# e.g. 5 rows with 0 days organ support from 1 patient
multiplerows <- multiplerows %>%
  distinct(Hospital_Admission_TK, transfusion_counter, .keep_all = TRUE)
# rename the organ support type to organ_support
multiplerows <- multiplerows %>%
  rename(organ_support = transfusion_counter)
# create the strings that are used for namint the 
organ_support_str_short="transfusion"
organ_support_str_long='Days of blood transfusion'

multiplerows <- multiplerows %>%
  rename(outcome = mort_1y)
outcome_str_short="mort_1y"
outcome_str_long='Mortality at 1 year (%)'

multiplerows <- multiplerows %>%
  rename(id = Hospital_Admission_TK)

# Create file paths
file_path_pdf<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".pdf", sep="")
file_path_png<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".png", sep="")
file_path_csv<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".csv", sep="")

# create survsum to find the threshold to truncate
survsum <- multiplerows %>%
  group_by(organ_support, malignant_hemat) %>%
  summarize(n = n(), outcome = mean(outcome), .groups = 'drop')

# Filter rows where malignant_hemat == 1 and n < truncate_threshold
lowest_organ_support <- survsum %>%
  filter(malignant_hemat == 1, n < truncate_threshold) %>%
  summarise(min_organ_support = min(organ_support, na.rm = TRUE)) %>%
  pull(min_organ_support)


truncate_boundry= lowest_organ_support-1
#### preproces function
preproces_func(truncate_boundry)

#### make spline function
make_spline_plot_func(iterations,truncate_boundry)


#####################################################################################
# 6. COMBINATION OF MINIMAL 2 ORGANS SUPPORTED INCL TRANSFUSION
#####################################################################################
#####################################################################################
multiplerows<-grouped_support_windows

# make only distinct rows for the organ support duration to avoid too much weight for repetative value 
# e.g. 5 rows with 0 days organ support from 1 patient
multiplerows <- multiplerows %>%
  distinct(Hospital_Admission_TK, organ_support_minimal_two_incl_trans_counter, .keep_all = TRUE)
# rename the organ support type to organ_support
multiplerows <- multiplerows %>%
  rename(organ_support = organ_support_minimal_two_incl_trans_counter)
# create the strings that are used for namint the 
organ_support_str_short="os_min_two_incl_trans"
organ_support_str_long='Days of minimal 2 organs supported incl. transfusion'

multiplerows <- multiplerows %>%
  rename(outcome = mort_1y)
outcome_str_short="mort_1y"
outcome_str_long='Mortality at 1 year (%)'
multiplerows <- multiplerows %>%
  rename(id = Hospital_Admission_TK)

# Create file paths
file_path_pdf<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".pdf", sep="")
file_path_png<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".png", sep="")
file_path_csv<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".csv", sep="")

# create survsum to find the threshold to truncate
survsum <- multiplerows %>%
  group_by(organ_support, malignant_hemat) %>%
  summarize(n = n(), outcome = mean(outcome), .groups = 'drop')

# Filter rows where malignant_hemat == 1 and n < truncate_threshold
lowest_organ_support <- survsum %>%
  filter(malignant_hemat == 1, n < truncate_threshold) %>%
  summarise(min_organ_support = min(organ_support, na.rm = TRUE)) %>%
  pull(min_organ_support)


truncate_boundry= lowest_organ_support-1
#### preproces function
preproces_func(truncate_boundry)

#### make spline function
make_spline_plot_func(iterations,truncate_boundry)


#####################################################################################
# 7. COMBINATION OF MINIMAL 3 ORGANS SUPPORTED INCL TRANSFUSION
#####################################################################################
#####################################################################################
multiplerows<-grouped_support_windows

# make only distinct rows for the organ support duration to avoid too much weight for repetative value 
# e.g. 5 rows with 0 days organ support from 1 patient
multiplerows <- multiplerows %>%
  distinct(Hospital_Admission_TK, organ_support_minimal_three_incl_trans_counter, .keep_all = TRUE)
# rename the organ support type to organ_support
multiplerows <- multiplerows %>%
  rename(organ_support = organ_support_minimal_three_incl_trans_counter)
# create the strings that are used for namint the 
organ_support_str_short="os_min_three_incl_trans"
organ_support_str_long='Days of minimal 3 organs supported incl. transfusion'

multiplerows <- multiplerows %>%
  rename(outcome = mort_1y)
outcome_str_short="mort_1y"
outcome_str_long='Mortality at 1 year (%)'
multiplerows <- multiplerows %>%
  rename(id = Hospital_Admission_TK)

# Create file paths
file_path_pdf<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".pdf", sep="")
file_path_png<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".png", sep="")
file_path_csv<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".csv", sep="")

# create survsum to find the threshold to truncate
survsum <- multiplerows %>%
  group_by(organ_support, malignant_hemat) %>%
  summarize(n = n(), outcome = mean(outcome), .groups = 'drop')

# Filter rows where malignant_hemat == 1 and n < truncate_threshold
lowest_organ_support <- survsum %>%
  filter(malignant_hemat == 1, n < truncate_threshold) %>%
  summarise(min_organ_support = min(organ_support, na.rm = TRUE)) %>%
  pull(min_organ_support)


truncate_boundry= lowest_organ_support-1
#### preproces function
preproces_func(truncate_boundry)

#### make spline function
make_spline_plot_func(iterations,truncate_boundry)

#####################################################################################
# 8. COMBINATION OF MINIMAL 3 ORGANS SUPPORTED INCL TRANSFUSION
#####################################################################################
#####################################################################################
multiplerows<-grouped_support_windows

# make only distinct rows for the organ support duration to avoid too much weight for repetative value 
# e.g. 5 rows with 0 days organ support from 1 patient
multiplerows <- multiplerows %>%
  distinct(Hospital_Admission_TK, organ_support_minimal_four_incl_trans_counter, .keep_all = TRUE)
# rename the organ support type to organ_support
multiplerows <- multiplerows %>%
  rename(organ_support = organ_support_minimal_four_incl_trans_counter)
# create the strings that are used for namint the 
organ_support_str_short="os_min_four_incl_trans"
organ_support_str_long='Days of minimal 4 organs supported incl. transfusion'

multiplerows <- multiplerows %>%
  rename(outcome = mort_1y)
outcome_str_short="mort_1y"
outcome_str_long='Mortality at 1 year (%)'
multiplerows <- multiplerows %>%
  rename(id = Hospital_Admission_TK)

# Create file paths
file_path_pdf<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".pdf", sep="")
file_path_png<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".png", sep="")
file_path_csv<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".csv", sep="")

# create survsum to find the threshold to truncate
survsum <- multiplerows %>%
  group_by(organ_support, malignant_hemat) %>%
  summarize(n = n(), outcome = mean(outcome), .groups = 'drop')

# Filter rows where malignant_hemat == 1 and n < truncate_threshold
lowest_organ_support <- survsum %>%
  filter(malignant_hemat == 1, n < truncate_threshold) %>%
  summarise(min_organ_support = min(organ_support, na.rm = TRUE)) %>%
  pull(min_organ_support)


truncate_boundry= lowest_organ_support-1
#### preproces function
preproces_func(truncate_boundry)

#### make spline function
make_spline_plot_func(iterations,truncate_boundry)

#####################################################################################
# 9. COMBINATION OF MINIMAL 2 ORGANS SUPPORTED EXCL TRANSFUSION
#####################################################################################
#####################################################################################
multiplerows<-grouped_support_windows

# make only distinct rows for the organ support duration to avoid too much weight for repetative value 
# e.g. 5 rows with 0 days organ support from 1 patient
multiplerows <- multiplerows %>%
  distinct(Hospital_Admission_TK, organ_support_minimal_two_excl_trans_counter, .keep_all = TRUE)
# rename the organ support type to organ_support
multiplerows <- multiplerows %>%
  rename(organ_support = organ_support_minimal_two_excl_trans_counter)
# create the strings that are used for namint the 
organ_support_str_short="os_min_two_excl_trans"
organ_support_str_long='Days of minimal 2 organs supported excl. transfusion'

multiplerows <- multiplerows %>%
  rename(outcome = mort_1y)
outcome_str_short="mort_1y"
outcome_str_long='Mortality at 1 year (%)'
multiplerows <- multiplerows %>%
  rename(id = Hospital_Admission_TK)

# Create file paths
file_path_pdf<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".pdf", sep="")
file_path_png<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".png", sep="")
file_path_csv<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".csv", sep="")

# create survsum to find the threshold to truncate
survsum <- multiplerows %>%
  group_by(organ_support, malignant_hemat) %>%
  summarize(n = n(), outcome = mean(outcome), .groups = 'drop')

# Filter rows where malignant_hemat == 1 and n < truncate_threshold
lowest_organ_support <- survsum %>%
  filter(malignant_hemat == 1, n < truncate_threshold) %>%
  summarise(min_organ_support = min(organ_support, na.rm = TRUE)) %>%
  pull(min_organ_support)


truncate_boundry= lowest_organ_support-1
#### preproces function
preproces_func(truncate_boundry)

#### make spline function
make_spline_plot_func(iterations,truncate_boundry)

#####################################################################################
# 10. COMBINATION OF MINIMAL 3 ORGANS SUPPORTED EXCL TRANSFUSION
#####################################################################################
#####################################################################################
multiplerows<-grouped_support_windows

# make only distinct rows for the organ support duration to avoid too much weight for repetative value 
# e.g. 5 rows with 0 days organ support from 1 patient
multiplerows <- multiplerows %>%
  distinct(Hospital_Admission_TK, organ_support_minimal_three_excl_trans_counter, .keep_all = TRUE)
# rename the organ support type to organ_support
multiplerows <- multiplerows %>%
  rename(organ_support = organ_support_minimal_three_excl_trans_counter)
# create the strings that are used for namint the 
organ_support_str_short="os_min_three_excl_trans"
organ_support_str_long='Days of minimal 3 organs supported excl. transfusion'

multiplerows <- multiplerows %>%
  rename(outcome = mort_1y)
outcome_str_short="mort_1y"
outcome_str_long='Mortality at 1 year (%)'
multiplerows <- multiplerows %>%
  rename(id = Hospital_Admission_TK)

# Create file paths
file_path_pdf<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".pdf", sep="")
file_path_png<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".png", sep="")
file_path_csv<-paste("C:\\motten\\projects\\hemat\\hemat\\results\\survival\\figures\\r_plots\\combined_AUMC_UMCU\\",outcome_str_short,"\\",whole_group_or_matched,"_",organ_support_str_short,"_",outcome_str_short,".csv", sep="")

# create survsum to find the threshold to truncate
survsum <- multiplerows %>%
  group_by(organ_support, malignant_hemat) %>%
  summarize(n = n(), outcome = mean(outcome), .groups = 'drop')

# Filter rows where malignant_hemat == 1 and n < truncate_threshold
lowest_organ_support <- survsum %>%
  filter(malignant_hemat == 1, n < truncate_threshold) %>%
  summarise(min_organ_support = min(organ_support, na.rm = TRUE)) %>%
  pull(min_organ_support)


truncate_boundry= lowest_organ_support-1
#### preproces function
preproces_func(truncate_boundry)

#### make spline function
make_spline_plot_func(iterations,truncate_boundry)


