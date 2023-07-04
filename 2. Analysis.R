################################

# Risk factors for undiagnosed hypertension

# R script to calculate:
# Weighted proportion with hypertension and undiagnosed HT by age and sex
# Weighted, age-standardised proportions with HT and uHT for all risk factors
# Logistic regression models for uHT by risk factors

# April 2023
# R 4.0.2

################################

# Load packages
library(srvyr)
library(survey)
library(dplyr)
library(tidyverse)
library(emmeans)
library(broom)

# Read in data
HSE_subset <- read.table("Data/Merged_Data/140 over 90 data/03_HSE_subset_140_90.tab", 
                         sep='\t', header=TRUE)


################################

# SET SURVEY DESIGN PARAMETERS USING BOTH SURVEY AND SRVYR

# srvyr will be used for weighted descriptive statistics
# survey will be used for modelling

# SURVEY ----------
# Set survey design for analysis where base is all adults 
hse_design <- svydesign(id = ~ psu, 
                        strata = ~ gor1,
                        weights = ~ wt_nurse_sc2, 
                        data = HSE_subset, 
                        nest = TRUE)

# Create subsets for analysing males and females separately 
# (NB these have to be done after setting the survey design to avoid errors 
# with domain estimation)
hse_design_m <- subset(hse_design, sex == "Male")
hse_design_f <- subset(hse_design, sex == "Female")

# Set survey design for analysis where base is adults with hypertension 
HSE_subset$wt_nurse_sc3 <- with(HSE_subset, case_when(hbp_yn == 1 ~ wt_nurse_sc3, 
                                                      hbp_yn == 0 ~ 1.0))
hse_design2 <- svydesign(id = ~ psu, 
                         strata = ~ gor1,
                         weights = ~ wt_nurse_sc3, 
                         data = HSE_subset, 
                         nest = TRUE)

# Subset for people with hypertension: 
HSE_hbp <- subset(hse_design2, hbp_yn == "1")

# Subset for males with hypertension:
HSE_hbp_m <- subset(hse_design2, hbp_yn == "1" & sex == "Male")

# Subset for females with hypertension: 
HSE_hbp_f <- subset(hse_design2, hbp_yn == "1" & sex == "Female")


# SRVYR ----------

# set survey design for analysis where base is all adults 
hsedesign_srvyr <- HSE_subset %>% 
  as_survey_design(ids=psu, strata = gor1, weight = wt_nurse_sc2, nest = TRUE)

# Set survey design for analysis where base is adults with hypertension 
HSE_subset$wt_nurse_sc3 <- with(HSE_subset, case_when(hbp_yn == 1 ~ wt_nurse_sc3, 
                                                      hbp_yn == 0 ~ 1.0))
hsedesign2_srvyr <- HSE_subset %>% 
  as_survey_design(ids=psu, strata = gor1, weight = wt_nurse_sc3, nest = TRUE)
HSE_hbp_y_srvyr <- subset(hsedesign2_srvyr, hbp_yn == "1")

################################

# WEIGHTED ESTIMATES OF HYPERTENSION AND UNDIAGNOSED HYPERTENSION --------
# FOR ALL ADULTS AND BY AGE AND SEX 

# Total hypertension (base pop: all adults) ---------

# All adults:
table <- hsedesign_srvyr %>% group_by(hbp_yn) %>%
  summarize(percentage = 100*survey_mean(vartype = "ci", level = 0.95),
            total = survey_total()) 

# By age and sex:
table <- hsedesign_srvyr %>% group_by(ag16g10, sex, hbp_yn) %>% 
  summarize(percentage = 100*survey_mean(vartype = "ci", level = 0.95), 
            total = survey_total())

# Undiagnosed hypertension (base pop: all adults) ---------

# All adults:
table <- hsedesign_srvyr %>% group_by(diagnosed_yn) %>%
  summarize(percentage = 100*survey_mean(vartype = "ci", level = 0.95),
            total = survey_total()) 

# By age and sex:
table <- hsedesign_srvyr %>% group_by(ag16g10, sex, diagnosed_yn) %>%
  summarize(percentage = 100*survey_mean(vartype = "ci", level = 0.95),
            total = survey_total())

# Undiagnosed hypertension (base pop: adults with hypertension) ---------

# All adults:
table <- HSE_hbp_y_srvyr %>% 
  group_by(diagnosed_yn) %>%
  summarize(percentage = 100*survey_mean(vartype = "ci", level = 0.95),
            total = survey_total()) 

# By age and sex: 
table1 <- HSE_hbp_y_srvyr %>% 
  group_by(ag16g10, sex, diagnosed_yn) %>%
  summarize(percentage = 100*survey_mean(vartype = "ci", level = 0.95),
            total = survey_total()) 


################################

# WEIGHTED, AGE-STANDARDISED ESTIMATES OF HYPERTENSION AND UNDIAGNOSED
# HYPERTENSION BY RISK FACTORS 

# OUTCOME: TOTAL HYPERTENSION ----------
# BASE: ALL ADULTS

# Sex -----------

# Model
hbp_sex <- svyglm(hbp_yn ~ sex + age_grp, design = hse_design, 
                  family = quasibinomial())

# emmeans
rates_hbp_sex<- ref_grid(object = hbp_sex, data = HSE_subset) %>%  
  emmeans::emmeans(specs = "sex", weights = "proportional", 
                   type = "response") %>%  
  as.data.frame() %>%  rename(prop = prob, 
                              prop_lci = asymp.LCL, prop_uci = asymp.UCL) %>%  
  select(-df)
rates_hbp_sex

# Create output table and export
rates_hbp_sex_output <- rates_hbp_sex %>% 
  dplyr::mutate(Percent = prop * 100,
                SE_percent = SE * 100,
                Percent_lci = prop_lci * 100,
                Percent_uci = prop_uci * 100)

write.csv(rates_hbp_sex_output,
          paste0(output_folder, 
                 "/Final_Outputs/140 over 90 outputs/Age standardised (weighted)/hbp_yn", 
                 paste0("_140_90_agest_sex.csv")),
          row.names = TRUE)


# All other risk factors (stratified by sex) ----------

exposure_variables <- c("origin_4", "gor1", "urban14b", "hpnssec5", 
                        "topqual4", "marstat_5", "bmivg3", "genhelf2", 
                        "cigsta3")

for (exposure_variable in exposure_variables) {
  ## Formula
  formula_exposure <- as.formula(paste("hbp_yn ~ age_grp", exposure_variable, sep = "+"))
  
  ## Models
  hbp_exposure_m <- svyglm(formula_exposure, design = hse_design_m, 
                           family = quasibinomial())
  hbp_exposure_f <- svyglm(formula_exposure, design = hse_design_f, 
                           family = quasibinomial())
  
  # emmeans 
  rates_hbp_exposure_m <- ref_grid(object = hbp_exposure_m, 
                                   data = HSE_subset) %>%  
    emmeans::emmeans(specs = exposure_variable, weights = "proportional", 
                     type = "response") %>%  
    as.data.frame() %>%  rename(prop = prob, 
                                prop_lci = asymp.LCL, prop_uci = asymp.UCL) %>%  
    select(-df)
  
  rates_hbp_exposure_f <- ref_grid(object = hbp_exposure_f, 
                                   data = HSE_subset) %>%  
    emmeans::emmeans(specs = exposure_variable, weights = "proportional", 
                     type = "response") %>%  
    as.data.frame() %>%  rename(prop = prob, 
                                prop_lci = asymp.LCL, prop_uci = asymp.UCL) %>%  
    select(-df)
  
  # View model outputs
  rates_hbp_exposure_m
  rates_hbp_exposure_f
  
  # Create output tables for males and females and combine
  rates_hbp_exposure_m_output <- rates_hbp_exposure_m %>%
    dplyr::mutate(Percent = prop * 100,
                  SE_percent = SE * 100,
                  Percent_lci = prop_lci * 100,
                  Percent_uci = prop_uci * 100,
                  sex = c("male")) 
  rates_hbp_exposure_f_output <- rates_hbp_exposure_f %>%
    dplyr::mutate(Percent = prop * 100,
                  SE_percent = SE * 100,
                  Percent_lci = prop_lci * 100,
                  Percent_uci = prop_uci * 100,
                  sex=c("female")) 
  
  rates_hbp_exposure_output <- rates_hbp_exposure_m_output %>%
    dplyr::union(rates_hbp_exposure_f_output) %>%  
    dplyr::relocate(.data[[exposure_variable]]) 
  
  # Export output table
  write.csv(rates_hbp_exposure_output,
            paste0(output_folder, 
                   "/Final_Outputs/140 over 90 outputs/Age standardised (weighted)/hbp_yn", 
                   paste0("_140_90_by_sex_agest_", exposure_variable, ".csv")),
            row.names = TRUE)
  
}


################################

# OUTCOME: UNDIAGNOSED HYPERTENSION (undiag_ht) ----------
# BASE: ALL ADULTS

# Sex ----------

# Model 
undiag_sex <- svyglm(undiag_ht ~ sex + age_grp, design = hse_design, 
                     family = quasibinomial())

# emmeans
rates_undiag_sex <- ref_grid(object = undiag_sex, data = HSE_subset) %>%  
  emmeans::emmeans(specs = "sex", weights = "proportional", 
                   type = "response") %>%  
  as.data.frame() %>%  rename(prop = prob, 
                              prop_lci = asymp.LCL, prop_uci = asymp.UCL) %>%  
  select(-df)
rates_undiag_sex

# Create output table and export
rates_undiag_sex_output <- rates_undiag_sex %>% 
  dplyr::mutate(Percent = prop * 100,
                SE_percent = SE * 100,
                Percent_lci = prop_lci * 100,
                Percent_uci = prop_uci * 100)

write.csv(rates_undiag_sex_output,
          paste0(output_folder, 
                 "/Final_Outputs/140 over 90 outputs/Age standardised (weighted)/diagnosed_yn", 
                 paste0("_140_90_agest_sex.csv")),
          row.names = TRUE)


# All other risk factors (stratified by sex) ----------

for (exposure_variable in exposure_variables) {
  ## Formula
  formula_exposure <- as.formula(paste("undiag_ht ~ age_grp", exposure_variable, sep = "+"))
  
  ## Models
  undiag_exposure_m <- svyglm(formula_exposure, design = hse_design_m, 
                              family = quasibinomial())
  undiag_exposure_f <- svyglm(formula_exposure, design = hse_design_f, 
                              family = quasibinomial())
  
  # emmeans 
  rates_undiag_exposure_m <- ref_grid(object = undiag_exposure_m, 
                                      data = HSE_subset) %>%  
    emmeans::emmeans(specs = exposure_variable, weights = "proportional", 
                     type = "response") %>%  
    as.data.frame() %>%  rename(prop = prob, 
                                prop_lci = asymp.LCL, prop_uci = asymp.UCL) %>%  
    select(-df)
  
  rates_undiag_exposure_f <- ref_grid(object = undiag_exposure_f, 
                                      data = HSE_subset) %>%  
    emmeans::emmeans(specs = exposure_variable, weights = "proportional", 
                     type = "response") %>%  
    as.data.frame() %>%  rename(prop = prob, 
                                prop_lci = asymp.LCL, prop_uci = asymp.UCL) %>%  
    select(-df)
  
  # View model outputs
  rates_undiag_exposure_m
  rates_undiag_exposure_f
  
  # Create output tables for males and females and combine
  rates_undiag_exposure_m_output <- rates_undiag_exposure_m %>%
    dplyr::mutate(Percent = prop * 100,
                  SE_percent = SE * 100,
                  Percent_lci = prop_lci * 100,
                  Percent_uci = prop_uci * 100,
                  sex = c("male")) 
  rates_undiag_exposure_f_output <- rates_undiag_exposure_f %>%
    dplyr::mutate(Percent = prop * 100,
                  SE_percent = SE * 100,
                  Percent_lci = prop_lci * 100,
                  Percent_uci = prop_uci * 100,
                  sex=c("female")) 
  
  rates_undiag_exposure_output <- rates_undiag_exposure_m_output %>%
    dplyr::union(rates_undiag_exposure_f_output) %>%  
    dplyr::relocate(.data[[exposure_variable]]) 
  
  # Export output table
  write.csv(rates_undiag_exposure_output,
            paste0(output_folder, 
                   "/Final_Outputs/140 over 90 outputs/Age standardised (weighted)/diagnosed_yn", 
                   paste0("_140_90_by_sex_agest_", exposure_variable, ".csv")),
            row.names = TRUE)
  
}


################################

# OUTCOME: UNDIAGNOSED HYPERTENSION (undiag_ht) ----------
# BASE: ADULTS WITH HYPERTENSION

# Sex ----------

# Model 
undiag_sex <- svyglm(undiag_ht ~ sex + age_grp, design = HSE_hbp, 
                     family = quasibinomial())

# emmeans
rates_undiag_sex <- ref_grid(object = undiag_sex, data = HSE_subset) %>%  
  emmeans::emmeans(specs = "sex", weights = "proportional", 
                   type = "response") %>%  
  as.data.frame() %>%  rename(prop = prob, 
                              prop_lci = asymp.LCL, prop_uci = asymp.UCL) %>%  
  select(-df)
rates_undiag_sex

# Create output table and export
rates_undiag_sex_output <- rates_undiag_sex %>% 
  dplyr::mutate(Percent = prop * 100,
                SE_percent = SE * 100,
                Percent_lci = prop_lci * 100,
                Percent_uci = prop_uci * 100)

write.csv(rates_undiag_sex_output,
          paste0(output_folder, 
                 "/Final_Outputs/140 over 90 outputs/Age standardised (weighted)/diagnosed_yn_has_bp", 
                 paste0("_140_90_agest_sex.csv")),
          row.names = TRUE)


# All other risk factors (stratified by sex)  ----------

for (exposure_variable in exposure_variables) {
  ## Formula
  formula_exposure <- as.formula(paste("undiag_ht ~ age_grp", exposure_variable, sep = "+"))
  
  ## Models
  undiag_exposure_m <- svyglm(formula_exposure, design = HSE_hbp_m, 
                              family = quasibinomial())
  undiag_exposure_f <- svyglm(formula_exposure, design = HSE_hbp_f, 
                              family = quasibinomial())
  
  # emmeans 
  rates_undiag_exposure_m <- ref_grid(object = undiag_exposure_m, 
                                      data = HSE_subset) %>%  
    emmeans::emmeans(specs = exposure_variable, weights = "proportional", 
                     type = "response") %>%  
    as.data.frame() %>%  rename(prop = prob, 
                                prop_lci = asymp.LCL, prop_uci = asymp.UCL) %>%  
    select(-df)
  
  rates_undiag_exposure_f <- ref_grid(object = undiag_exposure_f, 
                                      data = HSE_subset) %>%  
    emmeans::emmeans(specs = exposure_variable, weights = "proportional", 
                     type = "response") %>%  
    as.data.frame() %>%  rename(prop = prob, 
                                prop_lci = asymp.LCL, prop_uci = asymp.UCL) %>%  
    select(-df)
  
  # View model outputs
  rates_undiag_exposure_m
  rates_undiag_exposure_f
  
  # Create output tables for males and females and combine
  rates_undiag_exposure_m_output <- rates_undiag_exposure_m %>%
    dplyr::mutate(Percent = prop * 100,
                  SE_percent = SE * 100,
                  Percent_lci = prop_lci * 100,
                  Percent_uci = prop_uci * 100,
                  sex = c("male")) 
  rates_undiag_exposure_f_output <- rates_undiag_exposure_f %>%
    dplyr::mutate(Percent = prop * 100,
                  SE_percent = SE * 100,
                  Percent_lci = prop_lci * 100,
                  Percent_uci = prop_uci * 100,
                  sex=c("female")) 
  
  rates_undiag_exposure_output <- rates_undiag_exposure_m_output %>%
    dplyr::union(rates_undiag_exposure_f_output) %>%  
    dplyr::relocate(.data[[exposure_variable]]) 
  
  # Export output table
  write.csv(rates_undiag_exposure_output,
            paste0(output_folder, 
                   "/Final_Outputs/140 over 90 outputs/Age standardised (weighted)/diagnosed_yn_has_bp", 
                   paste0("_140_90_by_sex_agest_", exposure_variable, ".csv")),
            row.names = TRUE)
  
}

################################

# REGRESSION MODELS FOR UNDIAGNOSED HYPERTENSION 
# BASE POP: ADULTS WITH HYPERTENSION

# Minimally adjusted models ----------

# Sex
sex2 <- svyglm(undiag_ht ~ sex + ag16g10, design = HSE_hbp, 
               family = quasibinomial)
tidy(sex2, expo=TRUE, conf.int=TRUE)

# tidy output
sex2_output <- tidy_output(sex2)


# Age
age_m1 <- svyglm(undiag_ht ~ ag16g10, design = HSE_hbp_m, 
                 family = quasibinomial)
age_f1 <- svyglm(undiag_ht ~ ag16g10, design = HSE_hbp_f, 
                 family = quasibinomial)

age_m1_output <- tidy_output(age_m1)
age_f1_output <- tidy_output(age_f1)


# Ethnicity
ethnic_m1 <- svyglm(undiag_ht ~ origin_4 + ag16g10, design = HSE_hbp_m, 
                    family = quasibinomial)
ethnic_f1 <- svyglm(undiag_ht ~ origin_4 + ag16g10, design = HSE_hbp_f, 
                    family = quasibinomial)
ethnic_m1_output <- tidy_output(ethnic_m1)
ethnic_f1_output <- tidy_output(ethnic_f1)


# Region
region_m1 <- svyglm(undiag_ht ~ gor1 + ag16g10, design = HSE_hbp_m, 
                    family = quasibinomial)
region_f1 <- svyglm(undiag_ht ~ gor1 + ag16g10, design = HSE_hbp_f, 
                    family = quasibinomial)
region_m1_output <- tidy_output(region_m1)
region_f1_output <- tidy_output(region_f1)


# Urban/rural
urban_m1 <- svyglm(undiag_ht ~ urban14b + ag16g10, design = HSE_hbp_m, 
                   family = quasibinomial)
urban_f1 <- svyglm(undiag_ht ~ urban14b + ag16g10, design = HSE_hbp_f, 
                   family = quasibinomial)
urban_m1_output <- tidy_output(urban_m1)
urban_f1_output <- tidy_output(urban_f1)


# NS-SEC
nssec_m1 <- svyglm(undiag_ht ~ hpnssec5 + ag16g10, design = HSE_hbp_m, 
                   family = quasibinomial)
nssec_f1 <- svyglm(undiag_ht ~ hpnssec5 + ag16g10, design = HSE_hbp_f, 
                   family = quasibinomial)
nssec_m1_output <- tidy_output(nssec_m1)
nssec_f1_output <- tidy_output(nssec_f1)


# Education
qual_m1 <- svyglm(undiag_ht ~ topqual4 + ag16g10, design = HSE_hbp_m, 
                  family = quasibinomial)
qual_f1 <- svyglm(undiag_ht ~ topqual4 + ag16g10, design = HSE_hbp_f, 
                  family = quasibinomial)
qual_m1_output <- tidy_output(qual_m1)
qual_f1_output <- tidy_output(qual_f1)


# Relationship status
marstat_m1 <- svyglm(undiag_ht ~ marstat_5 + ag16g10, design = HSE_hbp_m, 
                     family = quasibinomial)
marstat_f1 <- svyglm(undiag_ht ~ marstat_5 + ag16g10, design = HSE_hbp_f, 
                     family = quasibinomial)
marstat_m1_output <- tidy_output(marstat_m1)
marstat_f1_output  <- tidy_output(marstat_f1)


# BMI
bmi_m1 <- svyglm(undiag_ht ~ bmivg3 + ag16g10, design = HSE_hbp_m, 
                 family = quasibinomial)
bmi_f1 <- svyglm(undiag_ht ~ bmivg3 + ag16g10, design = HSE_hbp_f, 
                 family = quasibinomial)
bmi_m1_output <- tidy_output(bmi_m1)
bmi_f1_output <- tidy_output(bmi_f1)


# General health
genhelf_m1 <- svyglm(undiag_ht ~ genhelf2 + ag16g10, design = HSE_hbp_m, 
                     family = quasibinomial)
genhelf_f1 <- svyglm(undiag_ht ~ genhelf2 + ag16g10, design = HSE_hbp_f, 
                     family = quasibinomial)
genhelf_m1_output <- tidy_output(genhelf_m1)
genhelf_f1_output <- tidy_output(genhelf_f1)


# Smoking status
smoke_m1 <- svyglm(undiag_ht ~ cigsta3 + ag16g10, design = HSE_hbp_m, 
                   family = quasibinomial)
smoke_f1 <- svyglm(undiag_ht ~ cigsta3 + ag16g10, design = HSE_hbp_f, 
                   family = quasibinomial)
smoke_m1_output <- tidy_output(smoke_m1)
smoke_f1_output <- tidy_output(smoke_f1)


# Fully adjusted models ----------

# Fully adjusted model for all sociodemographic risk factors
full_m1 <- svyglm(undiag_ht ~ ag16g10 + origin_4 + gor1 + urban14b + hpnssec5 +
                    topqual4 + marstat_5, 
                  design = HSE_hbp_m, family = quasibinomial)
full_f1 <- svyglm(undiag_ht ~ ag16g10 + origin_4 + gor1 + urban14b + hpnssec5 +
                    topqual4 + marstat_5, 
                  design = HSE_hbp_f, family = quasibinomial)
full_m1_output <- tidy_output(full_m1)
full_f1_output <- tidy_output(full_f1)


# Fully adjusted model for BMI (all sociodemographic factors + BMI in model)
full_bmi_m <- svyglm(undiag_ht ~ bmivg3 + ag16g10 + origin_4 + gor1 + urban14b + 
                       hpnssec5 + topqual4 + marstat_5, 
                     design = HSE_hbp_m, family = quasibinomial)
full_bmi_f <- svyglm(undiag_ht ~ bmivg3 + ag16g10 + origin_4 + gor1 + urban14b + 
                       hpnssec5 + topqual4 + marstat_5, 
                     design = HSE_hbp_f, family = quasibinomial)
full_bmi_m_output <- tidy_output(full_bmi_m)
full_bmi_f_output <- tidy_output(full_bmi_f)

# Fully adjusted model for general health (all sociodem factors + genhealth in model)
full_genhelf_m <- svyglm(undiag_ht ~ genhelf2 + ag16g10 + origin_4 + gor1 + urban14b + 
                           hpnssec5 + topqual4 + marstat_5, 
                         design = HSE_hbp_m, family = quasibinomial)
full_genhelf_f <- svyglm(undiag_ht ~ genhelf2 + ag16g10 + origin_4 + gor1 + urban14b + 
                           hpnssec5 + topqual4 + marstat_5, 
                         design = HSE_hbp_f, family = quasibinomial)
full_genhelf_m_output <- tidy_output(full_genhelf_m)
full_genhelf_f_output <- tidy_output(full_genhelf_f)

# Fully adjusted model for smoking (all sociodem factors + smoking in model)
full_smoke_m <- svyglm(undiag_ht ~ cigsta3 + ag16g10 + origin_4 + gor1 + urban14b + 
                         hpnssec5 + topqual4 + marstat_5, 
                       design = HSE_hbp_m, family = quasibinomial)
full_smoke_f <- svyglm(undiag_ht ~ cigsta3 + ag16g10 + origin_4 + gor1 + urban14b + 
                         hpnssec5 + topqual4 + marstat_5, 
                       design = HSE_hbp_f, family = quasibinomial)
full_smoke_m_output <- tidy_output(full_smoke_m)
full_smoke_f_output <- tidy_output(full_smoke_f)


##### END SCRIPT #####



