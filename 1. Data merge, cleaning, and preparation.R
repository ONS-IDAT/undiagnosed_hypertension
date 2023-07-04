################################

# Risk factors for undiagnosed hypertension

# R script to:
# Merge data years
# Rescale survey weights to subsets
# Clean data

# April 2023
# R 4.0.2

################################

library(plyr)
library(dplyr)  
library(tidyr)
library(tidyverse)

################################

# MERGE DATASETS ----------- 

# Load in data for each year 
HSE_2015 <- read.table("hse2015ai.tab", sep='\t', header=TRUE)
HSE_2016 <- read.table("hse2016_eul.tab", sep='\t', header=TRUE)
HSE_2017 <- read.table("hse17i_eul_v1.tab", sep='\t', header=TRUE)
HSE_2018 <- read.table("hse_2018_eul_15082022.tab", sep='\t', header=TRUE)
HSE_2019 <- read.table("hse_2019_eul_20211006.tab", sep='\t', header=TRUE)

# Convert column names to lower case 
names(HSE_2015) <- tolower(names(HSE_2015))
names(HSE_2016) <- tolower(names(HSE_2016))
names(HSE_2017) <- tolower(names(HSE_2017))
names(HSE_2018) <- tolower(names(HSE_2018))
names(HSE_2019) <- tolower(names(HSE_2019))

# Rename psu and cluster variables in 2018-19 to match 2015-17 
names(HSE_2018)[names(HSE_2018) == 'psu_scr'] <- 'psu'
names(HSE_2019)[names(HSE_2019) == 'psu_scr'] <- 'psu'
names(HSE_2018)[names(HSE_2018) == 'cluster195'] <- 'cluster'
names(HSE_2019)[names(HSE_2019) == 'cluster194'] <- 'cluster'

# Add year marker to weighting variables as per HSE guidance
colnames(HSE_2019)[which(names(HSE_2019) == "wt_int")] <- "wt_int_2019"
colnames(HSE_2019)[which(names(HSE_2019) == "wt_nurse")] <- "wt_nurse_2019"

colnames(HSE_2018)[which(names(HSE_2018) == "wt_int")] <- "wt_int_2018"
colnames(HSE_2018)[which(names(HSE_2018) == "wt_nurse")] <- "wt_nurse_2018"

colnames(HSE_2017)[which(names(HSE_2017) == "wt_int")] <- "wt_int_2017"
colnames(HSE_2017)[which(names(HSE_2017) == "wt_nurse")] <- "wt_nurse_2017"

colnames(HSE_2016)[which(names(HSE_2016) == "wt_int")] <- "wt_int_2016"
colnames(HSE_2016)[which(names(HSE_2016) == "wt_nurse")] <- "wt_nurse_2016"

colnames(HSE_2015)[which(names(HSE_2015) == "wt_int")] <- "wt_int_2015"
colnames(HSE_2015)[which(names(HSE_2015) == "wt_nurse")] <- "wt_nurse_2015"

# Add survey year variable to 2015 dataset (already present in other years)
HSE_2015['hseyr'] <- 2015

# Make variable names consistent across data years
names(HSE_2018)[names(HSE_2018) == 'urban14br'] <- 'urban14b'
names(HSE_2017)[names(HSE_2017) == 'urban14br'] <- 'urban14b'
names(HSE_2017)[names(HSE_2017) == 'cvd1'] <- 'everbp'
names(HSE_2019)[names(HSE_2019) == 'smkevr_19'] <- 'smkevr'
names(HSE_2019)[names(HSE_2019) == 'cigsta3_19'] <- 'cigsta3'

# Select variables to include in merged dataset
HSE_2019_subset <- subset(HSE_2019,select = c(
    'seriala', 'psu', 'cluster', 'cluster94_2019', 'cluster48_2019', 
    'wt_int_2019', 'wt_nurse_2019', 'hseyr',  'sex', 'ag16g10', 'age16g5', 
    'age35g', 'marstatd', 'origin2', 'hpnssec8', 'hpnssec5', 'hpnssec3', 
    'topqual3', 'topqual2', 'topqual4', 'gor1', 'qimd19', 'urban14b', 
    'genhelf', 'genhelf2', 'pregnowb', 'ncpregj', 'everbp', 'docbp', 'pregbp', 
    'bp1', 'bprespc', 'omdiast', 'omsyst', 'omdiaval', 'omsysval', 'bmivg3',
    'bmivg8', 'smkevr', 'cignow', 'cigsta3'))

HSE_2018_subset <- subset(HSE_2018,select = c(
  'seriala', 'psu', 'cluster', 'cluster95_2018', 'cluster48_2018',
  'wt_int_2018', 'wt_nurse_2018', 'hseyr', 'sex', 'ag16g10', 'age16g5', 
  'age35g', 'marstatd', 'origin2', 'hpnssec8', 'hpnssec5', 'hpnssec3',
  'topqual3', 'topqual2', 'topqual4', 'gor1', 'qimd', 'urban14b', 
  'genhelf', 'genhelf2', 'pregnowb', 'ncpregj', 'everbp', 'docbp', 'pregbp', 
  'bp1', 'bprespc', 'omdiast', 'omsyst', 'omdiaval', 'omsysval', 'bmivg3',
  'bmivg8', 'smkevr', 'cignow', 'cigsta3'))

HSE_2017_subset <- subset(HSE_2017,select = c(
  'seriala', 'psu', 'cluster', 'cluster_65_2017', 
  'wt_int_2017', 'wt_nurse_2017', 'hseyr', 'sex', 'ag16g10', 'age16g5', 
  'age35g', 'marstatd', 'origin2', 'hpnssec8', 'hpnssec5', 'hpnssec3',
  'topqual3', 'topqual2', 'topqual4', 'gor1', 'qimd', 'urban14b', 
  'genhelf', 'genhelf2', 'pregnowb', 'ncpregj', 'everbp', 'docbp', 'pregbp', 
  'bp1', 'bprespc', 'omdiast', 'omsyst', 'omdiaval', 'omsysval', 'bmivg3',
  'bmivg8', 'smkevr', 'cignow', 'cigsta3'))

HSE_2016_subset <- subset(HSE_2016,select = c(
  'seriala', 'psu', 'cluster', 'wt_int_2016', 'wt_nurse_2016', 'hseyr', 
  'sex', 'ag16g10', 'age16g5', 'age35g', 'marstatd', 'origin2', 'hpnssec8', 
  'hpnssec5', 'hpnssec3', 'topqual3', 'topqual2', 'topqual4', 'gor1', 'qimd', 
  'urban14b', 'genhelf', 'genhelf2', 'pregnowb', 'ncpregj', 'everbp', 'docbp', 
  'pregbp', 'bp1', 'bprespc', 'omdiast', 'omsyst', 'omdiaval', 'omsysval', 
  'bmivg3', 'bmivg8', 'smkevr', 'cignow', 'cigsta3'))

HSE_2015_subset <- subset(HSE_2015,select = c(
  'seriala', 'psu', 'cluster', 'cluster_adults_2015', 'wt_int_2015', 
  'wt_nurse_2015', 'hseyr', 'sex', 'ag16g10', 'age16g5', 'age35g', 
  'marstatd', 'origin2', 'hpnssec8', 'hpnssec5', 'hpnssec3', 'topqual3', 
  'topqual2', 'topqual4', 'gor1', 'qimd', 'urban14b', 'genhelf', 'genhelf2', 
  'pregnowb', 'ncpregj', 'everbp', 'docbp', 'pregbp', 'bp1', 'bprespc', 
  'omdiast', 'omsyst', 'omdiaval', 'omsysval', 'bmivg3', 'bmivg8', 'smkevr', 
  'cignow', 'cigsta3'))

# Merge  
first_merge <- rbind.fill(HSE_2019_subset, HSE_2018_subset)
second_merge <- rbind.fill(first_merge, HSE_2017_subset)
third_merge <- rbind.fill (second_merge, HSE_2016_subset)
HSE_merged <- rbind.fill(third_merge, HSE_2015_subset)

# Export merged dataset
write.table(HSE_merged, file = "HSE_merged2.tab", sep = "\t",
            row.names = TRUE, col.names = TRUE)


################################

# POOL AND RESCALE SURVEY WEIGHTS TO SUBSETS FOR ANALYSIS ----------- 

# Combine weights from each year into single variables
HSE_merged2$wt_int <- with(HSE_merged2, 
                           case_when(hseyr == 2015 ~ wt_int_2015,
                                     hseyr == 2016 ~ wt_int_2016,
                                     hseyr == 2017 ~ wt_int_2017,
                                     hseyr == 2018 ~ wt_int_2018,
                                     hseyr == 2019 ~ wt_int_2019))

HSE_merged2$wt_nurse <- with(HSE_merged2, 
                             case_when(hseyr == 2015 ~ wt_nurse_2015,
                                       hseyr == 2016 ~ wt_nurse_2016,
                                       hseyr == 2017 ~ wt_nurse_2017,
                                       hseyr == 2018 ~ wt_nurse_2018,
                                       hseyr == 2019 ~ wt_nurse_2019))


# Sub-sample 1: people aged 16 and over and not pregnant 
# Create subset
HSE_subset <- subset(HSE_merged2, age35g >6 & ncpregj != "1")

# Calculate mean value for weights in each year
HSE_subset %>% filter(hseyr == 2015) %>% select(wt_int) %>% summary() # mean 1.0 
HSE_subset %>% filter(hseyr == 2016) %>% select(wt_int) %>% summary() # mean 1.0006
HSE_subset %>% filter(hseyr == 2017) %>% select(wt_int) %>% summary() # mean 0.9994
HSE_subset %>% filter(hseyr == 2018) %>% select(wt_int) %>% summary() # mean 0.9997
HSE_subset %>% filter(hseyr == 2019) %>% select(wt_int) %>% summary() # mean 1.0 

HSE_subset %>% filter(hseyr == 2015) %>% select(wt_nurse) %>% summary() # mean 0.9815
HSE_subset %>% filter(hseyr == 2016) %>% select(wt_nurse) %>% summary() # mean 0.9716
HSE_subset %>% filter(hseyr == 2017) %>% select(wt_nurse) %>% summary() # mean 0.9838
HSE_subset %>% filter(hseyr == 2018) %>% select(wt_nurse) %>% summary() # mean 0.980
HSE_subset %>% filter(hseyr == 2019) %>% select(wt_nurse) %>% summary() # mean 0.982

# Rescale the weights
HSE_subset$wt_int_sc1 <- 
  with(HSE_subset, case_when(hseyr == 2015 ~ wt_int/1,
                             hseyr == 2016 ~ wt_int/1.0006,
                             hseyr == 2017 ~ wt_int/0.9994,
                             hseyr == 2018 ~ wt_int/0.9997,
                             hseyr == 2019 ~ wt_int/1.0))

HSE_subset$wt_nurse_sc1 <- 
  with(HSE_subset, case_when(hseyr == 2015 ~ wt_nurse/0.9815,
                             hseyr == 2016 ~ wt_nurse/0.9716,
                             hseyr == 2017 ~ wt_nurse/0.9838,
                             hseyr == 2018 ~ wt_nurse/0.980,
                             hseyr == 2019 ~ wt_nurse/0.982))

# Check weights now have mean 1.0
summary(HSE_subset$wt_int_sc1) 
summary(HSE_subset$wt_nurse_sc1) 


# Sub-sample 2: people aged 16 and over, not pregnant, with valid bp measurement
# Create the subset
HSE_subset_valid <- subset(HSE_merged2, age35g >6 & ncpregj != "1" & bprespc == "1")

# Calculate mean value for weights in each year
HSE_subset_valid %>% filter(hseyr == 2015) %>% select(wt_int) %>% summary() # mean 0.9818
HSE_subset_valid %>% filter(hseyr == 2016) %>% select(wt_int) %>% summary() # mean 0.9621
HSE_subset_valid %>% filter(hseyr == 2017) %>% select(wt_int) %>% summary() # mean 0.9598
HSE_subset_valid %>% filter(hseyr == 2018) %>% select(wt_int) %>% summary() # mean 0.9614
HSE_subset_valid %>% filter(hseyr == 2019) %>% select(wt_int) %>% summary() # mean 0.9703

HSE_subset_valid %>% filter(hseyr == 2015) %>% select(wt_nurse) %>% summary() # mean 0.9762
HSE_subset_valid %>% filter(hseyr == 2016) %>% select(wt_nurse) %>% summary() # mean 0.9580
HSE_subset_valid %>% filter(hseyr == 2017) %>% select(wt_nurse) %>% summary() # mean 0.9714
HSE_subset_valid %>% filter(hseyr == 2018) %>% select(wt_nurse) %>% summary() # mean 0.9610
HSE_subset_valid %>% filter(hseyr == 2019) %>% select(wt_nurse) %>% summary() # mean 0.9779

# Rescale the weights
HSE_subset_valid$wt_int_sc2 <- 
  with(HSE_subset_valid, case_when(hseyr == 2015 ~ wt_int/0.9818,
                                   hseyr == 2016 ~ wt_int/0.9621,
                                   hseyr == 2017 ~ wt_int/0.9598,
                                   hseyr == 2018 ~ wt_int/0.9614,
                                   hseyr == 2019 ~ wt_int/0.9703))

HSE_subset_valid$wt_nurse_sc2 <- 
  with(HSE_subset_valid, case_when(hseyr == 2015 ~ wt_nurse/0.9762,
                                   hseyr == 2016 ~ wt_nurse/0.9580,
                                   hseyr == 2017 ~ wt_nurse/0.9714,
                                   hseyr == 2018 ~ wt_nurse/0.9610,
                                   hseyr == 2019 ~ wt_nurse/0.9779))

# Check weights now have mean 1.0
summary(HSE_subset_valid$wt_int_sc2) 
summary(HSE_subset_valid$wt_nurse_sc2)


# Sub-sample 3: people aged 16 and over, not pregnant, who have hypertension
# Based on definition: measured BP >= 140/90 or self-report hypertension diagnosis

# Create the subset
HSE_hyperten <- subset(HSE_subset_valid, bp1 != "-8") %>%
  mutate(hbp_yn = case_when((omsysval < 140 & omdiaval <90 & bp1 == 2) ~ 0,
                            TRUE ~ 1)) 
HSE_hyperten <- subset(HSE_hyperten, hbp_yn == "1")

# Calculate mean value for weights in each year
HSE_hyperten %>% filter(hseyr == 2015) %>% select(wt_int) %>% summary() # mean 0.8830
HSE_hyperten %>% filter(hseyr == 2016) %>% select(wt_int) %>% summary() # mean 0.8882
HSE_hyperten %>% filter(hseyr == 2017) %>% select(wt_int) %>% summary() # mean 0.8695 
HSE_hyperten %>% filter(hseyr == 2018) %>% select(wt_int) %>% summary() # mean 0.8792 
HSE_hyperten %>% filter(hseyr == 2019) %>% select(wt_int) %>% summary() # mean 0.8884 

HSE_hyperten %>% filter(hseyr == 2015) %>% select(wt_nurse) %>% summary() # mean 0.8358  
HSE_hyperten %>% filter(hseyr == 2016) %>% select(wt_nurse) %>% summary() # mean 0.8216 
HSE_hyperten %>% filter(hseyr == 2017) %>% select(wt_nurse) %>% summary() # mean 0.8299   
HSE_hyperten %>% filter(hseyr == 2018) %>% select(wt_nurse) %>% summary() # mean 0.8256   
HSE_hyperten %>% filter(hseyr == 2019) %>% select(wt_nurse) %>% summary() # mean 0.8489   

# Rescale the weights
HSE_hyperten$wt_int_sc3 <- 
  with(HSE_hyperten, case_when(hseyr == 2015 ~ wt_int/0.8830,
                               hseyr == 2016 ~ wt_int/0.8882,
                               hseyr == 2017 ~ wt_int/0.8695,
                               hseyr == 2018 ~ wt_int/0.8792,
                               hseyr == 2019 ~ wt_int/0.8884))

HSE_hyperten$wt_nurse_sc3 <- 
  with(HSE_hyperten, case_when(hseyr == 2015 ~ wt_nurse/0.8358,
                               hseyr == 2016 ~ wt_nurse/0.8216,
                               hseyr == 2017 ~ wt_nurse/0.8299,
                               hseyr == 2018 ~ wt_nurse/0.8256,
                               hseyr == 2019 ~ wt_nurse/0.8489))

# Check weights now have mean 1.0
summary(HSE_hyperten$wt_int_sc3) 
summary(HSE_hyperten$wt_nurse_sc3) 


# Add rescaled weights to merged dataset ---------
HSE_subset_merge <- dplyr::select(.data = HSE_subset, 
                                  seriala, wt_int_sc1, wt_nurse_sc1)

HSE_subset_valid <- dplyr::select(.data = HSE_subset_valid, 
                                  seriala, wt_int_sc2, wt_nurse_sc2)

HSE_hyperten <- dplyr::select(.data = HSE_hyperten, 
                              seriala, wt_int_sc3, wt_nurse_sc3)

HSE_merged_rescaled <- HSE_merged2 %>% 
  dplyr::full_join(HSE_subset_merge, by = "seriala")

HSE_merged_rescaled <- HSE_merged_rescaled %>% 
  dplyr::full_join(HSE_subset_valid, by = "seriala")

HSE_merged_rescaled <- HSE_merged_rescaled %>% 
  dplyr::full_join(HSE_hyperten, by = "seriala")

# Export
write.table(HSE_merged_rescaled, 
            file = "02_HSE_merged_rescaled.tab", sep = "\t",
            row.names = TRUE)


################################

# DATA CLEANING ----------

# Filter sample to adults 16+, not pregnant, with valid BP data 
HSE_subset <- subset(HSE_merged_rescaled, age35g != "1" & age35g != "2" & 
                       age35g != "3" & age35g != "4" & age35g != "5" &
                       age35g != "6" & ncpregj != "1" & bprespc == "1" & 
                       bp1 != "-8")

# Make new variable for total hypertension
HSE_subset <- HSE_subset %>%
  mutate(hbp_yn = case_when((omsysval < 140 & omdiaval < 90 & bp1 == 2) ~ 0,
                            TRUE ~ 1))

# Make new variable for hypertension diagnosed
HSE_subset <- HSE_subset %>%
  mutate(diagnosed_yn = case_when((hbp_yn == 1 & bp1 == 1) ~ 1,
                                  (hbp_yn == 1 & bp1 != 1) ~ 0,
                                  TRUE ~ -1))

# Create binary version of undiagnosed hypertension where 1 = undiagnosed and
# 0 = all others (no hypertension or diagnosed)
HSE_subset <- HSE_subset %>%
  mutate(undiag_ht = recode(diagnosed_yn, '0'=1, '1'=0, '-1'= 0)) 
HSE_subset$undiag_ht <- factor(HSE_subset$undiag_ht, levels = c("0", "1"),
                               labels = c("No undiagnosed hypertension", 
                                          "Undiagnosed hypertension"))


# RECODE AND DERIVE VARIABLES AND CONVERT TO FACTORS ----------

# Sex
HSE_subset$sex <- factor(HSE_subset$sex, levels=c("1", "2"), 
                         labels=c("Male", "Female"))

# Age
HSE_subset$ag16g10 <- factor(HSE_subset$ag16g10, 
                             levels = c("1", "2", "3", "4", "5", "6", "7"), 
                             labels = c("16-24", "25-34", "35-44", "45-54", 
                                        "55-64", "65-74", "75+"))
# Ethnicity
HSE_subset$origin2 <- recode(HSE_subset$origin2, '1'=1, '2'=2, 
                             '3'=3, '4'=4, '5'=5)
HSE_subset$origin_4 <- recode(HSE_subset$origin2, '1'=1L, '2'=2L, '3'=3L, 
                              '4'=4L, '5'=4L)
table(HSE_subset$origin2, HSE_subset$origin_4)

HSE_subset$origin_4 <- factor(HSE_subset$origin_4, levels = c("1", "2", "3", "4"),
                              labels = c("White", "Black", "Asian", "Other"))

# Region
HSE_subset$gor1 <- factor(HSE_subset$gor1, levels = c("1", "2", "3", "4", "5", 
                                                      "6", "7", "8", "9"), 
                          labels = c("North East", "North West", "Yorkshire and the Humber",
                                     "East Midlands", "West Midlands", "East of England", 
                                     "London", "South East", "South West"))

# Urban/rural
HSE_subset$urban14b <- factor(HSE_subset$urban14b, levels = c("1", "2"), 
                              labels = c("Urban", "Rural"))

# NS-SEC
HSE_subset$hpnssec5 <- recode(HSE_subset$hpnssec5, '1'=1, '2'=2, '3'=3, '
                              4'=4, '5'=5, '99'=6)
HSE_subset$hpnssec5 <- factor(HSE_subset$hpnssec5, levels = c("1", "2", "3", 
                                                              "4", "5", "6"), 
                              labels = c("Managerial and professional occupations",
                                         "Intermediate occupations",
                                         "Small employers and own account workers",
                                         "Lower supervisory and technical occupations",
                                         "Semi-routine occupations",
                                         "Other"))

# Education
HSE_subset$topqual4 <- recode(HSE_subset$topqual4, '1'=1, '2'=2, '3'=3)
HSE_subset$topqual4 <- factor(HSE_subset$topqual4, levels = c("1", "2", "3"), 
                              labels = c("Degree or equiv", "Below degree", 
                                         "No qualification")) 

# Relationship status
HSE_subset$marstat_5 <- recode(HSE_subset$marstatd, '1'=1L, '2'=2L, '6'=3L, 
                               '3'=4L, '4'=4L, '5'=5L, '-9'=NA_integer_, 
                               '-8'=NA_integer_, '-1'=NA_integer_)
HSE_subset$marstat_5 <- factor(HSE_subset$marstat_5, levels = c("1", "2", "3", "4", "5"), 
                               labels = c("Single", "Married or civil partnership", 
                                          "Cohabiting", "Separated or divorced", 
                                          "Widowed or surviving partner"))

# General health
HSE_subset$genhelf2 <- recode(HSE_subset$genhelf2, '1'=1, '2'=2, '3'=3)
HSE_subset$genhelf2 <- factor(HSE_subset$genhelf2, levels = c("1", "2", "3"), 
                              labels = c("Very good or good", "Fair", 
                                         "Bad or very bad"))

# Smoking status
HSE_subset$cigsta3 <- recode(HSE_subset$cigsta3, '1'=1, '2'=2, '3'=3)
HSE_subset$cigsta3 <- factor(HSE_subset$cigsta3, levels = c("1", "2", "3"),
                             labels = c("Current cigarette smoker", 
                                        "Ex-regular cigarette smoker",
                                        "Never regular cigarette smoker"))

# BMI
HSE_subset$bmivg3 <- recode(HSE_subset$bmivg3, '1'=1, '2'=2, '3'=3)
HSE_subset$bmivg3 <- factor(HSE_subset$bmivg3, levels = c("1", "2", "3"), 
                            labels = c("Not overweight or obese", "Overweight", 
                                       "Obese"))

# Create consistent 5-year age bands for calculating age-standardised rates
HSE_subset <- HSE_subset %>%
  mutate(age_grp = case_when(age16g5 == 1 | age16g5 == 2 ~ "16-19",
                             age16g5 == 3 ~ "20-24",
                             age16g5 == 4 ~ "25-29",
                             age16g5 == 5 ~ "30-34",
                             age16g5 == 6 ~ "35-39",
                             age16g5 == 7 ~ "40-44",
                             age16g5 == 8 ~ "45-49",
                             age16g5 == 9 ~ "50-54",
                             age16g5 == 10 ~ "55-59",
                             age16g5 == 11 ~ "60-64",
                             age16g5 == 12 ~ "65-69",
                             age16g5 == 13 ~ "70-74",
                             age16g5 == 14 ~ "75-79",
                             age16g5 == 15 ~ "80-84",
                             age16g5 == 16 ~ "85-89",
                             age16g5 == 17 ~ "90+"),
         age_grp = factor(age_grp, levels = c("16-19", "20-24", "25-29", "30-34", 
                                              "35-39", "40-44", "45-49", "50-54",
                                              "55-59", "60-64", "65-69", "70-74",
                                              "75-79", "80-84", "85-89", "90+")))


# Export cleaned dataset for analysis ---------

write.table(HSE_subset, file = "03_HSE_subset_140_90.tab", sep = "\t",
            row.names = TRUE, col.names = TRUE)



##### END SCRIPT #####

