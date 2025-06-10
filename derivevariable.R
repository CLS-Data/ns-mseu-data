# This script is used to run the Next Steps MSEU 
# Load packages
library(haven)   # for reading SPSS/Stata files
library(dplyr)
library(purrr)

# Set folder path
data_path <- "/Users/alison/Library/CloudStorage/OneDrive-UniversityCollegeLondon/1_projectsrole/20222025_NextSteps/1_data/UKDA-5545-stata/stata/stata13/safeguarded_eul"

# Define sweep file names (edit as necessary)
sweeps <- list(
  S1familybackground = "wave_one_lsype_family_background_2020.dta",
  S1youngperson = "wave_one_lsype_young_person_2020.dta",
  S2familybackground = "wave_two_lsype_family_background_2020.dta",
  S2youngperson = "wave_two_lsype_young_person_2020.dta",
  S3familybackground = "wave_three_lsype_family_background_2020.dta",
  S3youngperson = "wave_three_lsype_young_person_2020.dta",
  S4familybackground = "wave_four_lsype_family_background_2020.dta",
  S4youngperson = "wave_four_lsype_young_person_2020.dta",
  S5familybackground = "wave_five_lsype_family_background_2020.dta",
  S5youngperson = "wave_five_lsype_young_person_2020.dta",
  S6youngperson = "wave_six_lsype_young_person_2020.dta",
  S7youngperson = "wave_seven_lsype_young_person_2020.dta",
  S8maininterview = "ns8_2015_main_interview.dta",
  S8selfcompletion = "ns8_2015_self_completion.dta",
  S8derivedvariable = "ns8_2015_derived.dta",
  S9maininterview = "ns9_2022_main_interview.dta",
  S9derivedvariable = "ns9_2022_derived_variables.dta")


# Load all datasets
ns_data <- map(sweeps, ~ read_dta(file.path(data_path, .x)))

#### sex ####
sex_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID, sex_S1 = W1sexYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% select(ID = NSID, sex_S2 = W2SexYP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>% select(ID = NSID, sex_S3 = W3sexYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% select(ID = NSID, boost = W4Boost, sex_S4 = W4SexYP),
  S5 = read_dta(file.path(data_path, sweeps$S5youngperson)) %>% select(ID = NSID, sex_S5 = W5SexYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% select(ID = NSID, sex_S6 = W6Sex),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>% select(ID = NSID, sex_S7 = W7Sex),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>% select(ID = NSID, sex_S8 = W8CMSEX),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% select(ID = NSID, sex_S9 = W9DSEX)
)

# Merge all sweeps by ID
sex_all <- reduce(sex_vars, full_join, by = "ID")

# Harmonised the missing values for S1-7
# Vector of S1–S7 variable names
sex_vars_s1_s7 <- paste0("sex_S", 1:7)

# Apply custom recode to S1–S7
sex_all <- sex_all %>%
  mutate(across(
    all_of(sex_vars_s1_s7),
    ~ case_when(
      .x == -92 ~ -9,
      .x == -91 ~ -1,
      .x == -99 ~ -3,
      TRUE ~ .x
    )
  ))

# Derive harmonised sex 
sex_all <- sex_all %>%
mutate(
  # First pass: positive values only
  sex_final_main = case_when(
    !is.na(sex_S9) & sex_S9 > 0 ~ sex_S9,
    !is.na(sex_S1) & sex_S1 > 0 ~ sex_S1,
    !is.na(sex_S2) & sex_S2 > 0 ~ sex_S2,
    !is.na(sex_S3) & sex_S3 > 0 ~ sex_S3,
    !is.na(sex_S4) & sex_S4 > 0 & boost == 2 ~ sex_S4,  # main
    !is.na(sex_S4) & sex_S4 > 0 & boost == 1 ~ sex_S4,  # boost
    !is.na(sex_S5) & sex_S5 > 0 ~ sex_S5,
    !is.na(sex_S6) & sex_S6 > 0 ~ sex_S6,
    !is.na(sex_S7) & sex_S7 > 0 ~ sex_S7,
    !is.na(sex_S8) & sex_S8 > 0 ~ sex_S8,
    TRUE ~ NA_real_
  ),
  
  # Second pass: fallback to non-positive values (< 0)
  sex_final = case_when(
    !is.na(sex_final_main) ~ sex_final_main,
    !is.na(sex_S1) & sex_S1 < 1 ~ sex_S1,
    !is.na(sex_S2) & sex_S2 < 1 ~ sex_S2,
    !is.na(sex_S3) & sex_S3 < 1 ~ sex_S3,
    !is.na(sex_S4) & sex_S4 < 1 ~ sex_S4,
    !is.na(sex_S5) & sex_S5 < 1 ~ sex_S5,
    !is.na(sex_S6) & sex_S6 < 1 ~ sex_S6,
    !is.na(sex_S7) & sex_S7 < 1 ~ sex_S7,
    !is.na(sex_S8) & sex_S8 < 1 ~ sex_S8,
    TRUE ~ NA_real_
  ),
  
  # Source tracking
  sex_source = case_when(
    !is.na(sex_S9) & sex_S9 > 0 ~ "S9",
    !is.na(sex_S1) & sex_S1 > 0 ~ "S1",
    !is.na(sex_S2) & sex_S2 > 0 ~ "S2",
    !is.na(sex_S3) & sex_S3 > 0 ~ "S3",
    !is.na(sex_S4) & sex_S4 > 0 & boost == 2 ~ "S4_main",
    !is.na(sex_S4) & sex_S4 > 0 & boost == 1 ~ "S4_boost",
    !is.na(sex_S5) & sex_S5 > 0 ~ "S5",
    !is.na(sex_S6) & sex_S6 > 0 ~ "S6",
    !is.na(sex_S7) & sex_S7 > 0 ~ "S7",
    !is.na(sex_S8) & sex_S8 > 0 ~ "S8",
    
    # Now for fallback to non-positive values
    !is.na(sex_S1) & sex_S1 < 1 ~ "S1_missing",
    !is.na(sex_S2) & sex_S2 < 1 ~ "S2_missing",
    !is.na(sex_S3) & sex_S3 < 1 ~ "S3_missing",
    !is.na(sex_S4) & sex_S4 < 1 & boost == 2 ~ "S4_main_missing",
    !is.na(sex_S4) & sex_S4 < 1 & boost == 1 ~ "S4_boost_missing",
    !is.na(sex_S5) & sex_S5 < 1 ~ "S5_missing",
    !is.na(sex_S6) & sex_S6 < 1 ~ "S6_missing",
    !is.na(sex_S7) & sex_S7 < 1 ~ "S7_missing",
    !is.na(sex_S8) & sex_S8 < 1 ~ "S8_missing",
    TRUE ~ NA_character_
  )
)

sex_all <- sex_all %>%
  mutate(
    sex_final_recode = case_when(
      sex_final == 1 ~ 0,  # 1 = male → 0
      sex_final == 2 ~ 1,  # 2 = female → 1
      TRUE ~ sex_final      # handle others or missing
    )
  )

#### ethnicity ####
# Load ethnicity variables from relevant sweeps
ethnicity_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID, eth_S1 = W1ethnic2YP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% select(ID = NSID, eth_S2 = W2ethnicYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% select(ID = NSID, eth_S4 = w4ethnic2YP),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>% select(ID = NSID, eth_S8 = W8DETHN15),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>% select(ID = NSID, eth_S9 = W9DETHN15)
)

# Merge into one dataset
eth_all <- reduce(ethnicity_vars, full_join, by = "ID")

# Derive ethnicity: use S1 if available, else later
eth_all <- eth_all %>%
  mutate(
    ethnicity = case_when(
      !is.na(eth_S1) & eth_S1 > 0 ~ eth_S1,
      !is.na(eth_S2) & eth_S2 > 0 ~ eth_S2,
      !is.na(eth_S4) & eth_S4 > 0 ~ eth_S4,
      !is.na(eth_S8) & eth_S8 > 0 ~ eth_S8,
      !is.na(eth_S9) & eth_S9 > 0 ~ eth_S9,
      !is.na(eth_S1) & eth_S1 < 1 ~ eth_S1,
      !is.na(eth_S2) & eth_S2 < 1 ~ eth_S2,
      !is.na(eth_S4) & eth_S4 < 1 ~ eth_S4,
      !is.na(eth_S8) & eth_S8 < 1 ~ eth_S8,
      !is.na(eth_S9) & eth_S9 < 1 ~ eth_S9,
      TRUE ~ NA_real_
    ),
    ethnicity_source = case_when(
      !is.na(eth_S1) & eth_S1 > 0 ~ "S1",
      !is.na(eth_S2) & eth_S2 > 0 ~ "S2",
      !is.na(eth_S4) & eth_S4 > 0 ~ "S4",
      !is.na(eth_S8) & eth_S8 > 0 ~ "S8",
      !is.na(eth_S9) & eth_S9 > 0 ~ "S9",
      !is.na(eth_S1) & eth_S1 < 1 ~ "S1_missing",
      !is.na(eth_S2) & eth_S2 < 1 ~ "S2_missing",
      !is.na(eth_S4) & eth_S4 < 1 ~ "S4_missing",
      !is.na(eth_S8) & eth_S8 < 1 ~ "S8_missing",
      !is.na(eth_S9) & eth_S9 < 1 ~ "S9_missing",
      TRUE ~ NA_character_
    )
  )