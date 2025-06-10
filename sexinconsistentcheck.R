library(haven)
library(dplyr)
library(tidyr)
library(purrr)

# Define paths
data_path <- "/Users/alison/Library/CloudStorage/OneDrive-UniversityCollegeLondon/1_projectsrole/20222025_NextSteps/1_data/UKDA-5545-stata/stata/stata13/safeguarded_eul"

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
  S9maininterview = "ns9_2022_main_interview.dta",
  S9derivedvariable = "ns9_2022_derived_variables.dta")


# Load and extract sex from each relevant sweep
sex_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID, sex_S1 = W1sexYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% select(ID = NSID, sex_S2 = W2SexYP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>% select(ID = NSID, sex_S3 = W3sexYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% select(ID = NSID, sex_S4 = W4SexYP),
  S5 = read_dta(file.path(data_path, sweeps$S5youngperson)) %>% select(ID = NSID, sex_S5 = W5SexYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% select(ID = NSID, sex_S6 = W6Sex),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>% select(ID = NSID, sex_S7 = W7Sex),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>% select(ID = NSID, sex_S8 = W8CMSEX),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% select(ID = NSID, sex_S9 = W9DSEX)
)

# Merge all sweeps by ID
sex_all <- reduce(sex_vars, full_join, by = "ID")

# Reshape to long format
sex_long <- sex_all %>%
  pivot_longer(cols = starts_with("sex_"), names_to = "sweep", values_to = "sex") %>%
  filter(!is.na(sex)) %>%
  distinct(ID, sex)

# Count number of unique sex values per person
sex_check <- sex_long %>%
  group_by(ID) %>%
  summarise(n_distinct_sex = n_distinct(sex, na.rm = TRUE)) %>%
  filter(n_distinct_sex > 1)

# View sample of inconsistencies
sex_inconsistencies <- sex_all %>% filter(ID %in% sex_check$ID)

# Print results
cat("Number of people with inconsistent sex reports:", n_inconsistent, "\n")