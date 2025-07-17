# This script is used to run the Next Steps MSEU 
# Load packages
library(haven)   # for reading SPSS/Stata files
library(dplyr)
library(purrr)
library(here)
library(labelled)

# Set folder path
data_path <- here("data", "UKDA-5545-stata", "stata", "stata13", "safeguarded_eul")

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
  S4history = "wave_four_lsype_history_2020.dta",
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
# Load sex variables from relevant sweeps
sex_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(ID = NSID, sex_S1 = W1sexYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(ID = NSID, sex_S2 = W2SexYP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>% 
    select(ID = NSID, sex_S3 = W3sexYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(ID = NSID, boost = W4Boost, sex_S4 = W4SexYP),
  S5 = read_dta(file.path(data_path, sweeps$S5youngperson)) %>% 
    select(ID = NSID, sex_S5 = W5SexYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% 
    select(ID = NSID, sex_S6 = W6Sex),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>% 
    select(ID = NSID, sex_S7 = W7Sex),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>% 
    select(ID = NSID, sex_S8 = W8CMSEX),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% 
    select(ID = NSID, sex_S9 = W9DSEX)
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
    sex = case_when(
      sex_final == 1 ~ 0,  # 1 = male → 0
      sex_final == 2 ~ 1,  # 2 = female → 1
      TRUE ~ sex_final      # handle others or missing
    )
  ) %>%
  select(ID, sex)

#### ethnicity ####
# Load ethnicity variables from relevant sweeps
ethnicity_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(ID = NSID, eth_S1 = W1ethnic2YP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(ID = NSID, eth_S2 = W2ethnicYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(ID = NSID, boost = W4Boost, eth_S4 = w4ethnic2YP),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>% 
    select(ID = NSID, eth_S8 = W8DETHN15),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>% 
    select(ID = NSID, eth_S9 = W9DETHN15)
)

# Merge into one dataset
eth_all <- reduce(ethnicity_vars, full_join, by = "ID")

# Harmonise missing values for S1–S4
# Create a vector of ethnicity variables
eth_vars <- c("eth_S1", "eth_S2", "eth_S4")

# Apply the recoding
eth_all <- eth_all %>%
  mutate(across(all_of(eth_vars), ~ case_when(
    .x == -999 ~ -2,
    .x == -998 ~ -2,
    .x == -997 ~ -2,
    .x == -99  ~ -3,
    .x == -94  ~ -2,
    .x == -92  ~ -9,
    .x == -91  ~ -1,
    .x == -1   ~ -8,
    TRUE ~ .x
  )))

# Derive ethnicity: use S1 if available, else later
eth_all <- eth_all %>%
  mutate(
    eth = case_when(
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
  )%>%
  select(ID, eth)

#### language ####
# Load relevant language variables
lang_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(ID = NSID, lang_S1 = W1englangYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(ID = NSID, lang_S2 = W2EnglangYP),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>% 
    select(ID = NSID, lang_S3 = W3englangHH),
  S4 = read_dta(file.path(data_path, sweeps$S4familybackground)) %>% 
    select(ID = NSID, lang_S4 = W4EngLangHH)
)

# Merge
lang_all <- reduce(lang_vars, full_join, by = "ID")

# Harmonise any special missing codes (from known labels)
lang_all <- lang_all %>%
  mutate(across(starts_with("lang_S"), ~ case_when(
    .x %in% c(-999, -998, -997, -995, -94) ~ -2,  # error/information lost
    .x == -99 ~ -3,                        # not interviewed
    .x == -92 ~ -9,                        # refused
    .x == -91 ~ -1,                        # not applicable
    .x == -1  ~ -8,                        # don't know
    TRUE ~ .x
  )))

# Derive final language variable: use S1, else S2, else S4
lang_all <- lang_all %>%
  mutate(
    lang = case_when(
      !is.na(lang_S1) & lang_S1 > 0 ~ lang_S1,
      !is.na(lang_S2) & lang_S2 > 0 ~ lang_S2,
      !is.na(lang_S3) & lang_S3 > 0 ~ lang_S3,
      !is.na(lang_S4) & lang_S4 > 0 ~ lang_S4,
      !is.na(lang_S1) & lang_S1 < 1 ~ lang_S1,
      !is.na(lang_S2) & lang_S2 < 1 ~ lang_S2,
      !is.na(lang_S3) & lang_S3 < 1 ~ lang_S3,
      !is.na(lang_S4) & lang_S4 < 1 ~ lang_S4,
      TRUE ~ NA_real_
    ),
    language_source = case_when(
      !is.na(lang_S1) & lang_S1 > 0 ~ "S1",
      !is.na(lang_S2) & lang_S2 > 0 ~ "S2",
      !is.na(lang_S3) & lang_S3 > 0 ~ "S3",
      !is.na(lang_S4) & lang_S4 > 0 ~ "S4",
      !is.na(lang_S1) & lang_S1 < 1 ~ "S1_missing",
      !is.na(lang_S2) & lang_S2 < 1 ~ "S2_missing",
      !is.na(lang_S3) & lang_S3 < 1 ~ "S3_missing",
      !is.na(lang_S4) & lang_S4 < 1 ~ "S4_missing",
      TRUE ~ NA_character_
    )
  )%>%
  select(ID, lang)

#### sexual orientation ####
# Load sexuality variables
sexuality_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(ID = NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(ID = NSID),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% 
    select(ID = NSID, W6SexualityYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>% 
    select(ID = NSID, W7SexualityYP),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>% 
    select(ID = NSID, W8SEXUALITY),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% 
    select(ID = NSID, W9SORI)
)

# Merge by ID
sexuality_all <- reduce(sexuality_vars, full_join, by = "ID")

# Harmonise missing values for S1–S4
sexuality_all <- sexuality_all %>%
  mutate(
    sori19 = case_when(
      W6SexualityYP == 1 ~ 1,
      W6SexualityYP == 2 ~ 2,
      W6SexualityYP == 3 ~ 3,
      W6SexualityYP == 4 ~ 4,
      W6SexualityYP %in% c(-100, -97, -3) ~ -3,
      W6SexualityYP %in% c(-92, -9) ~ -9,
      W6SexualityYP %in% c(-91, -1, -8) ~ -8,
      W6SexualityYP %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    ),
    
    sori20 = case_when(
      W7SexualityYP == 1 ~ 1,
      W7SexualityYP == 2 ~ 2,
      W7SexualityYP == 3 ~ 3,
      W7SexualityYP == 4 ~ 4,
      W7SexualityYP %in% c(-100, -97, -3) ~ -3,
      W7SexualityYP %in% c(-92, -9) ~ -9,
      W7SexualityYP %in% c(-91, -1, -8) ~ -8,
      W7SexualityYP %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    ),
    
    sori25 = case_when(
      W8SEXUALITY == 1 ~ 1,
      W8SEXUALITY == 2 ~ 2,
      W8SEXUALITY == 3 ~ 3,
      W8SEXUALITY == 4 ~ 4,
      W8SEXUALITY == -9 ~ -9,
      W8SEXUALITY == -8 ~ -8,
      W8SEXUALITY == -1 ~ -8,
      W8SEXUALITY %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    ),
    
    sori32 = case_when(
      W9SORI == 1 ~ 1,
      W9SORI == 2 ~ 2,
      W9SORI == 3 ~ 3,
      W9SORI == 4 ~ 4,
      W9SORI == 5 ~ -7,
      W9SORI == -9 ~ -9,
      W9SORI %in% c(-8, -1) ~ -8,
      W9SORI == -3 ~ -3,
      W9SORI %in% c(-999, -998, -997, -94) ~ -2,
      TRUE ~ -3
    )
  ) %>%
  select(ID, sori19, sori20, sori25, sori32)

#### partnership ####
# Load partnership variables from relevant sweeps
partnr_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(ID = NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(ID = NSID),
  S5 = read_dta(file.path(data_path, sweeps$S5youngperson)) %>% 
    select(ID = NSID, W5Marstat2YP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% 
    select(ID = NSID, W6MarStatYP),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>% 
    select(ID = NSID, W8DMARSTAT),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>% 
    select(ID = NSID, W9DMARSTAT)
)

# Merge all sweeps by ID
partnr_all <- reduce(partnr_vars, full_join, by = "ID")

# Harmonise missing values
partnr_all <- partnr_all %>%
  mutate(
    partnr18 = case_when(
      W5Marstat2YP == 1 ~ 1,         # married/civil partner
      W5Marstat2YP == 2 ~ 0, # engaged or neither → single
      W5Marstat2YP == 3 ~ 2, #  neither → other
      W5Marstat2YP == -92 ~ -9,
      W5Marstat2YP == -91 ~ -1,
      W5Marstat2YP == -1  ~ -8,
      TRUE ~ -3
    ),
    
    partnr19 = case_when(
      W6MarStatYP == 2 ~ 1,
      W6MarStatYP == 1 ~ 0,
      W6MarStatYP %in% c(3, 4, 5) ~ 2,
      W6MarStatYP == -92 ~ -9,
      W6MarStatYP == -91 ~ -1,
      W6MarStatYP == -1 ~ -8,
      W6MarStatYP %in% c(-997, -97) ~ -3,
      TRUE ~ -3
    ),
    
    partnr25 = case_when(
      W8DMARSTAT %in% c(2, 6) ~ 1,
      W8DMARSTAT == 1 ~ 0,
      W8DMARSTAT %in% c(3,4,5,7,8,9) ~ 2,
      W8DMARSTAT == -9 ~ -9,
      W8DMARSTAT == -8 ~ -8,
      W8DMARSTAT == -1 ~ -1,
      TRUE ~ -3
    ),
    
    partnr32 = case_when(
      W9DMARSTAT %in% c(2,6) ~ 1,
      W9DMARSTAT == 1 ~ 0,
      W9DMARSTAT %in% c(3,4,5,7,8,9) ~ 2,
      W9DMARSTAT == -9 ~ -9,
      W9DMARSTAT == -8 ~ -8,
      W9DMARSTAT == -1 ~ -1,
      TRUE ~ -3
    )
  )

partnr_all <- partnr_all %>%
  mutate(
    partnradu25 = case_when(
      W8DMARSTAT %in% 1:9 ~ W8DMARSTAT-1,
      W8DMARSTAT == -9 ~ -9,
      W8DMARSTAT == -8 ~ -8,
      W8DMARSTAT == -1 ~ -1,
      TRUE ~ -3
    ),
    
    partnradu32 = case_when(
      W9DMARSTAT %in% 1:9 ~ W9DMARSTAT-1,
      W9DMARSTAT == -9 ~ -9,
      W9DMARSTAT == -8 ~ -8,
      W9DMARSTAT == -1 ~ -1,
      TRUE ~ -3
    )
  )%>%
  select(ID, partnr18, partnr19, partnr25, partnr32,
         partnradu25, partnradu32)

#### region ####
# Load region variables from relevant sweeps
region_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% select(ID = NSID),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
    select(ID = NSID, regub15 = urbind, regov15 = gor),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>%
    select(ID = NSID, regub16 = urbind, regov16 = gor),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(ID = NSID, regor25 = W8DGOR),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(ID = NSID, regor32 = W9DRGN),
  S9_2 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(ID = NSID, regint32 = W9NATIONRES)
)

# Merge all region variables by ID
region_all <- reduce(region_vars, full_join, by = "ID")

# Harmonise region variables
region_all <- region_all %>%
  mutate(across(c(regub15, regub16), ~ case_when(
    .x %in% 1:8 ~ .x,
    .x == -94 ~ -2,
    TRUE ~ -3
  ))) %>%
  
  mutate(across(c(regov15, regov16), ~ case_when(
    .x %in% 1:9 ~ .x,
    .x == -94 ~ -2,
    TRUE ~ -3
  ))) %>%
  
  mutate(across(c(regor25, regor32), ~ case_when(
    .x %in% 1:12 ~ .x,
    .x == 13 ~ -2,                # faulty location
    .x == -9 ~ -9,                # refused
    .x == -8 ~ -8,                # don't know
    .x == -1 ~ -1,                # not applicable
    TRUE ~ -3                     # not participated
  ))) %>%
  
  mutate(regint32 = case_when(
    regint32 == 1 ~ 1,   # in the UK
    regint32 == 2 ~ 2,   # abroad
    regint32 == -9 ~ -9,
    regint32 == -8 ~ -8,
    regint32 == -3 ~ -3,
    regint32 == -1 ~ -1,
    TRUE ~ -3
  ))%>%
  select(ID, regub15, regov15, regub16, regov16,
         regor25, regor32, regint32)

#### education own ####
# Load education variables from relevant sweeps
educ_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson )) %>%
    select(ID = NSID, educ17_raw = w4saim),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>%
    select(ID = NSID, educ19_raw = W6Saim),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(ID = NSID, educ20_raw = W7SAim),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>%
    select(ID = NSID, starts_with("W8ACQU"), starts_with("W8VCQU")),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(ID = NSID, starts_with("W9ACQU"), starts_with("W9VCQU"))
)

# Merge by ID
educ_all <- reduce(educ_vars, full_join, by = "ID")

# Harmonise and derive education variables
educ_all <- educ_all %>%
  mutate(
    # Sweep 4
    educ17 = case_when(
      educ17_raw %in% 1:9 ~ 1,
      educ17_raw == 10 ~ 0,
      educ17_raw %in% 11:13 ~ -2,
      educ17_raw == 14 ~ 0,
      educ17_raw == -94 ~ -2,
      educ17_raw == -91 ~ -1,
      TRUE ~ -3  # Not interviewed/present
    ),
    
    # Sweep 6
    educ19 = case_when(
      educ19_raw %in% 1:4 ~ 2,
      educ19_raw %in% 5:13 ~ 1,
      educ19_raw %in% 14:15 ~ -2,
      educ19_raw == 16 ~ 0,
      educ19_raw == -94 ~ -2,
      educ19_raw == -91 ~ -1,
      TRUE ~ -3
    ),
    
    # Sweep 7
    educ20 = case_when(
      educ20_raw %in% 10:13 ~ 2,
      educ20_raw %in% 3:9 ~ 1,
      educ20_raw %in% 1:2 ~ 1,
      educ20_raw == 14 ~ -2,
      educ20_raw == -94 ~ -2,
      educ20_raw == -91 ~ 0,
      TRUE ~ -3
    ),
    
    # Sweep 8
    educ25 = case_when(
      W8ACQU0A == 1 | W8ACQU0B == 1 | W8ACQU0C == 1 |
        W8ACQU0D == 1 | W8ACQU0E == 1 |
        W8VCQU0K == 1 | W8VCQU0L == 1 | W8VCQU0M == 1 ~ 2,
     W8ACQU0F == 1 | W8ACQU0I == 1 | W8ACQU0L == 1 |
        W8VCQU0I == 1 | W8VCQU0J == 1 |
        W8VCQU0E == 1 | W8VCQU0G == 1 | W8VCQU0H == 1 ~ 1,
     W8ACQU0O == 1 | W8VCQU0P == 1 ~ 0,
     W8ACQU0Q == 1 | W8VCQU0R == 1 ~ -9,
     W8ACQU0P == 1 | W8VCQU0Q == 1 ~ -8,
     TRUE ~ -3
    ),
    
    # Sweep 9
    educ32 = case_when(
      W9ACQUCHK == 1 | W9ACQU0A == 1 | W9ACQU0B == 1 | W9ACQU0C == 1 |
        W9ACQU0D == 1 | W9ACQU0E == 1 | W9ACQU0F == 1 |
        W9VCQU0A == 1 | W9VCQU0B == 1 | W9VCQU0C == 1 |
        W9VCQU0S == 1 | W9VCQU0V == 1 | W9VCQUAC == 1 ~ 2,
      W9ACQU0G == 1 | W9ACQU0H == 1 | W9ACQU0I == 1 | W9ACQU0J == 1 |
        W9ACQU0K == 1 | W9ACQU0L == 1 | W9ACQU0M == 1 |
        W9ACQU0O == 1 | W9ACQU0P == 1 | W9ACQU0Q == 1 |
        W9VCQU0D == 1 | W9VCQU0G == 1 | W9VCQU0I == 1 |
        W9VCQU0L == 1 | W9VCQU0O == 1 | W9VCQU0R == 1 |
        W9VCQU0W == 1 | W9VCQU0Y == 1 | W9VCQUAD == 1 |
        W9VCQU0E == 1 | W9VCQU0H == 1 | W9VCQU0J == 1 |
        W9VCQU0M == 1 | W9VCQU0P == 1 | W9VCQU0T == 1 |
        W9VCQU0X == 1 | W9VCQU0Z == 1 | W9VCQUAA == 1 |
        W9VCQU0F == 1 | W9VCQU0K == 1 | W9VCQU0N == 1 |
        W9VCQU0Q == 1 | W9VCQU0U == 1 | W9VCQUAB == 1 | W9VCQUAE == 1 ~ 1,
      W9ACQU0N == 1 | W9ACQU0S == 1 | W9VCQUAG == 1 ~ 0,
      W9ACQU0T == 1 | W9VCQUAH == 1 ~ -8,
      W9ACQU0U == 1 | W9VCQUAI == 1 ~ -9,
      TRUE ~ -3
    )
  ) %>%
  select(ID, educ17, educ19, educ20, educ25, educ32)


#### education parents ####
# Step 1: Load and rename relevant variables from each sweep
parent_edu_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1familybackground)) %>%
    select(ID = NSID, educma_S1 = W1hiqualmum, educpa_S1 = W1hiqualdad),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
    select(ID = NSID, educma_S2 = W2hiqualmum, educpa_S2 = W2hiqualdad),
  S4 = read_dta(file.path(data_path, sweeps$S4familybackground)) %>%
    select(ID = NSID, educma_S4 = w4hiqualmum, educpa_S4 = w4hiqualdad)
)

# Step 2: Merge all sweeps by ID
parent_edu_all <- reduce(parent_edu_vars, full_join, by = "ID")

# Step 3: Harmonise negative codes for all parental education vars
parent_edu_all <- parent_edu_all %>%
  mutate(across(
    matches("educ(ma|pa)_S[1-4]"),
    ~ case_when(
      .x == -92 ~ -9,
      .x == -91 ~ -1,
      .x %in% c(-98) ~ -3,
      .x %in% c(-999, -99, -94, 19) ~ -2,
      TRUE ~ .x
    )
  ))

# Step 4: Derive full education/simple education
parent_edu_all <- parent_edu_all %>%
  mutate(
    #mother full education
    educma = case_when(
      !is.na(educma_S4) & educma_S4 > 0 ~ educma_S4,
      !is.na(educma_S2) & educma_S2 > 0 ~ educma_S2,
      !is.na(educma_S1) & educma_S1 > 0 ~ educma_S1,
      !is.na(educma_S4) & educma_S4 < 0 ~ educma_S4,
      !is.na(educma_S2) & educma_S2 < 0 ~ educma_S2,
      !is.na(educma_S1) & educma_S1 < 0 ~ educma_S1,
      TRUE ~ -3  # Not interviewed / present
    ), 
    #mother simple education
    educma_simp = case_when(
      educma %in% 1:4 ~ 2,
      educma %in% 5:17 ~ 1,
      educma == 18 ~ 0,
      educma == 20 ~ 0,
      TRUE ~ educma  # keep negatives as-is
    ),
    #father full education
    educpa = case_when(
      !is.na(educpa_S1) & educpa_S1 > 0 ~ educpa_S1,
      !is.na(educpa_S2) & educpa_S2 > 0 ~ educpa_S2,
      !is.na(educpa_S4) & educpa_S4 > 0 ~ educpa_S4,
      !is.na(educpa_S1) & educpa_S1 < 0 ~ educpa_S1,
      !is.na(educpa_S2) & educpa_S2 < 0 ~ educpa_S2,
      !is.na(educpa_S4) & educpa_S4 < 0 ~ educpa_S4,
      TRUE ~ -3
    ),
    #father simple education
    educpa_simp = case_when(
      educpa %in% 1:4 ~ 2,
      educpa %in% 5:17 ~ 1,
      educpa == 18 ~ 0,
      educpa == 20 ~ 0,
      TRUE ~ educpa  # keep negatives as-is
    )
  )%>%
  select(ID, educma, educma_simp, educpa, educpa_simp)


#### economic activity ####
ecoact_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(ID = NSID, ecoact_S4 = W4empsYP),
  S5 = read_dta(file.path(data_path, sweeps$S5youngperson)) %>%
    select(ID = NSID, ecoact_S5 = W5mainactYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>%
    select(ID = NSID, ecoact_S6 = W6TCurrentAct),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(ID = NSID, ecoact_S7 = W7TCurrentAct),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(ID = NSID, ecoact_S8 = W8DACTIVITYC),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(ID = NSID, ecoact_S9 = W9DACTIVITYC)
)

# Merge by ID
ecoact_all <- reduce(ecoact_vars, full_join, by = "ID")

# Harmonise missing values and derive economic activity variables
ecoact_all <- ecoact_all %>%
  mutate(
    ## Sweep 4
    ecoact17 = case_when(
      ecoact_S4 %in% 1:2 ~ 1,
      ecoact_S4 == 4 ~ 2,
      ecoact_S4 == 5 | ecoact_S4 == -91 ~ 3,
      ecoact_S4 == 3 ~ 4,
      ecoact_S4 == 6 ~ 5,
      ecoact_S4 %in% c(7, 8, 9) ~ 6,
      ecoact_S4 == -92 ~ -9,
      ecoact_S4 == -999 ~ -2,
      ecoact_S4 == -94 ~ -8,
      TRUE ~ -3
    ),
    
    ## Sweep 5
    ecoact18 = case_when(
      ecoact_S5 == 3 ~ 1,
      ecoact_S5 %in% c(1, 5, 6) ~ 2,
      ecoact_S5 %in% c(2, 4) ~ 3,
      ecoact_S5 == 7 ~ 4,
      ecoact_S5 == 8 ~ 5,
      ecoact_S5 %in% 9:11 ~ 6,
      ecoact_S5 == -94 ~ -8,
      TRUE ~ -3
    ),
    
    ## Sweep 6
    ecoact19 = case_when(
      ecoact_S6 == 3 ~ 1,
      ecoact_S6 %in% c(4, 5) ~ 2,
      ecoact_S6 %in% c(1, 2, 10) ~ 3,
      ecoact_S6 == 8 ~ 4,
      ecoact_S6 == 7 ~ 5,
      ecoact_S6 %in% c(6, 9, 11) ~ 6,
      ecoact_S6 == -91 ~ -8,
      TRUE ~ -3
    ),
    
    ## Sweep 7
    ecoact20 = case_when(
      ecoact_S7 == 3 ~ 1,
      ecoact_S7 %in% c(4, 5, 11) ~ 2,
      ecoact_S7 %in% c(1, 2, 9) ~ 3,
      ecoact_S7 == 8 ~ 4,
      ecoact_S7 == 7 ~ 5,
      ecoact_S7 %in% c(6, 10, 12:15) ~ 6,
      ecoact_S7 == -91 ~ -1,
      TRUE ~ -3
    ),
    
    ## Sweep 8
    ecoact25 = case_when(
      ecoact_S8 %in% c(1, 2) ~ 1,
      ecoact_S8 %in% c(6, 7) ~ 2,
      ecoact_S8 == 5 ~ 3,
      ecoact_S8 == 4 ~ 4,
      ecoact_S8 == 9 ~ 5,
      ecoact_S8 %in% c(3, 8, 10) ~ 6,
      ecoact_S8 == -9 ~ -9,
      ecoact_S8 == -8 ~ -8,
      ecoact_S8 == -1 ~ -1,
      TRUE ~ -3
    ),
    
    ## Sweep 9
    ecoact32 = case_when(
      ecoact_S9 %in% c(1, 2) ~ 1,
      ecoact_S9 %in% c(6, 7) ~ 2,
      ecoact_S9 == 5 ~ 3,
      ecoact_S9 == 4 ~ 4,
      ecoact_S9 == 9 ~ 5,
      ecoact_S9 %in% c(3, 8, 10) ~ 6,
      ecoact_S9 == -9 ~ -9,
      ecoact_S9 == -8 ~ -8,
      ecoact_S9 == -1 ~ -1,
      TRUE ~ -3
    ),
    
    ## Detailed versions (S8, S9 only)
    ecoactadu25 = case_when(
      !is.na(ecoact_S8) ~ ecoact_S8,
      is.na(ecoact_S8) ~ -3
    ),
    ecoactadu32 = case_when(
      !is.na(ecoact_S9) ~ ecoact_S9,
      is.na(ecoact_S9) ~ -3
    )
  ) %>%
  select(ID, ecoact17, ecoact18, ecoact19, ecoact20,
         ecoact25, ecoact32, ecoactadu25, ecoactadu32)

#### economic activity parents ####
# Load & select parental employment variables for Sweeps 1–4
ecoactDT_parents_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1familybackground)) %>%
    select(ID = NSID, ecoma_S1 = W1empsmum, ecopa_S1 = W1empsdad),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
    select(ID = NSID, ecoma_S2 = W2empsmum, ecopa_S2 = W2empsdad),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>%
    select(ID = NSID, ecoma_S3 = W3empsmum, ecopa_S3 = W3empsdad),
  S4 = read_dta(file.path(data_path, sweeps$S4familybackground)) %>%
    select(ID = NSID, ecoma_S4 = w4empsmum, ecopa_S4 = w4empsdad)
)

# Merge all
ecoactDT_parents_all <- reduce(ecoactDT_parents_vars, full_join, by = "ID")

# Recode helper function
recode_detailed <- function(x) {
  case_when(
    x == 1 ~ 1,  # FT
    x == 2 ~ 2,  # PT
    x == 3 ~ 3,  # Unemployed
    x == 4 ~ 4,  # Training
    x == 5 ~ 5,  # Education
    x == 6 ~ 6,  # Home
    x == 7 ~ 7,  # Retired
    x == 8 ~ 8,  # Sick/disabled
    x == 9 ~ 9,  # Other
    x == -94 ~ -8,
    x == -999 ~ -2,
    x %in% c(-98, -99) ~ -3,
    is.na(x) ~ -3,
    TRUE ~ NA_real_
  )
}

# Apply recode to each sweep
ecoactDT_parents_all <- ecoactDT_parents_all %>%
  mutate(
    ecoactdtma14 = recode_detailed(ecoma_S1),
    ecoactdtpa14 = recode_detailed(ecopa_S1),
    ecoactdtma15 = recode_detailed(ecoma_S2),
    ecoactdtpa15 = recode_detailed(ecopa_S2),
    ecoactdtma16 = recode_detailed(ecoma_S3),
    ecoactdtpa16 = recode_detailed(ecopa_S3),
    ecoactdtma17 = recode_detailed(ecoma_S4),
    ecoactdtpa17 = recode_detailed(ecopa_S4)
  )%>%
  select(ID, starts_with("ecoactdtma"), starts_with("ecoactdtpa"))

#### NS-SEC own ####
# Load NS-SEC variables from relevant sweeps
nssec_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(ID = NSID, nssec_S4 = W4nsseccatYP),
  S5 = read_dta(file.path(data_path, sweeps$S5youngperson)) %>%
    select(ID = NSID, nssec_S5 = W5nsseccatYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>%
    select(ID = NSID, nssec_S6 = w6nsseccatYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(ID = NSID, nssec_S7 = W7NSSECCat),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(ID = NSID, nssec_S8 = W8DNSSEC17),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(ID = NSID, nssec_S9 = W9NSSEC)
)

# Merge all NS-SEC variables by ID
nssec_all <- reduce(nssec_vars, full_join, by = "ID")

# Harmonise NS-SEC values and derive categories
nssec_all <- nssec_all %>%
  mutate(
    ## Sweep 4 (age 17)
    nsseccat17 = case_when(
      is.na(nssec_S4) ~ -3,
      floor(nssec_S4) %in% 1:17 ~ floor(nssec_S4),
      nssec_S4 == -91 ~ -1,
      nssec_S4 == -99 ~ -3,
      TRUE ~ -3
    ),
    
    ## Sweep 5 (age 18)
    nsseccat18 = case_when(
      is.na(nssec_S5) ~ -3,
      floor(nssec_S5) %in% 1:17 ~ floor(nssec_S5),
      nssec_S5 == -91 ~ -1,
      nssec_S5 == -99 ~ -3,
      TRUE ~ -3
    ),
    
    ## Sweep 6 (age 19)
    nsseccat19 = case_when(
      is.na(nssec_S6) ~ -3,
      floor(nssec_S6) %in% 1:17 ~ floor(nssec_S6),
      nssec_S6 == -91 ~ -1,
      nssec_S6 == -99 ~ -3,
      TRUE ~ -3
    ),
    
    ## Sweep 7 (age 20)
    nsseccat20 = case_when(
      is.na(nssec_S7) ~ -3,
      floor(nssec_S7) %in% 1:17 ~ floor(nssec_S7),
      nssec_S7 == -91 ~ -1,
      nssec_S7 == -99 ~ -3,
      TRUE ~ -3
    ),
    
    ## Sweep 8 (age 25)
    nsseccat25 = case_when(
      is.na(nssec_S8) ~ -3,
      floor(nssec_S8) %in% 1:14 ~ floor(nssec_S8),
      nssec_S8 == -1 ~ -8,
      TRUE ~ -3
    ),
    
    ## Sweep 9 (age 32)
    nsseccat32 = case_when(
      is.na(nssec_S9) ~ -3,
      nssec_S9 %in% 1:17 ~ nssec_S9,
      nssec_S9 == -9 ~ -9,
      nssec_S9 == -8 ~ -8,
      nssec_S9 == -1 ~ -1,
      TRUE ~ NA_real_
    )
  ) %>%
  select(ID, nsseccat17, nsseccat18, nsseccat19, nsseccat20,
         nsseccat25, nsseccat32)

#### NS-SEC parents ####
# Load and select parental NS-SEC variables from Sweeps 1–5
nssec_parents_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1familybackground)) %>%
    select(ID = NSID, nssecma_S1 = W1nsseccatmum, nssecda_S1 = W1nsseccatdad),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
    select(ID = NSID, nssecma_S2 = W2nsseccatmum, nssecda_S2 = W2nsseccatdad),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>%
    select(ID = NSID, nssecma_S3 = W3cnsseccatmum, nssecda_S3 = W3cnsseccatdad),
  S4 = read_dta(file.path(data_path, sweeps$S4familybackground)) %>%
    select(ID = NSID, nssecma_S4 = w4cnsseccatmum, nssecda_S4 = w4cnsseccatdad),
  S5 = read_dta(file.path(data_path, sweeps$S5familybackground)) %>%
    select(ID = NSID, nssecma_S5 = w5Cnsseccatmum, nssecda_S5 = w5Cnsseccatdad)
)

# Merge all parental NS-SEC variables by ID
nssec_parents_all <- reduce(nssec_parents_vars, full_join, by = "ID")

# Harmonise values (preserve decimals, apply missing codes)
recode_nssec_detail <- function(x) {
  case_when(
    floor(x) %in% 1:17 ~ floor(x)
    x %in% c(-999, -94) ~ -2,
    x %in% c(-99, -98) | is.na(x) ~ -3,
    TRUE ~ x
  )
}

# Apply recode and assign to derived variables
nssec_parents_all <- nssec_parents_all %>%
  mutate(
    nsseccatma14 = recode_nssec_detail(nssecma_S1),
    nsseccatpa14 = recode_nssec_detail(nssecda_S1),
    nsseccatma15 = recode_nssec_detail(nssecma_S2),
    nsseccatpa15 = recode_nssec_detail(nssecda_S2),
    nsseccatma16 = recode_nssec_detail(nssecma_S3),
    nsseccatpa16 = recode_nssec_detail(nssecda_S3),
    nsseccatma17 = recode_nssec_detail(nssecma_S4),
    nsseccatpa17 = recode_nssec_detail(nssecda_S4),
    nsseccatma18 = recode_nssec_detail(nssecma_S5),
    nsseccatpa18 = recode_nssec_detail(nssecda_S5)
  ) %>%
  select(ID, starts_with("nsseccat"), starts_with("nsseccatdad"))


#### income own ####
# Load and select income variables from relevant sweeps
income_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID), 
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(ID = NSID, income_S8 = W8DINCB),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(ID = NSID, income_S9 = W9DINCB)
)

# Merge all income variables by ID
income_all <- reduce(income_vars, full_join, by = "ID")

# Recode
income_all <- income_all %>%
  mutate(
    inc25 = case_when(
      is.na(income_S8) ~ -3,
      TRUE ~ income_S8
    ),
    inc32 = case_when(
      is.na(income_S9) ~ -3,
      TRUE ~ income_S9
    )
  ) %>%
  select(ID, inc25, inc32)

#### income parents ####
# Load and select household income variables
hh_income_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1familybackground)) %>%
    select(ID = NSID, income_S1 = W1GrsswkHH),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
    select(ID = NSID, income_S2 = W2GrsswkHH),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>%
    select(ID = NSID, income_S3 = W3incestw),
  S4 = read_dta(file.path(data_path, sweeps$S4familybackground)) %>%
    select(ID = NSID, income_S4 = w4IncEstW)
)

# Merge all household income variables by ID
hh_income_all <- reduce(hh_income_vars, full_join, by = "ID")

# Derive banded income for continuous measures (S1–S2)
convert_to_band <- function(x) {
  case_when(
    x < 0 ~ x,
    x < 50 ~ 1,
    x < 100 ~ 2,
    x < 200 ~ 3,
    x < 300 ~ 4,
    x < 400 ~ 5,
    x < 500 ~ 6,
    x < 600 ~ 7,
    x < 700 ~ 8,
    x < 800 ~ 9,
    x < 900 ~ 10,
    x < 1000 ~ 11,
    x >= 1000 ~ 12,
    TRUE ~ -3
  )
}

hh_income_all <- hh_income_all %>%
  mutate(
    # Sweep 1
    incwhh14 = case_when(
      is.na(income_S1) ~ -3,
      income_S1 == -92 ~ -9,
      income_S1 %in% c(-999, -992, -94) ~ -2,
      income_S1 == -99 ~ -3,
      income_S1 == -1 ~ -1,
      income_S1 == -3 ~ -3,
      TRUE ~ convert_to_band(income_S1)
    ),
    
    # Sweep 2
    incwhh15 = case_when(
      is.na(income_S2) ~ -3,
      income_S2 == -92 ~ -9,
      income_S2 %in% c(-999, -992, -94) ~ -2,
      income_S2 == -99 ~ -3,
      income_S2 == -1 ~ -1,
      income_S2 == -3 ~ -3,
      TRUE ~ convert_to_band(income_S2)
    ),
    
    # Sweep 3
    incwhh16 = case_when(
      is.na(income_S3) ~ -3,
      income_S3 == -99 ~ -3,
      income_S3 == -92 ~ -9,
      income_S3 == -1 ~ -1,
      income_S3 >= 1 & income_S3 <= 12 ~ income_S3,
      TRUE ~ -3
    ),
    
    # Sweep 4
    incwhh17 = case_when(
      is.na(income_S4) ~ -3,
      income_S4 == -99 ~ -3,
      income_S4 == -92 ~ -9,
      income_S4 == -1 ~ -1,
      income_S4 >= 1 & income_S4 <= 12 ~ income_S4,
      TRUE ~ -3
    )
  ) %>%
  select(ID, incwhh14, incwhh15, incwhh16, incwhh17)

#### IMD ####
# Load IMD variables from relevant sweeps
imd_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%select(ID = NSID),
  S2 = read_dta(file.path(data_path, sweeps$S2familybackground)) %>%
      select(ID = NSID, imd_S2 = IMDRSCORE),
  S3 = read_dta(file.path(data_path, sweeps$S3familybackground)) %>%
      select(ID = NSID, imd_S3 = IMDRSCORE),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
      select(ID = NSID, imd_S9 = W9DIMDD)
  )

# Merge all IMD variables by ID  
imd_all <- reduce(imd_vars, full_join, by = "ID")
  
# Recode derived variables
imd_all <- imd_all %>%
    mutate(
      imd15 = case_when(
        is.na(imd_S2) ~ -3,
        imd_S2 == -94 ~ -8,
        TRUE ~ imd_S2
      ),
      
      imd16 = case_when(
        is.na(imd_S3) ~ -3,
        imd_S3 == -94 ~ -8,
        TRUE ~ imd_S3
      ),
      
      imd32 = case_when(
        is.na(imd_S9) ~ -3,
        imd_S9 == -8 ~ -8,
        TRUE ~ imd_S9
      )
    ) %>%
  select(ID, imd15, imd16, imd32)

#### GHQ ####
# Load GHQ-12 derived score and item-level data
ghq_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(ID = NSID, ghq15 = W2ghq12scr,
           paste0("W2", c("concenYP", "nosleepYP", "usefulYP", "decideYP", "strainYP", 
                          "difficYP", "activYP", "probsYP", "depressYP", "noconfYP", 
                          "wthlessYP", "happyYP"))),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(ID = NSID, ghq17 = W4ghq12scr,
           paste0("W4", c("ConcenYP", "NoSleepYP", "UsefulYP", "DecideYP", "StrainYP", 
                          "DifficYP", "ActivYP", "ProbsYP", "DepressYP", "NoConfYP", 
                          "WthlessYP", "HappyYP"))),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>%
    select(ID = NSID, starts_with("W8GHQ12_")),
  S8_derive = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(ID = NSID, ghq25 = W8DGHQSC),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(ID = NSID, starts_with("W9GHQ12_")),
  S9_derive = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(ID = NSID, ghq32 = W9DGHQSC)
)

# Merge all sweeps by ID
ghq_all <- reduce(ghq_vars, full_join, by = "ID")

# Define item lists for sum scores
ghq_items <- list(
  ghqtl15 = paste0("W2", c("concenYP", "nosleepYP", "usefulYP", "decideYP", "strainYP", 
                           "difficYP", "activYP", "probsYP", "depressYP", "noconfYP", 
                           "wthlessYP", "happyYP")),
  ghqtl17 = paste0("W4", c("ConcenYP", "NoSleepYP", "UsefulYP", "DecideYP", "StrainYP", 
                           "DifficYP", "ActivYP", "ProbsYP", "DepressYP", "NoConfYP", 
                           "WthlessYP", "HappyYP")),
  ghqtl25 = paste0("W8GHQ12_", 1:12),
  ghqtl32 = paste0("W9GHQ12_", 1:12)
)

# Derive GHQ sum scores (0–12) with custom missing logic
ghq_all <- ghq_all %>%
  mutate(
    ghqtl15 = {
      items <- select(., all_of(ghq_items$ghqtl15))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE))
      all_na <- apply(items, 1, function(x) all(is.na(x)))
      score <- rowSums(items, na.rm = TRUE)
      
      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    },
    
    ghqtl17 = {
      items <- select(., all_of(ghq_items$ghqtl17))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE))
      all_na <- apply(items, 1, function(x) all(is.na(x)))
      score <- rowSums(items, na.rm = TRUE)
      
      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    },
    
    ghqtl25 = {
      items <- select(., all_of(ghq_items$ghqtl25))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE))
      all_na <- apply(items, 1, function(x) all(is.na(x)))
      score <- rowSums(items, na.rm = TRUE)
      
      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    },
    
    ghqtl32 = {
      items <- select(., all_of(ghq_items$ghqtl32))
      has_negative <- apply(items, 1, function(x) any(x < 0, na.rm = TRUE))
      all_na <- apply(items, 1, function(x) all(is.na(x)))
      score <- rowSums(items, na.rm = TRUE)
      
      case_when(
        all_na ~ -3,
        has_negative ~ -8,
        TRUE ~ score
      )
    }
  ) %>%
  mutate(
    ghq15 = case_when(
      is.na(ghq15) ~ -3,
      ghq15 %in% c(-97, -96, -92) ~ -9,
      TRUE ~ ghq15
    ),
    ghq17 = case_when(
      is.na(ghq17) ~ -3,
      ghq17 %in% c(-97, -96, -92) ~ -9,
      TRUE ~ ghq17
    ),
    ghq25 = if_else(is.na(ghq25), -3, ghq25),
    ghq32 = if_else(is.na(ghq32), -3, ghq32)
  )%>%
  select(ID, ghq15, ghq17, ghq25, ghq32,
         ghqtl15, ghqtl17, ghqtl25, ghqtl32)


#### life satisfaction ####
# Load life satisfaction variables from each sweep
lsat_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% select(ID = NSID),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(ID = NSID, lsat20_raw = W7OSatisYP),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>%
    select(ID = NSID, lsat25_raw = W8OSATIS),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(ID = NSID, lsat32_raw = W9OSATIS)
)

# Merge into a single dataset
lsat_all <- reduce(lsat_vars, full_join, by = "ID")

# Harmonise life satisfaction variables
lsat_all <- lsat_all %>%
  mutate(
    lsat20 = case_when(
      lsat20_raw %in% 1:5 ~ lsat20_raw,
      lsat20_raw %in% c(-97, -92) ~ -9,
      lsat20_raw == -91 ~ -1,
      lsat20_raw == -1 ~ -8,
      is.na(lsat20_raw) ~ -3,
      TRUE ~ -2
    ),
    lsat25 = case_when(
      lsat25_raw %in% 1:5 ~ lsat25_raw,
      lsat25_raw == -9 ~ -9,
      lsat25_raw == -8 ~ -8,
      lsat25_raw == -1 ~ -1,
      is.na(lsat25_raw) ~ -3,
      TRUE ~ -2
    ),
    lsat32 = case_when(
      lsat32_raw %in% 1:5 ~ lsat32_raw,
      lsat32_raw == -9 ~ -9,
      lsat32_raw == -8 ~ -8,
      lsat32_raw == -1 ~ -1,
      is.na(lsat32_raw) ~ -3,
      TRUE ~ -2
    )
  ) %>%
  select(ID, lsat20, lsat25, lsat32)

#### weight ####
# Load weight variables from each sweep
wt_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4history)) %>%
    select(ID = NSID, wt0_raw = W4BirthWbHS),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>%
    select(ID = NSID, wt25_raw = W8WEIGHT),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(ID = NSID, wt32_raw = W9WEIGHT)
)

# Merge datasets
wt_all <- reduce(wt_vars, full_join, by = "ID")

# Harmonise weight variables
wt_all <- wt_all %>%
  mutate(
    wt0 = case_when(
      wt0_raw > 0 ~ wt0_raw,
      wt0_raw == -92 ~ -9,
      wt0_raw == -91 ~ -1,
      wt0_raw == -1 ~ -8,
      wt0_raw %in% c(-996, -99) ~ -3,
      is.na(wt0_raw) ~ -3,
      TRUE ~ -2
    ),
    wt25 = case_when(
      wt25_raw > 0 ~ wt25_raw,
      wt25_raw == -9 ~ -9,
      wt25_raw == -8 ~ -8,
      wt25_raw == -1 ~ -1,
      is.na(wt25_raw) ~ -3,
      TRUE ~ -2
    ),
    wt32 = case_when(
      wt32_raw > 0 ~ wt32_raw,
      wt32_raw == -9 ~ -9,
      wt32_raw == -8 ~ -8,
      wt32_raw == -1 ~ -1,
      is.na(wt32_raw) ~ -3,
      TRUE ~ -2
    )
  ) %>%
  select(ID, wt0, wt25, wt32)

#### height ####
# Load height data from sweeps 8 and 9
ht_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% select(ID = NSID),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>%
    select(ID = NSID, ht25_raw = W8HEIGHT),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(ID = NSID, ht32_raw = W9HEIGHT)
)

# Merge datasets
ht_all <- reduce(ht_vars, full_join, by = "ID")

# Recode height
ht_all <- ht_all %>%
  mutate(
    ht25 = case_when(
      ht25_raw > 0 ~ ht25_raw,
      ht25_raw == -9 ~ -9,
      ht25_raw == -8 ~ -8,
      ht25_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    ht32 = case_when(
      ht32_raw > 0 ~ ht32_raw,
      ht32_raw == -9 ~ -9,
      ht32_raw == -8 ~ -8,
      ht32_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    ht25_32 = case_when(
      ht32 > 0 ~ ht32,
      ht25 > 0 ~ ht25,
      ht32 %in% c(-9, -8, -1) ~ ht32,
      ht25 %in% c(-9, -8, -1) ~ ht25, 
      TRUE ~ -3  
    )
  ) %>%
  select(ID, ht25, ht32, ht25_32)

#### BMI ####
# Load BMI data from relevant sweeps
bmi_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% select(ID = NSID),
  S8 = read_dta(file.path(data_path, sweeps$S8derivedvariable)) %>%
    select(ID = NSID, bmi25_raw = W8DBMI),
  S9 = read_dta(file.path(data_path, sweeps$S9derivedvariable)) %>%
    select(ID = NSID, bmi32_raw = W9DBMI)
)

# Merge all BMI data by ID
bmi_all <- reduce(bmi_vars, full_join, by = "ID")

# Recode BMI variables
bmi_all <- bmi_all %>%
  mutate(
    bmi25 = case_when(
      bmi25_raw > 0 ~ bmi25_raw,
      bmi25_raw == -9 ~ -9,
      bmi25_raw == -8 ~ -8,
      bmi25_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    bmi32 = case_when(
      bmi32_raw > 0 ~ bmi32_raw,
      bmi32_raw == -9 ~ -9,
      bmi32_raw == -8 ~ -8,
      bmi32_raw == -1 ~ -1,
      TRUE ~ -3
    )
  ) %>%
  select(ID, bmi25, bmi32)

#### self-rated general health ####
# Load relevant sweep files and select needed variables
health_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% select(ID = NSID),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(ID = NSID, ghea15_raw = W2hea1cYP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>% 
    select(ID = NSID, ghea16_raw = W3hea1cYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(ID = NSID, ghea17_raw = W4Hea1CYP),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>% 
    select(ID = NSID, ghea25_raw = W8GENA),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% 
    select(ID = NSID, ghea32_raw = W9HLTHGEN)
)

# Merge all data by ID
health_all <- reduce(health_vars, full_join, by = "ID")

# Harmonise adolescent responses (S2–S4): gheateenXX
health_all <- health_all %>%
  mutate(
    gheateen15 = case_when(
      ghea15_raw %in% c(1, 2, 3, 4) ~ ghea15_raw,
      ghea15_raw %in% c(-998, -997, -995, -99, -94) ~ -2,
      ghea15_raw %in% c(-97, -96, -92, -1) ~ -8,
      ghea15_raw == -91 ~ -1,
      TRUE ~ -3
      ),  
    gheateen16 = case_when(
      ghea16_raw %in% c(1, 2, 3, 4) ~ ghea16_raw,
      ghea16_raw %in% c(-998, -997, -995, -99, -94) ~ -2,
      ghea16_raw %in% c(-97, -96, -92, -1) ~ -8,
      ghea16_raw == -91 ~ -1,
      TRUE ~ -3
    ),
    gheateen17 = case_when(
      ghea17_raw %in% c(1, 2, 3, 4) ~ ghea17_raw,
      ghea17_raw %in% c(-998, -997, -995, -99, -94) ~ -2,
      ghea17_raw %in% c(-97, -96, -92, -1) ~ -8,
      ghea17_raw == -91 ~ -1,
      TRUE ~ -3
    )
  )

# Harmonise adult responses (S8–S9): gheaaduXX
health_all <- health_all %>%
  mutate(
    gheaadu25 = case_when(
      ghea25_raw %in% c(1, 2, 3, 4, 5) ~ ghea25_raw,
      ghea25_raw == -8 ~ -8,
      ghea25_raw == -1 ~ -1,
      ghea25_raw == -9 ~ -9,
      TRUE ~ -3
    ),
    gheaadu32 = case_when(
      ghea32_raw %in% c(1, 2, 3, 4, 5) ~ ghea32_raw,
      ghea32_raw == -8 ~ -8,
      ghea32_raw == -1 ~ -1,
      ghea32_raw == -9 ~ -9,
      TRUE ~ -3
    )
  )

# Binary general health: 1 = poor/fair, 0 = good/excellent
health_all <- health_all %>%
  mutate(
    ghea15 = case_when(gheateen15 %in% c(3, 4) ~ 1,
                       gheateen15 %in% c(1, 2) ~ 0,
                       TRUE ~ gheateen15),
    ghea16 = case_when(gheateen16 %in% c(3, 4) ~ 1,
                       gheateen16 %in% c(1, 2) ~ 0,
                       TRUE ~ gheateen16),
    ghea17 = case_when(gheateen17 %in% c(3, 4) ~ 1,
                       gheateen17 %in% c(1, 2) ~ 0,
                       TRUE ~ gheateen17),
    ghea25 = case_when(gheaadu25 %in% c(4, 5) ~ 1,
                       gheaadu25 %in% c(1, 2, 3) ~ 0,
                       TRUE ~ gheaadu25),
    ghea32 = case_when(gheaadu32 %in% c(4, 5) ~ 1,
                       gheaadu32 %in% c(1, 2, 3) ~ 0,
                       TRUE ~ gheaadu32)
  ) %>%
  select(ID, gheateen15, gheateen16, gheateen17,
         gheaadu25, gheaadu32,
         ghea15, ghea16, ghea17, ghea25, ghea32)

#### long-term illness ####
# Load relevant sweep files and select needed variables
long_term_illness_files <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(ID = NSID, lsi14_raw = W1chea1HS),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(ID = NSID, lsi15_raw = W2chea1HS),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(ID = NSID, lsi17_raw = W4Hea2YP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% 
    select(ID = NSID, lsi19_raw = W6HealthYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>% 
    select(ID = NSID, lsi20_raw = W7HealthYP),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>% 
    select(ID = NSID, lsi25_raw = W8LOIL),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% 
    select(ID = NSID, lsi32_raw = W9LOIL)
)

# Merge all data
lsi_all <- reduce(long_term_illness_files, full_join, by = "ID")

# Harmonise values
recode_lsi <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    x %in% c(-92, -9) ~ -9,
    x == -1 ~ -1,
    x %in% c(-998, -997, -995, -99, -98, -97) ~ -2,
    x %in% c(-91, -8) ~ -8,
    TRUE ~ -3  # Not present/interviewed or NA
  )
}

lsi_all <- lsi_all %>%
  mutate(
    lsi14_15 = case_when(
      !is.na(lsi14_raw) ~ recode_lsi(lsi14_raw),
      !is.na(lsi15_raw) ~ recode_lsi(lsi15_raw),
      TRUE ~ -3
    ),
    lsi17 = recode_lsi(lsi17_raw),
    lsi19 = recode_lsi(lsi19_raw),
    lsi20 = recode_lsi(lsi20_raw),
    lsi25 = recode_lsi(lsi25_raw),
    lsi32 = recode_lsi(lsi32_raw)
  ) %>%
  select(ID, lsi14_15, lsi17, lsi19, lsi20, lsi25, lsi32)

#### smoke ####
# Load smoking data from relevant sweeps
smoking_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(ID = NSID, smknw14_raw = W1cignowYP, smk14_raw = W1cigfreqYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(ID = NSID, smknw15_raw = W2cignowYP, smk15_raw = W2cigfreqYP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>%
    select(ID = NSID, smknw16_raw = W3cignowYP, smk16_raw = W3cigfreqYP),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>%
    select(ID = NSID, smk25_raw = W8SMOKING),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(ID = NSID, smk32_raw = W9SMOKING)
)

# Merge all sweeps
smoking_all <- reduce(smoking_vars, full_join, by = "ID")

# Recode smoke ever/frequency
recode_smk14_16 <- function(x) {
  case_when(
    x %in% c(1, 2, -91) ~ 0,
    x == 3 ~ 1,
    x %in% c(4, 5) ~ 2,
    x == 6 ~ 3,
    x %in% c(-99, -97, -96) ~ -2,
    x == -92 ~ -9,
    x == -1 ~ -8,
    TRUE ~ -3
  )
}

recode_smk25_32 <- function(x) {
  case_when(
    x > 0 ~ x - 1, # Convert 1-4 to 0-3
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    TRUE ~ -3
  )
}

# Recode smoke now
recode_smknw14_16 <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    x %in% c(-99, -97, -96) ~ -2,
    x == -92 ~ -9,
    x == -91 ~ -1,
    x == -1 ~ -8,
    TRUE ~ -3
  )
}

recode_smknw25_32 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ 0,
    x %in% c(3, 4) ~ 1, 
    x == -9 ~ -9,
    x == -1 ~ -1,
    x == -8 ~ -8,
    TRUE ~ -3
  )
}

# Apply recoding
smoking_all <- smoking_all %>%
  mutate(
    smk14 = recode_smk14_16(smk14_raw),
    smk15 = recode_smk14_16(smk15_raw),
    smk16 = recode_smk14_16(smk16_raw),
    smk25 = recode_smk25_32(smk25_raw),
    smk32 = recode_smk25_32(smk32_raw),
    smknw14 = recode_smknw14_16(smknw14_raw),
    smknw15 = recode_smknw14_16(smknw15_raw),
    smknw16 = recode_smknw14_16(smknw16_raw),
    smknw25 = recode_smknw25_32(smk25_raw),
    smknw32 = recode_smknw25_32(smk32_raw)
  ) %>%
  select(ID, smknw14, smknw15, smknw16, smknw25, smknw32,
         smk14, smk15, smk16, smk25, smk32)

#### alcohol ####
# Load and Select Variables
alc_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>% 
    select(ID = NSID, alcever_S1 = W1alceverYP, alcmon_S1 = W1alcmonYP, alcfreq_S1 = W1alcfreqYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>% 
    select(ID = NSID, alcever_S2 = W2alceverYP, alcfreq_S2 = W2alcfreqYP),
  S3 = read_dta(file.path(data_path, sweeps$S3youngperson)) %>% 
    select(ID = NSID, alcever_S3 = W3alceverYP, alcfreq_S3 = W3alcfreqYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>% 
    select(ID = NSID, alcever_S4 = W4AlcEverYP, alcfreq_S4 = W4AlcFreqYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>% 
    select(ID = NSID, alcever_S6 = W6AlcEverYP, alcfreq_S6 = W6AlcFreqYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>% 
    select(ID = NSID, alcever_S7 = W7AlcEverYP, alcfreq_S7 = W7AlcFreqYP),
  S8 = read_dta(file.path(data_path, sweeps$S8selfcompletion)) %>% 
    select(ID = NSID, 
           audita25 = W8AUDIT1, auditb25 = W8AUDIT2, auditc25 = W8AUDIT6),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>% 
    select(ID = NSID, 
           audita32 = W9AUDIT1, auditb32 = W9AUDIT2, auditc32 = W9AUDIT3)
)

# Merge all alcohol variables by ID
alc_all <- reduce(alc_vars, full_join, by = "ID")

# First Time Had Alcohol 
alc_all <- alc_all %>%
  rowwise() %>%
  mutate(
    ever_flags = list(c(
      ifelse(alcever_S1 == 1 & alcmon_S1 == 1, 14, NA),
      ifelse(alcever_S2 == 1, 15, NA),
      ifelse(alcever_S3 == 1, 16, NA),
      ifelse(alcever_S4 == 1, 17, NA),
      ifelse(alcever_S6 == 1, 19, NA),
      ifelse(alcever_S7 == 1, 20, NA),
      ifelse(audita25 > 1, 25, NA),
      ifelse(audita32 > 1, 32, NA)
    )),
    alcfst = case_when(
      any(ever_flags %in% 14:32, na.rm = TRUE) ~ min(unlist(ever_flags), na.rm = TRUE),
      all(c(alcever_S1, alcever_S2, alcever_S3, alcever_S4, alcever_S6, alcever_S7) == 2&c(audita25, audita32) == 1, na.rm = TRUE) ~ 99,
      TRUE ~ -8
    )
  ) %>%
  ungroup()

# Frequency Recode Across Sweeps
recode_freq <- function(x, sweep) {
  case_when(
    sweep %in% c("S1", "S2", "S3", "S4") ~ case_when(
      x == 1 ~ 4, 
      x == 2 ~ 3, 
      x == 3 ~ 2, 
      x == 4 ~ 2, 
      x == 5 ~ 1, 
      x == 6 ~ 0,
      x %in% c(-99, -97, -96) ~ -2,
      x == -92 ~ -9,
      x == -1 ~ -1,
      x == -91 ~ -1,
      TRUE ~ -3
    ),
    sweep %in% c("S6", "S7") ~ case_when(
      x %in% c(1, 2) ~ 4,
      x %in% c(3, 4) ~ 3,
      x == 5 ~ 2,
      x == 6 ~ 1,
      x %in% c(7, 8) ~ 0,
      x == -997 ~ -2,
      x == -97 ~ -9,
      x == -92 ~ -9,
      x == -91 ~ -1,
      x == -1 ~ -1,
      TRUE ~ -3
    )
  )
}

# Frequency Variables 
alc_all <- alc_all %>%
  mutate(
    alcfreq14 = recode_freq(alcfreq_S1, "S1"),
    alcfreq15 = recode_freq(alcfreq_S2, "S2"),
    alcfreq16 = recode_freq(alcfreq_S3, "S3"),
    alcfreq19 = recode_freq(alcfreq_S6, "S6"),
    alcfreq20 = recode_freq(alcfreq_S7, "S7")
  )

# AUDIT Recode 
recode_audit <- function(x) {
  case_when(
    x >= 0 & x <= 5 ~ x - 1,
    x == -9 ~ -9,
    x == -8 ~ -8,
    x == -1 ~ -1,
    TRUE ~ -3
  )
}

alc_all <- alc_all %>%
  mutate(
    audita25 = recode_audit(audita25),
    audita32 = recode_audit(audita32),
    auditb25 = recode_audit(auditb25),
    auditb32 = recode_audit(auditb32),
    auditc25 = recode_audit(auditc25),
    auditc32 = recode_audit(auditc32)
  ) %>%
  select(ID, alcfst, alcfreq14, alcfreq15, alcfreq16, alcfreq19, alcfreq20,
         audita25, audita32, auditb25, auditb32, auditc25, auditc32)

#### exercise ####
# Load relevant sweep files and select variables
exercise_vars <- list(
  S1 = read_dta(file.path(data_path, sweeps$S1youngperson)) %>%
    select(ID = NSID, spt14_raw = W1sportYP),
  S2 = read_dta(file.path(data_path, sweeps$S2youngperson)) %>%
    select(ID = NSID, spt15_raw = W2sportYP),
  S4 = read_dta(file.path(data_path, sweeps$S4youngperson)) %>%
    select(ID = NSID, spt17_raw = W4SportYP),
  S6 = read_dta(file.path(data_path, sweeps$S6youngperson)) %>%
    select(ID = NSID, spt19_raw = W6SportYP),
  S7 = read_dta(file.path(data_path, sweeps$S7youngperson)) %>%
    select(ID = NSID, spt20_raw = W7SportYP),
  S8 = read_dta(file.path(data_path, sweeps$S8maininterview)) %>%
    select(ID = NSID, spt25_raw = W8EXERCISE),
  S9 = read_dta(file.path(data_path, sweeps$S9maininterview)) %>%
    select(ID = NSID, spt32_raw = W9EXERCISEH)
)

# Merge all datasets
spt_all <- reduce(exercise_vars, full_join, by = "ID")

# Recode function
recode_exercise <- function(x) {
  case_when(
    x %in% c(1, 2, 3) ~ x - 1,      # Keep as is
    x %in% c(4, 5, 6) ~ 3,     # 4- 6 = less than once a week/hardly ever/never
    x == -92 ~ -9,          # Refused
    x %in% c(-91) ~ -1,     # Not applicable / insufficient info
    x %in% c(-99) ~ -3,     # Not interviewed
    TRUE ~ -3              # Everything else = error/lost
  )
}

# Apply recoding
spt_all <- spt_all %>%
  mutate(
    spt14 = recode_exercise(spt14_raw),
    spt15 = recode_exercise(spt15_raw),
    spt17 = recode_exercise(spt17_raw),
    spt19 = recode_exercise(spt19_raw),
    spt20 = recode_exercise(spt20_raw),  
    spt25 = case_when( # values from 0–7 days
      spt25_raw %in% c(5, 6, 7)  ~ 0,
      spt25_raw %in% c(2, 3, 4) ~ 1,
      spt25_raw == 1 ~ 2,
      spt25_raw == 0 ~ 3,
      spt25_raw == -9 ~ -9,
      spt25_raw == -8 ~ -8,
      spt25_raw == -1 ~ -1,
      TRUE ~ -3
    ),
    spt32 = case_when(
      spt32_raw %in% c(5, 6, 7)  ~ 0,
      spt32_raw %in% c(2, 3, 4) ~ 1,
      spt32_raw == 1 ~ 2,
      spt32_raw  == 0 ~ 3,
      spt32_raw == -9 ~ -9,
      spt32_raw == -8 ~ -8,
      spt32_raw == -1 ~ -1,
      TRUE ~ -3
    )
  ) %>%
  select(ID, spt14, spt15, spt17, spt19, spt20, spt25, spt32)

#### merge all datasets ####
# Merge all derived variables into a single dataset
derived_vars <- list(
  sex_all,
  eth_all,
  lang_all,
  sexuality_all,
  partnr_all,
  region_all,
  educ_all,
  parent_edu_all,
  ecoact_all,
  ecoactDT_parents_all,
  nssec_all,
  nssec_parents_all,
  income_all,
  hh_income_all,
  imd_all,
  ghq_all,
  lsat_all,
  wt_all,
  ht_all,
  bmi_all,
  health_all,
  lsi_all,
  smoking_all,
  alc_all,
  spt_all
)

# Combine all datasets by ID
derived_all <- reduce(derived_vars, full_join, by = "ID")

# set multiple labels at once; returns the modified data frame
derived_all <- set_variable_labels(
  derived_all,
  sex = "Sex (age 32-14y)",
  eth = "ethnicity (age 32-14y)",
  lang = "Language spoken at home (age 17-14y)",
  sori19 = "Sexuality (age 19y)",
  sori20 = "Sexuality (age 20y)",
  sori25 = "Sexuality (age 25y)",
  sori32 = "Sexuality (age 32y)",
  partnr18 = "Partner status simple (age 18y)",
  partnr19 = "Partner status simple (age 19y)",
  partnr25 = "Partner status simple (age 25y)",
  partnr32 = "Partner status simple (age 32y)",
  partnradu25 = "Partner status complete (age 25y)",
  partnradu32 = "Partner status complete (age 32y)",
  regub15 = "Region urban (age 15y)",
  regub16 = "Region urban (age 16y)",
  regov15 = "Region goverment area (age 15y)",
  regov16 = "Region goverment area (age 16y)",
  regor25 = "Region goverment area (age 25y)",
  regor32 = "Region goverment area (age 32y)",
  regint32 = "Region abroad (age 32y)",
  educ17 = "Highest education level 3-category (age 17y)",
  educ19 = "Highest education level 3-category (age 19y)",
  educ20 = "Highest education level 3-category (age 20y)",
  educ25 = "Highest education level 3-category (age 25y)",
  educ32 = "Highest education level 3-category (age 32y)",
  educma_simp = "Highest education level mother 3-category (age 17-14y)",
  educpa_simp = "Highest education level father 3-category (age 17-14y)",
  educma = "Highest education level mother detailed (age 17-14y)",
  educpa = "Highest education level father detailed (age 17-14y)",
  ecoact17 = "Economic activity (age 17y)",
  ecoact18 = "Economic activity (age 18y)",
  ecoact19 = "Economic activity (age 19y)",
  ecoact20 = "Economic activity (age 20y)",
  ecoact25 = "Economic activity (age 25y)",
  ecoact32 = "Economic activity (age 32y)",
  ecoactadu25 = "Economic activity detailed (age 25y)",
  ecoactadu32 = "Economic activity detailed (age 32y)",
  ecoactdtma14 = "Economic activity mother (age 14y)",
  ecoactdtpa14 = "Economic activity father (age 14y)",
  ecoactdtma15 = "Economic activity mother (age 15y)",
  ecoactdtpa15 = "Economic activity father (age 15y)",
  ecoactdtma16 = "Economic activity mother (age 16y)",
  ecoactdtpa16 = "Economic activity father (age 16y)",
  ecoactdtma17 = "Economic activity mother (age 17y)",
  ecoactdtpa17 = "Economic activity father (age 17y)",
  nsseccat17 = "NS-SEC 17-category (age 17y)",
  nsseccat18 = "NS-SEC 17-category (age 18y)",
  nsseccat19 = "NS-SEC 17-category (age 19y)",
  nsseccat20 = "NS-SEC 17-category (age 20y)",
  nsseccat25 = "NS-SEC 17-category (age 25y)",
  nsseccat32 = "NS-SEC 17-category (age 32y)",
  nsseccatma14 = "NS-SEC mother 17-category (age 14y)",
  nsseccatpa14 = "NS-SEC father 17-category (age 14y)",
  nsseccatma15 = "NS-SEC mother 17-category (age 15y)",
  nsseccatpa15 = "NS-SEC father 17-category (age 15y)",
  nsseccatma16 = "NS-SEC mother 17-category (age 16y)",
  nsseccatpa16 = "NS-SEC father 17-category (age 16y)",
  nsseccatma17 = "NS-SEC mother 17-category (age 17y)",
  nsseccatpa17 = "NS-SEC father 17-category (age 17y)",
  nsseccatma18 = "NS-SEC mother 17-category (age 18y)",
  nsseccatpa18 = "NS-SEC father 17-category (age 18y)",
  inc25 = "own income (pound, age 25)",
  inc32 = "own income (pound, age 32)",
  incwhh14 = "weekly household gross income banded (pound, age 14)",
  incwhh15 = "weekly household gross income banded (pound, age 15)",
  incwhh16 = "weekly household gross income banded (pound, age 16)",
  incwhh17 = "weekly household gross income banded (pound, age 17)",
  imd15 = "Index of Multiple Deprivation (IMD)  (age 15y)",
  imd25 = "Index of Multiple Deprivation (IMD)  (age 25y)",
  imd32 = "Index of Multiple Deprivation (IMD)  (age 32y)",
  ghq15 = "GHQ-12 12-item score (age 15y)",
  ghq17 = "GHQ-12 12-item score (age 17y)",
  ghq25 = "GHQ-12 12-item score (age 25y)",
  ghq32 = "GHQ-12 12-item score (age 32y)",
  ghqtl15 = "GHQ-12 sum score of the each item (0-36) (age 15y)",
  ghqtl17 = "GHQ-12 sum score of the each item (0-36) (age 17y)",
  ghqtl25 = "GHQ-12 sum score of the each item (0-36) (age 25y)",
  ghqtl32 = "GHQ-12 sum score of the each item (0-36) (age 32y)",
  lsat20 = "Life satisfaction (age 20y)",
  lsat25 = "Life satisfaction (age 25y)",
  lsat32 = "Life satisfaction (age 32y)",
  wt0 = "Weight (kilogram, age 0y)",
  wt25 = "Weight (kilogram, age 25y)",
  wt32 = "Weight (kilogram, age 32y)",
  ht25 = "Height (metre, age 25y)",
  ht32 = "Height (metre, age 32y)",
  ht25_32 = "Height (metre, age 25y or 32y)",
  bmi25 = "BMI (age 25y)",
  bmi32 = "BMI (age 32y)",
  ghea15 = "Self-rated general health binary (age 15y)", 
  ghea16 = "Self-rated general health binary (age 16y)", 
  ghea17 = "Self-rated general health binary (age 17y)", 
  ghea25 = "Self-rated general health binary (age 25y)", 
  ghea32 = "Self-rated general health binary (age 32y)",
  gheateen15 = "Self-rated general health adolescent 4-point (age 15y)", 
  gheateen16 = "Self-rated general health adolescent 4-point (age 16y)", 
  gheateen17 = "Self-rated general health adolescent 4-point (age 17y)",
  gheaadu25 = "Self-rated general health adult 5-point (age 25y)", 
  gheaadu32 = "Self-rated general health adult 5-point (age 32y)",
  lsi14_15 = "Long-term illness binary (age 14-15y)", 
  lsi17 = "Long-term illness binary (age 17y)", 
  lsi19 = "Long-term illness binary (age 19y)", 
  lsi20 = "Long-term illness binary (age 20y)", 
  lsi25 = "Long-term illness binary (age 25y)", 
  lsi32 = "Long-term illness binary (age 32y)",
  smknw14 = "Smoking now binary (age 14y)", 
  smknw15 = "Smoking now binary (age 15y)", 
  smknw16 = "Smoking now binary (age 16y)", 
  smknw25 = "Smoking now binary (age 25y)", 
  smknw32 = "Smoking now binary (age 32y)",
  smk14 = "Smoking ever and frequency (age 14y)", 
  smk15 = "Smoking ever and frequency (age 15y)", 
  smk16 = "Smoking ever and frequency (age 16y)", 
  smk25 = "Smoking ever and frequency (age 25y)", 
  smk32 = "Smoking ever and frequency (age 32y)",
  alcfst = "First time had alcohol (age 14-32y)", 
  alcfreq14 = "Alcohol frequency (age 14y)", 
  alcfreq15 = "Alcohol frequency (age 15y)", 
  alcfreq16 = "Alcohol frequency (age 16y)", 
  alcfreq19 = "Alcohol frequency (age 19y)", 
  alcfreq20 = "Alcohol frequency (age 20y)",
  audita25 = "AUDIT A (age 25y)", 
  audita32 = "AUDIT A (age 32y)", 
  auditb25 = "AUDIT B (age 25y)", 
  auditb32 = "AUDIT B (age 32y)", 
  auditc25 = "AUDIT C (age 25y)", 
  auditc32 = "AUDIT C (age 32y)",
  spt14 = "Exercise frequency (age 14y)", 
  spt15 = "Exercise frequency (age 15y)", 
  spt17 = "Exercise frequency (age 17y)", 
  spt19 = "Exercise frequency (age 19y)", 
  spt20 = "Exercise frequency (age 20y)", 
  spt25 = "Exercise frequency (age 25y)", 
  spt32 = "Exercise frequency (age 32y)"
)

write.csv(derived_all, file =  "derived_variables.csv", row.names = FALSE)