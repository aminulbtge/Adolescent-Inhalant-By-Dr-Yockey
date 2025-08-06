# Load necessary libraries
library(dplyr)
library(readr)

# Load the datasets
library(haven)
NSDUH_2021 <- read_sav("C:/Users/student/Desktop/Apu's work/Adolescent Inhalant Use/2021/NSDUH_2021.SAV")
View(NSDUH_2021)

NSDUH_2022 <- read_sav("C:/Users/student/Desktop/Apu's work/Adolescent Inhalant Use/2022/NSDUH_2022.sav")
View(NSDUH_2022)

NSDUH_2023 <- read_sav("C:/Users/student/Desktop/Apu's work/Adolescent Inhalant Use/2023/NSDUH-2023-DS0001-bndl-data-spss_v1/NSDUH_2023.sav")
View(NSDUH_2023)


# Add a year identifier 
NSDUH_2021$year <- 2021
NSDUH_2022$year <- 2022
NSDUH_2023$year <- 2023

# Combine datasets
full_data <- bind_rows(NSDUH_2021, NSDUH_2022, NSDUH_2023)


# Load libraries
library(dplyr)
library(broom)

# Convert relevant variables to factors
full_data <- full_data %>%
  mutate(
    IRSEX = factor(IRSEX),           # Biological sex
    NEWRACE2 = factor(NEWRACE2),     # Race
    TOBYR = factor(TOBYR),           # Tobacco use (past year)
    MRJYR = factor(MRJYR),           # Marijuana use (past year)
    INHALYR = factor(INHALYR),       # Past year inhalant use (binary outcome)
    INHALMON = factor(INHALMON),     # Past month inhalant use (binary outcome)
    IRPYUD5INH = factor(IRPYUD5INH)  # Inhalant use disorder (binary outcome)
  )

#Filter complete cases only for predictors
full_data_clean <- full_data %>%
  filter(!is.na(INHALYR), !is.na(INHALMON), !is.na(IRPYUD5INH),
         !is.na(IRSEX), !is.na(NEWRACE2), !is.na(TOBYR), !is.na(MRJYR))



# Model 1: Past year inhalant use
model1 <- glm(INHALYR ~ IRSEX + NEWRACE2 + TOBYR + MRJYR,
              data = full_data_clean, family = binomial)

# Model 2: Past month inhalant use
model2 <- glm(INHALMON ~ IRSEX + NEWRACE2 + TOBYR + MRJYR,
              data = full_data_clean, family = binomial)

# Model 3: Inhalant Use Disorder (past year)
model3 <- glm(IRPYUD5INH ~ IRSEX + NEWRACE2 + TOBYR + MRJYR,
              data = full_data_clean, family = binomial)


# Helper function to display OR and 95% CI
extract_or_ci <- function(model) {
  tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    select(term, estimate, conf.low, conf.high, p.value)
}

# Define a function to extract odds ratios and confidence intervals
extract_or_ci <- function(model) {
  broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    dplyr::select(term, estimate, conf.low, conf.high, p.value)
}

cat("\n--- Model 1: Past Year Inhalant Use ---\n")
print(extract_or_ci(model1))

cat("\n--- Model 2: Past Month Inhalant Use ---\n")
print(extract_or_ci(model2))

cat("\n--- Model 3: Inhalant Use Disorder ---\n")
print(extract_or_ci(model3))


























