#!/usr/bin/env Rscript

# ================================
# Load necessary libraries
# ================================
library(brms)
library(cmdstanr)
library(future)
library(parallel)

# ================================
# Set up parallel processing and backend
# ================================
plan(multisession)
options(mc.cores = parallel::detectCores())
options(brms.backend = "cmdstanr")

# ================================
# Read the data
# ================================
# Ensure that 'usa_file.csv' exists in your working directory
df <- read.csv("usa_file.csv", header = TRUE)

# ================================
# Select explanatory variables
# ================================
explanatory_vars <- names(df)[grepl(
  "^(X_sex|X_age2|X_marriage|X_full_employ|X_student|hierarchy_15_0|hierarchy_15_2|hierarchy_now_0|hierarchy_now_2|q1_us|q_service_|q7_|religious|X_company_|X_university|X_university_parent|X_graduate|X_graduate_parent|q13_1|q15_1|q15_2|reading_books|q20_us_1_inc|hierarchy_diff|q5_con|q5_pol|EconomicCapital|CulturalCapital)",
  names(df))
]

# ================================
# Create model formulas
# ================================
formula_Y1_ <- as.formula(paste("Y1 ~", paste(explanatory_vars, collapse = " + ")))
formula_Y1  <- bf(formula_Y1_, family = bernoulli())

formula_Y2_ <- as.formula(paste("Y2 ~ Y1 +", paste(explanatory_vars, collapse = " + ")))
formula_Y2  <- bf(formula_Y2_, family = bernoulli())

formula_Y31_ <- as.formula(paste("Y31 ~ Y2 +", paste(explanatory_vars, collapse = " + ")))
formula_Y31  <- bf(formula_Y31_, family = bernoulli())

formula_Y32_ <- as.formula(paste("Y32 ~ Y2 +", paste(explanatory_vars, collapse = " + ")))
formula_Y32  <- bf(formula_Y32_, family = bernoulli())

# ================================
# Run the model
# ================================
model <- brm(
  formula = formula_Y1 + formula_Y2 + formula_Y31 + formula_Y32,
  data = df,
  chains = 4,           # Number of chains
  iter = 2000,          # Total iterations per chain
  warmup = 1000,        # Warm-up (burn-in) period
  cores = 4,            # Number of cores for parallel processing
  control = list(adapt_delta = 0.95),  # HMC control parameters
  backend = "cmdstanr"  # Use cmdstanr backend (recommended)
)

# ================================
# Save the model
# ================================
saveRDS(model, file = "bayesian_hierarchical_bernoulli_model_usa.rds")
