library(brms)
library(cmdstanr)
library(parallel)

# ==============================================================================
# 1. Load Preprocessed Data
# ==============================================================================
if (!file.exists("processed_data_us.rds")) {
  stop("File 'processed_data.rds' not found. Please run the preprocessing code first.")
}

# Load data as a list and unpack
loaded_data <- readRDS("processed_data_us.rds") # or processed_data_ja.rds
df <- loaded_data$df
explanatory_vars <- loaded_data$explanatory_vars

cat("Data loaded. Starting model construction...\n")

# ==============================================================================
# 2. Define Model Formulas
# ==============================================================================
formula_Y1_ <- as.formula(paste("Y1 ~", paste(explanatory_vars, collapse = " + ")))
formula_Y1 <- bf(formula_Y1_, family = bernoulli())

formula_Y21_ <- as.formula(paste("Y21 ~ Y1 +", paste(explanatory_vars, collapse = " + ")))
formula_Y21 <- bf(formula_Y21_, family = bernoulli())

formula_Y31_ <- as.formula(paste("Y31 ~ Y21 +", paste(explanatory_vars, collapse = " + ")))
formula_Y31 <- bf(formula_Y31_, family = bernoulli())

formula_Y32_ <- as.formula(paste("Y32 ~ Y21 +", paste(explanatory_vars, collapse = " + ")))
formula_Y32 <- bf(formula_Y32_, family = bernoulli())

# ==============================================================================
# 3. Run Model
# ==============================================================================
options(mc.cores = parallel::detectCores()) # Parallel processing
options(brms.backend = "cmdstanr")

model <- brm(
  formula = formula_Y1 + formula_Y21 + formula_Y31 + formula_Y32,
  data = df,
  chains = 4,           # Number of chains
  iter = 2000,          # Total iterations per chain
  warmup = 1000,        # Burn-in (warmup) period
  cores = 4,            # Number of cores for parallel processing
  control = list(adapt_delta = 0.95),  # HMC control parameters
  backend = "cmdstanr"  # Use cmdstanr (recommended)
)

# Display results (if necessary)
print(summary(model))
