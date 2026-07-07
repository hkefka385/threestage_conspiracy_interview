#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(brms)
  library(cmdstanr)
  library(parallel)
})

args <- commandArgs(trailingOnly = TRUE)
dry_run <- any(args %in% c("--dry-run", "dry-run"))

data_path <- "processed_data_ja.rds"
output_path <- "bayesian_model_japan.rds"

if (!file.exists(data_path)) {
  stop("File '", data_path, "' not found. Run this script from the repository root.")
}

loaded_data <- readRDS(data_path)
df <- loaded_data$df
explanatory_vars <- loaded_data$explanatory_vars

required_columns <- c("Y1", "Y21", "Y31", "Y32", explanatory_vars)
missing_columns <- setdiff(required_columns, names(df))
if (length(missing_columns) > 0) {
  stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
}

cat("Japan data loaded: ", nrow(df), " rows, ", ncol(df), " columns.\n", sep = "")
cat("Constructing four-stage model formulas...\n")

formula_Y1 <- bf(
  as.formula(paste("Y1 ~", paste(explanatory_vars, collapse = " + "))),
  family = bernoulli()
)
formula_Y21 <- bf(
  as.formula(paste("Y21 ~ Y1 +", paste(explanatory_vars, collapse = " + "))),
  family = bernoulli()
)
formula_Y31 <- bf(
  as.formula(paste("Y31 ~ Y21 +", paste(explanatory_vars, collapse = " + "))),
  family = bernoulli()
)
formula_Y32 <- bf(
  as.formula(paste("Y32 ~ Y21 +", paste(explanatory_vars, collapse = " + "))),
  family = bernoulli()
)

model_formula <- formula_Y1 + formula_Y21 + formula_Y31 + formula_Y32

if (dry_run) {
  cat("\nDry run only. Model formula:\n")
  print(model_formula)
  quit(save = "no", status = 0)
}

options(mc.cores = parallel::detectCores())
options(brms.backend = "cmdstanr")

priors <- c(
  prior(normal(0, 1.5), class = "b"),
  prior(student_t(3, 0, 2.5), class = "Intercept")
)

model <- brm(
  formula = model_formula,
  data = df,
  prior = priors,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  control = list(adapt_delta = 0.95),
  backend = "cmdstanr",
  seed = 20260707
)

saveRDS(model, output_path)
cat("Saved fitted model to ", output_path, "\n", sep = "")
print(summary(model))
