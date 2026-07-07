# Three-stage conspiracy interview analysis

This repository contains supplementary data and model scripts for the paper:

> "Recognition," "belief," and "action" regarding conspiracy theories: An empirical study using large-scale samples from the United States and Japan.

## Files

- `processed_data_us.rds`: preprocessed U.S. analysis data.
- `processed_data_ja.rds`: preprocessed Japan analysis data.
- `run_model_usa.R`: fits the main four-stage U.S. Bayesian model.
- `run_model_japan.R`: fits the main four-stage Japan Bayesian model.
- `NHB2025_ConspiracyInterview_supplementary.pdf`: supplementary information.

## Requirements

The model scripts require R and the following R packages:

- `brms`
- `cmdstanr`
- `parallel`

`cmdstanr` must be configured with a working CmdStan installation.

## Reproducing the main models

From the repository root, run:

```bash
Rscript run_model_usa.R
Rscript run_model_japan.R
```

The Japan script saves the fitted model as `bayesian_model_japan.rds`.

To check the Japan model formula without fitting the model, run:

```bash
Rscript run_model_japan.R --dry-run
```
