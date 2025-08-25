## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BMIselect)
seed = 12345
set.seed(seed)

## ----sim_b--------------------------------------------------------------------
# Simulate data
data <- sim_A(
  n = 100,
  p = 20,
  type = "MAR",
  seed = seed,
  n_imp = 5
)

str(data, max.level = 1)

## ----mi-----------------------------------------------------------------------
str(data$data_MI)

## ----Horseshoe----------------------------------------------------------------
bmilasso <- BMI_LASSO(data$data_MI$X, data$data_MI$Y, model = "Horseshoe",
                 nburn = 4000, npost = 4000, output_verbose = TRUE, seed = seed)

str(bmilasso, max.level = 1)

## -----------------------------------------------------------------------------
# selection vector of optimal (sub)model
bmilasso$best_select


# posterior draws of coefficients, intercept and regression variance for the optimal (sub)model
str(bmilasso$posterior_best_models)


# the summary table of post-selection inference
bmilasso$summary_table_selected

## -----------------------------------------------------------------------------
# selection matrix of all (sub)models
bmilasso$select


# BICs and degrees of freedom
bmilasso$bic_models

## -----------------------------------------------------------------------------
# posterior draws of parameters of fitted full model in the first stage (before projection predictive variable selection procedure)
str(bmilasso$posterior)


# the summary table of coefficients, intercept, and regression variance from the fitted full model
bmilasso$summary_table_full

## -----------------------------------------------------------------------------
# specify hyperparameters for Multi-Laplace model
bmilasso_ML <- BMI_LASSO(data$data_MI$X, data$data_MI$Y, model = "Multi_Laplace",
                 nburn = 4000, npost = 4000, seed = seed, h = 10, v = 0.5)


# specify hyperparameters for Spike-Laplace model. To save time, we give an example without running
# bmilasso_SL <- BMI_LASSO(data$data_MI$X, data$data_MI$Y, model = "Spike_Laplace",
#                  nburn = 4000, npost = 4000, seed = seed, a = 10, b = 1)

## -----------------------------------------------------------------------------
# run 2 chains using 2 cores in parallel
start = Sys.time()
bmilasso_parallel <- BMI_LASSO(data$data_MI$X, data$data_MI$Y, model = "Horseshoe",
                 nburn = 4000, npost = 4000, output_verbose = FALSE, nchains = 2, ncores = 2, seed = seed)
end = Sys.time()
print(paste("The running time for two chains in parallel:", difftime(end, start, units = "mins"), "mins"))

## -----------------------------------------------------------------------------
# the posterior draws of optimal submodels of two chains
str(bmilasso_parallel$posterior_best_models)

# summary table by pooling two chains
bmilasso_parallel$summary_table_selected

## -----------------------------------------------------------------------------
# fit MI-LASSO
milasso = MI_LASSO(data$data_MI$X, data$data_MI$Y, ncores = 2)

str(milasso)

