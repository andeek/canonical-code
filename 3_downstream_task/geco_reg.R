## File: geco_reg.R
## Purpose: Perform canonicalization and downstream task on geco data sets
## Author: Andee Kaplan
## Date: 06/19/2020

# pass from command line which noise level, 1, 2, or 5
# Rscript geco_reg 1 geco ../data/geco/ ../results/lambdas/ ../results/downstream/
args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 5) stop("Pass in the noise level (1, 2, or 5), data (geco or geco_x), data folder location, lambdas location, and output location", call.=FALSE)
if (!(args[1] %in% c(1, 2, 5))) stop("Pass in the duplication level (1, 2, or 5)", call.=FALSE)
if (!(args[2] %in% c("geco", "geco_x"))) stop("Pass in the data (geco or geco_x)", call.=FALSE)
noise <- args[1]
bp_sigma <- as.numeric(noise)
data_set <- args[2]
data_folder <- args[3]
linkage_folder <- args[4]
output_folder <- args[5]

# library ----
library(igraph)
library(dplyr)
library(tidyr)
library(rstanarm)
library(representr)

# reproducible
set.seed(1234)

# load data ----
load(paste0(data_folder, data_set, "_bpsigma_", noise, ".Rdata"))

# load dblink results ----
load(paste0(linkage_folder, "geco1_results.Rdata"))

# linkage samples
mcmclink <- lambda

# get estimated links mpmms
mpmms_clust <- mpmms_lambda

# canonicalization ----
# params for the proto functions
col_type <- c("string", "string", "numeric", "numeric", "numeric", "categorical", "ordinal", "numeric", "numeric", "categorical")
weights <- c(.2, .2, .05, .05, .05, .05, .05, .15, .15, .05)
orders <- list(education = c("Less than a high school diploma", "High school graduates, no college", "Some college or associate degree", "Bachelor's degree only", "Advanced degree"))

# params for KL div function
num_vars <- c("income", "bp")
cat_vars <- c("sex", "high_bp")
l_m <- 10

## true data for emp kl divergence
truth <- data[originals_idx, c(numeric_vars, cat_vars)]

# simulating multiple representative datasets ----
# how many original records are selected over multiple representative "data sets" 
dt <- 100

mse_minimax <- rep(NA, dt)
mse_random <- rep(NA, dt)
mse_average <- rep(NA, dt)
mse_thresh <- rep(NA, dt)
mse_weighted <- rep(NA, dt)

kl_div <- data.frame(method = c(), kl_div = c(), iter = c())
m0_coefs <- data.frame()
m1_coefs <- data.frame()

# format function ----
format_coefs <- function(stan_model) {
  model_summary <- data.frame(t(as.data.frame(summary(stan_model))))
  model_summary$statistic <- rownames(model_summary)
  rownames(model_summary) <- NULL
  data.frame(model_summary[model_summary$statistic %in% c("mean", "2.5%", "97.5%"), ])
}

for(i in seq_len(dt)) {
  # single record proto methods ----
  id_random <- represent(data, mpmms_clust, "proto_random")
  id_minimax <- represent(data, mpmms_clust, "proto_minimax", distance = dist_col_type, col_type = col_type, weights = weights, orders = orders, scale = TRUE)
  
  # average record methods ----
  df_average <- represent(data, mpmms_clust, "composite", col_type = col_type)  
  df_average$sex <- as.character(df_average$sex)
  
  # pp weighted and thresh ----
  # for each iteration, get the minimax prototypes and count how many times each record is included
  pp_weights <- pp_weights(data, mcmclink, rep_method = "proto_minimax", col_type = col_type, distance = dist_col_type, weights = weights, orders = orders, scale = TRUE, parallel = FALSE)
  
  # data summaries ----
  # get the KL divergence stat
  kl_random <- emp_kl_div(truth, data[id_random, ], cat_vars, num_vars)
  kl_minimax <- emp_kl_div(truth, data[id_minimax, ], cat_vars, num_vars)
  kl_composite <- emp_kl_div(truth, df_average[, ], cat_vars, num_vars)
  kl_thresh <- emp_kl_div(truth, data[pp_weights >= 0.5, ], cat_vars, num_vars)
  kl_weight <- emp_kl_div(truth, data[, ], cat_vars, num_vars, weights = pp_weights)
  
  data.frame(prototype = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold"),
             kl_div = c(kl_random, kl_composite, kl_minimax, kl_weight, kl_thresh)) %>%
    mutate(iter = i) %>%
    bind_rows(kl_div) -> kl_div
  
  # models ----
  # linear regression models
  m0.minimax <- stan_glm(bp ~ sex*income, data = data[unlist(id_minimax),], seed = 1234,
                          family = gaussian(), prior = normal(), prior_intercept = normal())
  m0.random <- stan_glm(bp ~ sex*income, data = data[unlist(id_random),], seed = 1234,
                         family = gaussian(), prior = normal(), prior_intercept = normal())
  m0.average <- stan_glm(bp ~ sex*income, data = df_average, seed = 1234,
                        family = gaussian(), prior = normal(), prior_intercept = normal())
  m0.weighted <- stan_glm(bp ~ sex*income, data = data, seed = 1234, weights = pp_weights,
                        family = gaussian(), prior = normal(), prior_intercept = normal())
  m0.thresh <- stan_glm(bp ~ sex*income, data = data[pp_weights >= 0.5,], seed = 1234,
                        family = gaussian(), prior = normal(), prior_intercept = normal())
  
  
  # store model results ----
  # store summary of coefficients
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.minimax), model = "minimax", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.random), model = "random", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.average), model = "composite", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.thresh), model = "pp_thresh", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.weighted), model = "pp_weighted", iter =i))

  # get mse for each linear model
  mse_minimax[i] <- mean((colSums(posterior_predict(m0.minimax, newdata = test_data, draws = 1000, seed = 1234))/1000 - test_data$bp)^2)
  mse_random[i] <- mean((colSums(posterior_predict(m0.random, newdata = test_data, draws = 1000, seed = 1234))/1000 - test_data$bp)^2)
  mse_average[i] <- mean((colSums(posterior_predict(m0.average, newdata = test_data, draws = 1000, seed = 1234))/1000 - test_data$bp)^2)
  mse_thresh[i] <- mean((colSums(posterior_predict(m0.thresh, newdata = test_data, draws = 1000, seed = 1234))/1000 - test_data$bp)^2)
  mse_weighted[i] <- mean((colSums(posterior_predict(m0.weighted, newdata = test_data, draws = 1000, seed = 1234))/1000 - test_data$bp)^2)
}

# true data model ----
m0.true <- stan_glm(bp ~ sex*income, data = data[originals_idx,], seed = 1234,
                    family = gaussian(), prior = normal(), prior_intercept = normal())

# summaries
mse_true <- mean((colSums(posterior_predict(m0.true, newdata = test_data, draws = 1000, seed = 1234))/1000 - test_data$bp)^2)

# summary of coefs
m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.true), model = "true", iter = 1))

# put things together ----
# mse for each for the regression model
df_mse <- data.frame(random = mse_random,
                     minimax = mse_minimax,
                     average = mse_average,
                     true = mse_true,
                     pp_weighted = mse_weighted,
                     pp_thresh = mse_thresh)



# save objects ----
save(df_mse, kl_div, m0_coefs, mcmclink, mpmms_clust, mpmms_error,
     mse_random, mse_minimax, mse_true, mse_average, mse_weighted, mse_thresh,
     file = paste0(output_folder, data_set, "_dblink_bpsigma_", noise,".Rdata"))


