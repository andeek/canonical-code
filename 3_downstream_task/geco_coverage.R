## File: geco_coverage.R
## Purpose: Get empirical coverage of downstream tasks after canonicalization for geco datasets
## Date: 02/03/2022

# pass from command line which noise level, 1, 2, or 5
# Rscript geco_reg 1 geco ../data/geco/ ../results/lambdas/ ../results/canonical/
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
library(dplyr)
library(tidyr)
library(rstanarm)
library(representr)

# reproducible
set.seed(1234)

# load data ----
load(paste0(data_folder, data_set, "_bpsigma_", noise, ".Rdata"))
org_data <- data # store for later

# load dblink results ----
load(paste0(linkage_folder, "geco1_results.Rdata"))
mcmclink <- lambda

# get cluster assignments
mpmms_clust <- mpmms_lambda

# canonicalization ----
# params for the proto functions
col_type <- c("string", "string", "numeric", "numeric", "numeric", "categorical", "ordinal", "numeric", "numeric", "categorical")
weights <- c(.25, .25, .1, .1, .1, .1, .1, 1, 1, 0)
orders <- list(education = c("Less than a high school diploma", "High school graduates, no college", "Some college or associate degree", "Bachelor's degree only", "Advanced degree"))

# store results
m0_coefs <- data.frame()
mse <- data.frame(model = c(), mse = c(), iter = c())
kl_div <- data.frame(method = c(), kl_div = c(), iter = c())
betas <- data.frame()

# params ----
total_num <- 100

# data generation function ----
generate_data <- function(data, originals_idx, identity, bp_sigma, beta, data_set, noise_type, noise_sd = NULL) {
  dat <- data
  
  # systolic blood pressue from sex and income
  # "true" relationship is bp = b0 + b1*I(male) + b2*log(income) + b3*log(income)^2 + b4*I(male)*log(income) + noise
  # female and high income => lower bp  
  if(noise_type == "low" & is.null(noise_sd)) stop("Must include noise sd for low noise scenario.")
  
  bp_mean_curve <- function(income, male, beta) {
    x <- cbind(rep(1, length(income)), male, income, male*income)
    x %*% beta
  }
  
  dat[originals_idx,] %>%
    rowwise() %>%
    mutate(fuzzy_exp = rnorm(1, bp_mean_curve(income, sex == "M", beta), bp_sigma)) %>%
    mutate(bp = round(fuzzy_exp)) %>%
    mutate(high_bp = rbinom(1, 1, 1/(1 + exp(-(fuzzy_exp - 130))))) %>%
    select(-fuzzy_exp) -> dat[originals_idx,] 
  
  # probability of distorting each column
  if(noise_type == "low") {
    dist_probs <- list(sex = .25, income = 0.25, bp = .25, high_bp = 0)
  } else {
    dist_probs <- list(sex = 0, income = 0, bp = .25, high_bp = 0)
  }
  for(id in unique(identity)) {
    recs <- dat[identity == id,]
    
    if(nrow(recs) == 1) next() # no dups to distort
    
    idx_dup <- which(which(identity == id) != originals_idx[identity[originals_idx] == id])
    idx_org <- which(which(identity == id) == originals_idx[identity[originals_idx] == id])
    
    ## try generating income and sex also
    recs[, "bp"] <- recs[idx_org, "bp"]
    recs[, "high_bp"] <- recs[idx_org, "high_bp"]
    
    if(noise_type == "low") {
      recs[, "income"] <- recs[idx_org, "income"]
      recs[, "sex"] <- recs[idx_org, "sex"]
    }
    
    
    # get date so can distort
    # recs$date <- as.Date(tidyr::unite(recs, date, by, bm, bd, sep = "-") %>% .$date)
    distorted <- recs[idx_dup, ] != recs[rep(idx_org, length(idx_dup)), ]
    
    # pick ones to distort
    add_noise <- 5 - unlist(lapply(rowSums(distorted), function(x) max(0, x)))
    col <- lapply(add_noise, function(x) { sample(names(dist_probs)[dist_probs > 0], max(0, min(sum(dist_probs > 0), x)))})
    if(data_set == "geco_x") col <- lapply(col, function(x) setdiff(x, "bp"))
    
    for(dup in seq_along(col)) {
      if("income" %in% col[[dup]]) {
        if(noise_type == "low") {
          recs[idx_dup,][dup, "income"] <- round(rnorm(1, recs[idx_dup, "income"][dup], noise_sd))
        } else {
          recs[idx_dup,][dup, "income"] <- sample(dat[,"income"], 1, FALSE)
        }
      }
      if("bp" %in% col[[dup]]) {
        if(noise_type == "low") {
          recs[idx_dup,][dup, "bp"] <- round(rnorm(1, recs[idx_dup, "bp"][dup], noise_sd))
        } else {
          recs[idx_dup,][dup, "bp"] <- sample(dat[,"bp"], 1, FALSE)
        }
      }
      if("sex" %in% col[[dup]]) {
        recs[idx_dup,][dup, "sex"] <- sample(c("M", "F"), 1)
      }
      
      # recs[idx_dup,][dup, "income"] <- round(rnorm(1, recs[idx_dup, "income"][dup], 1))
    }
    
    # add in to df
    dat[identity == id, names(dat)] <- recs[, names(dat)]
  }
  return(dat)
}

generate_test_data <- function(test_data, bp_sigma, beta) {
  test_dat <- test_data
  
  # systolic blood pressue from sex and income
  # "true" relationship is bp = b0 + b1*I(male) + b2*log(income) + b3*log(income)^2 + b4*I(male)*log(income) + noise
  # female and high income => lower bp  
  
  bp_mean_curve <- function(income, male, beta) {
    x <- cbind(rep(1, length(income)), male, income, male*income)
    x %*% beta
  }
  
  test_dat %>%
    rowwise() %>%
    mutate(fuzzy_exp = rnorm(1, bp_mean_curve(income, sex == "M", beta), bp_sigma)) %>%
    mutate(bp = round(fuzzy_exp)) %>%
    mutate(high_bp = rbinom(1, 1, 1/(1 + exp(-(fuzzy_exp - 130))))) %>%
    select(-fuzzy_exp) -> test_dat
  
  
  return(test_dat)
}

# format function ----
format_coefs <- function(stan_model) {
  if("stanreg" %in% class(stan_model)) {
    model_summary <- data.frame(t(as.data.frame(summary(stan_model, probs = c(0.025, 0.05, 0.95, .975)))))
    model_summary$statistic <- rownames(model_summary)
    rownames(model_summary) <- NULL
    return(data.frame(model_summary[model_summary$statistic %in% c("mean","2.5%", "5%", "95%", "97.5%"), c(1:length(coef(stan_model)), ncol(model_summary))]))
  } else if("stanfit" %in% class(stan_model)) {
    draws <- cbind(rstan::extract(stan_model, "alpha")[[1]], rstan::extract(stan_model, "beta")[[1]])
    
    model_summary <- data.frame(rbind(apply(draws, 2, mean), apply(draws, 2, quantile, probs = c(0.025, .05, .95, .975))))
    model_summary$statistic <- c("mean","2.5%", "5%", "95%", "97.5%")
    rownames(model_summary) <- NULL
    return(model_summary)
  }
}

## compile stan models
# lm_weighted_power <- stan_model("lm_weighted_power.stan")
lm_weighted_var <- stan_model("lm_weighted_var.stan")

# beta to generate with
beta <- c(160, 10, -1, 0)

# repeat everything total_num times
for(i in 1:total_num) {
  # generate a new dataset
  data <- generate_data(org_data, originals_idx, bp_sigma, beta, data_set, "low", 1)
  
  ## universal bins for KL
  truth <- data_new[originals_idx, c(numeric_vars, cat_vars)]
  true_cut_list <- lapply(split(truth, truth[, cat_vars]), function(dat) {
    T_m <- floor((nrow(dat)/l_m)^(1/length(numeric_vars)))
    nest_cuts(dat, numeric_vars, T_m)
  })
  
  # do all the canonicalization
  # single record proto methods ----
  id_random <- represent(data, mpmms_clust, "proto_random")
  id_minimax <- represent(data, mpmms_clust, "proto_minimax", distance = dist_col_type, col_type = col_type, weights = weights, orders = orders, ties_fn = "within_category_compare_cpp", scale = TRUE)

  # average record methods ----
  df_average <- represent(data, mpmms_clust, "composite", col_type = col_type)
  df_average$sex <- as.character(df_average$sex)

  # pp weighted and thresh ----
  # for each iteration, get the minimax prototypes and count how many times each record is included
  pp_weights <- pp_weights(data, mcmclink, rep_method = "proto_minimax", col_type = col_type, distance = dist_col_type, weights = weights, orders = orders, ties_fn = "within_category_compare_cpp", scale = TRUE, parallel = TRUE)
  
  # KL div ----
  # get the KL divergence stat
  get_freqs(data_new[unlist(id_minimax),], cat_vars, numeric_vars, true_cut_list) %>%
    left_join(get_freqs(data_new[unlist(id_random),], cat_vars, numeric_vars, true_cut_list), by = c(numeric_vars, cat_vars)) %>%
    rename(minimax = freq.x, random = freq.y) %>%
    left_join(get_freqs(data_new[originals_idx,], cat_vars, numeric_vars, true_cut_list), by = c(numeric_vars, cat_vars)) %>%
    rename(true = freq) %>%
    left_join(get_freqs(df_average, cat_vars, numeric_vars, true_cut_list), by = c(numeric_vars, cat_vars)) %>%
    rename(average = freq) %>%
    left_join(get_freqs(data_new[pp_weights > .5,], cat_vars, numeric_vars, true_cut_list), by = c(numeric_vars, cat_vars)) %>%
    rename(pp_thresh = freq) %>%
    left_join(get_freqs(data_new, cat_vars, numeric_vars, true_cut_list, pp_weights), by = c(numeric_vars, cat_vars)) %>%
    rename(pp_weighted = freq) %>%
    filter(true > 0) %>%
    gather(method, freq, minimax, random, average, pp_thresh, pp_weighted) %>%
    group_by(method) %>%
    mutate(prob = freq/sum(freq)) %>% # get probability distributions that are abs. cont wrt truth
    mutate(true_prob = true/sum(true)) %>%
    mutate(inner = ifelse(prob == 0, 0, prob*log(prob/true_prob))) %>% # when P(i) = 0, assume contribution = 0
    summarise(kl_div = sum(inner)) %>%
    mutate(iter = i) %>%
    bind_rows(kl_div) -> kl_div
  
  # models ----
  # linear regression models
  m0.minimax <- stan_glm(bp ~ sex*income, data = data_new[unlist(id_minimax),], seed = 1234,
                         family = gaussian(), refresh = 0)
  m0.random <- stan_glm(bp ~ sex*income, data = data_new[unlist(id_random),], seed = 1234,
                        family = gaussian(), refresh = 0)
  m0.average <- stan_glm(bp ~ sex*income, data = df_average, seed = 1234,
                         family = gaussian(), refresh = 0)
  ## pp methods
  X_full <- model.matrix(bp ~ sex*income, data = data[pp_weights > 0,])[,-1]
  X_full_test <- model.matrix(bp ~ sex*income, data = test_data)[,-1]
  m0.weighted_var <- sampling(lm_weighted_var,
                              data = list(N = nrow(X_full), p = ncol(X_full), X = X_full, y = data[pp_weights > 0,]$bp, weight = pp_weights[pp_weights > 0],
                                          N_tilde = nrow(X_full_test), X_tilde = X_full_test),
                              seed = 1234, refresh = 0)
  m0.thresh <- stan_glm(bp ~ sex*income, data = data[pp_weights >= 0.5,], seed = 1234,
                        family = gaussian(), refresh = 0)
  
  # true data model
  m0.true <- stan_glm(bp ~ sex*income, data = data_new[originals_idx,], seed = 1234,
                      family = gaussian(), refresh = 0)
  
  # store model results ----
  # store summary of coefficients
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.minimax), model = "minimax", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.random), model = "random", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.average), model = "composite", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.thresh), model = "pp_thresh", iter = i))
  tmp <- format_coefs(m0.weighted_var)
  names(tmp) <- names(m0_coefs)[1:(ncol(m0_coefs) - 2)]
  m0_coefs <- bind_rows(m0_coefs, data.frame(tmp, model = "pp_weighted", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.true), model = "true", iter = i))
  
  # store betas for later
  names(beta) <- names(m0_coefs)[1:p]
  betas <- rbind(betas, data.frame(t(beta), iter = i))
  
  ## MSE
  # get mse for each linear model
  test_mat <- matrix(test_data_new$bp, ncol = length(test_data_new$bp), nrow = 1000, byrow = TRUE)
  test_mat2 <- matrix(test_data_new$bp, ncol = length(test_data_new$bp), nrow = 4000, byrow = TRUE)
  mse <- bind_rows(mse, data.frame(model = "minimax", mse = mean(rowMeans((posterior_predict(m0.minimax, newdata = test_data_new, draws = 1000) - test_mat)^2)), iter = i))
  mse <- bind_rows(mse, data.frame(model = "random", mse = mean(rowMeans((posterior_predict(m0.random, newdata = test_data_new, draws = 1000) - test_mat)^2)), iter = i))
  mse <- bind_rows(mse, data.frame(model = "average", mse = mean(rowMeans((posterior_predict(m0.average, newdata = test_data_new, draws = 1000) - test_mat)^2)), iter = i))
  mse <- bind_rows(mse, data.frame(model = "pp_thresh", mse = mean(rowMeans((posterior_predict(m0.thresh, newdata = test_data_new, draws = 1000) - test_mat)^2)), iter = i))
  mse <- bind_rows(mse, data.frame(model = "pp_weighted", mse = mean(rowMeans((extract(m0.weighted_var, "y_tilde")[[1]] - test_mat2)^2)), iter = i))
  mse <- bind_rows(mse, data.frame(model = "true", mse = mean(rowMeans((posterior_predict(m0.true, newdata = test_data_new, draws = 1000) - test_mat)^2)), iter = i))
}

m0_coefs_coverage <- m0_coefs #rename for paper ease later

# save objects ----
save(m0_coefs_coverage, betas, mse, kl_div, file = paste0(output_folder, data_set, "_dblink_coverage_bpsigma_", noise,".Rdata"))




