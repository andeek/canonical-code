## File: geco_coverage.R
## Purpose: Get empirical coverage of downstream tasks after canonicalization for geco datasets
## Author: Andee Kaplan
## Date: 06/19/2020

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
library(igraph)
library(dplyr)
library(tidyr)
library(rstanarm)
library(representr)

# reproducible
set.seed(1234)

# load data ----
load(paste0(data_folder, data_set, "_bpsigma_", noise, ".Rdata"))
org_data <- data # store for later

# linkage samples
mcmclink <- lambda

# get cluster assignments
mpmms_clust <- mpmms_lambda

# canonicalization ----
# params for the proto functions
col_type <- c("string", "string", "numeric", "numeric", "numeric", "categorical", "ordinal", "numeric", "numeric", "categorical")
weights <- c(.2, .2, .05, .05, .05, .05, .05, .15, .15, .05)
orders <- list(education = c("Less than a high school diploma", "High school graduates, no college", "Some college or associate degree", "Bachelor's degree only", "Advanced degree"))

# store results
m0_coefs <- data.frame()
betas <- data.frame()

# params ----
total_num <- 100

# data generation function ----
generate_data <- function(data, originals_idx, bp_sigma, beta) {
  # systolic blood pressue from sex and income
  # "true" relationship is bp = b0 + b1*I(male) + b2*log(income) + b3*log(income)^2 + b4*I(male)*log(income) + noise
  # female and high income => lower bp  
  bp_mean_curve <- function(income, male, beta) {
    x <- cbind(rep(1, length(income)), male, income, male*income)
    x %*% beta
  }
  
  data[originals_idx,] %>%
    rowwise() %>%
    mutate(fuzzy_exp = rnorm(1, bp_mean_curve(income, sex == "M", beta), bp_sigma)) %>%
    mutate(bp = round(fuzzy_exp)) %>%
    mutate(high_bp = rbinom(1, 1, 1/(1 + exp(-(fuzzy_exp - 130))))) %>%
    select(-fuzzy_exp) -> data[originals_idx,]
  
  # probability of distorting each column
  dist_probs <- list(date = .25, sex = .1, education = .3, income = .2, bp = .25, high_bp = .25)
  for(id in unique(identity)) {
    recs <- data[identity == id,]
    
    if(nrow(recs) == 1) next() # no dups to distort
    
    idx_dup <- which(which(identity == id) != originals_idx[identity[originals_idx] == id])
    idx_org <- which(which(identity == id) == originals_idx[identity[originals_idx] == id])
    
    recs[, "bp"] <- recs[idx_org, "bp"]
    recs[, "high_bp"] <- recs[idx_org, "high_bp"]
    
    # get date so can distort
    recs$date <- as.Date(tidyr::unite(recs, date, by, bm, bd, sep = "-") %>% .$date)
    distorted <- recs[idx_dup, names(dist_probs)] != recs[rep(idx_org, length(idx_dup)), names(dist_probs)]
    
    ## these don't have enough distortions
    
    # pick one to distort
    col <- sample(c("bp", "high_bp"), 1, prob = unlist(dist_probs)[c("bp", "high_bp")])
    recs[idx_dup[rowSums(distorted) == 2], col] = sample(unique(data[, col]), sum(rowSums(distorted) == 2), replace = TRUE)
    
    # distort both!
    recs[idx_dup[rowSums(distorted) == 1], "bp"] = sample(unique(data[, "bp"]), sum(rowSums(distorted) == 1), replace = TRUE)
    recs[idx_dup[rowSums(distorted) == 1], "high_bp"] = sample(unique(data[, "high_bp"]), sum(rowSums(distorted) == 1), replace = TRUE)
    
    # add in to df
    data[identity == id, names(data)] <- recs[, names(data)]
  }
  return(data)
}

# format function ----
format_coefs <- function(stan_model) {
  model_summary <- data.frame(t(as.data.frame(summary(stan_model))))
  model_summary$statistic <- rownames(model_summary)
  rownames(model_summary) <- NULL
  data.frame(model_summary[model_summary$statistic %in% c("mean", "2.5%", "97.5%"), ])
}

# true data model ----
# fit once to get priors to generate beta from for coverage
m0.init <- stan_glm(bp ~ sex*income, data = org_data[originals_idx,], seed = 1234,
                    family = gaussian(), prior = normal(), prior_intercept = normal())
m0.init.priors <- prior_summary(m0.init)
p <- length(m0.init.priors$prior$location) + 1

# repeat everything total_num times
for(i in 1:total_num) {
  # generate beta from prior
  beta <- rep(NA, p)
  beta[1] <- rnorm(1, m0.init.priors$prior_intercept$location, m0.init.priors$prior_intercept$adjusted_scale)
  for(j in 2:p) {
    beta[j] <- rnorm(1, m0.init.priors$prior$location[j - 1], m0.init.priors$prior$adjusted_scale[j - 1])
  }
  
  # generate a new dataset
  data <- generate_data(org_data, originals_idx, bp_sigma, beta)
  
  # do all the canonicalization
  # single record proto methods ----
  id_random <- represent(data, mpmms_clust, "proto_random")
  id_minimax <- represent(data, mpmms_clust, "proto_minimax", distance = dist_col_type, col_type = col_type, weights = weights, orders = orders, scale = TRUE)

  # average record methods ----
  df_average <- represent(data, mpmms_clust, "composite", col_type = col_type)
  df_average$sex <- as.character(df_average$sex)

  # pp weighted and thresh ----
  # for each iteration, get the minimax prototypes and count how many times each record is included
  pp_weights <- pp_weights(data, mcmclink, rep_method = "proto_minimax", col_type = col_type, distance = dist_col_type, weights = weights, orders = orders, scale = TRUE, parallel = FALSE)

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
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.prandom), model = "pw_random", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.random), model = "random", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.average), model = "composite", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.thresh), model = "pp_thresh", iter = i))
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.weighted), model = "pp_weighted", iter = i))

  # true data model ----
  m0.true <- stan_glm(bp ~ sex*income, data = data[originals_idx,], seed = 1234,
                      family = gaussian(), prior = normal(), prior_intercept = normal())
  
  # summary of coefs
  m0_coefs <- rbind(m0_coefs, data.frame(format_coefs(m0.true), model = "true", iter = i))
  
  # store betas for later
  names(beta) <- names(m0_coefs)[1:p]
  betas <- rbind(betas, data.frame(t(beta), iter = i))
}

m0_coefs_coverage <- m0_coefs #rename for paper ease later

# save objects ----
save(m0_coefs_coverage, betas, file = paste0(output_folder, data_set, "_dblink_coverage_bpsigma_", noise,".Rdata"))




