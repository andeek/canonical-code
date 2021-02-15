## File: caswell_voters_reg.R
## Purpose: Perform downstream task on caswell voters data
## Date: 02/15/2021

# TODO ----
# 0. Be sure the record linkage results and canonicalization results have completed for caswell county.
# 1. denote caswell record linkage results folder location
#    where are your lambdas stored as a result of 1_record_linkage/lambdas/get_lambdas.R?
linkage_folder <- "./"
# 2. denote caswell data folder location
#    where is your data stored as a result of 0_create_data/ncvoter/caswell_voters.R?
data_folder <- "./"
# 3. denote canonicalization output folder location
#    where are the canonicalization results stored as a result of 2_canonicalization/caswell_voters_canonical.R?
canonical_folder <- "./"
# 4. denote output folder location
#    where to save the results of this script?
output_folder <- "./"

## libraries
library(representr)
library(dplyr)
library(tidyr)
library(rstanarm) ## regression
library(BayesPostEst) ## posterior analysis

set.seed(1234)

# load data ----
load(paste0(data_folder, "caswell_voters.Rdata"))
caswell_voters <- caswell_voters[caswell_voters$party_cd %in% c("DEM", "REP"),] # only care about democrats and republicans
rownames(caswell_voters) <- 1:nrow(caswell_voters) # renumber

## load lambda
load(paste0(linkage_folder, "caswell_voters_results.Rdata"))

# get estimated links mpmms
# functions for working with mpmms
memb2clust <- function(membership, include.singletons=FALSE) {
  membership.freqs <- table(membership)
  inv.index <- vector(mode="list", length=length(membership.freqs))
  names(inv.index) <- names(membership.freqs)
  if (is.null(names(membership))) {
    named.membership = FALSE
  } else {
    named.membership = TRUE
  }
  for (i in seq_along(membership)) {
    membership.id <- as.character(membership[i])
    if (named.membership) {val.i <- names(membership)[i]} else {val.i <- i}
    inv.index[[membership.id]] <- c(inv.index[[membership.id]],val.i)
  }
  if (!include.singletons) {
    sizes <- sapply(inv.index, length)
    inv.index <- inv.index[sizes > 1]
  }
  return(inv.index)
}
clust2pairs <- function(clusters) {
  pairs <- list()
  for (i in seq_along(clusters)) {
    clust.i <- clusters[[i]]
    n.i <- length(clust.i)
    if (n.i == 2) {
      # set is already a pair
      pairs[[length(pairs)+1]] <- clust.i
    } else if (n.i > 2) {
      # break up cluster into pairs
      for(j1 in seq(1, n.i-1)) {
        for(j2 in seq(j1+1, n.i)) {
          pairs[[length(pairs)+1]] <- c(clust.i[j1],clust.i[j2])
        }
      }
    }
  }
  return(pairs)
}
mpmms_clust <- memb2clust(mpmms_lambda)
mpmms_pair <- clust2pairs(memb2clust(mpmms_lambda))

# prep data
proto_vars <- c("last_name", "first_name", "midl_name", "house_num", "street_name", "race_code", "ethnic_code", "party_cd", "sex_code", "age", "birth_place")
data <- caswell_voters[, proto_vars] %>% mutate_at(.vars = proto_vars[col_type != "numeric"], .funs = coalesce, "") ## replace NAs with "" for non numeric variables
data$birth_place[data$birth_place == ""] <- "99_BLANK"
data$birth_place[data$birth_place == "GU"] <- "99_BLANK" ## this is throwing errors, 4 records with GU

test_data <- caswell_test_voters[, proto_vars]

# load prototyping results
load(paste0(canonical_folder, "caswell_voters_proto.Rdata"))

# regression models ----
# fit models of party on demographic info
data$party_bin <- as.integer(data$party_cd == "DEM") # All outcome values must be 0 or 1 for Bernoulli models.
test_data$party_bin <- as.integer(test_data$party_cd == "DEM") # All outcome values must be 0 or 1 for Bernoulli models.
df_average$party_bin <- as.integer(df_average$party_cd == "DEM") # All outcome values must be 0 or 1 for Bernoulli models.

# get rid of factor columns
for(i in seq_along(df_average)) {
  if(class(df_average[, i]) == "factor") df_average[, i] <- as.character(df_average[, i])
}

# regression models
m1.minimax <- stan_glm(party_bin ~ sex_code + age + race_code + ethnic_code, data = data[unlist(id_minimax),], seed = 1234,
                       family = binomial(link = "logit"), prior = normal(), prior_intercept = normal())
m1.random <- stan_glm(party_bin ~ sex_code + age + race_code + ethnic_code, data = data[unlist(id_random),], seed = 1234,
                      family = binomial(link = "logit"), prior = normal(), prior_intercept = normal())
m1.average <- stan_glm(party_bin ~ sex_code + age + race_code + ethnic_code, data = df_average, seed = 1234,
                       family = binomial(link = "logit"), prior = normal(), prior_intercept = normal())
m1.weighted <- stan_glm(party_bin ~ sex_code + age + race_code + ethnic_code, data = data, seed = 1234, weights = pp_weights,
                        family = binomial(link = "logit"), prior = normal(), prior_intercept = normal())
m1.thresh <- stan_glm(party_bin ~ sex_code + age + race_code + ethnic_code, data = data[pp_weights >= 0.5,], seed = 1234,
                      family = binomial(link = "logit"), prior = normal(), prior_intercept = normal())
m1.weighted_time <- stan_glm(party_bin ~ sex_code + age + race_code + ethnic_code, data = data, seed = 1234, weights = pp_weights_time,
                        family = binomial(link = "logit"), prior = normal(), prior_intercept = normal())
m1.thresh_time <- stan_glm(party_bin ~ sex_code + age + race_code + ethnic_code, data = data[pp_weights_time >= 0.5,], seed = 1234,
                      family = binomial(link = "logit"), prior = normal(), prior_intercept = normal())

# store results
x_vars <- c("sex_code", "age", "race_code", "ethnic_code")

# look at posterior dsns of precision and recall
pred_vals <- function(model, y, newdata = NULL, draws = 1000, seed = 1234) {
  if(is.null(newdata)) newdata <- model$model
  preds <- posterior_predict(model, newdata = newdata, draws = draws, seed = seed)
  confusion <- apply(preds, 1, function(pred) {
    df <- data.frame(as.data.frame(table(pred, y)), stat = c("tn", "fp", "fn", "tp"))
    spread(df[, -(1:2)], stat, Freq)
  })
  do.call(rbind, confusion)
}

## store results ----
mutate(pred_vals(m1.random, data$party_bin, data[, x_vars]), model = "random") %>%
  bind_rows(mutate(pred_vals(m1.minimax, data$party_bin, data[, x_vars]), model = "minimax")) %>%
  bind_rows(mutate(pred_vals(m1.average, data$party_bin, data[, x_vars]), model = "composite")) %>%
  bind_rows(mutate(pred_vals(m1.weighted, data$party_bin, data[, x_vars]), model = "pp_weighted")) %>%
  bind_rows(mutate(pred_vals(m1.thresh, data$party_bin, data[, x_vars]), model = "pp_thresh")) %>%
  bind_rows(mutate(pred_vals(m1.weighted_time, data$party_bin, data[, x_vars]), model = "pp_weighted_time")) %>%
  bind_rows(mutate(pred_vals(m1.thresh_time, data$party_bin, data[, x_vars]), model = "pp_thresh_time")) %>%
  mutate(precision = tp/(tp + fp), recall = tp/(tp + fn)) -> data_pred_results

mutate(pred_vals(m1.random, test_data$party_bin, test_data[, x_vars]), model = "random") %>%
  bind_rows(mutate(pred_vals(m1.minimax, test_data$party_bin, test_data[, x_vars]), model = "minimax")) %>%
  bind_rows(mutate(pred_vals(m1.average, test_data$party_bin, test_data[, x_vars]), model = "composite")) %>%
  bind_rows(mutate(pred_vals(m1.weighted, test_data$party_bin, test_data[, x_vars]), model = "pp_weighted")) %>%
  bind_rows(mutate(pred_vals(m1.thresh, test_data$party_bin, test_data[, x_vars]), model = "pp_thresh")) %>%
  bind_rows(mutate(pred_vals(m1.weighted_time, test_data$party_bin, test_data[, x_vars]), model = "pp_weighted_time")) %>%
  bind_rows(mutate(pred_vals(m1.thresh_time, test_data$party_bin, test_data[, x_vars]), model = "pp_thresh_time")) %>%
  mutate(precision = tp/(tp + fp), recall = tp/(tp + fn)) -> test_data_pred_results

# format function ----
format_coefs <- function(stan_model) {
  model_summary <- data.frame(t(as.data.frame(summary(stan_model))))
  model_summary$statistic <- rownames(model_summary)
  rownames(model_summary) <- NULL
  data.frame(model_summary[model_summary$statistic %in% c("mean", "2.5%", "97.5%"), ])
}

## coefs ----
m1_coefs <- data.frame()
m1_coefs <- rbind(m1_coefs, data.frame(format_coefs(m1.minimax), model = "minimax"))
m1_coefs <- rbind(m1_coefs, data.frame(format_coefs(m1.random), model = "random"))
m1_coefs <- rbind(m1_coefs, data.frame(format_coefs(m1.average), model = "composite"))
m1_coefs <- rbind(m1_coefs, data.frame(format_coefs(m1.thresh), model = "pp_thresh"))
m1_coefs <- rbind(m1_coefs, data.frame(format_coefs(m1.weighted), model = "pp_weighted"))
m1_coefs <- rbind(m1_coefs, data.frame(format_coefs(m1.thresh_time), model = "pp_thresh_time"))
m1_coefs <- rbind(m1_coefs, data.frame(format_coefs(m1.weighted_time), model = "pp_weighted_time"))

## kl_div
temp <- tempfile()
download.file("https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvoter17.zip", temp)
truth_caswell <- read.delim(unz(temp, "ncvoter17.txt"), stringsAsFactors = FALSE)
unlink(temp)

## match column names and values
keep_vars <- c("last_name", "first_name", "middle_name", "res_street_address", "race_code", "ethnic_code", "party_cd", "gender_code", "birth_age", "birth_state")

truth_caswell_p <- truth_caswell[truth_caswell$party_cd %in% c("DEM", "REP") & truth_caswell$voter_status_desc %in% c("ACTIVE", "INACTIVE"), keep_vars]
truth_caswell_p <- droplevels(truth_caswell_p)
rm(truth_caswell)

names(truth_caswell_p)[3] <- "midl_name"
names(truth_caswell_p)[8] <- "sex_code"
names(truth_caswell_p)[9] <- "age"
names(truth_caswell_p)[10] <- "birth_place"

truth_caswell_p$birth_place[truth_caswell_p$birth_place == ""] <- "99_BLANK"

cat_vars <- c("race_code", "ethnic_code", "party_cd", "sex_code", "birth_place")
num_vars <- c("age")
vars <- c(cat_vars, num_vars)

kl_random <- emp_kl_div(truth_caswell_p[, vars], data[id_random, vars], cat_vars, num_vars)
kl_minimax <- emp_kl_div(truth_caswell_p[, vars], data[id_minimax, vars], cat_vars, num_vars)
kl_composite <- emp_kl_div(truth_caswell_p[, vars], df_average[, vars], cat_vars, num_vars)
kl_thresh <- emp_kl_div(truth_caswell_p[, vars], data[pp_weights >= 0.5, vars], cat_vars, num_vars)
kl_weight <- emp_kl_div(truth_caswell_p[, vars], data[, vars], cat_vars, num_vars, weights = pp_weights)
kl_thresh_time <- emp_kl_div(truth_caswell_p[, vars], data[pp_weights_time >= 0.5, vars], cat_vars, num_vars)
kl_weight_time <- emp_kl_div(truth_caswell_p[, vars], data[, vars], cat_vars, num_vars, weights = pp_weights_time)

data.frame(prototype = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "PC Weighted (Timestamp)", "PC Threshold (Timestamp)"),
           kl_div = c(kl_random, kl_composite, kl_minimax, kl_weight, kl_thresh, kl_weight_time, kl_thresh_time)) -> res_kl_div

## AUC functions ----
test_auc <- function(y, p) {
  res <- data.frame()
  for(cutoff in unique(p)) {
    conf <- table(pred = factor(p > cutoff, levels = c(FALSE, TRUE)),true = y)
    res <- rbind(res, data.frame(p = cutoff, tpr = conf[2, 2]/(conf[2, 2] + conf[1, 2]), fpr = conf[2, 1]/(conf[2, 1] + conf[1, 1])))  
  }
  res <- res[order(res$fpr),]
  
  ## estimate area under the curve with Simpson's rule
  sum(diff(res$fpr) * res$tpr[-1])
}

test_auc_dsn <- function(model, y, newdata, draws = 1000, seed = 1234) {
  # predicted probabilities
  preds <- posterior_epred(model, newdata = newdata, draws = draws, seed = seed)
  
  # return dsn
  apply(preds, 1, test_auc, y = y)
}

## get AUC ----
b <- 1000

auc_dsn <- data.frame(Model = "Random", auc = test_auc_dsn(m1.random, test_data$party_bin, test_data[, x_vars], draws = b)) %>%
  bind_rows(data.frame(Model = "Average", auc = test_auc_dsn(m1.average, test_data$party_bin, test_data[, x_vars], draws = b))) %>%
  bind_rows(data.frame(Model = "Minimax", auc = test_auc_dsn(m1.minimax, test_data$party_bin, test_data[, x_vars], draws = b))) %>%
  bind_rows(data.frame(Model = "PC Threshold", auc = test_auc_dsn(m1.thresh, test_data$party_bin, test_data[, x_vars], draws = b))) %>%
  bind_rows(data.frame(Model = "PC Weighted", auc = test_auc_dsn(m1.weighted, test_data$party_bin, test_data[, x_vars], draws = b))) %>%
  bind_rows(data.frame(Model = "PC Threshold TS", auc = test_auc_dsn(m1.thresh_time, test_data$party_bin, test_data[, x_vars], draws = b))) %>%
  bind_rows(data.frame(Model = "PC Weighted TS", auc = test_auc_dsn(m1.weighted_time, test_data$party_bin, test_data[, x_vars], draws = b)))

## posterior average probabilities ----
## model matrix
mm <- model.matrix(party_bin ~ sex_code + age + race_code + ethnic_code, data = test_data)

## function to get the predicted probability at sex = male while all other covariates are held at a "typical" value
## King, Gary, Michael Tomz, and Jason Wittenberg. 2000. "Making the Most of Statistical Analyses: Improving Interpretation and Presentation." American Journal of Political Science 44 (2): 347–61. https://doi.org/10.2307/2669316.
get_ave_prob <- function(model, mm, xcol = 2, xrange = 1) {
  mcmc_mat <- as.matrix(model)
  mcmcAveProb(modelmatrix = mm, mcmcout = mcmc_mat[, 1:ncol(mm)], xcol = xcol, xrange = xrange, link = "logit", ci = c(0.025, 0.975), fullsims = TRUE)
}

ave_prob_dsn <- data.frame(Model = "Random", get_ave_prob(m1.random, mm)) %>%
  bind_rows(data.frame(Model = "Composite", get_ave_prob(m1.average, mm))) %>%
  bind_rows(data.frame(Model = "Minimax", get_ave_prob(m1.minimax, mm))) %>%
  bind_rows(data.frame(Model = "PC Threshold", get_ave_prob(m1.thresh, mm))) %>%
  bind_rows(data.frame(Model = "PC Weighted", get_ave_prob(m1.weighted, mm))) %>%
  bind_rows(data.frame(Model = "PC Threshold TS", get_ave_prob(m1.thresh_time, mm))) %>%
  bind_rows(data.frame(Model = "PC Weighted TS", get_ave_prob(m1.weighted_time, mm)))

## save results ----
save(data_pred_results, test_data_pred_results, m1_coefs, res_kl_div, auc_dsn, ave_prob_dsn,
     file = paste0(output_folder, "caswell_voters_reg.Rdata"))


