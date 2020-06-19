## File: caswell_voters_reg.R
## Purpose: Perform downstream task on caswell voters data, including noisy versions
## Author: Andee Kaplan
## Date: 06/19/2020

# pass from command line which noise level: 0, 0.05, 0.15, 0.30
# Rscript caswell_voters.R 0 ../data/caswell/ ../results/lambdas/ ../results/downstream/
args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 4) stop("Pass in the noise level.", call.=FALSE)
noise <- as.numeric(args[1])
data_folder <- args[2]
linkage_folder <- args[3]
output_folder <- args[4]

## libraries
library(representr)
library(eber)
library(dplyr)
library(tidyr)
library(rstanarm) ## regression

set.seed(1234)

## prototyping for caswell
# load data ----
load(paste0(data_folder, "caswell_voters.Rdata"))
caswell_voters <- caswell_voters[caswell_voters$party_cd %in% c("DEM", "REP"),] # only care about democrats and republicans
rownames(caswell_voters) <- 1:nrow(caswell_voters) # renumber

## load lambda
load(paste0(linkage_folder, "caswell_voters_results.Rdata"))

# get estimated links mpmms
mpmms_clust <- memb2clust(mpmms_lambda)
mpmms_pair <- clust2pairs(memb2clust(mpmms_lambda))

proto_vars <- c("last_name", "first_name", "midl_name", "house_num", "street_name", "race_code", "ethnic_code", "party_cd", "sex_code", "age", "birth_place")
col_type <- c("string", "string", "string", "numeric", "string", "categorical", "categorical", "categorical", "categorical", "numeric", "categorical")
# weights <- rep(1, length(col_type))/length(col_type)
data <- caswell_voters[, proto_vars] %>% mutate_at(.vars = proto_vars[col_type != "numeric"], .funs = coalesce, "") ## replace NAs with "" for non numeric variables
data$birth_place[data$birth_place == ""] <- "99_BLANK"
data$birth_place[data$birth_place == "GU"] <- "99_BLANK" ## this is throwing errors, 4 records with GU

##add noise to data here ----
## 1. select records to add noise to
if(noise > 0) {
  n <- nrow(data)
  n_noise <- ceiling(noise*nrow(data))
  noise_ids <- sample(seq_len(n), n_noise)
  
  ## 2. how many fields to distort
  noise_fields <- sample(1:2, n_noise, replace = TRUE)
  
  ## 3. add noise to age and party 
  ## these are the columns not involved with record linkage.
  ## 1 response and 1 explanatory
  data[noise_ids,][noise_fields == 2, "party_cd"] <- ifelse(data[noise_ids,][noise_fields == 2, "party_cd"] == "REP", "DEM", "REP")
  data[noise_ids,][noise_fields == 2, "age"] <- round(data[noise_ids,][noise_fields == 2, "age"] + rnorm(sum(noise_fields == 2), 0, sd(data$age)))
  data[noise_ids,][noise_fields == 2, "age"] <- ifelse(data[noise_ids,][noise_fields == 2, "age"] < 18 | data[noise_ids,][noise_fields == 2, "age"] > 102, ifelse(data[noise_ids,][noise_fields == 2, "age"] < 18, 18, 102), data[noise_ids,][noise_fields == 2, "age"])
  
  ## add noise to those that only change 1 var
  noise_col <- sample(c("party_cd", "age"), sum(noise_fields == 1), replace = TRUE)
  data[noise_ids,][noise_fields == 1,][noise_col == "party_cd", "party_cd"] <- ifelse(data[noise_ids,][noise_fields == 1,][noise_col == "party_cd", "party_cd"] == "REP", "DEM", "REP")
  data[noise_ids,][noise_fields == 1,][noise_col == "age", "age"] <- round(data[noise_ids,][noise_fields == 1,][noise_col == "age", "age"] + rnorm(sum(noise_col == "age"), 0, sd(data$age)))
  data[noise_ids,][noise_fields == 1,][noise_col == "age", "age"] <- ifelse(data[noise_ids,][noise_fields == 1,][noise_col == "age", "age"] < 18 | data[noise_ids,][noise_fields == 1,][noise_col == "age", "age"] > 102, ifelse(data[noise_ids,][noise_fields == 1,][noise_col == "age", "age"] < 18, 18, 102), data[noise_ids,][noise_fields == 1,][noise_col == "age", "age"])
}

# single record proto methods ----
id_minimax <- represent(data, mpmms_lambda, "proto_minimax", distance = dist_col_type, col_type = col_type, scale = TRUE, parallel = FALSE)
id_random <- represent(data, mpmms_lambda, "proto_random")

# average record methods ----
df_average <- represent(data, mpmms_lambda, "composite", col_type = col_type)

# posterior weights
pp_weights <- pp_weights(data, lambda, "proto_minimax", distance = dist_col_type, col_type = col_type, scale = TRUE)

# take time into account
dist_col_type_time <- function(a, b, col_type, string_dist = utils::adist, weights = rep(1/length(a), length(a)), orders = NULL, limits = NULL, ...) {
  ## error handling
  if(length(a) != length(b))
    stop("Records must be the same number of columns.")
  if(length(col_type) != length(a))
    stop("Must have column type for each column.")
  if(!all(names(table(col_type)) %in% c("categorical", "ordinal", "string", "numeric", "time")))
    stop("Column type must be 'categorical', 'ordinal', 'string', 'numeric', opr 'time'")
  if(class(string_dist) != "function")
    stop("string_dist must be a function.")
  if(length(weights) != length(a)) {
    stop("Weights must be of same length as number of columns")
  } else if(!identical(round(sum(weights), 15), 1)) {
    stop("Weights must sum to 1.")
  }
  if("ordinal" %in% col_type) {
    if(sum(col_type == "ordinal") != length(orders)) stop("Please provide a named list containing the order of the levels in each ordinal column.")
    
    if(names(a)[col_type == "ordinal"] != names(orders)) stop("Orders must be a named list corresponding to the levels in each ordinal column.")
    
    for(idx in which(col_type == "ordinal")) {
      if(!all(c(a[,idx], b[,idx]) %in% orders[[names(a)[idx]]])) stop("Orders must be a named list corresponding to the levels in each ordinal column.")
    }
  }
  if("time" %in% col_type) {
    if(sum(col_type == "time") != length(limits)) stop("Please provide a named list containing the limits of each time column.")
    
    if(names(a)[col_type == "time"] != names(limits)) stop("Limits must be a named list corresponding to the the limits of each time column.")
    
    for(lim in limits) {
      if(length(lim) != 2) {
        stop("Limits  must be a named list corresponding to the the limits (max and min) of each time column.")
      } else if(lim[2] < lim[1]) {
        stop("Limits  must be a named list corresponding to the the limits (min, max) of each time column. ")
      }
    }
  }
  
  ## combine all column type distances
  p <- length(a)
  dist <- 0
  for(i in seq_len(p)) {
    if(col_type[i] == "numeric")
      # numeric: weighted absolute difference in values
      dist <- dist + weights[i]*abs(a[[i]] - b[[i]])
    else if(col_type[i] == "categorical")
      # categorical: weighted binary distance
      dist <- dist + weights[i]*(a[[i]] != b[[i]])
    else if(col_type[i] == "ordinal") {
      # ordinal: look up absolute level distance
      ord <- orders[[names(a)[i]]]
      dist <- dist + weights[i]*abs(which(ord == a[[i]]) - which(ord == b[[i]]))
    } else if(col_type[i] == "time") {
      lim <- limits[[names(a)[i]]]
      dist <- dist + weights[i]*(2*lim[2] - a[[i]] - b[[i]])/(2*(lim[2] - lim[1]))
    }else
      # string: weighted string distance
      dist <- dist + weights[i]*string_dist(as.character(a[[i]]), as.character(b[[i]]), ...)
  }
  return(dist)
}

proto_vars <- c(proto_vars, "db_id")
col_type <- c(col_type, "time")
weights <- c(rep(1, length(col_type) - 1)/(length(col_type) - 1), .5) # time is weighted much higher than the rest, over 5x
limits <- list(db_id = range(caswell_voters$db_id))

data <- data.frame(data, db_id = caswell_voters$db_id)
test_data <- caswell_test_voters[, proto_vars]

pp_weights_time <- pp_weights(data, lambda, "proto_minimax", distance = dist_col_type_time, col_type = col_type, weights = weights, limits = limits, scale = TRUE)

# save results
save(id_random, id_minimax, df_average, pp_weights, pp_weights_time, file = paste0(output_folder, "caswell_voters_proto_", noise, ".Rdata"))

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

save(data_pred_results, test_data_pred_results, m1_coefs, res_kl_div,
     file = paste0(output_folder, "caswell_voters_reg_noise_", noise,".Rdata"))



