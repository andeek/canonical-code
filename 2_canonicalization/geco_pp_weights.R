## File: geco_pp_weights.R
## Purpose: get distribution of PC weights for each record
## Date: 06/19/2020

# pass from command line which noise level, 1, 2, or 5
# Rscript geco_pp_weights 1 geco ../data/geco/ ../results/lambdas/ ../results/canonical/
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
library(representr)

# reproducible
set.seed(1234)


# load data ----
load(paste0(data_folder, data_set, "_sim/geco_bpsigma_", noise, ".Rdata"))
org_data <- data # store for later

# load dblink results ----
load(paste0(linkage_folder, "geco1_results.Rdata"))

# posterior probs ----
mcmclink <- lambda

# prototyping ----
# params for the proto functions
col_type <- c("string", "string", "numeric", "numeric", "numeric", "categorical", "ordinal", "numeric", "numeric", "categorical")
weights <- c(.2, .2, .05, .05, .05, .05, .05, .15, .15, .05)
orders <- list(education = c("Less than a high school diploma", "High school graduates, no college", "Some college or associate degree", "Bachelor's degree only", "Advanced degree"))

# params ----
total_num <- 100

# store results
pp_weights <- matrix(NA, ncol = nrow(org_data), nrow = total_num)

# repeat everything total_num times
for(i in 1:total_num) {
  cat(paste("iter: ", i, "\r"))
  
  # pp weighted and thresh ----
  # for each iteration, get the minimax prototypes and count how many times each record is included
  pp_weights[i, ] <- pp_weights(org_data, mcmclink, rep_method = "proto_minimax", col_type = col_type, distance = dist_col_type, weights = weights, orders = orders, scale = TRUE, parallel = TRUE)
}


# save objects ----
save(pp_weights, file = paste0(output_folder, data_set, "_dblink_pp_weights_bpsigma_", noise,".Rdata"))




