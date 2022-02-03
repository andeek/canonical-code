## File: caswell_voters_canonical.R
## Purpose: perform canonicalization on caswell voter data
## Date: 02/03/2022

# TODO ----
# 0. Be sure the record linkage results have completed for caswell county.
# 0. Be sure to run collect the lambdas using 1_record_linkage/lambdas/get_lambdas.R
# 1. denote caswell record linkage results folder location
#    where are your lambdas stored as a result of 1_record_linkage/lambdas/get_lambdas.R?
linkage_folder <- "./"
# 2. denote caswell data folder location
#    where is your data stored as a result of 0_create_data/ncvoter/caswell_voters.R?
data_folder <- "./"
# 3. denote output folder location
#    where to save the results of this script?
output_folder <- "./"

## libraries
library(representr)

## prototyping for caswell
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

proto_vars <- c("last_name", "first_name", "midl_name", "house_num", "street_name", "race_code", "ethnic_code", "party_cd", "sex_code", "age", "birth_place")
col_type <- c("string", "string", "string", "numeric", "string", "categorical", "categorical", "categorical", "categorical", "numeric", "categorical")
# weights <- rep(1, length(col_type))/length(col_type)
data <- caswell_voters[, proto_vars] %>% mutate_at(.vars = proto_vars[col_type != "numeric"], .funs = coalesce, "") ## replace NAs with "" for non numeric variables
test_data <- caswell_test_voters[, proto_vars]
data$birth_place[data$birth_place == ""] <- "99_BLANK"
data$birth_place[data$birth_place == "GU"] <- "99_BLANK" ## this is throwing errors, 4 records with GU

# single record proto methods ----
id_minimax <- represent(data, mpmms_lambda, "proto_minimax", distance = dist_col_type, col_type = col_type, weights = weights, scale = TRUE, parallel = FALSE)
id_random <- represent(data, mpmms_lambda, "proto_random")

# average record methods ----
df_average <- represent(data, mpmms_lambda, "composite", col_type = col_type)

# posterior weights
pp_weights <- pp_weights(data, lambda, "proto_minimax", distance = dist_col_type, col_type = col_type, weights = weights, scale = TRUE)

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

proto_vars <- c("last_name", "first_name", "midl_name", "house_num", "street_name", "race_code", "ethnic_code", "party_cd", "sex_code", "age", "birth_place", "db_id")
col_type <- c("string", "string", "string", "numeric", "string", "categorical", "categorical", "categorical", "categorical", "numeric", "categorical", "time")
weights <- c(rep(1, length(col_type) - 1)/(length(col_type) - 1), .5) # time is weighted much higher than the rest, over 5x
limits <- list(db_id = range(caswell_voters$db_id))

data <- caswell_voters[, proto_vars] %>% mutate_at(.vars = proto_vars[!col_type %in% c("numeric", "time")], .funs = coalesce, "") ## replace NAs with "" for non numeric variables
test_data <- caswell_test_voters[, proto_vars]
data$birth_place[data$birth_place == ""] <- "99_BLANK"
data$birth_place[data$birth_place == "GU"] <- "99_BLANK" ## this is throwing errors, 4 records with GU

## realign db id to upweight newer databases
data$db_id <- limits$db_id[2] - data$db_id + 1

pp_weights_time <- pp_weights(data, lambda, "proto_minimax", distance = dist_col_type_time, col_type = col_type, weights = weights, limits = limits, scale = TRUE)

# save results
save(id_random, id_minimax, df_average, pp_weights, pp_weights_time, file = paste0(output_folder, "caswell_voters_proto.Rdata"))

