## File: section_5.R
## Purpose: Create figures and plots from Section 5 of paper.
## Date: 02/21/2021

# TODO ----
# 0. Be sure you have run all code in folders 0_create_data through 3_downstream_task
# 1. denote caswell record linkage results folder location
#    where are your lambdas stored as a result of 1_record_linkage/lambdas/get_lambdas.R?
linkage_folder <- "./"
#    where are the raw linkage results stored as a result of 1_record_linkage/caswell_voters/caswell.conf?
results_folder <- "./"
# 2. denote caswell data folder location
#    where is your data stored as a result of 0_create_data/ncvoter/caswell_voters.R?
data_folder <- "./"
# 3. denote canonicalization output folder location
#    where are the canonicalization results stored as a result of 2_canonicalization/caswell_voters_canonical.R?
canonical_folder <- "./"
# 4. denote downstream task output folder location
#    where are the canonicalization results stored as a result of 3_downstream_task/caswell_voters_reg.R?
reg_folder <- "./"

## libraries ----
library(tidyverse) # plotting, data manip, etc.
library(knitr) # tables
library(kableExtra)
library(ggridges) # ridge plots

theme_set(theme_bw(base_family = "serif"))
set.seed(42)

# Section 5 ----
## caswell-rl-results
caswell_rl_res <- readLines(paste0(results_folder, "evaluation-results.txt"))
caswell_precision <- as.numeric(stringr::str_extract(caswell_rl_res[4], "\\d.\\d+"))
caswell_recall <- as.numeric(stringr::str_extract(caswell_rl_res[5], "\\d.\\d+"))

caswell_diag <- read.csv(paste0(results_folder, "caswell_voters_results/diagnostics.csv"))
load(pasteo(linkage_folder, "caswell_voters_results.Rdata"))

n_ci <- quantile(caswell_diag$numObservedEntities, c(.025, .975))

## Figure 2 ----
# load canonicalization results
load(paste0(canonical_folder, "caswell_voters_proto.Rdata"))

ggplot() + 
  geom_histogram(aes(pp_weights), binwidth = .001) +
  xlab("PC Weights") +
  ylab("") 


## Figure 3 ----
ggplot() + 
  geom_histogram(aes(pp_weights_time), binwidth = .001) +
  xlab("PC Weights with Timestamp Distance") +
  ylab("") 

## Table 5 ----
# load data
# caswell data
load(paste0(data_folder, "caswell_voters.Rdata"))
data <- caswell_voters[caswell_voters$party_cd %in% c("DEM", "REP"),] # only care about democrats and republicans
caswell_voters <- caswell_voters[caswell_voters$party_cd %in% c("DEM", "REP"),]
rownames(data) <- 1:nrow(data) # renumber
rownames(caswell_voters) <- 1:nrow(caswell_voters) # renumber

# truth 
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

## prototyping for caswell
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

data <- caswell_voters[, proto_vars] %>% mutate_at(.vars = proto_vars[col_type != "numeric"], .funs = coalesce, "") ## replace NAs with "" for non numeric variables
test_data <- caswell_test_voters[, proto_vars]
data$birth_place[data$birth_place == ""] <- "99_BLANK"
data$birth_place[data$birth_place == "GU"] <- "99_BLANK" ## this is throwing errors, 4 records with GU

## match proto results approx to truth
data$res_street_addess <- paste(data$house_num, data$street_name, sep = " ")
df_average$res_street_addess <- paste(df_average$house_num, df_average$street_name, sep = " ")
df_average$birth_place <- as.character(df_average$birth_place)
df_average$birth_place[df_average$birth_place == ""] <- "99_BLANK"
df_average$birth_place[df_average$birth_place == "GU"] <- "99_BLANK" ## this is throwing errors, 1 records with GU

kl_random <- emp_kl_div(truth_caswell_p[, vars], data[id_random, vars], cat_vars, num_vars)
kl_minimax <- emp_kl_div(truth_caswell_p[, vars], data[id_minimax, vars], cat_vars, num_vars)
kl_composite <- emp_kl_div(truth_caswell_p[, vars], df_average[, vars], cat_vars, num_vars)
kl_thresh <- emp_kl_div(truth_caswell_p[, vars], data[pp_weights >= 0.5, vars], cat_vars, num_vars)
kl_weight <- emp_kl_div(truth_caswell_p[, vars], data[, vars], cat_vars, num_vars, weights = pp_weights)
kl_thresh_time <- emp_kl_div(truth_caswell_p[, vars], data[pp_weights_time >= 0.5, vars], cat_vars, num_vars)
kl_weight_time <- emp_kl_div(truth_caswell_p[, vars], data[, vars], cat_vars, num_vars, weights = pp_weights_time)

data.frame(prototype = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "PC Weighted (Timestamp)", "PC Threshold (Timestamp)"),
           kl_div = c(kl_random, kl_composite, kl_minimax, kl_weight, kl_thresh, kl_weight_time, kl_thresh_time)) %>%
  mutate(kl_div = round(kl_div, 4)) %>%
  mutate(kl_div = ifelse(kl_div == min(kl_div), paste0("\\textbf{", kl_div, "}"), kl_div)) %>%
  kable(caption = "\\label{tab:caswell_kl_div_table}Evaluation of all canonicalization methods for the NCVD data set in Caswell Country using empirical KL divergence ($\\hat{D}_{KL}$). The lowest empirical KL divergence is indicated in bold.", 
        escape = FALSE, align = c("l", "r"),
        booktabs = TRUE, col.names = c("Method", "$\\hat{D}_{KL}$"), linesep = "") 

## Table 6 ----
est_clusters[[which(uni_clust == 5)[3]]] %>%
  unique() %>%
  rownames() %>%
  as.numeric() -> which_example

est_clusters[[which(uni_clust == 5)[3]]] %>%
  unique() %>%
  mutate(pp_weights = pp_weights[which_example], pp_weights_time = pp_weights_time[which_example]) %>%
  mutate(pp_weights = ifelse(pp_weights >= 0.5, paste0("\\textbf{", pp_weights, "}"), as.character(pp_weights)),
         pp_weights_time = ifelse(pp_weights_time >= 0.5, paste0("\\textbf{", pp_weights_time, "}"), as.character(pp_weights_time))) %>%
  rowwise() %>%
  rename_("First" = "first_name", "Last" = "last_name", "Sex" = "sex",
          "Race" = "race_desc", "Age" = "age", "Party" = "party_cd", 
          "PC Weight" = "pp_weights", "PC Weight (Timestamp)" = "pp_weights_time") %>%
  select(Last, First, Race, Sex, Age, Party, `PC Weight`, `PC Weight (Timestamp)`) %>%
  rename(`\\multicolumn{1}{>{\\centering}p{.9in}}{PC Weight (Timestamp)}` = `PC Weight (Timestamp)`) %>%
  kable(caption = "\\label{tab:ex-records-weights} Five records that represent the same voter with their respective PC weights both with and without timestamp information. PC weights above $\\tau_{PC} = 0.5$ are bolded.", digits = 0, 
        escape = FALSE,
        booktabs = TRUE, align = c("l", "l", "l", "l", "l", "l", "r", "r")) 

## Figure 4 ----
# noise = 0
load(paste0(canonical_folder, "caswell_voters_proto.Rdata"))
pp_weights_df <- data.frame(weights = pp_weights, noise = 0, type = "col_wise")
pp_weights_df <- bind_rows(pp_weights_df, data.frame(weights = pp_weights_time, noise = 0, type = "timestamp"))

load(paste0(reg_folder, "caswell_voters_reg.Rdata"))

auc_dsn %>%
  mutate(Model = factor(Model, 
                        labels = rev(c("Random", "Composite", "Minimax", "PC Threshold", "PC Weighted", "PC Threshold TS", "PC Weighted TS")), 
                        levels = rev(c("Random", "Average", "Minimax", "PC Threshold", "PC Weighted", "PC Threshold TS", "PC Weighted TS")))) %>%
  ggplot(aes(y = Model, x = auc)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), vline_color = "white") +
  xlab("Posterior Test Area Under ROC Curve") + ylab("")

ave_prob_dsn %>%
  mutate(Model = factor(Model, 
                        levels = rev(c("Random", "Composite", "Minimax", "PC Threshold", "PC Weighted", "PC Threshold TS", "PC Weighted TS")))) %>%
  ggplot(aes(y = Model, x = pp)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.025, 0.5, 0.975), vline_color = "white") +
  xlab("Posterior Predicted Pr(DEM | Male)") + ylab("")