## File: section_3.R
## Purpose: Create figures and tables from Section 3 of paper
## Date: 02/03/2022

# TODO ----
# 0. Be sure you have run all code in folders 0_create_data through 3_downstream_task
# 1. denote geco record linkage results folder location
#    where are your lambdas stored as a result of 1_record_linkage/lambdas/get_lambdas.R?
linkage_folder <- "./"
#    where are the raw linkage results stored as a result of 1_record_linkage/geco/geco.conf?
results_folder <- "./"
# 2. denote geco data folder location
#    where is your data stored as a result of 0_create_data/geco/geco.R and 0_create_data/geco/geco_x.R??
data_folder <- "./"
# 3. denote downstream task output folder location
#    where are the canonicalization results stored as a result of 3_downstream_task/geco_coverage.R?
reg_folder <- "./"

## libraries ----
library(tidyverse) # plotting, data manip, etc.
library(knitr) # tables
library(kableExtra)

theme_set(theme_bw(base_family = "serif"))
set.seed(42)

# Section 4 ----
load(paste0(data_folder, "geco_bpsigma_1.Rdata"))
data_1 <- data
test_data_1 <- test_data
identity_1 <- identity
originals_idx_1 <- originals_idx

load(paste0(data_folder, "geco_bpsigma_2.Rdata"))
data_2 <- data
test_data_2 <- test_data
identity_2 <- identity
originals_idx_2 <- originals_idx

load(paste0(data_folder, "geco_bpsigma_5.Rdata"))
data_5 <- data
test_data_5 <- test_data
identity_5 <- identity
originals_idx_5 <- originals_idx

rm(data)
rm(test_data)
rm(identity)
rm(originals_idx)

## Estimated entities
perf <- readLines(paste0(results_folder, "evaluation-results.txt"))
mpmms_error <- 1 - c(as.numeric(stringr::str_extract(perf[5], "\\d+.\\d+")), as.numeric(stringr::str_extract(perf[4], "\\d+.\\d+")))
mcmc_perf <- 1 - mpmms_error
mcmc_sd <- c(NA, NA)

geco_diag <- read.csv(paste0(results_folder, "diagnostics.csv"))
load(paste0(linkage_folder, "geco1_results.Rdata"))

n_ci_geco <- quantile(geco_diag$numObservedEntities, c(.025, .975))

## load downstream results
# noise = 1
load(paste0(reg_folder, "geco_dblink_coverage_bpsigma_1.Rdata"))
df_mse <- mse |> mutate(noise = 1)
kl_div_df <- kl_div %>% mutate(noise = 1)
coverage_df <- m0_coefs_coverage %>% mutate(noise = 1)
betas_df <- betas %>% mutate(noise = 1)

# noise = 2
df_mse <- bind_rows(df_mse,  mse |> mutate(noise = 2))
kl_div_df <- bind_rows(kl_div_df, kl_div %>% mutate(noise = 2))
coverage_df <- bind_rows(coverage_df, m0_coefs_coverage %>% mutate(noise = 2))
betas_df <- bind_rows(betas_df, betas %>% mutate(noise = 2))

# noise = 5
load(paste0(reg_folder, "geco_dblink_coverage_bpsigma_5.Rdata"))
df_mse <- bind_rows(df_mse,  mse |> mutate(noise = 5))
kl_div_df <- bind_rows(kl_div_df, kl_div %>% mutate(noise = 5))
coverage_df <- bind_rows(coverage_df, m0_coefs_coverage %>% mutate(noise = 5))
betas_df <- bind_rows(betas_df, betas %>% mutate(noise = 5))

true_coefs <- data.frame(var = c("X.Intercept.", "sexM", "income", "sexM.income"), true_val = c(160, 10, -1, 0.5), stringsAsFactors = FALSE)

## MSE results
df_mse %>% 
  mutate(model = factor(model, levels = c("random", "average", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  group_by(model, noise) %>%
  dplyr::summarise(sd = sd(mse), mse = mean(mse)) %>%
  mutate(value = ifelse(sd == 0, as.character(round(mse, 2)), paste0(round(mse, 2), " (", scales::number(sd, accuracy = .01), ")"))) %>%
  dplyr::select(-sd) %>%
  group_by(noise) %>%
  dplyr::mutate(min = mse[model != "True"][order(mse[model != "True"])][1]) %>%
  dplyr::mutate(value = ifelse(round(mse, 2) == round(min, 2), paste0("\\textbf{", value, "}"), value)) %>%
  dplyr::select(-min, -mse) %>%
  dplyr::rename(mse = value) -> mse_table

## KL results
kl_div_df %>% 
  dplyr::rename(model = method) %>%
  dplyr::mutate(model = factor(model, levels = c("random", "average", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  group_by(model, noise) %>%
  dplyr::summarise(sd = sd(kl_div), kl_div = mean(kl_div)) %>%
  dplyr::mutate(value = ifelse(sd == 0, as.character(round(kl_div, 2)), paste0(round(kl_div, 4), " (", scales::number(sd, accuracy = .000001), ")"))) %>%
  dplyr::select(-sd) %>%
  group_by(noise) %>%
  dplyr::mutate(min = kl_div[model != "True"][order(kl_div[model != "True"])][1]) %>%
  dplyr::mutate(value = ifelse(round(kl_div, 4) == round(min, 4), paste0("\\textbf{", value, "}"), value)) %>%
  dplyr::select(-min, -kl_div) %>%
  dplyr::rename(kl_div = value) -> kl_div_table

## coverage results
coverage_df %>% 
  dplyr::mutate(model = factor(model, levels = c("random", "composite", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>% 
  dplyr::rename(Model = model) %>%
  gather(var, val, X.Intercept.:sexM.income) %>%
  dplyr::filter(statistic != "mean") %>%
  spread(statistic, val) %>% 
  dplyr::left_join(betas_df %>% gather(var, true_val, -iter, -noise)) %>% 
  dplyr::mutate(covered = true_val >= `5%` & true_val <= `95%`) %>%
  group_by(Model, noise, var) %>%
  dplyr::summarise(coverage = sum(covered)/n()) %>%
  group_by(noise, var) %>%
  dplyr::mutate(max = min(abs(coverage[Model != "True"] - .9))) %>%
  dplyr::mutate(coverage = ifelse(round(abs(coverage - .9), 2) == round(max, 2) & Model != "True", paste0("\\textbf{", round(coverage, 2), "}"), round(coverage, 2))) %>%
  ungroup() %>%
  dplyr::select(-max) -> coverage_table

## Bias results
coverage_df %>% 
  dplyr::mutate(model = factor(model, levels = c("random", "composite", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  gather(var, val, X.Intercept.:sexM.income) %>%
  dplyr::left_join(betas_df %>% gather(var, true_val, -iter, -noise)) %>% 
  dplyr::mutate(var = factor(var, levels = c("X.Intercept.", "sexM", "income", "sexM.income"))) %>%
  dplyr::mutate(bias = val - true_val) %>%
  dplyr::filter(statistic == "mean") %>%
  dplyr::select(-true_val, -val) %>%
  group_by(var, model, noise) %>%
  dplyr::summarise(mean_bias = mean(bias), sd_bias = sd(bias)) -> bias_tmp

bias_tmp %>%
  dplyr::filter(model != "True") %>% 
  group_by(var, noise) %>%
  dplyr::summarise(min_mean_bias = min(abs(mean_bias))) -> min_mean_bias

bias_tmp %>%
  dplyr::left_join(min_mean_bias) %>%
  group_by(var, model, noise) %>%
  dplyr::transmute(bias = ifelse(abs(round(mean_bias, 5)) == round(min_mean_bias, 5),
                                 paste0("\\textbf{", round(mean_bias, 5), " (", scales::number(sd_bias, accuracy = .01), ")}"),
                                 paste0(round(mean_bias, 5), " (", scales::number(sd_bias, accuracy = .01), ")"))) %>%
  dplyr::rename(Model = model) %>%
  ungroup() -> m0_bias_df

## load data x
load(paste0(data_folder, "geco_x_bpsigma_1.Rdata"))
data_1 <- data
test_data_1 <- test_data
identity_1 <- identity
originals_idx_1 <- originals_idx

load(paste0(data_folder, "geco_x_bpsigma_2.Rdata"))
data_2 <- data
test_data_2 <- test_data
identity_2 <- identity
originals_idx_2 <- originals_idx

load(paste0(data_folder, "geco_x_bpsigma_5.Rdata"))
data_5 <- data
test_data_5 <- test_data
identity_5 <- identity
originals_idx_5 <- originals_idx

rm(data)
rm(test_data)
rm(identity)
rm(originals_idx)

## load results x
# noise = 1
load(paste0(reg_folder, "geco_x_dblink_coverage_bpsigma_1.Rdata"))
df_mse <- mse |> mutate(noise = 1)
kl_div_df <- kl_div %>% mutate(noise = 1)
coverage_df <- m0_coefs_coverage %>% mutate(noise = 1)
betas_df <- betas %>% mutate(noise = 1)

# noise = 2
load(paste0(reg_folder, "geco_x_dblink_coverage_bpsigma_2.Rdata"))
df_mse <- bind_rows(df_mse, mse |> mutate(noise = 2))
kl_div_df <- bind_rows(kl_div_df, kl_div %>% mutate(noise = 2))
coverage_df <- bind_rows(coverage_df, m0_coefs_coverage %>% mutate(noise = 2))
betas_df <- bind_rows(betas_df, betas %>% mutate(noise = 2))

# noise = 5
load(paste0(reg_folder, "geco_x_dblink_coverage_bpsigma_5.Rdata"))
df_mse <- bind_rows(df_mse, mse |> mutate(noise = 5))
kl_div_df <- bind_rows(kl_div_df, kl_div %>% mutate(noise = 5))
coverage_df <- bind_rows(coverage_df, m0_coefs_coverage %>% mutate(noise = 5))
betas_df <- bind_rows(betas_df, betas %>% mutate(noise = 5))

## ----format-results-x---------------------------------------------------------
## MSE results ---
df_mse %>% 
  # gather(prototype, mse, -noise) %>%
  mutate(model = factor(model, levels = c("random", "average", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  group_by(model, noise) %>%
  dplyr::summarise(sd = sd(mse), mse = mean(mse)) %>%
  mutate(value = ifelse(sd == 0, as.character(round(mse, 2)), paste0(round(mse, 2), " (", scales::number(sd, accuracy = .01), ")"))) %>%
  dplyr::select(-sd) %>%
  group_by(noise) %>%
  dplyr::mutate(min = mse[model != "True"][order(mse[model != "True"])][1]) %>%
  dplyr::mutate(value = ifelse(round(mse, 2) == round(min, 2), paste0("\\textbf{", value, "}"), value)) %>%
  dplyr::select(-min, -mse) %>%
  dplyr::rename(mse = value) -> mse_table1

## KL results
kl_div_df %>% 
  dplyr::rename(model = method) %>%
  dplyr::mutate(model = factor(model, levels = c("random", "average", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  group_by(model, noise) %>%
  dplyr::summarise(sd = sd(kl_div), kl_div = mean(kl_div)) %>%
  dplyr::mutate(value = ifelse(sd == 0, as.character(round(kl_div, 4)), paste0(round(kl_div, 4), " (", scales::number(sd, accuracy = .000001), ")"))) %>%
  dplyr::select(-sd) %>%
  group_by(noise) %>%
  dplyr::mutate(min = kl_div[model != "True"][order(kl_div[model != "True"])][1]) %>%
  dplyr::mutate(value = ifelse(round(kl_div, 4) == round(min, 4), paste0("\\textbf{", value, "}"), value)) %>%
  dplyr::select(-min, -kl_div) %>%
  dplyr::rename(kl_div = value) -> kl_div_table1

## coverage results
coverage_df %>% 
  dplyr::mutate(model = factor(model, levels = c("random", "composite", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>% 
  dplyr::rename(Model = model) %>%
  gather(var, val, X.Intercept.:sexM.income) %>%
  dplyr::filter(statistic != "mean") %>%
  spread(statistic, val) %>% 
  dplyr::left_join(betas_df %>% gather(var, true_val, -iter, -noise)) %>% 
  dplyr::mutate(covered = true_val >= `5%` & true_val <= `95%`) %>%
  group_by(Model, noise, var) %>%
  dplyr::summarise(coverage = sum(covered)/n()) %>%
  group_by(noise, var) %>%
  dplyr::mutate(max = min(abs(coverage[Model != "True"] - .9))) %>%
  dplyr::mutate(coverage = ifelse(round(abs(coverage - .9), 2) == round(max, 2) & Model != "True", paste0("\\textbf{", round(coverage, 2), "}"), round(coverage, 2))) %>%
  ungroup() %>%
  dplyr::select(-max) -> coverage_table1

## Bias results
coverage_df %>% 
  dplyr::mutate(model = factor(model, levels = c("random", "composite", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  gather(var, val, X.Intercept.:sexM.income) %>%
  dplyr::left_join(betas_df %>% gather(var, true_val, -iter, -noise)) %>% 
  dplyr::mutate(var = factor(var, levels = c("X.Intercept.", "sexM", "income", "sexM.income"))) %>%
  dplyr::mutate(bias = val - true_val) %>%
  dplyr::filter(statistic == "mean") %>%
  dplyr::select(-true_val, -val) %>%
  group_by(var, model, noise) %>%
  dplyr::summarise(mean_bias = mean(bias), sd_bias = sd(bias)) -> bias_tmp

bias_tmp %>%
  dplyr::filter(model != "True") %>% 
  group_by(var, noise) %>%
  dplyr::summarise(min_mean_bias = min(abs(mean_bias))) -> min_mean_bias

bias_tmp %>%
  dplyr::left_join(min_mean_bias) %>%
  group_by(var, model, noise) %>%
  dplyr::transmute(bias = ifelse(abs(round(mean_bias, 5)) == round(min_mean_bias, 5) & model != "True", paste0("\\textbf{", round(mean_bias, 5), " (", scales::number(sd_bias, accuracy = .01), ")}"), paste0(round(mean_bias, 5), " (", scales::number(sd_bias, accuracy = .01), ")"))) %>%
  rename(Model = model) %>%
  ungroup() -> m0_bias_df1

## Table 2 ----
mse_table %>% rename(Model = model)  %>%
  full_join(m0_bias_df, by = c("Model", "noise")) %>%
  full_join(coverage_table, by = c("Model", "noise", "var")) %>%
  full_join(mse_table1 %>% rename(Model = model), by = c("Model", "noise")) %>%
  full_join(m0_bias_df1, by = c("Model", "noise", "var")) %>%
  full_join(coverage_table1, by = c("Model", "noise", "var")) %>%
  ungroup() -> all_m0_df

all_m0_df %>% 
  filter(var == "income") %>% 
  arrange(noise, Model) %>% 
  select(-noise, -var) %>%
  kable("latex", align = "c", booktabs = T, 
        caption = "\\label{tab:m0-bias-coverage-supp} Mean and standard deviation (in parenthesis) for MSE, bias, and coverage of the 90\\% credible interval for income for regression based on five canonicalization methods and the true data set for levels of noise $\\sigma = 1, 2, 5$. Results are based on 100 simulated data sets.", escape = FALSE, 
        col.names = c("Method", rep(c("MSE", "Bias", "Coverage"), 2))) %>%
  pack_rows("$\\\\sigma = 1$", 1, 6, escape = FALSE) %>%
  pack_rows("$\\\\sigma = 2$", 7, 12, escape = FALSE) %>%  
  pack_rows("$\\\\sigma = 5$", 13, 18, escape = FALSE) %>%
  add_header_above(c(" ", "Errors in All Downstream Variables" = 3, "Errors in Explanatory Variables Only" = 3))


