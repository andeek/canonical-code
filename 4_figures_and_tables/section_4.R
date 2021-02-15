## File: section_4.R
## Purpose: Create figures and plots from Section 4 of paper
## Date: 02/15/2021

# TODO ----
# 0. Be sure you have run all code in folders 0_create_data through 3_downstream_task
# 1. denote geco record linkage results folder location
#    where are your lambdas stored as a result of 1_record_linkage/lambdas/get_lambdas.R?
linkage_folder <- "./"
#    where are the raw linkage results stored as a result of 1_record_linkage/caswell_voters/caswell.conf?
results_folder <- "./"
# 2. denote geco data folder location
#    where is your data stored as a result of 0_create_data/geco/geco.R and 0_create_data/geco/geco_x.R??
data_folder <- "./"
# 3. denote canonicalization output folder location
#    where are the canonicalization results stored as a result of 2_canonicalization/geco_pp_weights.R?
canonical_folder <- "./"
# 4. denote downstream task output folder location
#    where are the canonicalization results stored as a result of 3_downstream_task/caswell_voters_reg.R?
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

## Figure 1 ----
load(paste0(canonical_folder, "geco_dblink_pp_weights_bpsigma_1.Rdata"))
pp_weights_df <- data.frame(iter = 1:100, pp_weights) %>%
  gather(record_id, pp_weight, -iter) %>%
  separate(record_id, into = c("junk", "record_id"), sep = "X") %>%
  select(-junk) %>%
  mutate(record_id = as.numeric(record_id), noise = 1)

load(paste0(canonical_folder, "geco_dblink_pp_weights_bpsigma_2.Rdata"))
pp_weights_df <- data.frame(iter = 1:100, pp_weights) %>%
  gather(record_id, pp_weight, -iter) %>%
  separate(record_id, into = c("junk", "record_id"), sep = "X") %>%
  select(-junk) %>%
  mutate(record_id = as.numeric(record_id), noise = 2) %>%
  bind_rows(pp_weights_df)

load(paste0(canonical_folder, "geco_dblink_pp_weights_bpsigma_5.Rdata"))
pp_weights_df <- data.frame(iter = 1:100, pp_weights) %>%
  gather(record_id, pp_weight, -iter) %>%
  separate(record_id, into = c("junk", "record_id"), sep = "X") %>%
  select(-junk) %>%
  mutate(record_id = as.numeric(record_id), noise = 5) %>%
  bind_rows(pp_weights_df)

pp_weights_df %>%
  group_by(record_id) %>%
  summarise(mean_pp_weight = mean(pp_weight)) %>%
  right_join(pp_weights_df, by = c("record_id")) %>%
  mutate(dup = ! record_id %in% originals_idx) %>% 
  ungroup() %>%
  dplyr::mutate(record_id = factor(record_id)) %>%
  dplyr::mutate(record_id = fct_reorder(record_id, mean_pp_weight)) %>%
  dplyr::mutate(noise = paste0(expression("sigma[epsilon]=="), noise)) -> pp_weights_df

pp_weights_df %>%
  ggplot() +
  geom_boxplot(aes(record_id, pp_weight, fill = dup, colour = dup)) +
  geom_hline(aes(yintercept = 0.5), lty = 2) +
  facet_grid(noise~., labeller = label_parsed) +
  theme(legend.position = "bottom",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  xlab("Record") + ylab("PC Weight Distribution (100 repetitions)") +
  scale_colour_discrete("Duplicate") +
  scale_fill_discrete("Duplicate")

pp_weights_df %>%
  group_by(noise, iter) %>%
  summarise(n = sum(pp_weight >= 0.5)) %>%
  group_by(noise) %>%
  summarise(mean_n = mean(n), median_n = median(n), sd_n = sd(n), ub_n = quantile(n, .975), lb_n = quantile(n, .025)) -> summary_n

## load downstream results
# noise = 1
load(paste0(reg_folder, "geco_dblink_bpsigma_1.Rdata"))
load(paste0(reg_folder, "geco_dblink_coverage_bpsigma_1.Rdata"))
df_mse_1 <- df_mse
kl_div_df <- kl_div %>% mutate(noise = 1)
m0_coefs_df <- m0_coefs %>% mutate(noise = 1)
coverage_df <- m0_coefs_coverage %>% mutate(noise = 1)
betas_df <- betas %>% mutate(noise = 1)

# noise = 2
load(paste0(reg_folder, "geco_dblink_bpsigma_2.Rdata"))
load(paste0(reg_folder, "geco_dblink_coverage_bpsigma_2.Rdata"))
df_mse_2 <- df_mse
kl_div_df <- bind_rows(kl_div_df, kl_div %>% mutate(noise = 2))
m0_coefs_df <- bind_rows(m0_coefs_df, m0_coefs %>% mutate(noise = 2))
coverage_df <- bind_rows(coverage_df, m0_coefs_coverage %>% mutate(noise = 2))
betas_df <- bind_rows(betas_df, betas %>% mutate(noise = 2))

# noise = 5
load(paste0(reg_folder, "geco_dblink_bpsigma_5.Rdata"))
load(paste0(reg_folder, "geco_dblink_coverage_bpsigma_5.Rdata"))
df_mse_5 <- df_mse
kl_div_df <- bind_rows(kl_div_df, kl_div %>% mutate(noise = 5))
m0_coefs_df <- bind_rows(m0_coefs_df, m0_coefs %>% mutate(noise = 5))
coverage_df <- bind_rows(coverage_df, m0_coefs_coverage %>% mutate(noise = 5))
betas_df <- bind_rows(betas_df, betas %>% mutate(noise = 5))

true_coefs <- data.frame(var = c("X.Intercept.", "sexM", "income", "sexM.income"), true_val = c(160, 10, -1, 0.5), stringsAsFactors = FALSE)

## organize and format results
# combine results
df_mse <- bind_rows(df_mse_1 %>% mutate(noise = 1),
                    df_mse_2 %>% mutate(noise = 2),
                    df_mse_5 %>% mutate(noise = 5))

## MSE results
df_mse %>% 
  gather(prototype, mse, -noise) %>%
  mutate(prototype = factor(prototype, levels = c("pw_random", "pw_average", "pw_minimax", "random", "average", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("PW Random", "PW Composite", "PW Minimax", "Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  filter(substr(prototype, 1, 2) != "PW") %>%
  group_by(prototype, noise) %>%
  dplyr::summarise(sd = sd(mse), mse = mean(mse)) %>%
  mutate(value = ifelse(sd == 0, as.character(round(mse, 2)), paste0(round(mse, 2), " (", scales::number(sd, accuracy = .0001), ")"))) %>%
  dplyr::select(-sd) %>%
  group_by(noise) %>%
  dplyr::mutate(min = mse[prototype != "True"][order(mse[prototype != "True"])][1]) %>%
  dplyr::mutate(value = ifelse(round(mse, 2) == round(min, 2), paste0("\\textbf{", value, "}"), value)) %>%
  dplyr::select(-min, -mse) %>%
  dplyr::rename(mse = value) -> mse_table

## KL results
kl_div_df %>% 
  dplyr::rename(prototype = method) %>%
  dplyr::mutate(prototype = ifelse(prototype == "paverage", "pw_average", 
                                   ifelse(prototype == "pminimax", "pw_minimax", 
                                          ifelse(prototype == "prandom", "pw_random", prototype)))) %>%
  dplyr::mutate(prototype = factor(prototype, levels = c("pw_random", "pw_average", "pw_minimax", "random", "average", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("PW Random", "PW Composite", "PW Minimax", "Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  dplyr::filter(substr(prototype, 1, 2) != "PW") %>%
  group_by(prototype, noise) %>%
  dplyr::summarise(sd = sd(kl_div), kl_div = mean(kl_div)) %>%
  dplyr::mutate(value = ifelse(sd == 0, as.character(round(kl_div, 4)), paste0(round(kl_div, 4), " (", scales::number(sd, accuracy = .000001), ")"))) %>%
  dplyr::select(-sd) %>%
  group_by(noise) %>%
  dplyr::mutate(min = kl_div[prototype != "True"][order(kl_div[prototype != "True"])][1]) %>%
  dplyr::mutate(value = ifelse(round(kl_div, 4) == round(min, 4), paste0("\\textbf{", value, "}"), value)) %>%
  dplyr::select(-min, -kl_div) %>%
  dplyr::rename(kl_div = value) -> kl_div_table

## coverage results
coverage_df %>%
  dplyr::select(X.Intercept.:sexM.income, statistic, model, iter, noise) %>%
  dplyr::mutate(model = factor(model, levels = c("pw_random", "pw_composite", "pw_minimax", "random", "composite", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("PW Random", "PW Composite", "PW Minimax", "Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>% 
  dplyr::filter(substr(model, 1, 2) != "PW") %>%
  dplyr::rename(Model = model) %>%
  gather(var, val, X.Intercept.:sexM.income) %>%
  dplyr::filter(statistic != "mean") %>%
  spread(statistic, val) %>% 
  dplyr::left_join(betas_df %>% gather(var, true_val, -iter, -noise)) %>% 
  dplyr::mutate(covered = true_val >= `2.5%` & true_val <= `97.5%`) %>%
  group_by(Model, noise, var) %>%
  dplyr::summarise(coverage = sum(covered)/n()) %>%
  group_by(noise, var) %>%
  dplyr::mutate(max = min(abs(coverage[Model != "True"] - .95))) %>%
  dplyr::mutate(coverage = ifelse(round(abs(coverage - .95), 2) == round(max, 2) & Model != "True", paste0("\\textbf{", round(coverage, 2), "}"), round(coverage, 2))) %>%
  ungroup() %>%
  dplyr::select(-max) -> coverage_table

## Bias results
m0_coefs_df %>% 
  dplyr::mutate(model = factor(model, levels = c("pw_random", "pw_composite", "pw_minimax", "random", "composite", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("PW Random", "PW Composite", "PW Minimax", "Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  dplyr::filter(substr(model, 1, 2) != "PW") %>%
  dplyr::select(X.Intercept.:sexM.income, statistic, model, iter, noise) %>%
  gather(var, val, X.Intercept.:sexM.income) %>%
  dplyr::left_join((true_coefs)) %>%
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
  dplyr::transmute(bias = ifelse(abs(round(mean_bias, 2)) == round(min_mean_bias, 2), 
                                 paste0("\\textbf{", round(mean_bias, 2), " (", scales::number(sd_bias, accuracy = .00001), ")}"), 
                                 paste0(round(mean_bias, 2), " (", scales::number(sd_bias, accuracy = .00001), ")"))) %>%
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
load(paste0(reg_folder, "geco_x_dblink_bpsigma_1.Rdata"))
load(paste0(reg_folder, "geco_x_dblink_coverage_bpsigma_1.Rdata"))
df_mse_1 <- df_mse
kl_div_df <- kl_div %>% mutate(noise = 1)
m0_coefs_df <- m0_coefs %>% mutate(noise = 1)
coverage_df <- m0_coefs_coverage %>% mutate(noise = 1)
betas_df <- betas %>% mutate(noise = 1)

# noise = 2
load(paste0(reg_folder, "geco_x_dblink_bpsigma_2.Rdata"))
load(paste0(reg_folder, "geco_x_dblink_coverage_bpsigma_2.Rdata"))
df_mse_2 <- df_mse
kl_div_df <- bind_rows(kl_div_df, kl_div %>% mutate(noise = 2))
m0_coefs_df <- bind_rows(m0_coefs_df, m0_coefs %>% mutate(noise = 2))
coverage_df <- bind_rows(coverage_df, m0_coefs_coverage %>% mutate(noise = 2))
betas_df <- bind_rows(betas_df, betas %>% mutate(noise = 2))

# noise = 5
load(paste0(reg_folder, "geco_x_dblink_bpsigma_5.Rdata"))
load(paste0(reg_folder, "geco_x_dblink_coverage_bpsigma_5.Rdata"))
df_mse_5 <- df_mse
kl_div_df <- bind_rows(kl_div_df, kl_div %>% mutate(noise = 5))
m0_coefs_df <- bind_rows(m0_coefs_df, m0_coefs %>% mutate(noise = 5))
coverage_df <- bind_rows(coverage_df, m0_coefs_coverage %>% mutate(noise = 5))
betas_df <- bind_rows(betas_df, betas %>% mutate(noise = 5))


## ----organize-rl-results-x----------------------------------------------------
# combine results for results table
df_mse <- bind_rows(df_mse_1 %>% mutate(noise = 1),
                    df_mse_2 %>% mutate(noise = 2),
                    df_mse_5 %>% mutate(noise = 5))

## ----format-results-x---------------------------------------------------------
## MSE results ---
df_mse %>% 
  gather(prototype, mse, -noise) %>%
  dplyr::mutate(prototype = factor(prototype, levels = c("pw_random", "pw_average", "pw_minimax", "random", "average", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("PW Random", "PW Composite", "PW Minimax", "Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  dplyr::filter(substr(prototype, 1, 2) != "PW") %>%
  group_by(prototype, noise) %>%
  dplyr::summarise(sd = sd(mse), mse = mean(mse)) %>%
  dplyr::mutate(value = ifelse(sd == 0, as.character(round(mse, 2)), paste0(round(mse, 2), " (", scales::number(sd, accuracy = .00001), ")"))) %>%
  dplyr::select(-sd) %>%
  group_by(noise) %>%
  dplyr::mutate(min = mse[prototype != "True"][order(mse[prototype != "True"])][1]) %>%
  dplyr::mutate(value = ifelse(round(mse, 2) == round(min, 2) & prototype != "True", paste0("\\textbf{", value, "}"), value)) %>%
  dplyr::select(-min, -mse) %>%
  dplyr::rename(mse = value) -> mse_table1

## KL results
kl_div_df %>% 
  rename(prototype = method) %>%
  mutate(prototype = ifelse(prototype == "paverage", "pw_average", 
                            ifelse(prototype == "pminimax", "pw_minimax", 
                                   ifelse(prototype == "prandom", "pw_random", prototype)))) %>%
  mutate(prototype = factor(prototype, levels = c("pw_random", "pw_average", "pw_minimax", "random", "average", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("PW Random", "PW Composite", "PW Minimax", "Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  filter(substr(prototype, 1, 2) != "PW") %>%
  group_by(prototype, noise) %>%
  summarise(sd = sd(kl_div), kl_div = mean(kl_div)) %>%
  mutate(value = ifelse(sd == 0, as.character(round(kl_div, 4)), paste0(round(kl_div, 4), " (", scales::number(sd, .000001), ")"))) %>%
  dplyr::select(-sd) %>%
  group_by(noise) %>%
  mutate(min = kl_div[prototype != "True"][order(kl_div[prototype != "True"])][1]) %>%
  mutate(value = ifelse(round(kl_div, 4) == round(min, 4) & prototype != "True", paste0("\\textbf{", value, "}"), value)) %>%
  dplyr::select(-min, -kl_div) %>%
  rename(kl_div = value) -> kl_div_table1

## coverage results
coverage_df %>%
  select(X.Intercept.:sexM.income, statistic, model, iter, noise) %>%
  mutate(model = factor(model, levels = c("pw_random", "pw_composite", "pw_minimax", "random", "composite", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("PW Random", "PW Composite", "PW Minimax", "Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  filter(substr(model, 1, 2) != "PW") %>%
  rename(Model = model) %>%
  gather(var, val, X.Intercept.:sexM.income) %>%
  filter(statistic != "mean") %>%
  spread(statistic, val) %>%
  left_join(betas_df %>% gather(var, true_val, -iter, -noise)) %>% 
  mutate(covered = true_val >= `2.5%` & true_val <= `97.5%`) %>%
  group_by(Model, noise, var) %>%
  summarise(coverage = sum(covered)/n()) %>%
  group_by(noise, var) %>%
  mutate(max = min(abs(coverage[Model != "True"] - .95))) %>%
  mutate(coverage = ifelse(round(abs(coverage - .95), 2) == round(max, 2) & Model != "True", paste0("\\textbf{", round(coverage, 2), "}"), round(coverage, 2))) %>%
  ungroup() %>%
  select(-max) -> coverage_table1

## Bias results
m0_coefs_df %>% 
  dplyr::mutate(model = factor(model, levels = c("pw_random", "pw_composite", "pw_minimax", "random", "composite", "minimax", "pp_weighted", "pp_thresh", "true"), labels = c("PW Random", "PW Composite", "PW Minimax", "Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "True"))) %>%
  dplyr::filter(substr(model, 1, 2) != "PW") %>%
  dplyr::select(X.Intercept.:sexM.income, statistic, model, iter, noise) %>%
  gather(var, val, X.Intercept.:sexM.income) %>%
  dplyr::left_join((true_coefs)) %>%
  dplyr::mutate(var = factor(var, levels = c("X.Intercept.", "sexM", "income", "sexM.income"))) %>%
  dplyr::mutate(bias = val - true_val) %>%
  filter(statistic == "mean") %>%
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
  dplyr::transmute(bias = ifelse(abs(round(mean_bias, 2)) == round(min_mean_bias, 2) & model != "True", paste0("\\textbf{", round(mean_bias, 2), " (", scales::number(sd_bias, accuracy = .00001), ")}"), paste0(round(mean_bias, 2), " (", scales::number(sd_bias, accuracy = .00001), ")"))) %>%
  rename(Model = model) %>%
  ungroup() -> m0_bias_df1

## Table 3 ----
kl_div_table %>%
  full_join(kl_div_table1, by = c("prototype", "noise")) %>%
  ungroup() %>%
  mutate_if(is.character, coalesce, "0") -> kl_div_df

kl_div_df %>% arrange(noise, prototype) %>% select(-noise) %>%
  kable("latex", align = "c", booktabs = T, 
        caption = "\\label{tab:kl_div_table_2} Mean and standard deviation (in parenthesis) of KL divergence for all five canonicalization methods in two data scenarios -- errors in all downstream variables (left) and errors in explanatory variables only (right) -- for three noise levels, $\\sigma = 1, 2, 5$. The evaluation metrics are based on 100 representative data sets generated for each method.", escape = FALSE, 
        col.names = c("Method", rep(c("$\\hat{D}_{KL}$"), 2))) %>%
  pack_rows("$\\\\sigma_{\\\\epsilon} = 1$", 1, 5, escape = FALSE) %>%
  pack_rows("$\\\\sigma_{\\\\epsilon} = 2$", 6, 10, escape = FALSE) %>%  
  pack_rows("$\\\\sigma_{\\\\epsilon} = 5$", 11, 15, escape = FALSE) %>%
  add_header_above(c(" ", "Errors in All Downstream Variables" = 1, "Errors in Explanatory Variables Only" = 1))

## Table 4 ----
mse_table %>% rename(Model = prototype) %>%
  full_join(m0_bias_df, by = c("Model", "noise")) %>%
  full_join(coverage_table, by = c("Model", "noise", "var")) %>%
  full_join(mse_table1 %>% rename(Model = prototype), by = c("Model", "noise")) %>%
  full_join(m0_bias_df1, by = c("Model", "noise", "var")) %>%
  full_join(coverage_table1, by = c("Model", "noise", "var")) %>%
  ungroup() -> all_m0_df

all_m0_df %>% 
  filter(var == "income") %>% 
  arrange(noise, Model) %>% 
  select(-noise, -var) %>%
  kable("latex", align = "c", booktabs = T, 
        caption = "\\label{tab:m0-bias-coverage-supp} Mean and standard deviation (in parenthesis) for MSE and bias, and coverage of the 95\\% credible interval of the regression coefficient for income for five canonicalization methods and the true data set for levels of noise $\\sigma = 1, 2, 5$. Results are based on 100 representative data sets generated for each method.", escape = FALSE, 
        col.names = c("Method", rep(c("MSE", "Bias", "Coverage"), 2))) %>%
  pack_rows("$\\\\sigma_{\\\\epsilon} = 1$", 1, 6, escape = FALSE) %>%
  pack_rows("$\\\\sigma_{\\\\epsilon} = 2$", 7, 12, escape = FALSE) %>%  
  pack_rows("$\\\\sigma_{\\\\epsilon} = 5$", 13, 18, escape = FALSE) %>%
  add_header_above(c(" ", "Errors in All Downstream Variables" = 3, "Errors in Explanatory Variables Only" = 3))




