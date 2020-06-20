## File: section_B.R
## Purpose: Create figures and plots from Section B of appendix.
## Date: 06/19/2020

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

theme_set(theme_bw(base_family = "serif"))
set.seed(42)

# Section B.2 ----
## Figure 1 ----
diag <- read_csv(paste0(results_folder, "diagnostics.csv"))

diag %>%
  dplyr::select(-`systemTime-ms`) %>%
  gather(-iteration, key = "metric", value = "value") %>%
  ggplot() +
  geom_line(aes(iteration, value, group = metric)) +
  facet_wrap(.~metric, scales = "free_y")

# Section B.3 ----
# noise = ,05
load(paste0(canonical_folder, "caswell_voters_proto_0.05.Rdata"))
pp_weights_df <- bind_rows(pp_weights_df, data.frame(weights = pp_weights, noise = .05, type = "col_wise"))
pp_weights_df <- bind_rows(pp_weights_df, data.frame(weights = pp_weights_time, noise = .05, type = "timestamp"))

load(paste0(reg_folder, "caswell_voters_reg_noise_0.05.Rdata"))
m1_coefs_df <- bind_rows(m1_coefs_df, data.frame(m1_coefs, noise = .05))
kl_div_df <- bind_rows(kl_div_df, data.frame(res_kl_div, noise = .05))
data_pred_df <- bind_rows(data_pred_df, data.frame(data_pred_results, noise = .05))
test_data_pred_df <- bind_rows(test_data_pred_df, data.frame(test_data_pred_results, noise = .05))

# noise = ,1
load(paste0(canonical_folder, "caswell_voters_proto_0.1.Rdata"))
pp_weights_df <- bind_rows(pp_weights_df, data.frame(weights = pp_weights, noise = .1, type = "col_wise"))
pp_weights_df <- bind_rows(pp_weights_df, data.frame(weights = pp_weights_time, noise = .1, type = "timestamp"))

load(paste0(reg_folder, "caswell_voters_reg_noise_0.1.Rdata"))
m1_coefs_df <- bind_rows(m1_coefs_df, data.frame(m1_coefs, noise = .1))
kl_div_df <- bind_rows(kl_div_df, data.frame(res_kl_div, noise = .1))
data_pred_df <- bind_rows(data_pred_df, data.frame(data_pred_results, noise = .1))
test_data_pred_df <- bind_rows(test_data_pred_df, data.frame(test_data_pred_results, noise = .1))

# noise = ,15
load(paste0(canonical_folder, "caswell_voters_proto_0.15.Rdata"))
pp_weights_df <- bind_rows(pp_weights_df, data.frame(weights = pp_weights, noise = .15, type = "col_wise"))
pp_weights_df <- bind_rows(pp_weights_df, data.frame(weights = pp_weights_time, noise = .15, type = "timestamp"))

load(paste0(reg_folder, "caswell_voters_reg_noise_0.15.Rdata"))
m1_coefs_df <- bind_rows(m1_coefs_df, data.frame(m1_coefs, noise = .15))
kl_div_df <- bind_rows(kl_div_df, data.frame(res_kl_div, noise = .15))
data_pred_df <- bind_rows(data_pred_df, data.frame(data_pred_results, noise = .15))
test_data_pred_df <- bind_rows(test_data_pred_df, data.frame(test_data_pred_results, noise = .15))

# noise = ,3
load(paste0(canonical_folder, "caswell_voters_proto_0.3.Rdata"))
pp_weights_df <- bind_rows(pp_weights_df, data.frame(weights = pp_weights, noise = .3, type = "col_wise"))
pp_weights_df <- bind_rows(pp_weights_df, data.frame(weights = pp_weights_time, noise = .3, type = "timestamp"))

load(paste0(reg_folder, "caswell_voters_reg_noise_0.3.Rdata"))
m1_coefs_df <- bind_rows(m1_coefs_df, data.frame(m1_coefs, noise = .3))
kl_div_df <- bind_rows(kl_div_df, data.frame(res_kl_div, noise = .3))
data_pred_df <- bind_rows(data_pred_df, data.frame(data_pred_results, noise = .3))
test_data_pred_df <- bind_rows(test_data_pred_df, data.frame(test_data_pred_results, noise = .3))

## Table 2 ----
kl_div_df %>%
  filter(noise %in% c(.05, .15)) %>%
  group_by(noise) %>%
  dplyr::mutate(min_kl = min(kl_div)) %>%
  dplyr::mutate(kl_div = ifelse(round(kl_div, 4) == round(min_kl, 4), paste0("\\textbf{", round(kl_div, 4), "}"), round(kl_div, 4))) %>%
  select(noise, prototype, kl_div) %>%
  rename(Method = prototype) -> kl_div_tbl

test_data_pred_df %>%
  filter(noise %in% c(.05, .15)) %>%
  group_by(model, noise) %>%
  summarise(mean_prec = mean(precision), sd_prec = sd(precision), mean_recall = mean(recall), sd_recall = sd(recall)) %>%
  dplyr::mutate(Method = factor(model, levels = c("random", "composite", "minimax", "pp_weighted", "pp_thresh", "pp_weighted_time", "pp_thresh_time"), labels = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "PC Weighted (TS)", "PC Threshold (TS)"))) %>%
  dplyr::mutate(Precision = paste0(round(mean_prec, 4), " (", scales::number(sd_prec, accuracy = .001), ")")) %>%
  dplyr::mutate(Recall = paste0(round(mean_recall, 4), " (", scales::number(sd_recall, accuracy = .001), ")")) %>%
  group_by(noise) %>%
  dplyr::mutate(max_prec = max(mean_prec), max_recall = max(mean_recall)) %>%
  dplyr::mutate(Precision = ifelse(round(mean_prec, 4) == round(max_prec, 4), paste0("\\textbf{", Precision, "}"), Precision)) %>%
  dplyr::mutate(Recall = ifelse(round(mean_recall, 4) == round(max_recall, 4), paste0("\\textbf{", Recall, "}"), Recall)) %>%
  ungroup() %>%
  select(noise, Method:Recall) %>%
  left_join(kl_div_tbl) %>%
  dplyr::mutate(Method = factor(Method, levels = c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "PC Weighted (TS)", "PC Threshold (TS)"))) %>%
  dplyr::arrange(noise, Method) %>%
  select(Method, kl_div, Precision, Recall) %>%
  kable("latex", booktabs = T, escape = FALSE, align = c("l", "r", "r", "r"),
        caption = "\\label{tab:prec_recall_3}Empirical KL Divergence, posterior mean and standard deviation (in parentheses) of precision and recall of out-of-sample prediction for each canonicalization method after adding noise to different amounts of the Caswell Voter data. The lowest $\\hat{D}_{KL}$ and highest precision and recall for each metric within each noise level are highlighted in bold.",
        col.names = c("Method", "$\\hat{D}_{KL}$", "Precision", "Recall")) %>%
  pack_rows("5\\\\% Noise", 1, 7, escape = FALSE) %>%
  pack_rows("15\\\\% Noise", 8, 14, escape = FALSE) 

## Figure 2 ----
m1_coefs_df %>%
  filter(noise > 0) %>%
  select(X.Intercept.:ethnic_codeUN, statistic, model, noise) %>%
  gather(var, value, X.Intercept.:ethnic_codeUN) %>%
  spread(statistic, value) %>%
  rename(LB = `2.5%`, UB = `97.5%`) %>% 
  mutate(signif = LB*UB > 0) %>%
  mutate(model = factor(model, 
                        levels = rev(c("random", "composite", "minimax", "pp_weighted", "pp_thresh", "pp_weighted_time", "pp_thresh_time")),
                        labels = rev(c("Random", "Composite", "Minimax", "PC Weighted", "PC Threshold", "PC Weighted (TS)", "PC Threshold (TS)")))) %>%
  mutate(var = factor(var, 
                      levels = c("age", "ethnic_codeNL", "ethnic_codeUN", "race_codeB", "race_codeI", "race_codeM", "race_codeO", "race_codeU", "race_codeW", "sex_codeM", "sex_codeU", "X.Intercept."),
                      labels = c("Age", "EthnicNL", "EthnicUN", "RaceB", "RaceI", "RaceM", "RaceO", "RaceU", "RaceW", "SexM", "SexU", "Intercept"))) %>%
  mutate(noise = factor(noise, labels = paste0(unique(noise)*100, "% Noise"))) %>%
  filter(var != "Intercept") %>%
  ggplot() +
  geom_segment(aes(x = LB, xend = UB, y = model, yend = model, colour = signif)) +
  geom_point(aes(x = mean, y = model, colour = signif)) +
  facet_grid(noise~var, scales = "free_x") +
  scale_color_manual(expression(0 %notin% CI), values = c("black", "red")) +
  ylab("") + xlab("95% CI") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))

