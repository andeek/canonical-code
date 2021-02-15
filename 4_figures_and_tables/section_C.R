## File: section_C.R
## Purpose: Create figures and plots from Section C of appendix.
## Date: 02/15/2021

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

# Section C.2 ----
## Figure 3 ----
caswell_rl_res <- readLines(paste0(results_folder, "evaluation-results.txt"))
caswell_precision <- as.numeric(stringr::str_extract(caswell_rl_res[4], "\\d.\\d+"))
caswell_recall <- as.numeric(stringr::str_extract(caswell_rl_res[5], "\\d.\\d+"))

caswell_diag <- read.csv(paste0(results_folder, "caswell_voters_results/diagnostics.csv"))
load(pasteo(linkage_folder, "caswell_voters_results.Rdata"))

n_ci <- quantile(caswell_diag$numObservedEntities, c(.025, .975))

n_ests <- data.frame(est_type = c("n_map", "ncsbe"),
                     n = c(as.numeric(names(table(caswell_diag$numObservedEntities))[table(caswell_diag$numObservedEntities) == max(table(caswell_diag$numObservedEntities))]), 14740))

caswell_diag %>%
  ggplot() +
  geom_density(aes(numObservedEntities)) +
  labs(x = "Estimated number of voters", y = "Posterior density") -> dens.p

dens.df <- ggplot_build(dens.p)$data[[1]]

dens.p + 
  geom_area(data = dens.df %>% filter(x > n_ci[1] & x < n_ci[2]), aes(x = x, y = y), fill = "grey") +
  geom_vline(aes(xintercept = n, colour = est_type, lty = est_type), data = n_ests) +
  scale_colour_discrete(expression(paste(hat(n), " estimate")), labels = c("MAP", "NCSBE")) +
  scale_linetype_discrete(expression(paste(hat(n), " estimate")), labels = c("MAP", "NCSBE"))


# Section C.3 ----
## Figure 4 ----
diag <- read_csv(paste0(results_folder, "diagnostics.csv"))

diag %>%
  dplyr::select(-`systemTime-ms`) %>%
  gather(-iteration, key = "metric", value = "value") %>%
  ggplot() +
  geom_line(aes(iteration, value, group = metric)) +
  facet_wrap(.~metric, scales = "free_y")
