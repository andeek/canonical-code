## File: section_C.R
## Purpose: Create figures and tables from Section C of appendix.
## Date: 02/03/2022

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

# Section C.3 ----
## Figure 2 ----
diag <- read_csv(paste0(results_folder, "diagnostics.csv"))

diag %>%
  dplyr::select(-`systemTime-ms`) %>%
  gather(-iteration, key = "metric", value = "value") %>%
  ggplot() +
  geom_line(aes(iteration, value, group = metric)) +
  facet_wrap(.~metric, scales = "free_y")
