## File: section_1.R
## Purpose: Create figures and plots from Section 1 of paper.
## Date: 02/03/2022

# TODO ----
# 0. Be sure you have run all code in folders 0_create_data through 3_downstream_task
# 1. denote data folder location
data_folder <- "./"

## libraries ----
library(tidyverse) # plotting, data manip, etc.
library(knitr) # tables
library(kableExtra)

theme_set(theme_bw(base_family = "serif"))
set.seed(42)

## ----caswell-data- ----
#example data
load(paste0(data_folder, "caswell_voters.Rdata"))
data <- caswell_voters[caswell_voters$party_cd %in% c("DEM", "REP"),] # only care about democrats and republicans
caswell_voters <- caswell_voters[caswell_voters$party_cd %in% c("DEM", "REP"),]
rownames(data) <- 1:nrow(data) # renumber
rownames(caswell_voters) <- 1:nrow(caswell_voters) # renumber

vars <- c("last_name", "first_name", "race_desc", "party_cd", "sex", "age")

est_clusters <- split(data[, vars], data$ncid)

clust <- unlist(lapply(est_clusters, nrow))
uni_clust <- unlist(lapply(est_clusters, function(x) nrow(unique(x))))

## Table 1 ----
est_clusters[[which(uni_clust == 5)[1]]] %>%
  unique() %>%
  rowwise() %>%
  rename_("First" = "first_name", "Last" = "last_name", "Sex" = "sex",
          "Race" = "race_desc", "Age" = "age", "Party" = "party_cd") %>%
  select(Last, First, Race, Sex, Age, Party) %>%
  kable(caption = "\\label{tab:ex-records} Five records that represent the same voter in the NCSBE data set.", digits = 0, booktabs = TRUE, row.names = FALSE)
