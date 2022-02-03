## File: get_lambdas.R
## Purpose: save object of distribution of linkage for each data set
## Date: 02/03/2022

# pass from command line which results folder (from running dblink)
# Rscript get_lambdas.R geco1_results
args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) stop("Pass in the folder name", call.=FALSE)
results_folder_name <- args[1]

# libraries
library(dplyr)
library(sparklyr) ## manipulate results from dblink
library(sparklyr.nested) ## manipulate results from dblink devtools::install_github("mitre/sparklyr.nested")

# connect to spark
conf <- spark_config()
conf$`sparklyr.shell.driver-memory` <- "16G"
conf$spark.memory.fraction <- 0.9
sc <- spark_connect(master = "local", config = conf)

# try to read in the table
spark_lambda_con <- spark_read_parquet(sc, path = paste(results_folder_name,"linkage-chain.parquet", sep = "/"))
spark_mpmms_con <- spark_read_text(sc, path = paste(results_folder_name,"shared-most-probable-clusters.csv", sep = "/"))

# get the shared mpmms
n_mpmms <- spark_mpmms_con %>% sdf_nrow()

mpmms_lambda <- spark_mpmms_con %>%
  sdf_with_sequential_id(id = "cluster") %>%
  mutate(line = split(line, ", ")) %>% 
  sdf_explode(line, is_map = FALSE) %>%
  rename(record_id = line) %>%
  mutate(record_id = as.numeric(record_id)) %>%
  arrange(record_id) %>% 
  select(cluster) %>%
  collect() %>%
  .$cluster

## need posterior draws
spark_lambda_map <- spark_lambda_con %>% 
  sdf_explode(linkageStructure, is_map = TRUE) %>% # expand the map column
  rename(cluster = key, record_id = value) %>% # cluster label is the key, record id is the value
  select(-partitionId)

spark_lambda <- spark_lambda_map %>% 
  sdf_explode(record_id, is_map = FALSE) %>%  # expand the array
  mutate(record_id = as.numeric(record_id)) # convert record id to number

# iteration vector for looping
lambda_iter <- spark_lambda_map %>%
  select(iteration) %>%
  distinct(iteration) %>%
  collect() %>%
  .$iteration %>%
  sort()
n_lambda_iter <- length(lambda_iter)

rm(spark_lambda_map) # save some space

n_iter_block <- 100
n_iter <- 1000 # 9000 is too many, just look at 1000 of these
lambda <- matrix(NA, nrow = n_iter, ncol = length(mpmms_lambda))
idx <- split(seq_len(n_iter), ceiling(seq_len(n_iter)/n_iter_block)) # split these into blocks, get the last ones
last_idx <- (n_lambda_iter - n_iter + 1):n_lambda_iter

# loop through lambdas in blocks
for(j in seq_along(idx)) {
  # get individual lambda
  iter <- lambda_iter[last_idx[idx[[j]]]]
  lambda_i <- spark_lambda %>%
    filter(iteration %in% iter) %>%
    arrange(iteration, record_id) %>%
    mutate(cluster = as.numeric(cluster)) %>%
    select(iteration, record_id, cluster) %>%
    sdf_pivot(record_id ~ iteration, fun.aggregate = list(cluster = "max")) %>% 
    arrange(record_id) %>%
    select(-record_id) %>%
    collect() %>%
    as.matrix() %>%
    t()
  
  # incremental saves
  write.csv(lambda_i, file = paste0(results_folder_name, ".csv"), append = TRUE, row.names = FALSE, col.names = FALSE)
  
  lambda[idx[[j]],] <- lambda_i
}

# disconnect spark
spark_disconnect(sc)

save(lambda, mpmms_lambda, file = paste0(results_folder_name, ".Rdata"))
