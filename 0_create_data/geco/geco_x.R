## File: geco_x.R
## Purpose: save geco_x data in Rdata format (no dups in y)
## Date: 06/19/2020

# TODO ----
# 0. Be sure you have run geco.R
# 1. create output folder location
output_folder <- "./"

## don't duplicate the response!
# pass from command line which noise level, 1, 2, or 5
# Rscript blink_script_geco.R 1
args <- commandArgs(trailingOnly=TRUE)
if (length(args)==0) stop("Pass in the noise level (1, 2, or 5) and error calculation flag.", call.=FALSE)
if (!(args[1] %in% c(1, 2, 5))) stop("Pass in the duplication level (1, 3, or 5)", call.=FALSE)
noise <- args[1]

set.seed(1022)

# load geco data ----
load(paste0(output_folder, "geco_bpsigma_", noise, ".Rdata"))

# make dup y's not distorted
for(id in unique(identity)) {
  data[identity == id, "bp"] <- data[originals_idx[identity[originals_idx] == id], "bp"]
  data[identity == id, "high_bp"] <- data[originals_idx[identity[originals_idx] == id], "high_bp"]
}

save(data, identity, originals_dup, originals_idx, test_data, file = paste0(output_folder, "geco_x_bpsigma_", noise, ".Rdata"))

