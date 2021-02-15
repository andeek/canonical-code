## File: caswell_voters.R
## Purpose: save caswell county data in Rdata format and additional copy for dblink
## Date: 02/15/2021

# TODO ----
# 0. Be sure you have created the caswell_voters db table, see sql scripts in this folder.
# 1. denote ncvoter folder location
#    where is your data stored as a result of caswell_voters_create.sql?
ncvoter_folder <- "./"

## load data
caswell_voters <- read.csv(paste0(ncvoter_folder, "caswell_voters.csv"), header = FALSE, stringsAsFactors = FALSE, na.strings = c("NA", "\\\\0"), allowEscapes = TRUE, strip.white = TRUE)
caswell_test_voters <- read.csv(paste0(ncvoter_folder, "caswell_test_voters.csv"), header = FALSE, stringsAsFactors = FALSE, na.strings = c("NA", "\\\\0"), allowEscapes = TRUE, strip.white = TRUE)
names(caswell_voters) <- names(caswell_test_voters) <- c("db_id", "snapshot_dt", "county_desc", "county_id", "ncid", "last_name", 
                                                       "first_name", "midl_name", "house_num", "street_dir", "street_name", 
                                                       "unit_designator", "res_city_desc", "state_cd", "zip_code", "phone_num", 
                                                       "race_code", "race_desc", "ethnic_code", "ethnic_desc", "party_cd", 
                                                       "party_desc", "sex_code", "sex", "age", "birth_place", "registr_dt", 
                                                       "precinct_abbrv", "load_dt")


# blanks to NA
for(col in names(caswell_voters)) {
  caswell_voters[caswell_voters[, col] %in% c(" ", "  ", "", "\t"), col] <- NA
  caswell_test_voters[caswell_test_voters[, col] %in% c(" ", "  ", "", "\t"), col] <- NA
}

## dblink needs record id
caswell_voters$rec_id <- seq_len(nrow(caswell_voters))
caswell_voters <- caswell_voters[caswell_voters$party_cd %in% c("DEM", "REP"),] # only care about democrats and republicans

## save data
save(caswell_voters, caswell_test_voters, file = paste0(ncvoter_folder, "caswell_voters.Rdata"))

## save clean copy for dlink
write.csv(caswell_voters, "record_linkage/dblink/caswell_voters/caswell_voters.csv", row.names = FALSE)
