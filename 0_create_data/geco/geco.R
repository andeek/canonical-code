## File: geco.R
## Purpose: save geco data in Rdata format and additional copy for dblink
## Author: Andee Kaplan
## Date: 06/19/2020

# TODO ----
# 0. Be sure you have generated the names files with generate-names.py and generate-test-names.py
# 1. first download and save education in from 
#    https://census.gov/data/tables/2017/demo/education-attainment/cps-detailed-tables.html
#    as education.xlsx in this folder
# 2. create output folder location
output_folder <- "./"

# libraries ----
library(dplyr)
library(tidyr)
library(babynames)
library(readxl)
library(rvest)
library(ggplot2)

set.seed(1022)

# this takes values 1, 2, 5
bp_sigma <- 5

# load names/birthdate data ----
# obtained from running generate-names.py and generate-test-names.py
dat <- read.csv("names.csv")
test_dat <- read.csv("test-names.csv")

# remove dup
test_dat <- test_dat[unlist(lapply(stringr::str_split(test_dat$rec.id, "-"), function(x) x[3])) != "dup",]

# add variables ----
# add sex according to first name
dat %>%
  separate(rec.id, into = c("rec", "id", "type"), extra = "drop") %>%
  separate(birthdate, into = c("bm", "bd", "by")) %>%
  mutate_at(c("bd", "bm", "by"), as.numeric) %>%
  filter(type == "org") %>%
  left_join(babynames %>% mutate(name = tolower(name)), by = c("fname" = "name")) %>%
  group_by(fname, by) %>%
  filter(abs(year - by) == min(abs(year - by))) %>%
  filter(prop == max(prop)) %>% # get the highest prob gender for each name/year combo
  select(rec, id, type, fname, lname, bd, bm, by, sex) -> dat_sex

test_dat %>%
  separate(rec.id, into = c("rec", "id", "type"), extra = "drop") %>%
  separate(birthdate, into = c("bm", "bd", "by")) %>%
  mutate_at(c("bd", "bm", "by"), as.numeric) %>%
  filter(type == "org") %>%
  left_join(babynames %>% mutate(name = tolower(name)), by = c("fname" = "name")) %>%
  group_by(fname, by) %>%
  filter(abs(year - by) == min(abs(year - by))) %>%
  filter(prop == max(prop)) %>% # get the highest prob gender for each name/year combo
  select(rec, id, type, fname, lname, bd, bm, by, sex) -> test_dat_sex

# some names do not appear in the baby names dataset, for these randomly sample gender
dat %>%
  separate(rec.id, into = c("rec", "id", "type"), extra = "drop") %>%
  separate(birthdate, into = c("bm", "bd", "by")) %>%
  mutate_at(c("bd", "bm", "by"), as.numeric) %>%
  filter(type == "org") %>%
  filter(as.numeric(id) %in% setdiff(seq(0, 349), as.numeric(names(table(dat_sex$id))))) %>%
  mutate(sex = sample(c("F", "M"), n(), replace = TRUE)) %>%
  bind_rows(dat_sex) -> dat_sex

test_dat %>%
  separate(rec.id, into = c("rec", "id", "type"), extra = "drop") %>%
  separate(birthdate, into = c("bm", "bd", "by")) %>%
  mutate_at(c("bd", "bm", "by"), as.numeric) %>%
  filter(type == "org") %>%
  filter(as.numeric(id) %in% setdiff(seq(0, 499), as.numeric(names(table(test_dat_sex$id))))) %>%
  mutate(sex = sample(c("F", "M"), n(), replace = TRUE)) %>%
  bind_rows(test_dat_sex) -> test_dat_sex

# add education level according to us census props (gender + age)
# https://census.gov/data/tables/2017/demo/education-attainment/cps-detailed-tables.html
edu_freq <- read_excel("education.xlsx", skip = 5)
names(edu_freq)[1] <- "age"

# get the gender separated numbers
edu_freq %>%
  mutate(sex = ifelse(row_number() > which(edu_freq$age == "Female"), "F", ifelse(row_number() > which(edu_freq$age == "Male"), "M", NA))) %>%
  filter(substr(age, 1, 2) == ".." & sex %in% c("M", "F")) %>%
  gather(education, freq, -age, -Total, -sex) %>%
  # combine levels and match income
  mutate(education = ifelse(education %in% c("None", "1st - 4th grade", "5th - 6th grade", 
                                             "7th - 8th grade", "9th grade", "10th grade", 
                                             "11th grade2"), "Less than a high school diploma", 
                            ifelse(education %in% c("Associate's degree, occupational", 
                                                    "Associate's degree, academic",
                                                    "Some college, no degree"), 
                                   "Some college or associate degree", 
                                   ifelse(education %in% c("High school graduate"),
                                          "High school graduates, no college",
                                          ifelse(education %in% c("Bachelor's degree"),
                                                 "Bachelor's degree only", 
                                                 ifelse(education %in% c("Doctoral degree", 
                                                                         "Master's degree", "Professional degree"), 
                                                        "Advanced degree", NA)))))) %>%
  group_by(age, sex, education) %>%
  summarise(freq = sum(freq)) %>%
  group_by(age, sex) %>%
  mutate(age_sex_total = sum(freq)) %>%
  mutate(age_sex_prop = freq/age_sex_total) %>%
  mutate(age_min = as.numeric(gsub("[..]", "", gsub("([0-9]+).*$", "\\1", age)))) %>%
  mutate(age_max = as.numeric(gsub(".* to ([0-9]+) years$", "\\1", age))) -> edu_freq_lookup

edu_freq_lookup[is.na(edu_freq_lookup$age_max), "age_max"] <- Inf # no max age for largest bucket

# use the lookup to add education levels randomly by proportion
dat_sex %>%
  left_join(edu_freq_lookup) %>%
  filter(2017 - by >= age_min & 2017 - by <= age_max) %>%
  group_by(id) %>%
  sample_n(1, weight = age_sex_prop) %>%
  select(id, type, fname, lname, bd, bm, by, sex, education) -> dat_sex_edu

test_dat_sex %>%
  left_join(edu_freq_lookup) %>%
  filter(2017 - by >= age_min & 2017 - by <= age_max) %>%
  group_by(id) %>%
  sample_n(1, weight = age_sex_prop) %>%
  select(id, type, fname, lname, bd, bm, by, sex, education) -> test_dat_sex_edu


# add quant variables - income + bloodpressure

# income from Labor Force Statistics from the Current Population Survey
# https://www.bls.gov/opub/ted/2015/median-weekly-earnings-by-education-gender-race-and-ethnicity-in-2014.htm

read_html("https://www.bls.gov/opub/ted/2015/median-weekly-earnings-by-education-gender-race-and-ethnicity-in-2014.htm") %>%
  html_node("table") %>%
  html_table() %>% # get table
  rename(education = `Education level`) %>% # ugly col name
  select(education, Men, Women) %>%
  gather(sex, median_income, -education) %>% # tidy form
  mutate(sex = ifelse(sex == "Men", "M", "F")) %>% # reformat sex
  mutate(median_income = 52*as.numeric(gsub("[$]", "", gsub(",", "", median_income)))) %>% #yearly income
  mutate(median_income = median_income/1000) %>%
  filter(education != "Total, all education levels") -> income_lookup # remove total

income_lookup %>% 
  right_join(dat_sex_edu) %>%
  rowwise() %>%
  mutate(income = round(rnorm(1, median_income, sd = 5))) %>%
  select(id, type, fname, lname, bd, bm, by, sex, education, income) -> dat_sex_edu_income

income_lookup %>% 
  right_join(test_dat_sex_edu) %>%
  rowwise() %>%
  mutate(income = round(rnorm(1, median_income, sd = 5))) %>%
  select(id, type, fname, lname, bd, bm, by, sex, education, income) -> test_dat_sex_edu_income


# systolic blood pressue from sex and income
# "true" relationship is bp = b0 + b1*I(male) + b2*log(income) + b3*log(income)^2 + b4*I(male)*log(income) + noise
# female and high income => lower bp  
bp_mean_curve <- function(income, male, beta) {
  x <- cbind(rep(1, length(income)), male, income, male*income)
  x %*% beta
}
beta <- matrix(c(160, 10, -1, .5))

# inspect curve
curve_dat <- expand.grid(income = seq(20, 100, by = 1), male = c(1, 0))
curve_dat %>% mutate(bp = bp_mean_curve(income, male, beta)) -> curve_dat

dat_sex_edu_income %>%
  rowwise() %>%
  mutate(fuzzy_exp = rnorm(1, bp_mean_curve(income, sex == "M", beta), bp_sigma)) %>%
  mutate(bp = round(fuzzy_exp)) %>%
  mutate(high_bp = rbinom(1, 1, 1/(1 + exp(-(fuzzy_exp - 130))))) %>%
  select(-fuzzy_exp) -> dat_clean

test_dat_sex_edu_income %>%
  rowwise() %>%
  mutate(fuzzy_exp = rnorm(1, bp_mean_curve(income, sex == "M", beta), bp_sigma)) %>%
  mutate(bp = round(fuzzy_exp)) %>%
  mutate(high_bp = rbinom(1, 1, 1/(1 + exp(-(fuzzy_exp - 130))))) %>%
  select(-fuzzy_exp, -id, -type) -> test_data

# add noise ----
dat %>% apply(2, as.character) %>% data.frame() %>%
  separate(rec.id, into = c("rec", "id", "type"), extra = "drop") %>%
  separate(birthdate, into = c("bm", "bd", "by")) %>%
  mutate_at(c("bm", "bd", "by"), as.numeric) %>%
  left_join(dat_clean) -> dat_full

# probability of distorting each column
dist_probs <- list(date = .25, sex = .1, education = .3, income = .2, bp = .25, high_bp = .25)
for(id in unique(dat_full$id)) {
  recs <- dat_full[dat_full$id == id,]
  if(nrow(recs) == 1) next() # no dups to distort
  
  idx_dup <- which(recs$type == "dup")
  
  # get date so can distort
  recs$date <- as.Date(paste(recs[-idx_dup, c("by", "bm", "bd")], collapse = "-"))
  
  # if dups, select 3/5 fields to distort and then fill in the non-distorted fields with org value
  for(dup in idx_dup) {
    which_dist <- sample(names(dist_probs), size = 3, prob = unlist(dist_probs))
    for(col in c("date", "sex", "education", "income", "bp", "high_bp")) {
      if(col %in% which_dist) {
        #distort values
        recs[dup, col] <- 
          switch (col,
                  date = as.Date(recs[-idx_dup, col] + round(rnorm(1, 0, 5))),
                  sex = sample(unique(dat_clean$sex), 1),
                  education = sample(unique(dat_clean$education), 1),
                  income = sample(unique(dat_clean$income), 1),
                  bp = sample(unique(dat_clean$bp), 1),
                  high_bp = sample(unique(dat_clean$high_bp), 1)
          )
        
      } else {
        #fill in org value
        recs[dup, col] <- recs[-idx_dup, col]
      }
    }
  }
  
  # separate date
  recs %>% 
    separate(date, into = c("by", "bm", "bd")) -> recs
  
  # add in to df
  dat_full[dat_full$id == id, names(dat_full)] <- recs[, names(dat_full)]
}

# format dataset ----
dat_full %>%
  mutate(lname = as.character(lname)) %>%
  mutate_at(c("bm", "bd", "by"), as.numeric) %>%
  select(-rec, -id, -type) -> data

# get duplication identity
dat_full %>%
  .$id %>%
  as.numeric() -> identity

# store which records are the original, non-corrupted data  
dat_full %>%
  mutate(rownum = 1:n()) %>%
  group_by(id) %>%
  mutate(count = n()) %>%
  filter(count > 1, type == "org") %>%
  select(id, rownum) -> originals_dup

dat_full %>%
  mutate(rownum = 1:n()) %>%
  group_by(id) %>%
  filter(type == "org") %>%
  .$rownum -> originals_idx


save(data, identity, originals_dup, originals_idx, test_data, file = paste0(output_folder, "geco_bpsigma_", round(bp_sigma, 2), ".Rdata"))

## save clean copy for dlink
data$id <- identity + 1
data$rec_id <- 1:nrow(data)

write.csv(data, "record_linkage/dblink/geco/geco1.csv", row.names = FALSE)


