# generate-geco-data.py
# Python script to generate synthetic data based on English look-up and error tables
# using the geCO module.
# This requires the contents of the geco-data-generator-corruptor folder to be unzipped in the same folder
# the geco-data-generator-corruptor is not included in the repo, you can obtain from https://dmm.anu.edu.au/geco/index.php
# script modified from the geCO documentation
# written: 04/30/2018

# Import the necessary other modules of the data generator
import basefunctions # Helper functions
import attrgenfunct # Functions to generate independent attribute values
import contdepfunct # Functions to generate dependent continuous attribute

# values
import generator # Main classes to generate records and the data set
import corruptor # Main classes to corrupt attribute values and records
import random
random.seed(42) # Set seed for random generator, so data generation can be repeated

# function for generating a random date
# see: https://stackoverflow.com/questions/553303/generate-a-random-date-between-two-other-dates
from random import randrange
from datetime import timedelta
from datetime import datetime

def random_date(start, end):
    """
    This function will return a random datetime between two datetime
    objects.
    """
    delta = end - start
    int_delta = delta.days
    random_day = randrange(int_delta)
    random_date = start + timedelta(days = random_day)
    return random_date.strftime('%m/%d/%Y')

# Set the Unicode encoding for this data generation project. This needs to be
# changed to another encoding for different Unicode character sets.
# Valid encoding strings are listed here:
# http://docs.python.org/library/codecs.html#standard-encodings
unicode_encoding_used = 'ascii'

# The name of the record identifier attribute (unique value for each record).
# This name cannot be given as name to any other attribute that is generated.
rec_id_attr_name = 'rec-id'

# Set the file name of the data set to be generated (this will be a comma
# separated values, CSV, file).
out_file_name = 'names.csv'

# Set how many original and how many duplicate records are to be generated.
# This will be 30% duplication
num_org_rec = 350
num_dup_rec = 150

# Set the maximum number of duplicate records can be generated per original
# record.
max_duplicate_per_record = 5

# Set the probability distribution used to create the duplicate records for one
# original record (possible values are: 'uniform', 'poisson', 'zipf').
num_duplicates_distribution = 'uniform'

# Set the maximum number of modification that can be applied to a single
# attribute (field).
max_modification_per_attr = 2

# Set the number of modification that are to be applied to a record.
num_modification_per_record = 2

# Check if the given the unicode encoding selected is valid.
basefunctions.check_unicode_encoding_exists(unicode_encoding_used)

# ----
# Define the attributes to be generated (using methods from the generator.py module).
fname_attr = generator.GenerateFreqAttribute(attribute_name = 'fname',
  freq_file_name = 'lookup-files/givenname_freq.csv',
  has_header_line = False,
  unicode_encoding = unicode_encoding_used)

lname_attr = generator.GenerateFreqAttribute(attribute_name = 'lname',
  freq_file_name = 'lookup-files/surname-freq.csv',
  has_header_line = False,
  unicode_encoding = unicode_encoding_used)

bdate_attr = generator.GenerateFuncAttribute(attribute_name = 'birthdate',
  function = random_date,
  parameters = [datetime.strptime('1/1/1950', '%m/%d/%Y'), datetime.strptime('1/1/1990', '%m/%d/%Y')])

# ----
# Define how the generated records are to be corrupted (using methods from
# the corruptor.py module).
# For the value edit corruptor, the sum or the four probabilities given must
# be 1.0.

edit_corruptor = corruptor.CorruptValueEdit(position_function = corruptor.position_mod_normal,
  char_set_funct = basefunctions.char_set_ascii,
  insert_prob = 0.25,
  delete_prob = 0.25,
  substitute_prob = 0.25,
  transpose_prob = 0.25)

lname_misspell_corruptor = corruptor.CorruptCategoricalValue(lookup_file_name = 'lookup-files/surname-misspell.csv',
  has_header_line = False,
  unicode_encoding = unicode_encoding_used)

ocr_corruptor = corruptor.CorruptValueOCR(position_function = corruptor.position_mod_normal,
  lookup_file_name = 'lookup-files/ocr-variations.csv',
  has_header_line = False,
  unicode_encoding = unicode_encoding_used)

keyboard_corruptor = corruptor.CorruptValueKeyboard(position_function = corruptor.position_mod_normal,
  row_prob = 0.5,
  col_prob = 0.5)

phonetic_corruptor = corruptor.CorruptValuePhonetic(lookup_file_name = 'lookup-files/phonetic-variations.csv',
  has_header_line = False,
  unicode_encoding = unicode_encoding_used)

# ----
# Define the attributes to be generated for this data set, and the data set
attr_name_list = ['fname', 'lname', 'birthdate']
attr_data_list = [fname_attr, lname_attr, bdate_attr]

# set-up the data set generation object.
test_data_generator = generator.GenerateDataSet(output_file_name = out_file_name,
  write_header_line = True,
  rec_id_attr_name = rec_id_attr_name,
  number_of_records = num_org_rec,
  attribute_name_list = attr_name_list,
  attribute_data_list = attr_data_list,
  unicode_encoding = unicode_encoding_used)

# Define the probability distribution of how likely an attribute will be selected for a modification.
# Each of the given probability values must be between 0 and 1, and the sum of them must be 1.0.
# If a probability is set to 0 for a certain attribute, then no modification will be applied on this attribute.
attr_mod_prob_dictionary = {'fname': 0.5, 'lname': 0.5, 'birthdate': 0.0}

# Define the actual corruption (modification) methods that will be applied on the different attributes.
# For each attribute, the sum of probabilities given must sum to 1.0.
attr_mod_data_dictionary = {'lname': [(0.7, lname_misspell_corruptor),
                                          (0.1, ocr_corruptor),
                                          (0.1, keyboard_corruptor),
                                          (0.1, phonetic_corruptor)],
                            'fname': [(0.1, edit_corruptor),
                                           (0.1, ocr_corruptor),
                                           (0.1, keyboard_corruptor),
                                           (0.7, phonetic_corruptor)],
                            'birthdate': [(1.0, edit_corruptor)]}

# set-up the data set corruption object
#
test_data_corruptor = corruptor.CorruptDataSet(number_of_org_records = num_org_rec,
  number_of_mod_records = num_dup_rec,
  attribute_name_list = attr_name_list,
  max_num_dup_per_rec = max_duplicate_per_record,
  num_dup_dist = num_duplicates_distribution,
  max_num_mod_per_attr = max_modification_per_attr,
  num_mod_per_rec = num_modification_per_record,
  attr_mod_prob_dict = attr_mod_prob_dictionary,
  attr_mod_data_dict = attr_mod_data_dictionary)

# Start the data generation process
rec_dict = test_data_generator.generate()
assert len(rec_dict) == num_org_rec # Check the number of generated records

# Corrupt (modify) the original records into duplicate records
rec_dict = test_data_corruptor.corrupt_records(rec_dict)
assert len(rec_dict) == num_org_rec+num_dup_rec # Check total number of records

# Write generate data into a file
test_data_generator.write()
