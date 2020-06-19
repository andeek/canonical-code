# Create ncvoter data

Order of files to run:

1. ncvoter_db_create.sql
2. ncvoter_db_snapshots_create.sql
3. caswell_voters_create.sql
4. caswell_voters.R

Output will be Rdata object of Caswell county voters and test records to a user specified location and a csv copy saved for use with the record linkage package dblink.