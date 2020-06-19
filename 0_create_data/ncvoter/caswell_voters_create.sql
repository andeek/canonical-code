-- File: ncvoter_voters_create.sql
-- Purpose: create voters tables for caswell county
-- Author: Andee Kaplan
-- Date: 06/19/2020

-- location of csv saved can be changed in lines 69 and 76 below.

-- login to mysql
-- mysql -u root -p

-- Use ncvoter db
USE ncvoter;

CREATE TABLE caswell_base
AS
SELECT *
FROM voters_20190430
WHERE voter_status_desc IN ("ACTIVE", "INACTIVE")
AND county_desc = "CASWELL"
AND party_cd IN ("DEM", "REP");

CREATE TABLE caswell_voters
AS
SELECT "1" as db_id, snapshot_dt, county_desc, county_id, ncid, last_name, first_name, midl_name, house_num, street_dir, street_name, unit_designator, res_city_desc, state_cd, zip_code, phone_num, race_code, race_desc, ethnic_code, ethnic_desc, party_cd, party_desc, sex_code, sex, age, birth_place, registr_dt, precinct_abbrv, load_dt
FROM caswell_base
UNION
SELECT "2" as db_id, snapshot_dt, county_desc, county_id, ncid, last_name, first_name, midl_name, house_num, street_dir, street_name, unit_designator, res_city_desc, state_cd, zip_code, phone_num, race_code, race_desc, ethnic_code, ethnic_desc, party_cd, party_desc, sex_code, sex, age, birth_place, registr_dt, precinct_abbrv, load_dt
FROM voters_20190101
WHERE ncid IN
(
SELECT DISTINCT ncid
FROM caswell_base
)
UNION
SELECT "3" as db_id, snapshot_dt, county_desc, county_id, ncid, last_name, first_name, midl_name, house_num, street_dir, street_name, unit_designator, res_city_desc, state_cd, zip_code, phone_num, race_code, race_desc, ethnic_code, ethnic_desc, party_cd, party_desc, sex_code, sex, age, birth_place, registr_dt, precinct_abbrv, load_dt 
FROM voters_20181106
WHERE ncid IN
(
SELECT DISTINCT ncid
FROM caswell_base
)
UNION
SELECT "4" as db_id, snapshot_dt, county_desc, county_id, ncid, last_name, first_name, midl_name, house_num, street_dir, street_name, unit_designator, res_city_desc, state_cd, zip_code, phone_num, race_code, race_desc, ethnic_code, ethnic_desc, party_cd, party_desc, sex_code, sex, age, birth_place, registr_dt, precinct_abbrv, load_dt
FROM voters_20180508
WHERE ncid IN
(
SELECT DISTINCT ncid
FROM caswell_base
)
UNION
SELECT "5" as db_id, snapshot_dt, county_desc, county_id, ncid, last_name, first_name, midl_name, house_num, street_dir, street_name, unit_designator, res_city_desc, state_cd, zip_code, phone_num, race_code, race_desc, ethnic_code, ethnic_desc, party_cd, party_desc, sex_code, sex, age, birth_place, registr_dt, precinct_abbrv, load_dt 
FROM voters_20180101
WHERE ncid IN
(
SELECT DISTINCT ncid
FROM caswell_base
);

CREATE TABLE caswell_test_voters
AS
SELECT "test" as db_id, snapshot_dt, county_desc, county_id, ncid, last_name, first_name, midl_name, house_num, street_dir, street_name, unit_designator, res_city_desc, state_cd, zip_code, phone_num, race_code, race_desc, ethnic_code, ethnic_desc, party_cd, party_desc, sex_code, sex, age, birth_place, registr_dt, precinct_abbrv, load_dt
FROM voters_20190514
WHERE voter_status_desc IN ("ACTIVE", "INACTIVE")
AND county_desc = "CASWELL"
AND party_cd IN ("DEM", "REP");

SELECT DISTINCT * 
FROM caswell_voters
INTO OUTFILE 'caswell_voters.csv' -- Be sure to add desired location here
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n';

SELECT DISTINCT * 
FROM caswell_test_voters
INTO OUTFILE 'caswell_test_voters.csv' -- Be sure to add desired location here
FIELDS TERMINATED BY ','
ENCLOSED BY '"'
LINES TERMINATED BY '\n';