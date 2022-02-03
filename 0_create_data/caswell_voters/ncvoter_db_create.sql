-- File: ncvoter_db_create.sql
-- Purpose: create ncvoter mysql database
-- Date: 02/03/2022

-- login to mysql
-- mysql -u root -p

-- Create the ncvoter database
SHOW DATABASES;
CREATE DATABASE ncvoter;

-- Use ncvoter db
USE ncvoter;

-- Create empty prototype table of voters
CREATE TABLE voters (
  snapshot_dt			VARCHAR(10),
  county_id			VARCHAR(3),
  county_desc			VARCHAR(15),
  voter_reg_num			VARCHAR(12),
  ncid				VARCHAR(12),
  status_cd			VARCHAR(1),
  voter_status_desc		VARCHAR(10),
  reason_cd			VARCHAR(2),
  voter_status_reason_desc	VARCHAR(60),
  absent_ind			VARCHAR(1),
  name_prefx_cd			VARCHAR(4),
  last_name			VARCHAR(25),
  first_name			VARCHAR(20),
  midl_name			VARCHAR(20),
  name_sufx_cd			VARCHAR(4),
  house_num			VARCHAR(10),
  half_code			VARCHAR(1),
  street_dir			VARCHAR(2),
  street_name			VARCHAR(30),
  street_type_cd			VARCHAR(4),
  street_sufx_cd			VARCHAR(4),
  unit_designator			VARCHAR(4),
  unit_num			VARCHAR(7),
  res_city_desc			VARCHAR(20),
  state_cd			VARCHAR(2),
  zip_code			VARCHAR(9),
  mail_addr1			VARCHAR(40),
  mail_addr2			VARCHAR(40),
  mail_addr3			VARCHAR(40),
  mail_addr4			VARCHAR(40),
  mail_city			VARCHAR(30),
  mail_state			VARCHAR(2),
  mail_zipcode			VARCHAR(9),
  area_cd				VARCHAR(3),
  phone_num			VARCHAR(7),
  race_code			VARCHAR(3),
  race_desc			VARCHAR(35),
  ethnic_code			VARCHAR(2),
  ethnic_desc			VARCHAR(30),
  party_cd			VARCHAR(3),
  party_desc			VARCHAR(12),
  sex_code			VARCHAR(1),
  sex				VARCHAR(6),
  age				VARCHAR(3),
  birth_place			VARCHAR(2),
  registr_dt			VARCHAR(10),
  precinct_abbrv			VARCHAR(6),
  precinct_desc			VARCHAR(30),
  municipality_abbrv		VARCHAR(4),
  municipality_desc		VARCHAR(30),
  ward_abbrv			VARCHAR(4),
  ward_desc			VARCHAR(30),
  cong_dist_abbrv			VARCHAR(4),
  cong_dist_desc			VARCHAR(30),
  super_court_abbrv		VARCHAR(4),
  super_court_desc		VARCHAR(30),
  judic_dist_abbrv		VARCHAR(4),
  judic_dist_desc			VARCHAR(30),
  NC_senate_abbrv			VARCHAR(4),
  NC_senate_desc			VARCHAR(30),
  NC_house_abbrv			VARCHAR(4),
  NC_house_desc			VARCHAR(30),
  county_commiss_abbrv		VARCHAR(4),
  county_commiss_desc		VARCHAR(30),
  township_abbrv			VARCHAR(6),
  township_desc			VARCHAR(30),
  school_dist_abbrv		VARCHAR(6),
  school_dist_desc		VARCHAR(30),
  fire_dist_abbrv			VARCHAR(4),
  fire_dist_desc			VARCHAR(30),
  water_dist_abbrv		VARCHAR(4),
  water_dist_desc			VARCHAR(30),
  sewer_dist_abbrv		VARCHAR(4),
  sewer_dist_desc			VARCHAR(30),
  sanit_dist_abbrv		VARCHAR(4),
  sanit_dist_desc			VARCHAR(30),
  rescue_dist_abbrv		VARCHAR(4),
  rescue_dist_desc		VARCHAR(30),
  munic_dist_abbrv		VARCHAR(4),
  munic_dist_desc			VARCHAR(30),
  dist_1_abbrv			VARCHAR(4),
  dist_1_desc			VARCHAR(30),
  dist_2_abbrv			VARCHAR(4),
  dist_2_desc			VARCHAR(30),
  confidential_ind		VARCHAR(1),
  cancellation_dt			VARCHAR(10),
  vtd_abbrv			VARCHAR(6),
  vtd_desc			VARCHAR(30),
  load_dt				VARCHAR(10),
  age_group			VARCHAR(35)
);

-- add index
ALTER TABLE `voters` ADD INDEX `ncid` (`ncid`);

-- create table to load in snapshot
CREATE TABLE voters_20190101 LIKE voters;

-- load the data in
LOAD DATA LOCAL INFILE 'VR_Snapshot_20190101_UTF8_no_slash.txt'
INTO TABLE voters_20190101
CHARACTER SET UTF8
FIELDS TERMINATED BY '\t'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;


