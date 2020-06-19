-- File: ncvoter_db_snapshots_create.sql
-- Purpose: create snapshot tables
-- Author: Andee Kaplan
-- Date: 06/19/2020

-- First convert snapshot files to UTF-8 for loading into db
-- See https://dev.mysql.com/doc/mysql-reslimits-excerpt/5.6/en/charset-restrictions.html

-- Do this for each snapshot downloaded
-- These snapshots can be downloaded from: https://dl.ncsbe.gov/?prefix=data/Snapshots/
-- e.g.:

-- iconv -f UTF-16LE -t UTF-8 raw/VR_Snapshot_20181106.txt -o VR_Snapshot_20181106_UTF8.txt
-- iconv -f UTF-16LE -t UTF-8 raw/VR_Snapshot_20180508.txt -o VR_Snapshot_20180508_UTF8.txt
-- iconv -f UTF-16LE -t UTF-8 raw/VR_Snapshot_20180101.txt -o VR_Snapshot_20180101_UTF8.txt
-- iconv -f UTF-16LE -t UTF-8 raw/VR_Snapshot_20171107.txt -o VR_Snapshot_20171107_UTF8.txt
-- iconv -f UTF-16LE -t UTF-8 raw/VR_Snapshot_20190430.txt -o VR_Snapshot_20190430_UTF8.txt
-- iconv -f UTF-16LE -t UTF-8 raw/VR_Snapshot_20190514.txt -o VR_Snapshot_20190514_UTF8.txt

-- sed 's/\\//' VR_Snapshot_20181106_UTF8.txt > VR_Snapshot_20181106_UTF8_no_slash.txt
-- sed 's/\\//' VR_Snapshot_20180508_UTF8.txt > VR_Snapshot_20180508_UTF8_no_slash.txt
-- sed 's/\\//' VR_Snapshot_20180101_UTF8.txt > VR_Snapshot_20180101_UTF8_no_slash.txt
-- sed 's/\\//' VR_Snapshot_20171107_UTF8.txt > VR_Snapshot_20171107_UTF8_no_slash.txt
-- sed 's/\\//' VR_Snapshot_20190430_UTF8.txt > VR_Snapshot_20190430_UTF8_no_slash.txt
-- sed 's/\\//' VR_Snapshot_20190514_UTF8.txt > VR_Snapshot_20190514_UTF8_no_slash.txt

-- login to mysql
-- mysql -u root -p

-- Use ncvoter db
USE ncvoter;

-- create tables

CREATE TABLE voters_20181106 LIKE voters;
CREATE TABLE voters_20180508 LIKE voters;
CREATE TABLE voters_20180101 LIKE voters;
CREATE TABLE voters_20171107 LIKE voters;
CREATE TABLE voters_20190430 LIKE voters;
CREATE TABLE voters_20190514 LIKE voters;


-- load the data in
LOAD DATA LOCAL INFILE 'snapshots/VR_Snapshot_20181106_UTF8_no_slash.txt'
INTO TABLE voters_20181106
CHARACTER SET UTF8
FIELDS TERMINATED BY '\t'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'snapshots/VR_Snapshot_20180508_UTF8_no_slash.txt'
INTO TABLE voters_20180508
CHARACTER SET UTF8
FIELDS TERMINATED BY '\t'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'snapshots/VR_Snapshot_20180101_UTF8_no_slash.txt'
INTO TABLE voters_20180101
CHARACTER SET UTF8
FIELDS TERMINATED BY '\t'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'snapshots/VR_Snapshot_20171107_UTF8_no_slash.txt'
INTO TABLE voters_20171107
CHARACTER SET UTF8
FIELDS TERMINATED BY '\t'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'snapshots/VR_Snapshot_20190430_UTF8_no_slash.txt'
INTO TABLE voters_20190430
CHARACTER SET UTF8
FIELDS TERMINATED BY '\t'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;

LOAD DATA LOCAL INFILE 'snapshots/VR_Snapshot_20190514_UTF8_no_slash.txt'
INTO TABLE voters_20190514
CHARACTER SET UTF8
FIELDS TERMINATED BY '\t'
LINES TERMINATED BY '\n'
IGNORE 1 ROWS;