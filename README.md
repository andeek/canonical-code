# canonical-code

Code appendix accompanying the paper "Entity Resolution and the Downstream Task: A Case Study of North Carolina Voter Registration Records"

This appendix contains the following folders:

1. representr -- An R package for performing the canonicalization methods used in the paper.
2. 0_create_data -- A folder of scripts to either simulate data (geco) or clean data available from the web used in the paper (caswell_voters).
3. 1_record_linkage -- Configuration files for performing record linkage using the dblink (v0.1.0) package. Also includes an R script for organizing the resulting posterior draws.
4. 2_canonicalization -- Scripts for performing canonicalization on the Caswell county and GeCO data sets.
5. 3_downstream_task -- Scripts for performing regression tasks after canonicalization has occurred.
6. 4_figures_and_tables -- Scripts to create all figures and tables in the paper after all other code has been run.

The code is presented in the order it should be run, from the folder labeled 0 up to the folder labeled 4.

## Dependencies

The following is a list of packages and technologies that must be installed and where they can be found.

**MySQL**

See https://dev.mysql.com/doc/mysql-getting-started/en/ for details on getting started with MySQL.

**R version 3.6.3**

1. ggplot (CRAN)
2. dplyr (CRAN)
3. tidyr (CRAN)
4. babynames (CRAN)
5. readxl (CRAN)
6. rvest (CRAN)
7. sparklyr (CRAN)
8. sparklyr.nested (https://github.com/mitre/sparklyr.nested)
9. representr (representr folder)
10. rstanarm (CRAN)
11. tidyverse (CRAN)
12. knitr (CRAN)
13. kableExtra (CRAN)

**Python 2.7.16**

1. geco-data-generator-corruptor (https://dmm.anu.edu.au/geco/index.php)
2. datetime
3. random

**Apache Spark**

1. spark-2.3.1
2. dblink v0.1.0 (https://github.com/cleanzr/dblink/releases/tag/v0.1)

