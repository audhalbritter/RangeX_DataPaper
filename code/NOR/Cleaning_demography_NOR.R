################################################################################
### DATA CLEANING SCRIPT: YEARLY SIZE 2021/ 22 #################################
################################################################################

################################################################################

### Data used           : 
### Date last modified  : 31.10.2023
### Purpose             : Transform the raw 2021 yearly size data set from NOR into the data paper format.

################################################################################

rm(list = ls()) # emptying global environment


### packages etc. ##############################################################

# basic packages
#library(dplyr); library(tidyr) # data manipulation
#library(ggplot2) # test-plotting
#library(stringr) # working with regex

library(tidyverse) # instead of tidyr, strinr etc. (data manipulation)
install.packages("janitor")
library(janitor) # clean up data (i.e. get rid of empty spaces)
#install.packages("tidylog")
library(tidylog) # how many lines of data deleted/ manipulated etc.
library(dataDownloader)

# task-specific packages (include short description of what it is used for)

### LOAD DATA SET ##############################################################

# download data from OSF to computer
get_file(node = "bg2mu",
         file = "data_sa2.txt",
         path = "data/ZAF",
         remote_path = "focal_level/demographics/raw data/ZAF")

get_file(node = "bg2mu",
         file = "RangeX_Metadata_21_22_ZAF.csv",
         path = "data/ZAF",
         remote_path = "metadata")

