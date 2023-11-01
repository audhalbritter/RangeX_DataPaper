################################################################################
### DATA CLEANING SCRIPT: YEARLY SIZE 2021/ 22 #################################
################################################################################

################################################################################

### Data used           : data_sa2.txt
### Date last modified  : 31.10.2023
### Purpose             : Transform the raw 2021 yearly size data set from ZAF into the data paper format.

################################################################################

rm(list = ls()) # emptying global environment


### packages etc. ##############################################################

# basic packages
#library(dplyr); library(tidyr) # data manipulation
#library(ggplot2) # test-plotting
#library(stringr) # working with regex

library(tidyverse)
install.packages("janitor")
library(janitor)
install.packages("tidylog")
library(tidylog)

# task-specific packages (include short description of what it is used for)

### LOAD DATA SET ##############################################################

# load demographic data
dat_YS21 <- read_delim("data/ZAF/data_sa2.txt") %>%
  clean_names()


# load treatment key
key <- read_csv("data/ZAF/RangeX_Metadata_21_22_ZAF.csv") %>%
  clean_names()

# define useful vector
#species_names <- c("Brachypodium pinnatum" = "brapin", "Bromus erectus" = "broere", "Daucus carota" = "daucar", "Hypericum perforatum" = "hypper",
#                   "Medicago lupulina" = "medlup", "Plantago media" = "plamed", "Silene vulgaris" = "silvul", "Scabiosa columbaria" = "scacol",
#                   "Centaurea jacea" = "cenjac", "Salvia pratensis" = "salpra")

# wanted columns & data types
final_columns <- c("unique_plant_id", "species", "functional_group", "year", "collector", "survival", "height_vegetative_str", "height_reproductive_str", "height_vegetative", "height_reproductive", "vegetative_width", "height_nathan",
                   "stem_diameter", "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", "petiole_length", "number_leaves", "number_tillers", "number_branches", "number_flowers", 
                   "mean_inflorescence_size", "herbivory")

integer_cols <- c("year", "height_vegetative_str", "height_reproductive_str", "height_vegetative", "height_reproductive", "vegetative_width", "height_nathan",
                  "stem_diameter", "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", "petiole_length", "number_leaves", "number_tillers", "number_branches", "number_flowers", 
                  "mean_inflorescence_size")
string_cols <- c("unique_plant_id", "species", "collector")
factor_cols <- c("herbivory")



################################################################################
### 2021 - upper site ##########################################################
################################################################################


### CLEAN COLUMN NAMES & DATA CLASSES ##########################################

# check data classes
str(dat_YS21)

# change column names: get column names
dput(colnames(dat_YS21))

# change them to new names
dat_YS21 <- dat_YS21 %>%
  rename("year" = "date",
         "height_vegetative_str" = "vh",
         "number_leaves" = "nlc",
         "leaf_length1" = "lll",
         "vegetative_width" = "vw",
         "number_branches" = "nb",
         "stem_diameter" = "dia",
         "functional_group" = "flunctional_group",
         "position_id_original" = "position")


# delete unnecessyr columns
dat_YS21 <- dat_YS21 %>%
  dplyr::select(where(~!all(is.na(.x)))) %>%
  mutate("collector" = "OG",
         "survival" = ifelse(is.na(height_vegetative_str) == TRUE, 0, 1))
  
# get empty columns
present_columns <- colnames(dat_YS21)
missing_col <- setdiff(final_columns, present_columns)

dat_YS21[, missing_col] <- NA

# add columns for merging
dat_YS21 <- dat_YS21 %>%
  separate_wider_delim(plot, delim = ".", names = c("block_id_original", "plot_id_original")) %>%
  mutate(region = "ZAF",
         site = "hi") %>%
  dplyr::select(-"treat_otc", -"treat_veg", -"species", -"unique_plant_id") %>%
  filter(grepl("2022-03", year)) %>%
  mutate(year = 2022)

# change plot 9.3 to 9.4
dat_YS21 <- dat_YS21 %>%
  mutate(plot_id_original = ifelse(block_id_original == 9 & plot_id_original == 3 & site == "hi", 4, plot_id_original))


### ADD TREATMENTS ETC. ########################################################

# prepare treatment key
key <- key %>%
  filter(region == "ZAF" & site == "hi") %>%
  mutate(block_id_original = as.character(block_id_original),
         position_id_original = as.integer(position_id_original),
         plot_id_original = as.character(plot_id_original))

# merge treatments to 2021 size data frame
dat_YS21_merged <- full_join(dat_YS21, key, by = c("region", "site", "block_id_original", "plot_id_original", "position_id_original"))



### PROBLEM FIXING #############################################################


### MISSING ENTRIES/ VALUES/ NA's ##############################################

# check whether there are any mistaken NA in metadata columns
dat_YS21_na <- dat_YS21_merged %>% 
  filter(is.na(unique_plant_id) | is.na(year) | is.na(species)) 


# all data from plot 9.4 at high site are missing, but they are in key
# all key entries from plot 9.3 at high site are missing
# --> collected data is 9.3, key data is 9.4

# solve that! but before merging

# shrub in 1.3: must be a typo, no individuals missing in other block 1 plots
dat_YS21_merged <- dat_YS21_merged[!c(dat_YS21_merged$block_id_original == 1 & dat_YS21_merged$plot_id_original == 3),]

# position 10 and 11 are double in plot 1.2 at high site
dat_YS21_merged <- dat_YS21_merged[!c(dat_YS21_merged$block_id_original == 1 & dat_YS21_merged$plot_id_original == 2 & 
                                        dat_YS21_merged$position_id_original == 11 & dat_YS21_merged$leaf_length1 != 4.0),] # position 11: in one row one measurement is missing; delete other

dat_YS21_merged <- dat_YS21_merged %>% # delete dublicated position 10 plot 1.2 row
  distinct() %>%
  tidylog::drop_na()
  


# check for NAs: filter all rows with NA's in vegetative height and number of leaves (they are probably dead --> can only be checked with 2023 survival check or phenology data)
#dat_YS21_na <- dat_YS21 %>% 
#  filter(is.na(height_vegetative_str) | is.na(number_leaves)) 


### DELETE COLUMNS & CHANGE DATATYPES ##########################################

 
## delete all columns not in final data frame plus rearrange in right order
#dat_YS21 <- dat_YS21 %>% 
#  dplyr::select(any_of(final_columns))

# change data types
dat_YS21_merged <- dat_YS21_merged %>%
  mutate(across(all_of(factor_cols), as.factor),
         across(all_of(integer_cols), as.integer),
         across(all_of(string_cols), as.character))


