################################################################################
### DATA CLEANING SCRIPT: YEARLY SIZE 2021/ 22 #################################
################################################################################

################################################################################

### Data used           : 2021_2_data.xls, RangeX_Metadata_21_22_ZAF.csv
### Date last modified  : 31.10.2023
### Purpose             : Transform the raw 2021 yearly size data set from ZAF into the data paper format.

################################################################################

rm(list = ls()) # emptying global environment


### packages etc. ##############################################################

library(tidyverse) # instead of tidyr, strinr etc. (data manipulation)
#install.packages("janitor")
library(janitor) # clean up data (i.e. get rid of empty spaces)
#install.packages("tidylog")
library(tidylog) # how many lines of data deleted/ manipulated etc.
library(dataDownloader)

# task-specific packages (include short description of what it is used for)

### LOAD DATA SET ##############################################################

# download data from OSF to computer
get_file(node = "bg2mu",
         file = "2021_2_data_lowSite.csv",
         path = "data/ZAF",
         remote_path = "focal_level/demographics/raw data/ZAF") # high site 2021/22

get_file(node = "bg2mu",
         file = "2021_2_data_highSite.csv",
         path = "data/ZAF",
         remote_path = "focal_level/demographics/raw data/ZAF") # high site 2021/22

get_file(node = "bg2mu",
         file = "RangeX_Metadata_21_22_ZAF.csv",
         path = "data/ZAF",
         remote_path = "metadata")

get_file(node = "bg2mu",
         file = "meta_21_22.csv",
         path = "data/ZAF",
         remote_path = "focal_level/demographics/raw data/ZAF")


# import data into R studio
# load demographic data
raw_dat_YS21_lo <- read_csv("data/ZAF/2021_2_data_lowSite.csv") %>%
  clean_names()

raw_dat_YS21_hi <- read_csv("data/ZAF/2021_2_data_highSite.csv") %>%
  clean_names()

raw_meta <- read_csv("data/ZAF/meta_21_22.csv") %>%
  clean_names()

dat_YS21_lo <- raw_dat_YS21_lo
dat_YS21_hi <- raw_dat_YS21_hi
meta <- raw_meta

# load treatment key
key <- read_csv("data/ZAF/RangeX_Metadata_21_22_ZAF.csv") %>%
  clean_names()

# define useful vector

# wanted columns & data types
final_columns <- c("unique_plant_id", "species", "functional_group", "date_measurement", "date_planting", "collector", "survival", "height_vegetative_str", "height_reproductive_str", "height_vegetative", "height_reproductive", "vegetative_width", "height_nathan",
                   "stem_diameter", "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", "petiole_length", "number_leaves", "number_tillers", "number_branches", "number_flowers", 
                   "mean_inflorescence_size", "herbivory")

integer_cols <- c("height_vegetative_str", "height_reproductive_str", "height_vegetative", "height_reproductive", "vegetative_width", "height_nathan",
                  "stem_diameter", "leaf_length1", "leaf_length2", "leaf_length3", "leaf_width", "petiole_length", "number_leaves", "number_tillers", "number_branches", "number_flowers", 
                  "mean_inflorescence_size")
date_cols <- c("date_measurement", "date_planting")
string_cols <- c("unique_plant_id", "species", "collector")
factor_cols <- c("herbivory")



################################################################################
### 2021 - upper site ##########################################################
################################################################################


### CLEAN COLUMN NAMES & DATA CLASSES ##########################################

integer_cols_org <- c("vh_nov_21", "vw_nov_21", "nlc_nov_21", "nb_nov_21", "lll_nov_21", "dia_nov_21", "vh_jan_22", "vw_jan_22", "nlc_jan_22", "nb_jan_22", "lll_jan_22", "dia_jan_22", "vh_mar_22", "vw_mar_22", "nlc_mar_22", "nb_mar_22", "lll_mar_22", "dia_mar_22", "vh_oct_22", "vw_oct_22", "nlc_oct_22", "nb_oct_22", "lll_oct_22", "dia_oct_22")

# correct column naming typo
colnames(dat_YS21_hi)[12] <- "lll_nov_21"
colnames(dat_YS21_lo)[11] <- "lll_nov_21"

# delete unnecessary columns, make plot id column identical in both low and high data frames, add site column
dat_YS21_hi <- dat_YS21_hi %>%
  dplyr::select(-treat_veg, -treat_otc, -id) %>%
  mutate(plot = gsub("[[:upper:]]", "", plot),
         plot = gsub("^\\.", "", plot),
         across(all_of(integer_cols_org), as.numeric),
         site = "hi")

dat_YS21_lo <- dat_YS21_lo %>%
  dplyr::select(-date, -treatment) %>%
  mutate(plot = gsub("[[:upper:]]", "", plot),
         plot = gsub("^\\.", "", plot),
         across(all_of(integer_cols_org), as.numeric),
         site = "lo")

# merge high and low data sets
dat_YS21 <- bind_rows(dat_YS21_hi, dat_YS21_lo)


# make long format
dat_YS21 <- dat_YS21 %>%
  pivot_longer(
    cols = !c(flunctional_group, species, plot,  position, site),
    names_to = c("variable", "month", "year"),
    names_sep = "_",
    values_to = "value") %>%
  pivot_wider(names_from = variable,
              values_from = value)

# make some manipulations to be able to merge key on
dat_YS21 <- dat_YS21 %>%
  mutate(region = "ZAF") %>%
  separate_wider_delim(plot, delim = ".", names = c("block_id_original", "plot_id_original")) %>%
  dplyr::select(-"month", -"year")



# change them to new names
dat_YS21 <- dat_YS21 %>%
  rename("height_vegetative_str" = "vh",
         "number_leaves" = "nlc",
         "leaf_length1" = "lll",
         "vegetative_width" = "vw",
         "number_branches" = "nb",
         "stem_diameter" = "dia",
         "functional_group" = "flunctional_group",
         "position_id_original" = "position")


# get empty columns
present_columns <- colnames(dat_YS21)
missing_col <- setdiff(final_columns, present_columns)

dat_YS21[, missing_col] <- NA


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


