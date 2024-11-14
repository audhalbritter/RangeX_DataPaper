################################################################################
### DATA CLEANING SCRIPT: METADATA PREPARATION #################################
################################################################################

################################################################################

### Data used           : RangeX_Metadata_21_22_ZAF.csv, RangeX_Metadata_22_23_ZAF.csv, meta_21_22.csv
### Date last modified  : 03.11.2023
### Purpose             : Clean metadata of all regions. Add unique_plant_ID to metadata.

################################################################################



### packages etc. ##############################################################

library(tidyverse) # instead of tidyr, stringr etc. (data manipulation)
library(janitor) # clean up data (i.e. get rid of empty spaces)
library(tidylog) # how many lines of data deleted/ manipulated etc.
#library(dataDownloader)

# task-specific packages (include short description of what it is used for)

### LOAD DATA SET ##############################################################

## download data from OSF to computer
#get_file(node = "bg2mu",
#         file = "RangeX_Metadata_21_22_ZAF.csv",
#         path = "data/ZAF",
#         remote_path = "metadata")
#
#get_file(node = "bg2mu",
#         file = "RangeX_Metadata_22_23_ZAF.csv",
#         path = "data/ZAF",
#         remote_path = "metadata")
#
#get_file(node = "bg2mu",
#         file = "meta_21_22.csv",
#         path = "data/ZAF",
#         remote_path = "focal_level/demographics/raw data/ZAF")


# import data into R studio
# load demographic data
#meta_ZAF21_raw <- read_csv("data/ZAF/RangeX_Metadata_21_22_ZAF.csv") %>%
#  clean_names()
meta_ZAF21_raw <- read_delim("/Users/eviseli/Desktop/Retreat ZAF/RangeX_Metadata_21_22_ZAF.csv") %>%
  clean_names()

#meta_ZAF22_raw <- read_csv("data/ZAF/RangeX_Metadata_22_23_ZAF.csv") %>%
#  clean_names()
meta_ZAF22_raw <- read_csv("/Users/eviseli/Desktop/Retreat ZAF/RangeX_Metadata_ZAF_22_23_final1.csv") %>%
  clean_names()

plant_id_ZAF_raw <- read_csv("data/ZAF/meta_21_22.csv") %>%
  clean_names()

meta_ZAF21 <- meta_ZAF21_raw
meta_ZAF22 <- meta_ZAF22_raw
plant_id_ZAF <- plant_id_ZAF_raw


################################################################################
### South Africa (ZAF) #########################################################
################################################################################


# add plant ID to current 2021/ 22 metadata (which only has one position ID, 1)
meta_ZAF21 <- meta_ZAF21 %>%
  left_join(plant_id_ZAF[, c(1:3)], by = "unique_position_id" )


# relocate columns in sensible order, make date into date, create new unique_plant_id
meta_ZAF21 <- meta_ZAF21 %>%
  #relocate(plant_id_2021, .after = position_id_original) %>%
  rename("plant_id_original" = "plant_id_2021",
         "planting_date" = "planting_date_2021") %>%
  #relocate(planting_date, .after = plant_id_original) %>%
  mutate(planting_date = as.Date("2021-11-17"),
         unique_plant_id = paste(unique_position_id, plant_id_original, sep = ".")) 


# do the same for the 2022/ 23 data
# add plant ID to current 2022/ 23 metadata (which only has a position ID)
meta_ZAF22 <- meta_ZAF22 %>%
  left_join(plant_id_ZAF[, c(1, 4, 5)], by = "unique_position_id" )

# delete the plant ID and planting date that is already in the new meta_ZAF22 (it is wrong - where does it come from?)
meta_ZAF22 <- meta_ZAF22 %>%
  dplyr::select(-plant_id_original, -planting_date)

# relocate columns in sensible order, make date into date, create new unique_plant_id
meta_ZAF22 <- meta_ZAF22 %>%
  #relocate(plant_id_2022, .after = position_id_original) %>%
  rename("plant_id_original" = "plant_id_2022",
         "planting_date" = "planting_date_2022") %>%
  #relocate(planting_date, .after = plant_id_original) %>%
  mutate(planting_date = as.Date("2022-10-19"),
         unique_plant_id = paste(unique_position_id, plant_id_original, sep = ".")) 

# delete all rows with plant_id = 1 (they will already be in meta_ZAF21 with no change - otherwise there will be a wrong planting date)
meta_ZAF22 <- meta_ZAF22 %>%
  filter(plant_id_original != 1)

# check for NA's
meta_ZAF22_na <- meta_ZAF22 %>% 
  filter(is.na(unique_plant_id) | is.na(planting_date) | is.na(species)) 

meta_ZAF21_na <- meta_ZAF21 %>% 
  filter(is.na(unique_plant_id) | is.na(planting_date) | is.na(species)) 

# none, amazing!

# bind 21 and 22
meta_ZAF21_22 <- bind_rows(meta_ZAF21, meta_ZAF22) %>% 
  mutate(species = case_when(species == "aloemac" ~ "alomac",
                             species == "leucser" ~ "leuser",
                             species == "zervis" ~ "xervis",
                             .default = species))

# done!

# save
write_csv(meta_ZAF21_22, "data/ZAF/RangeX_Metadata_ZAF_clean_2.csv")




