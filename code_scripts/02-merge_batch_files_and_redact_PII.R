## -- During the follower network data harvesting from Twitter, MP follower adjacency lists and accompanying profile -- ##
## -- metadata were saved out in batches. The purpose of this code is to merge these individual CSV files (4,602)    -- ##
## -- into combined files. Duplicate rows are filtered out from the joined follower adjacency list and follower      -- ##
## -- profile metadata is filtered, and any PII is redacted in both. Owing to the volume of data, the data.table     -- ##
## -- package and dtplyr are used for easier wrangling of data.                                                      -- ##
                                                                             
options(scipen = 999)

#### ---- LOAD PACKAGES ---- ####

library(dplyr)
library(data.table)
library(dtplyr)

#### ---- LOAD BATCH DATA AND MERGE ---- ####

## -- MP Follower Adjacency Lists -- ##

# Load in each of the MP follower adjacency list batch files

mp_adj_lists_batch_files <- list.files("follower_adj_lists",pattern = "*.csv",full.names = TRUE) %>% 
  lapply(fread) 

# Bind these individual batch files together into one unified adjacency list

mp_adj_list_joined <- rbindlist(mp_adj_lists_batch_files)

# There may be some instances in the data files where observations have been duplicated during the data collection
# process. These are rare but need to be filtered from the final dataset before saving.

duplicates <- mp_adj_list_joined[duplicated(mp_adj_list_joined)] # this will be needed to also filter the metadata

fwrite(duplicates,"duplicates.csv",row.names = F)

mp_adj_list_joined_no_dupes <- mp_adj_list_joined %>% distinct()  # 1,182 duplicate rows 

# Save the final joined adjacency list as a single CSV

fwrite(mp_adj_list_joined_no_dupes,file = "mp_follower_adj_list_joined_unfiltered.csv",row.names = FALSE)

## -- Follower Profile Metadata Files -- ##

# Load in each of the follower profile metadata batch files 

follower_meta_batch_lists <- list.files("follower_profile_metadata",pattern = "*.csv",full.names = TRUE) %>% 
  lapply(fread,drop="withheld")

# There was a bug in the data collection process that stored the values in the profile 'created_at' column 
# as date/time class for some batch files and as a character in others. This prevents the datasets from being
# able to merge. A function is created to convert all created_at columns that are stored as characters to a 
# date/time class and applied to all metadata dataframes in the list. 

convert_dt <- function(df){
  if (inherits(df$created_at,"character") == TRUE){
    df <- transform.data.frame(df,created_at = as.POSIXct(created_at))
    }
  return(df)
}

follower_meta_batch_lists_2 <- lapply(follower_meta_batch_lists,convert_dt)

# To prevent any corruption of data due to the length of some of the user ids, these are converted to characters
# before merging and saving to CSV.

follower_meta_batch_lists_3 <- lapply(follower_meta_batch_lists_2,transform.data.frame,id = as.character(id))

follower_metadata_joined <- rbindlist(follower_meta_batch_lists_3)

fwrite(follower_metadata_joined,file = "follower_profile_metadata_joined_unfiltered.csv",row.names = FALSE)

#### ---- CREATE REDACTED VERSIONS OF THE FINAL DATASETS FOR PUBLIC USE ---- ####

# Create an additional column in the follower profile metadata that contains how many MPs each of the users 
# follow. This is calculated by how many unique observations each user has in the profile metadata as a new 
# row is generated for every time they appear in the follower adjacency list.

follower_profile_metadata <- fread("follower_profile_metadata_joined_unfiltered.csv")

follower_profile_metadata <- follower_profile_metadata %>% 
  group_by(id) %>%
  summarise(mps_followed = n()) %>%
  left_join(follower_profile_metadata,by="id") %>% 
  as_tibble()

# Now that the number of MPs followed column has been added to the follower profile metadata, the duplicate rows 
# can now be removed. Only the most recent occurrence of each unique user in the dataset is kept. 

follower_profile_metadata_filtered <- distinct(follower_profile_metadata, id, .keep_all = TRUE)

nrow(follower_profile_metadata_filtered) # 11,071,104 unique users

# Some additional rows existed in the profile metadata due to duplication (as found in the adjacency list). To
# account for this, the number of MPs a user follows is subtracted by however duplicates appeared in the adjacency
# list. (This is a very small number of cases, but is important for maintaining accurate data)

duplicate_user_obs <- duplicates %>% 
  group_by(source_id) %>% 
  summarise(no_of_dupes = n()) %>% 
  as_tibble() %>%
  rename(id = source_id)

follower_profile_metadata_filtered <- left_join(follower_profile_metadata_filtered,duplicate_user_obs,by="id")

follower_profile_metadata_filtered$mps_followed <- ifelse(!is.na(follower_profile_metadata_filtered$no_of_dupes), 
                                                  follower_profile_metadata_filtered$mps_followed  - 
                                                    follower_profile_metadata_filtered$no_of_dupes,
                                                  follower_profile_metadata_filtered$mps_followed)

follower_profile_metadata_filtered <- follower_profile_metadata_filtered %>% select(-no_of_dupes)
  
# The follower profile metadata contains personally identifiable information (PII) about ordinary Twitter users.
# In the interest of protecting anonymity, all PII data is removed from the dataset and a random user ID is given 
# to each follower so that they cannot be identified through their original Twitter user ID. 

# Create a new follower ID for each unique user in the metadata  

follower_profile_metadata_filtered$follower_id <- paste0("user_",1:nrow(follower_profile_metadata_filtered))

# First, save the filtered follower profile metadata to CSV (this makes sure that the original metadata contains the
# new follower ID if I want to rematch the redacted dataset back in future). 

fwrite(follower_profile_metadata_filtered,"follower_profile_metadata_joined_no_duplicates.csv",
       row.names = FALSE)

# Convert the corresponding Twitter user IDs in the MP follower adjacency list to the newly created follower IDs

metadata_follower_id <- follower_profile_metadata_filtered %>% 
  select(id,follower_id) %>% 
  rename(source_id = id)

mp_adj_list_joined_user_redacted <- left_join(mp_adj_list_joined_no_dupes,metadata_follower_id,by="source_id") %>%
  select(target_id,follower_id) %>%
  as_tibble()

# Drop PII columns in the follower profile metadata

follower_profile_metadata_redacted <- follower_profile_metadata_filtered %>% 
  select(follower_id,created_at,verified,followers_count,following_count,tweet_count,listed_count,mps_followed)

# Save out the redacted follower profile metadata to CSV 

fwrite(follower_profile_metadata_redacted,"follower_profile_metadata_joined_redacted.csv",
       row.names = FALSE)

# For better clarity, the MP Twitter IDs in the redacted MP follower adjacency list are also replaced with the official 
# usernames of each MP. (This is PII for the MPs but given their high-profile status, this is acceptable)

mp_profile_metadata <- read.csv("mp_profile_metadata_22-08-22.csv")

mp_usernames <- mp_profile_metadata %>% 
  select(id,username) %>%
  rename(target_id = id)

mp_usernames$target_id <- as.character(mp_usernames$target_id)
mp_adj_list_joined_user_redacted$target_id <- as.character(mp_adj_list_joined_user_redacted$target_id)

# Due to an unknown bug in the data collection process, the last 1-2 digits of the MP Twitter IDs in the adj list 
# data do not match their corresponding ID in the profile metadata. Therefore, the last two digits of the MP Twitter 
# IDs in both datasets are truncated in order to merge the datasets correctly. This is not ideal but works in the 
# case of these datasets.

mp_usernames$truncated_id <- substr(mp_usernames$target_id, 1, nchar(mp_usernames$target_id) - 2)
mp_adj_list_joined_user_redacted$truncated_id <- substr(mp_adj_list_joined_user_redacted$target_id, 1, 
                                                        nchar(mp_adj_list_joined_user_redacted$target_id) - 2)

mp_adj_list_joined_redacted_final <- left_join(mp_adj_list_joined_user_redacted,mp_usernames,by="truncated_id") %>%
  select(username,follower_id) %>%
  rename(mp_username = username) %>%
  as_tibble()

# Save out the redacted MP follower adjacency list to CSV 

fwrite(mp_adj_list_joined_redacted_final,"mp_follower_adjacency_list_joined_redacted.csv",
       row.names = FALSE)

#### ---- END ---- ####
