## -- The purpose of this code is to calculate summary statistics of the final follower profile network data -- ##
## -- and the foller profile metadata. These datasets are then filtered following the criteria laid out in   -- ##
## -- the accompanying research note. These filtered datasets are saved out to CSV, ready for analysis.      -- ##

options(scipen = 999)

#### ---- LOAD PACKAGES ---- ####

library(dplyr)
library(data.table)
library(dtplyr)
library(moments) # for the Skewness function

#### ---- LOAD FINAL DATAFILES ---- ####

# MP political attributes 

mp_attributes <- read.csv("mp_political_attributes_22-08-22.csv")

# MP profile metadata 

mp_profile_metadata <- read.csv("mp_profile_metadata_22-08-22.csv")

# MP follower adjacency list

mp_follower_adj_list <- fread("mp_follower_adjacency_list_joined_redacted.csv")

# Follower profile metadata

follower_profile_metadata <- fread("follower_profile_metadata_joined_redacted.csv")

#### ---- DATASET SUMMARY STATISTICS ---- ####

## -- MP Twitter Profile Metadata -- ##

# Match the MP Twitter profile metadata to the MP's political party

mp_profile_metadata <- mp_attributes %>% 
  select(screen_name,party) %>% 
  rename(username = screen_name) %>%
  full_join(mp_profile_metadata,by="username")

# Summarise MP Twitter profile data overall 

mp_profile_metadata_summary <- data.frame(overall = nrow(mp_profile_metadata),
                                          on_twitter = sum(!is.na(mp_profile_metadata$username)),
                                          median_followers = median(mp_profile_metadata$followers_count,na.rm=T),
                                          median_following = median(mp_profile_metadata$following_count,na.rm=T),
                                          median_tweets = median(mp_profile_metadata$tweet_count,na.rm=T))

mp_profile_metadata_summary$perc_on_twitter <- round(mp_profile_metadata_summary$on_twitter/
                                                     mp_profile_metadata_summary$overall,2)

# Summarise MP Twitter profile data by party

mp_profile_metadata_summary_by_party <- mp_profile_metadata %>% 
  group_by(party) %>% 
  summarise(overall = n(),
            on_twitter = sum(!is.na(username)),
            median_followers = median(followers_count,na.rm=T),
            median_following = median(following_count,na.rm=T),
            median_tweets = median(tweet_count,na.rm=T))

mp_profile_metadata_summary_by_party$perc_on_twitter <- round(
  mp_profile_metadata_summary_by_party$on_twitter/mp_profile_metadata_summary_by_party$overall,2)
    
## -- MP Follower Adjacency List -- ## 

# Number of overall connections 

nrow(mp_follower_adj_list) # 34,653,181

# Number of MPs

mps <- unique(mp_follower_adj_list$mp_username) 
length(mps) # 591

# Number of ordinary users

users <- unique(mp_follower_adj_list$follower_id)
length(users) # 11,071,104

## -- Follower Profile Metadata -- ##

follower_profile_metadata_summary <- data.frame(overall = nrow(follower_profile_metadata),
                                                no_verified = sum(follower_profile_metadata$verified == TRUE),
                                                median_followers = median(follower_profile_metadata$followers_count, na.rm = T),
                                                median_following = median(follower_profile_metadata$following_count, na.rm = T),
                                                median_tweets = median(follower_profile_metadata$tweet_count, na.rm = T),
                                                median_listed = median(follower_profile_metadata$listed_count, na.rm = T),
                                                median_mps_followed = median(follower_profile_metadata$mps_followed,na.rm = T))

follower_profile_metadata_summary$perc_verified <- round(follower_profile_metadata_summary$no_verified/
                                                           follower_profile_metadata_summary$overall,3)

#### ---- APPLY FILTERING CRITERIA TO THE DATAFILES ---- ####

# Firstly, filter out profiles that have less than 25 followers and have sent less than 100 tweets.
# This follows Barbera's (2015) filtering criteria to remove inactive or spam accounts. 

follower_profile_metadata_filtered <- follower_profile_metadata %>%
  filter(followers_count >= 25 & tweet_count >= 100) %>%
  as_tibble()

nrow(follower_profile_metadata_filtered) # 4,460,657

# Summary statistics of ordinary user metadata after applying filters

follower_profile_metadata_filtered_summary <- data.frame(overall = nrow(follower_profile_metadata_filtered),
                                                no_verified = sum(follower_profile_metadata_filtered$verified == TRUE),
                                                median_followers = median(follower_profile_metadata_filtered$followers_count, na.rm = T),
                                                median_following = median(follower_profile_metadata_filtered$following_count, na.rm = T),
                                                median_tweets = median(follower_profile_metadata_filtered$tweet_count, na.rm = T),
                                                median_listed = median(follower_profile_metadata_filtered$listed_count, na.rm = T),
                                                median_mps_followed = median(follower_profile_metadata_filtered$mps_followed,na.rm = T))

follower_profile_metadata_filtered_summary$perc_verified <- round(follower_profile_metadata_filtered_summary$no_verified/
                                                           follower_profile_metadata_filtered_summary$overall,3)

# Next, filter the dataset to include only the most politically informative users. Again, following Barbera's 
# original criteria, exclude users who follow less than 10 MPs. 

median(follower_profile_metadata_filtered$mps_followed) # 1 MP

# Plot the distribution of the number of MPs followed by each user as a histogram 

mps_followed_histogram <- ggplot(follower_profile_metadata_filtered,aes(mps_followed)) +
  geom_histogram(bins = 100) +
  scale_y_log10() +
  labs(x = "MPs Followed",
       y = "Frequency (Log10)") +
  theme_bw()

ggsave("mps_followed_histogram.png",
       mps_followed_histogram,
       units="in", width=7, height=4, dpi=300,
       bg="white")

# Summary statistics of the number of MPs followed by each user in the filtered dataset

nrow(follower_profile_metadata_filtered) # 4,460,657
summary(follower_profile_metadata_filtered$mps_followed)
sd(follower_profile_metadata_filtered$mps_followed) # 12.64
skewness(follower_profile_metadata_filtered$mps_followed) # 14.85

# Apply the follows at least 10 MPs filter to the dataset (informative users)

follower_profile_metadata_informative <- follower_profile_metadata_filtered %>%
  filter(mps_followed >= 10) %>%
  as_tibble()

# Summary statistics of the especially informative subset of ordinary users

follower_profile_metadata_informative_summary <- data.frame(overall = nrow(follower_profile_metadata_informative),
                                                         no_verified = sum(follower_profile_metadata_informative$verified == TRUE),
                                                         median_followers = median(follower_profile_metadata_informative$followers_count, na.rm = T),
                                                         median_following = median(follower_profile_metadata_informative$following_count, na.rm = T),
                                                         median_tweets = median(follower_profile_metadata_informative$tweet_count, na.rm = T),
                                                         median_listed = median(follower_profile_metadata_informative$listed_count, na.rm = T),
                                                         median_mps_followed = median(follower_profile_metadata_informative$mps_followed,na.rm = T))

follower_profile_metadata_informative_summary$perc_verified <- round(follower_profile_metadata_informative_summary$no_verified/
                                                                    follower_profile_metadata_informative_summary$overall,3)

# Save the profile metadata for the especially informative subset of ordinary users

fwrite(follower_profile_metadata_informative,"follower_profile_metadata_informative_users.csv",row.names = FALSE)

# Now that the ordinary user profile metadata has been filtered to only include especially informative users, 
# filter the adjacency list to only include these informative users

informative_user_ids <- follower_profile_metadata_informative$follower_id

mp_follower_adj_list_informative <- mp_follower_adj_list %>% 
  filter(follower_id %in% informative_user_ids) %>%
  as_tibble()

# Summary statistics of the informative MP follower adjacency list 

# Number of MPs

length(unique(mp_follower_adj_list_informative$mp_username)) # 591

# Number of ordinary users 

length(unique(mp_follower_adj_list_informative$follower_id)) # 424,297

# Number of actual follower connections

nrow(mp_follower_adj_list_informative) # 11,443,165

# Number of potential connections 

length(unique(mp_follower_adj_list_informative$mp_username)) * 
  length(unique(mp_follower_adj_list_informative$follower_id)) # 250,759,527 

# Percentage of potential connections realised 

nrow(mp_follower_adj_list_informative) / (length(unique(mp_follower_adj_list_informative$mp_username)) * 
  length(unique(mp_follower_adj_list_informative$follower_id))) # 5%

# Save the MP follower adjacency list for the especially informative subset of ordinary users

fwrite(mp_follower_adj_list_informative,"mp_follower_adjacency_list_informative_users.csv",row.names = FALSE)

#### ---- END ---- ####
