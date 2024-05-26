## -- The purpose of this script is to take the MP follower adjacency list containing the subset of especially -- ##
## -- informative users and convert it to an adjacency matrix. This matrix is large and sparse, saved as an -- ##
## -- rdata file for efficiency. MP usernames form the columns and follower redacted IDs form the rows.     -- ##

options(scipen = 999)

#### ---- LOAD PACKAGES ---- ####

library(dplyr)
library(Matrix)
library(data.table)

#### ---- LOAD DATA ---- ####

# MP Follower adjacency list

mp_follower_adj_list <- fread("mp_follower_adjacency_list_informative_users.csv")

# Follower profile metadata 

follower_profile_metadata <- fread("follower_profile_metadata_informative_users.csv")

# MP profile metadata

mp_profile_metadata <- read.csv("mp_profile_metadata_22-08-22.csv")

#### ---- CONVERT ADJACENCY LIST TO MATRIX ---- ####

# Generate matrix i,j coordinates

mps <- unique(mp_follower_adj_list$mp_username)
followers <- unique(mp_follower_adj_list$follower_id)

mp_coords <- data.frame(mp_username = mps,
                        j_coord = c(1:length(mps)))

follower_coords <- data.frame(follower_id = followers,
                              i_coord = c(1:length(followers)))

adj_mat_coords <- mp_follower_adj_list %>%
  left_join(mp_coords,by="mp_username") %>%
  left_join(follower_coords,by="follower_id") %>%
  as_tibble()

i <- adj_mat_coords$i_coord
j <- adj_mat_coords$j_coord

# Populate sparse adjacency matrix 

mp_follower_adj_matrix <- sparseMatrix(i=i,j=j,x=1)

rownames(mp_follower_adj_matrix) <- followers
colnames(mp_follower_adj_matrix) <- mps

# Save MP follower adjacency matrix out as rdata

save(mp_follower_adj_matrix,file="mp_follower_adjacency_matirx_informative_users.rdata")
