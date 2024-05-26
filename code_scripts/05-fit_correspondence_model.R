## -- The purpose of this script is to run correspondence analysis on the MP follower adjacency matrix   -- ##
## -- with the informative subset of users. It fits two correspondence models: one excluding nationalist -- ##
## -- MPs as supplementary columns and the second including them for comparison. Both models are saved   -- ##
## -- out as rdata files.                                                                                -- ##

options(scipen=999)

#### ---- LOAD PACKAGES ---- ####

library(ca) # package used for correspondence analysis
library(Matrix)
library(dplyr)
library(data.table)

#### ---- LOAD DATA ---- ####

# MP follower adjacency matrix (informative users)

load("mp_follower_adjacency_matirx_informative_users.rdata")

# Follower profile metadata 

follower_profile_metadata <- fread("follower_profile_metadata_informative_users.csv")

# MP profile metadata

mp_profile_metadata <- read.csv("mp_profile_metadata_22-08-22.csv")

# MP political attributes 

mp_attributes <- read.csv("mp_political_attributes_22-08-22.csv")

#### ---- FIT THE CORRESPONDENCE MODEL ---- #### 

# Convert MP follower adjacency matrix to standard matrix format 

mp_follower_adj_matrix <- as.matrix(mp_follower_adj_matrix)

# The shared regional component for MPs from nationalist parties in the U.K overwhelms the ideological 
# one and subsequently skews the ideological scale. Therefore, MPs from nationalist parties are initially
# excluded from the model scaling and added in as supplementary columns retroactively. This is done using
# the supcol parameter in the ca function. 

# National parties 

national_parties <- c("Alba","Alliance","DUP","Plaid Cymru","SDLP","Sinn Fein","SNP")

# National party MPs

national_mps <- mp_attributes %>% 
  filter(party %in% national_parties) %>%
  select(screen_name) %>%
  filter(!is.na(screen_name))

national_mps <- national_mps$screen_name

# Set national party MPs as supplementary columns by finding their positions in the matrix 

supcol <- which(colnames(mp_follower_adj_matrix) %in% national_mps)

colnames(mp_follower_adj_matrix)[supcol] 

# There are a small number of users who exclusively follow only national MPs. This means that when 
# treating national party MPs as supplementary columns, these ordinary users do not have any MPs in 
# the matrix left that they follow. Therefore, in order for the CA model to run, these users will need
# to be specified as supplementary rows. 

# Find users who only follow national party MPs

adj_matrix_no_national_mps <- mp_follower_adj_matrix[,setdiff(colnames(mp_follower_adj_matrix),national_mps)]

suprow <- which(rowSums(adj_matrix_no_national_mps) == 0)

# Fit the correspondence model with 3 dimensions returned in the output, including supplementary columns
# and rows

set.seed(12345)

start_time <- Sys.time()
mp_follower_ca_model <- ca(mp_follower_adj_matrix, nd=3, supcol = supcol, suprow = suprow)
end_time <- Sys.time()

elapsed_time <- end_time - start_time # approx. 10 mins

save(mp_follower_ca_model, file="mp_follower_ca_model.rdata")

# Fit the correspondence model with 3 dimensions returned in the output, without supplementary columns
# and rows (this allows for comparisons between ideal points when not excluding nationalist MPs)

set.seed(12345)

start_time <- Sys.time()
mp_follower_ca_model <- ca(mp_follower_adj_matrix, nd=3)
end_time <- Sys.time()

elapsed_time <- end_time - start_time # approx. 10 mins

save(mp_follower_ca_model, file="mp_follower_ca_model_including_national_mps.rdata")

