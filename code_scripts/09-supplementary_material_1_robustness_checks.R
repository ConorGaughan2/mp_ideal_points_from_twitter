## -- The purpose of this script is to run robustness checks on the CA model performance using -- ##
## -- different informative user sample thresholds. For the main paper, the sample of follower -- ##
## -- profiles is filtered to only include users who follow at least 10 MPs. This script sets -- ##
## -- different filter thresholds and runs mutiple CA models using these different subsets of -- ##
## -- followers. At smaller sample thresholds, fitting the CA model can be computationally    -- ##
## -- intensive. A HPC was used in the original analysis to help reduce the load (recommended). -- ##

options(scipen = 999)

#### ---- LOAD PACKAGES ---- ####

library(data.table)
library(dplyr)
library(ca)
library(ggplot2)
library(Matrix)
library(tidyr)

#### ---- LOAD DATA ---- ####

# Follower profile metadata 

follower_profile_metadata <- fread("follower_profile_metadata_joined_redacted.csv")

# MP profile metadata

mp_profile_metadata <- fread("mp_profile_metadata_22-08-22.csv")

# MP political attributes 

mp_attributes <- fread("mp_political_attributes_22-08-22.csv")

# Follower adjacency list 

mp_follower_adj_list <- fread("mp_follower_adjacency_list_joined_redacted.csv")

# Expert survey validation estimates

expert_estimates <- read.csv("ideology_scores_expert_survey_aggregated.csv") %>%
  filter(Group == "MP") %>%
  rename(name = Name)

#### ---- RUN THRESHOLD ROBUSTNESS CHECKS ---- ####

# Firstly, filter out profiles that have less than 25 followers and have sent less than 100 tweets.
# This follows Barbera's (2015) filtering criteria to remove inactive or spam accounts. 

follower_profile_metadata_filtered <- follower_profile_metadata %>%
  filter(followers_count >= 25 & tweet_count >= 100) %>%
  as_tibble()

nrow(follower_profile_metadata_filtered) # 4,460,657

rm(follower_profile_metadata) # Remove original metadata to save memory

# Then, check the robustness of the CA model estimates to changes in the informative user sample threshold. This is dictated by 
# the number of MPs a user follows. This is done by creating a for loop that runs the CA model on a number of follower adjacency 
# matrices that have filtered users at different thresholds of MPs followed. 

# Create the list sample thresholds to iterate through 

sample_thresholds <- c(1, 2, 3, 4, 5, 10, 15, 20, 30, 40, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500)

# Create an empty dataframe to store the model estimates for each iteration

model_estimates <- data.frame(screen_name = character(0), 
                              ideal_point = numeric(0),
                              iteration = numeric(0),
                              stringsAsFactors = F)

# Create an empty dataframe to store information about the sample data generated in each iteration

sample_details <- data.frame(iteratation = numeric(0),
                             threshold = numeric(0),
                             no_mps = numeric(0),
                             no_users = numeric(0),
                             no_connections = numeric(0),
                             stringsAsFactors = F)

# Loop through different sample thresholds and store the resulting estimates in the empty dataframes

set.seed(12345)
iteration <- 0

for(threshold in sample_thresholds){
  
  iteration <- iteration + 1
  
  # Filter the follower metadata including only users who follow at least the given threshold of MPs
  metadata_sample <- subset(follower_profile_metadata_filtered, mps_followed >= threshold)
  
  # Generate list of follower IDs from the filtered metadata
  sample_user_ids <- metadata_sample$follower_id
  
  # Filter the follower adjacency list using the list of follower IDs
  adj_list_sample <- mp_follower_adj_list %>% 
    filter(follower_id %in% sample_user_ids) %>%
    as_tibble()
  
  # Generate matrix i,j coordinates using sample data
  mps <- unique(adj_list_sample$mp_username)
  followers <- unique(adj_list_sample$follower_id)
  
  mp_coords <- data.frame(mp_username = mps,
                          j_coord = c(1:length(mps)))
  
  follower_coords <- data.frame(follower_id = followers,
                                i_coord = c(1:length(followers)))
  
  adj_mat_coords <- adj_list_sample %>%
    left_join(mp_coords,by="mp_username") %>%
    left_join(follower_coords,by="follower_id") %>%
    as_tibble()
  
  i <- adj_mat_coords$i_coord
  j <- adj_mat_coords$j_coord
  
  # Populate sparse adjacency matrix 
  adj_matrix_sample <- sparseMatrix(i=i,j=j,x=1)
  
  rownames(adj_matrix_sample) <- followers
  colnames(adj_matrix_sample) <- mps
  
  # Convert MP follower adjacency matrix to standard matrix format 
  adj_matrix_sample <- as.matrix(adj_matrix_sample)
  
  # Set national party MPs to supplementary columns
  national_parties <- c("Alba","Alliance","DUP","Plaid Cymru","SDLP","Sinn Fein","SNP")
  
  national_mps <- mp_attributes %>% 
    filter(party %in% national_parties) %>%
    select(screen_name) %>%
    filter(!is.na(screen_name))
  
  national_mps <- national_mps$screen_name
  
  supcol <- which(colnames(adj_matrix_sample) %in% national_mps)
  
  # Set users who only follow national MPs to supplementary rows 
  matrix_no_national_mps <- adj_matrix_sample[,setdiff(colnames(adj_matrix_sample),national_mps)]
  
  suprow <- which(rowSums(matrix_no_national_mps) == 0)
  
  # Fit the correspondence model with 3 dimensions returned in the output
  sample_ca_model <- ca(adj_matrix_sample, nd=3, supcol = supcol, suprow = suprow)
  
  # Store model results in the respective dataframes
  model_estimates <- rbind(model_estimates, data.frame(screen_name = sample_ca_model$colnames,
                                                       ideal_point = sample_ca_model$colcoord[,1],
                                                       iteration = iteration))
  
  sample_details <- rbind(sample_details, data.frame(iteration = iteration,
                                                     threshold = threshold,
                                                     no_mps = length(mps),
                                                     no_users = length(followers),
                                                     no_connections = nrow(adj_list_sample)))
  
  # Remove created objects to free up memory before starting the next iteration
  rm(metadata_sample,sample_user_ids,adj_list_sample,mps,followers,mp_coords,
     follower_coords,adj_mat_coords,i,j,adj_matrix_sample,national_parties,
     national_mps,supcol,matrix_no_national_mps,suprow,sample_ca_model)
  
  # Free memory 
  gc()
  
  print(paste("Iteration",iteration,"complete!"))
  
}

# Convert model estimates dataframe from long format to wide so that the ideal points for each MP 
# are side-by-side 

model_estimates_wide <- model_estimates %>%
  spread(key = iteration, value = ideal_point)

# Save both dataframes to CSV

fwrite(model_estimates_wide,"sample_robustness_checks_estimates.csv",row.names = F)

fwrite(sample_details,"sample_robustness_checks_details.csv",row.names = F)

# Produce plot that shows the model accuracy at each sample threshold, including both between and 
# within-party validation scores

# First, join the model estimates for each iteration with the expert validation estimates

colnames(model_estimates_wide) <- as.character(model_estimates_wide[1,])

model_estimates_wide <- model_estimates_wide %>% 
  filter(screen_name != "screen_name") %>% as.data.frame()

model_estimates_validation <- mp_attributes %>% 
  select(name,screen_name,party) %>%
  inner_join(model_estimates_wide,by="screen_name") %>%
  inner_join(expert_estimates,by="name") 


model_accuracy_scores <- data.frame(iteration = character(0),
                                    threshold = character(0),
                                    between_score = numeric(0),
                                    within_con_score = numeric(0),
                                    within_lab_score = numeric(0),
                                    stringsAsFactors = T)

# Run a for loop that evaluates the between and within-party accuracy of ideal points against the expert 
# estimates at every threshold

iteration <- 0

for(threshold in 1:length(sample_thresholds)){
  
  iteration <- iteration + 1
  col_name <- as.character(threshold)
  
  between_accuracy <- abs(cor(model_estimates_validation[col_name],
                              model_estimates_validation$Mean))
  
  con_mp_estimates <- subset(model_estimates_validation, party == "Conservative")
  lab_mp_estimates <- subset(model_estimates_validation, party == "Labour")
  
  within_con_accuracy <- abs(cor(con_mp_estimates[col_name],
                                 con_mp_estimates$Mean))
  within_lab_accuracy <- abs(cor(lab_mp_estimates[col_name],
                                 lab_mp_estimates$Mean))
  
  
  model_accuracy_scores <- rbind(model_accuracy_scores, data.frame(iteration = iteration,
                                                                   threshold = sample_thresholds[threshold],
                                                                   between_score = between_accuracy,
                                                                   within_con_score = within_con_accuracy,
                                                                   within_lab_score = within_lab_accuracy))
}

sample_details <- as.data.frame(left_join(sample_details,model_accuracy_scores,by="iteration"))

xtable::xtable(sample_details)

# Create a plot to compare model performance
model_metrics <- ggplot() +
  geom_line(data = model_accuracy_scores, aes(x = iteration, y = between_score, color = "Between-Party Accuracy", linetype = "Between-Party Accuracy")) +
  geom_point(data = model_accuracy_scores, aes(x = iteration, y = between_score, color = "Between-Party Accuracy")) +
  geom_line(data = model_accuracy_scores, aes(x = iteration, y = within_con_score, color = "Within-Party Accuracy (CON)", linetype = "Within-Party Accuracy (CON)")) +
  geom_point(data = model_accuracy_scores, aes(x = iteration, y = within_con_score, color = "Within-Party Accuracy (CON)")) +
  geom_line(data = model_accuracy_scores, aes(x = iteration, y = within_lab_score, color = "Within-Party Accuracy (LAB)", linetype = "Within-Party Accuracy (LAB)")) +
  geom_point(data = model_accuracy_scores, aes(x = iteration, y = within_lab_score, color = "Within-Party Accuracy (LAB)")) +
  labs(
    x = "Iteration",
    y = "Correlation Coefficient"
  ) +
  scale_color_manual(name = "Party",
                     values = c("Between-Party Accuracy" = "black", "Within-Party Accuracy (CON)" = "deepskyblue", "Within-Party Accuracy (LAB)" = "red"),
                     labels = c("Between-Party Accuracy", "Within-Party Accuracy (CON)", "Within-Party Accuracy (LAB)")
  ) +
  scale_x_continuous(breaks = seq(1, max(model_accuracy_scores$iteration), by = 1)) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 6, linetype = "dashed", color = "grey") +
  annotate("text",x = 4.2, y = 0.5, label = "Optimal Threshold", color = "black", size=2) +
  annotate("text",x = 7.3, y = 0.5, label = "Barberá's Threshold", color = "black", size=2) +
  scale_linetype_manual(name = "Party",
                        values = c("Between-Party Accuracy" = "solid", "Within-Party Accuracy (CON)" = "dotted", "Within-Party Accuracy (LAB)" = "dashed")
  ) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "top")

ggsave("ca_model_accuracy_thresholds.png",
       model_metrics,
       units="in", width=7, height=4, dpi=300,
       bg="white")
