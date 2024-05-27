## -- The purpose of this script is to accomplish two things: 1) plot the ideal point estimates for MPs against the -- ##
## -- second modelled dimension in the CA model, and 2) extract the ordinary user ideal points from the CA model row -- ##
## -- coordinates and plot them along the first dimension. Ordinary users are also subsetted to only include elite -- ##
## -- accounts and plotted along the first dimension to assess the ideal point distribution of wider elite profiles -- ##

options(scipen=999)

#### ---- LOAD PACKAGES ---- ####

library(ca)
library(dplyr)
library(data.table)
library(ggplot2)

#### ---- LOAD DATA ---- ####

# Original CA model estimates

ca_model_estimates <- fread("mp_follower_ca_model_estimates.csv")

# Original CA model fit

load("mp_follower_ca_model.rdata")

# Follower profile metadata 

follower_profile_metadata <- fread("follower_profile_metadata_informative_users.csv")

#### ---- TWO-DIMENSIONAL PLOT ---- ####

# Plot the ideal point estimates against the second modelled dimension (whatever this may reflect...)

ca_mod_2d_plot <- ggplot(ca_model_estimates, aes(x = svd.phi_1, y = svd.phi_2, color = party)) +
  geom_point() +
  labs(x = "Dim 1 (L/R Ideology)", y = "Dim 2", color = "Party") +
  theme_minimal() +
  expand_limits(x=c(-2,2)) +
  geom_vline(xintercept = 0,linetype="dashed") +
  geom_hline(yintercept = 0,linetype="dashed")+
  scale_color_manual(values = c("blue2","gold4","deepskyblue","firebrick","green2","snow4","red","orange","seagreen","palegreen3","palegreen2","yellow2","black"))

ggsave("ca_follower_model_2_dimensions.png",
       ca_mod_2d_plot,
       units="in", width=7, height=4, dpi=300,
       bg="white")

#### ---- USER IDEAL POINTS ---- ####

# Extract user ideal points from the CA model row coordinates 

ca_model_user_estimates <- data.frame(screen_name = mp_follower_ca_model$rownames,
                                      svd.phi_1 = mp_follower_ca_model$rowcoord[,1],
                                      svd.phi_2 = mp_follower_ca_model$rowcoord[,2],
                                      svd.phi_3 = mp_follower_ca_model$rowcoord[,3],
                                      stringsAsFactors=F)

# Save estimates to CSV

fwrite(ca_model_user_estimates,file = "ca_model_user_estimates.csv",row.names = F)

# Plot distribution of all informative users along the first dimension 

user_dim1_distribution <- ggplot(ca_model_user_estimates, aes(x=svd.phi_1)) +
  geom_histogram(bins = 100, alpha = 0.8) +
  labs(x = "Ideal Point",
       y = "Count") +
  expand_limits(x=c(-2,2)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  theme_minimal()

ggsave("user_ideal_points_all.png",
       user_dim1_distribution,
       units="in", width=7, height=4, dpi=300,
       bg="white")

# Plot distribution of 'elite' users along the first dimension. This is ascertained by including users who are
# either verified and/or have more than 30,000 followers (Ofcom rule)

ca_model_user_estimates <- ca_model_user_estimates %>% 
  rename(follower_id = screen_name) 

ca_model_user_estimates <- left_join(ca_model_user_estimates,follower_profile_metadata,by = "follower_id")

ca_model_estimates_elite <- ca_model_user_estimates %>% filter(verified == "TRUE" | followers_count >= 30000) 

elite_dim1_distribution <- ggplot(ca_model_estimates_elite,aes(x=svd.phi_1)) +
  geom_histogram(bins = 100, alpha = 0.8) +
  labs(x = "Ideal Point",
       y = "Count") +
  expand_limits(x=c(-2,2)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  theme_minimal()

ggsave("user_ideal_points_elite.png",
       elite_dim1_distribution,
       units="in", width=7, height=4, dpi=300,
       bg="white")

#### ---- END ---- ####
