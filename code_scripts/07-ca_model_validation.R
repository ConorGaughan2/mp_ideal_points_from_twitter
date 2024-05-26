options(scipen = 999)

#### ---- LOAD PACKAGES ---- ####

library(tidyverse)
library(ggplot2)
library(ggrepel)

#### ---- SET DIRECTORIES ---- ####

setwd("C:/Users/cg1g21/OneDrive - University of Southampton/PhD Politics/academic_projects/bayesian_spatial_following_model/data/")

#### ---- LOAD DATA ---- ####

# MP political attributes 

mp_attributes <- read.csv("final_data_files/final_datasets/mp_political_attributes_22-08-22.csv")

# CA model estimates 

ca_model_estimates <- read.csv("final_data_files/final_datasets/mp_follower_ca_model_estimates.csv")

# Expert survey validation estimates

expert_estimates <- read.csv("final_data_files/final_datasets/ideology_scores_expert_survey_aggregated.csv") %>%
  filter(Group == "MP") %>%
  rename(name = Name)

#### ---- VALIDATE THE CA FOLLOWER MODEL IDEAL POINT ESTIMATES AGAINST MEAN EXPERT ESTIMATES ---- #### 

# Merge the CA model estimates with the expert survey estimates dataset

ca_model_estimates <- mp_attributes %>% select(name,screen_name) %>% left_join(ca_model_estimates,by="screen_name")

estimates_compared <- left_join(expert_estimates,ca_model_estimates,by="name")

# Run correlation tests (Pearson's R)

# Overall

cor(estimates_compared$svd.phi_1,estimates_compared$Mean) # 0.97

# Just Conservative MPs

estimates_compared_con <- estimates_compared %>% filter(party == "Conservative")

cor(estimates_compared_con$svd.phi_1,estimates_compared_con$Mean)  # 0.84

# Just Labour MPs

estimates_compared_lab <- estimates_compared %>% filter(party == "Labour")

cor(estimates_compared_lab$svd.phi_1,estimates_compared_lab$Mean)  # 0.81

# Fit a weighted OLS linear regression model that predicts the expert estimates using ideal point estimates from
# the follower CA model. Weights are applied to the expert estimates using their standard errors to account for
# their degree of uncertainty

estimates_compared$std.error <- estimates_compared$Std.Deviation/sqrt(estimates_compared$Count)

model_weights <- 1 / estimates_compared$std.error

ca_follower_validation_mod <- lm(Mean ~ svd.phi_1,data = estimates_compared,weights = model_weights)
summary(ca_follower_validation_mod) # R-Squared = 0.93

# Plot the validation model as a scatterplot coloured by party

expert_validation_mod_plot <- ggplot(estimates_compared, 
                                  aes(x = svd.phi_1, y = Mean, color = party, shape = party, label = name)) +
  geom_point() +  
  geom_text_repel(
    aes(label = name),
    size = 5/.pt, # font size 5 pt
    point.padding = 0.1, 
    box.padding = 0.6,
    min.segment.length = 0,
    max.overlaps = 1000,
    seed = 7654) +
  geom_smooth(method = "lm",aes(group = 1),color="black") +  
  labs(x = "Follower CA Model Estimates", 
       y = "Expert Estimates",color="Party",shape="Party")+
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_manual(values=c("deepskyblue","green2","snow4","red","orange")) +
  annotate("text", x = -1, y = 11, label = paste("R^2 ==",0.93),parse = TRUE) +
  annotate("text", x = 0.1, y = 9.7, label = "Con") +
  annotate("text", x = 0.33, y = 9.7, label = paste("italic(r) ==",0.84), parse = TRUE) +
  annotate("text", x = 0, y = 3, label = "Lab") +
  annotate("text", x = 0.23, y = 3, label = paste("italic(r) ==",0.81), parse = TRUE)
  
ggsave("final_data_files/final_graphs/follower_ca_mod_validated_against_expert_estimates.png",
       expert_validation_mod_plot,
       units="in", width=7, height=4, dpi=300,
       bg="white")


