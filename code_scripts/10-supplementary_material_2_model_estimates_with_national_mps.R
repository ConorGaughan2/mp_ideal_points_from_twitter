## -- The purpose of this script is to compare the original CA modelideal point estimates against -- ##
## -- the ideal point estimates from the model when including nationalist MPs in the original scaling -- ##
## -- procedure. Ideal points from the nationalist model are plotted in a single beeswarm plot, and also -- ##
## -- plotted as a scatterplot against the original model estimates for comaprison.                      -- ##

options(scipen=999)

#### ---- LOAD PACKAGES ---- ####

library(dplyr)
library(data.table)
library(ggplot2)
library(ggbeeswarm)

#### ---- LOAD DATA ---- ####

# Original CA model estimates

ca_model_estimates <- fread("mp_follower_ca_model_estimates.csv")

# CA model including national MPs

load("mp_follower_ca_model_including_national_mps.rdata")

# MP political attributes 

mp_attributes <- read.csv("mp_political_attributes_22-08-22.csv")

##### ---- MODEL ESTIMATES COMPARISON ---- #####

# First, extract the positional coordinates for the three dimensions in the model and store them in a
# dataframe (model including nationalist MPs)

ca_model_estimates_with_nations <- data.frame(screen_name = mp_follower_ca_model$colnames,
                                              svd.phi_1 = mp_follower_ca_model$colcoord[,1],
                                              svd.phi_2 = mp_follower_ca_model$colcoord[,2],
                                              svd.phi_3 = mp_follower_ca_model$colcoord[,3],
                                              stringsAsFactors=F)

# Reverse the ideal points (coordinates scale out randomly in either direction. Ita appears that the right/left
# axis has been reversed in the national model)

ca_model_estimates_with_nations$svd.phi_1_reversed <- ca_model_estimates_with_nations$svd.phi_1 * -1

# Match MPs to their political parties

ca_model_estimates_with_nations <- mp_attributes %>% 
  select(screen_name,party) %>%
  inner_join(ca_model_estimates_with_nations,by="screen_name")

# Beeswarm 1-dimensional ideal point plot of nationalist model 

party_dim1_plot_with_nations <- ggplot(ca_model_estimates_with_nations,aes(x=party,y=svd.phi_1_reversed,color=party)) +
  geom_beeswarm(cex = 0.7,size = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title="",
       y = "Ideal Point",x = "") +
  theme(legend.position = "None") +
  geom_hline(yintercept = 0,linetype="dashed") +
  scale_color_manual(values = c("blue2","gold4","deepskyblue","firebrick","green2","snow4","red","orange","seagreen","palegreen3","palegreen2","yellow2","black"))

ggsave("ca_follower_model_1st_dimension_plot_with_nations.png",
       party_dim1_plot_with_nations,
       units="in", width=7, height=4, dpi=300,
       bg="white")

# Plot original estimates against the estimates from the model that includes nationalist party MPs

ca_model_estimates_compared <- inner_join(ca_model_estimates,ca_model_estimates_with_nations,by="screen_name",suffix=c("_original","_nations"))

ca_model_compare <- ggplot(ca_model_estimates_compared,aes(x=svd.phi_1_original,y=svd.phi_1_reversed,color=party_original)) +
  geom_point() +
  labs(x = "Original Model Estimates",
       y = "Model Estimates Including Nationalist MPs",
       color = "Party") +
  theme_minimal() +
  scale_color_manual(values = c("blue2","gold4","deepskyblue","firebrick","green2","snow4","red","orange","seagreen","palegreen3","palegreen2","yellow2","black"))

ggsave("ca_follower_model_compare_with_nations.png",
       ca_model_compare ,
       units="in", width=7, height=4, dpi=300,
       bg="white")

#### ---- END ---- ####