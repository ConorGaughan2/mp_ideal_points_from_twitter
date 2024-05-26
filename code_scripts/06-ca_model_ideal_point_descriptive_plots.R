## -- The purpose of this script is to 


options(scipen = 999)

#### ---- LOAD PACKAGES ---- ####

library(dplyr) 
library(data.table) 
library(ggbeeswarm) # for generating beeswarm plots
library(ggplot2) 
library(moments) 
library(factoextra) # for extracting CA model coefficients

#### ---- LOAD DATA ---- ####

# MP profile metadata

mp_profile_metadata <- fread("mp_profile_metadata_22-08-22.csv")

# MP political attributes 

mp_attributes <- fread("mp_political_attributes_22-08-22.csv")

# MP follower correspondence model 

load("mp_follower_ca_model.rdata")

##### ---- MODEL ESTIMATES DESCRIPTIVE ANALYSIS ---- #####

# First, extract the positional coordinates for the three dimensions in the model and store them in a
# dataframe

ca_model_estimates <- data.frame(screen_name = mp_follower_ca_model$colnames,
                                 svd.phi_1 = mp_follower_ca_model$colcoord[,1],
                                 svd.phi_2 = mp_follower_ca_model$colcoord[,2],
                                 svd.phi_3 = mp_follower_ca_model$colcoord[,3],
                                 stringsAsFactors=F)

# Match MPs to their political parties

ca_model_estimates <- mp_attributes %>% 
  select(screen_name,party) %>%
  inner_join(ca_model_estimates,by="screen_name")

# Add Sinn Fein accent for plotting!

ca_model_estimates$party <- ifelse(ca_model_estimates$party == "Sinn Fein", "Sinn Féin", ca_model_estimates$party)

# Change Liberal Democrats to Lib Dems for easier plot interpretability 

ca_model_estimates$party <- ifelse(ca_model_estimates$party == "Liberal Democrats", "Lib Dem", ca_model_estimates$party)

# Save CA model estimates to CSV 

fwrite(ca_model_estimates,file="mp_follower_ca_model_estimates.csv",row.names = F)

## -- CA MODEL SUMMARY STATISTICS -- ##

# CA Model Eigenvalues

eigenvalues <- get_eigenvalue(mp_follower_ca_model)
eigenvalues

# Generate scree plot (This shows the informativeness of each model dimension)

ca_model_scree <- fviz_screeplot(mp_follower_ca_model, ncp = 10, addlabels = T)

ggsave("ca_follower_model_scree_plot.png",
       ca_model_scree ,
       units="in", width=7, height=4, dpi=300,
       bg="white")

## -- DIMENSION 1 - OVERALL PARTY DISTRIBUTION -- ##

# Overall party level summary statistics

party_summary_dim1 <- ca_model_estimates %>% 
  group_by(party) %>%
  summarise(n = n(),
            mean = mean(svd.phi_1),
            median = median(svd.phi_1),
            s.d = sd(svd.phi_1),
            min = min(svd.phi_1),
            max = max(svd.phi_1),
            skewness = skewness(svd.phi_1)) 

# Rank the beeswarm plot by median party ideal point

ca_model_estimates$party <- factor(ca_model_estimates$party, levels = party_summary_dim1$party[order(party_summary_dim1$median, decreasing = FALSE)])

# Plot the first dimension (ideal points) on a discrete beeswarm graph, grouped by party

party_dim1_plot <- ggplot(ca_model_estimates,aes(x=party,y=svd.phi_1,color=party)) +
  geom_beeswarm(cex = 0.7,size = 1, alpha = 0.7) +
  coord_flip() +
  theme_bw() +
  labs(y = "Ideal Point",x = "") +
  theme(legend.position = "None") +
  theme(axis.text.y = element_text(size = 12)) +
  geom_hline(yintercept = 0,linetype="dashed") +
  expand_limits(y=c(-2,2)) +
  scale_color_manual(values = rev(c("deepskyblue", "firebrick", "black", "gold4", "seagreen", 
                                "palegreen3", "palegreen2", "blue2", "orange", "yellow2", 
                                "snow4", "red", "green2")))

ggsave("ca_follower_model_1st_dimension_plot.png",
       party_dim1_plot,
       units="in", width=7, height=4, dpi=300,
       bg="white")

## -- DIMENSION 1 - WITHIN PARTY DISTRIBUTION -- ##

# Merge the CA model estimates with the MP political attributes dataset

ca_model_estimates_by_factions <- inner_join(ca_model_estimates,mp_attributes,by="screen_name") %>%
  rename(party = party.x)

# Select only Conservative and Labour MPs

ca_model_estimates_by_factions <- ca_model_estimates_by_factions %>% 
  filter(party %in% c("Conservative","Labour"))
  
# Duplicate the faction data three times, selecting only one faction at a time in order to get the ideological factions into one column
# for plotting. A new column will be created for each which contains the faction grouping type. 

ca_model_faction_abortion <- ca_model_estimates_by_factions %>% 
  select(party,svd.phi_1,abortion_vote_2022) %>%
  mutate(faction_type = "Abortion Stance") %>%
  rename(group = abortion_vote_2022)

ca_model_faction_brexit <- ca_model_estimates_by_factions %>% 
  select(party,svd.phi_1,brexit_stance) %>%
  mutate(faction_type = "Brexit Stance") %>%
  rename(group = brexit_stance)

ca_model_faction_party <- ca_model_estimates_by_factions %>% 
  select(party,svd.phi_1,party_faction) %>%
  mutate(faction_type = "Party Faction") %>%
  rename(group = party_faction)

ca_model_estimates_by_factions <- rbind(ca_model_faction_abortion,ca_model_faction_brexit,ca_model_faction_party)

# Drop NA values and convert Brexit values to meaningful labels and Party Faction to shorthand

ca_model_estimates_by_factions <- ca_model_estimates_by_factions %>% 
  filter(!is.na(group)) %>%
  mutate(group = case_when(
    group == "1" ~ "Leave",
    group == "2" ~ "Remain",
    group == "Socialist Campaign Group" ~ "SCG",
    group == "European Research Group" ~ "ERG",
    TRUE ~ group  
  ))

# Plot ideal points boxplots with jitter points, grouped by different faction types on the y-axis, coloured by party

faction_plot <- ggplot(ca_model_estimates_by_factions, aes(x = svd.phi_1, y = group, fill = party)) +
  geom_boxplot(width = 0.5, alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(colour = party), alpha = 0.1, size = 0.5) +  
  facet_wrap(~ faction_type, scales = "free_y", nrow = 3) +
  labs(x = "Ideal Point",
       y = "",
       fill = "Party") +
  theme_bw() +
  xlim(-2, 2) +  
  geom_vline(xintercept = 0, linetype = "solid") +  
  geom_vline(xintercept = party_summary_dim1$median[party_summary_dim1$party == "Conservative"], 
             linetype = "dashed", colour = "deepskyblue") +
  geom_vline(xintercept = party_summary_dim1$median[party_summary_dim1$party == "Labour"], 
             linetype = "dashed", colour = "red") + 
  annotate("text", x = 1.05, y = 1.4, label = "Median Con MP", size = 2, color = "deepskyblue") +
  annotate("text", x = -0.47, y = 1.4, label = "Median Lab MP", size = 2, color = "red") +
  theme(
    legend.position = "top"  
  ) +
  guides(color = "none") +
  scale_fill_manual(values = c("red", "deepskyblue")) +
  scale_color_manual(values = c("red", "deepskyblue"))

ggsave("overall_party_ideal_points_factions_boxplot_tall.png",
       faction_plot,
       units="in", width=7, height=5, dpi=300,
       bg="white")

#### ---- END ---- ####