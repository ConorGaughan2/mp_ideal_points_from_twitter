## -- The purpose of this script is to fit binary logistic regression models using the GLM function,        -- ##
## -- predicting Conservative party leadership candidate endorsement based on an MP's modelled ideal point. -- ##
## -- Descriptive statistics of candidate endorsement based on MP ideal points and a select group of other  -- ##
## -- relevant variables are also calculated and plotted.                                                   -- ## 

options(scipen = 999)

#### ---- LOAD PACKAGES ---- ####

library(dplyr)
library(ggplot2)
library(data.table)

#### ---- LOAD DATA ---- ####

# MP political attributes 

mp_attributes <- fread("mp_political_attributes_22-08-22.csv")

# MP constituency attributes 

mp_constituency <- fread("mp_constituency_attributes_22-08-22.csv") %>%
  rename(name = Name)

# CA model estimates 

ca_model_estimates <- fread("mp_follower_ca_model_estimates.csv")

# Conservative MP leadership nominations

con_nominations <- fread("conservative_home_nominations_sept_2022.csv")

#### ---- PREDICTING PARTY LEADERSHIP ENDORSEMENT USING MP IDEAL POINTS  ---- ####

# Bind CA model estimates with Conservative candidate nominations by name

ca_model_estimates <- mp_attributes %>% 
  select(screen_name,name) %>% 
  inner_join(ca_model_estimates,by="screen_name")
  
con_nominations_ideal_points <- left_join(con_nominations,ca_model_estimates,by="name") %>%
  filter(!is.na(screen_name))

nrow(con_nominations_ideal_points) # 278 observations

# Overall summary statistics of the ideal point estimates of all Conservative MPs

just_con_mps <- ca_model_estimates %>% filter(party == "Conservative")

nrow(just_con_mps) # n = 312
mean(just_con_mps$svd.phi_1) # mean = 1.26
median(just_con_mps$svd.phi_1) # median = 1.29
sd(just_con_mps$svd.phi_1) # s.d = 0.28
min(just_con_mps$svd.phi_1) # min = 0.16
max(just_con_mps$svd.phi_1) # max = 1.93

# Summary of the ideal point estimates of Conservative MPs who endorsed each candidate in the first round (Initial)

con_initial_nominations_ideal_point_summary <- con_nominations_ideal_points %>%
  group_by(initial_endorsement) %>%
  summarise(count = n(),
            mean_ideal_point = mean(svd.phi_1),
            median_ideal_point = median(svd.phi_1),
            s.d = sd(svd.phi_1),
            min_ideal_point = min(svd.phi_1),
            max_ideal_point = max(svd.phi_1))

# Rank the candidate boxplots by median MP ideal point

con_nominations_ideal_points$initial_endorsement <- factor(con_nominations_ideal_points$initial_endorsement, 
                                                           levels = con_initial_nominations_ideal_point_summary$initial_endorsement[order(con_initial_nominations_ideal_point_summary$median_ideal_point, 
                                                                                                                                          decreasing = TRUE)])

# Plot ideal point distribution of Conservative MPs by the candidate they backed as boxplots

con_initial_nominations_ideal_point_boxplot <- ggplot(con_nominations_ideal_points, aes(x = svd.phi_1, y = initial_endorsement)) +
  geom_boxplot(width = 0.3, alpha = 0.2, outlier.shape = NA, fill = "deepskyblue") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.3, color = "deepskyblue") +
  geom_vline(xintercept = median(just_con_mps$svd.phi_1),linetype="dashed") +
  labs(title = "Median Con MP",x = "Ideal Point", y = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.64,size = 8),
        axis.text.y = element_text(size = 12)) 

ggsave("conservative_initial_leadership_endorsement_boxplot.png",
       con_initial_nominations_ideal_point_boxplot,
       units="in", width=7, height=5, dpi=300,
       bg="white")

# Summary of the ideal point estimates of Conservative MPs who endorsed each candidate in the membership round (Final)

con_final_nominations_ideal_point_summary <- con_nominations_ideal_points %>%
  group_by(final_endorsement) %>%
  summarise(count = n(),
            mean_ideal_point = mean(svd.phi_1),
            median_ideal_point = median(svd.phi_1),
            s.d = sd(svd.phi_1),
            min_ideal_point = min(svd.phi_1),
            max_ideal_point = max(svd.phi_1))

# Again, rank the candidate boxplots by median MP ideal point

con_nominations_ideal_points$final_endorsement <- factor(con_nominations_ideal_points$final_endorsement, 
                                                           levels = c("Truss","Sunak"))                                                                     

# Plot ideal point distribution of Conservative MPs by the candidate they backed as boxplots

con_final_nominations_ideal_point_boxplot <- ggplot(con_nominations_ideal_points[!is.na(con_nominations_ideal_points$final_endorsement),], aes(x = svd.phi_1, y = final_endorsement)) +
  geom_boxplot(width = 0.3, alpha = 0.2, outlier.shape = NA, fill = "deepskyblue") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.3, color = "deepskyblue") +
  geom_vline(xintercept = median(just_con_mps$svd.phi_1),linetype="dashed") +
  labs(title = "Median Con MP",x = "Ideal Point", y = "") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.61,size = 8),
        axis.text.y = element_text(size = 12))

ggsave("conservative_final_leadership_endorsement_boxplot.png",
       con_final_nominations_ideal_point_boxplot,
       units="in", width=7, height=5, dpi=300,
       bg="white")

#### -- FIT LOGISTIC REGRESSION MODELS TO PREDICT CANDIDATE ENDORSEMENT USING MP IDEAL POINTS -- ####

# Summary statistics of model control variables (complete observations)

complete_obs <- left_join(con_nominations,mp_attributes,by="name") %>%
  left_join(mp_constituency, by = "name")

summarize_by_column <- function(column_name) {
  complete_obs %>%
    group_by(final_endorsement, .data[[column_name]]) %>%
    summarise(
      count = n(),
      .groups = 'drop'
    ) %>%
    # Calculate the total count within each final_endorsement group
    group_by(final_endorsement) %>%
    mutate(
      total = sum(count),
      percentage = count / total * 100
    )
}

control_variables_summary <- lapply(c("gender","ethnic_minority","school_type","uni_type",
                                      "cohort","minister_sept"), summarize_by_column) 

print(control_variables_summary)

constituency_majority_summary <- complete_obs %>%
  group_by(final_endorsement) %>%
  summarise(median(Maj19sh))

print(constituency_majority_summary)

# Merge Conservative nomination data with the MP attributes dataset

con_nominations_ideal_points <- left_join(con_nominations_ideal_points,mp_attributes,by="name")

# Then, merge Conservative nomination data with the constituency attributes dataset 

con_nominations_ideal_points <- left_join(con_nominations_ideal_points,mp_constituency,by="name")

# Convert categorical variables to factors and set their levels for the model 

con_nominations_ideal_points$final_endorsement <- factor(con_nominations_ideal_points$final_endorsement,levels = c("Sunak","Truss"))

con_nominations_ideal_points$gender <- factor(con_nominations_ideal_points$gender,levels = c(0,1))
con_nominations_ideal_points$ethnic_minority <- factor(con_nominations_ideal_points$ethnic_minority,levels = c(0,1))
con_nominations_ideal_points$school_type <- factor(con_nominations_ideal_points$school_type,levels = c("Independent/Fee-Paying",
                                                                                                       "Grammar",
                                                                                                       "Comprehensive/Academy/State-Funded Non-Selective"))
con_nominations_ideal_points$uni_type <- factor(con_nominations_ideal_points$uni_type, levels = c("Oxbridge",
                                                                                                  "Russell Group",
                                                                                                  "Non-Russell Group",
                                                                                                  "None"))

con_nominations_ideal_points$cohort <- factor(con_nominations_ideal_points$cohort, levels = c("Pre-1997",
                                                                                              "1997-2010",
                                                                                              "2010-2015",
                                                                                              "2015-2019",
                                                                                              "2019"))

con_nominations_ideal_points$minister_sept <- factor(con_nominations_ideal_points$minister_sept, levels = c("Current",
                                                                                                            "Former",
                                                                                                            "Never")) 

# Model 1: Just ideal points

model_1 <- glm(final_endorsement ~ svd.phi_1, data=con_nominations_ideal_points,na.action = na.omit,family = binomial)
summary(model_1)

round(exp(coef(model_1)),2) # convert model coefficients to odds ratios

round(DescTools::PseudoR2(model_1,which = c("McFadden", "CoxSnell", "Nagelkerke")),2) # psuedo R2

# Model 2: Ideal points + demographic variables (Gender, education, ethnicity, age)

model_2 <- glm(final_endorsement ~ svd.phi_1 + gender + ethnic_minority + year_of_birth + school_type + uni_type, data=con_nominations_ideal_points,na.action = na.omit,family = binomial)
summary(model_2)

round(exp(coef(model_2)),2) # convert model coefficients to odds ratios

round(DescTools::PseudoR2(model_2,which = c("McFadden", "CoxSnell", "Nagelkerke")),2) # psuedo R2

# Model 3: Ideal points + demographic variables + political variables (cohort, ministerial background, seat marginality)

model_3 <- glm(final_endorsement ~ svd.phi_1 + gender + ethnic_minority + year_of_birth + school_type + uni_type + cohort + minister_sept + Maj19sh, data=con_nominations_ideal_points,na.action = na.omit,family = binomial)
summary(model_3)

round(exp(coef(model_3)),2) # convert model coefficients to odds ratios

round(DescTools::PseudoR2(model_3,which = c("McFadden", "CoxSnell", "Nagelkerke")),2) # psuedo R2


#### ---- END ---- ####
