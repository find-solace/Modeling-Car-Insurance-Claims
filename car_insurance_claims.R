#Loading libraries
library(pacman)
p_load(tidyverse, ggfortify, glue, yardstick, visdat)

#loading and inspecting the df
insurance_df <- read_csv("car_insurance.csv")

glimpse(insurance_df)

#removing customer id column
insurance_df <- insurance_df %>% select(-id)

#Examining null values
insurance_df %>% select_if(~any(is.na(.))) %>% summarize_all(~sum(is.na(.)))
vis_miss(insurance_df)

##1.1% of the data is missing, in credit_score(10% of the variable) 
#and annual_mileage(10% of the variable). 

insurance_df %>% mutate(missing_credit_score = is.na(credit_score)) %>%
  group_by(missing_credit_score) %>% 
  summarize(across(everything(), median, na.rm = T))

insurance_df %>% mutate(missing_annual_mileage = is.na(annual_mileage)) %>%
  group_by(missing_annual_mileage) %>% 
  summarize(across(everything(), median, na.rm = T))

#For missing values in both columns (credit_score and annual_mileage),
#the median gender is male, while for non-missing values, the median gender is female.

summary(insurance_df)

# The structure suggests that the cols with missing values 
#are normally distributed. Hence, imputing with the mean is acceptable.


clean_df <- insurance_df %>% group_by(gender) %>% 
  mutate(credit_score_filled = 
           ifelse(is.na(credit_score), mean(credit_score, na.rm = T), credit_score)) %>%
  mutate(annual_mileage_filled = ifelse(is.na(annual_mileage), mean(annual_mileage, na.rm = T), annual_mileage)) %>%
  ungroup() %>% select(-c(credit_score, annual_mileage))

##Creating a features df and logistic models

features_df <- data.frame(features = c(names(subset(clean_df, select = -c(outcome)))))

accuracies <- c()

for(col in features_df$features){
  formula_string <- glue('outcome ~ {col}')
  formula <- as.formula(formula_string)
  
  model <- glm(formula, data = clean_df, family = 'binomial')
  
  predictions <- round(fitted(model))
  
  accuracy <- length(which(predictions == clean_df$outcome)) / length(clean_df$outcome)
  
  features_df[which(features_df$features == col), "accuracy"] = accuracy
  
}

best_feature <- features_df$features[which.max(features_df$accuracy)]

best_accuracy <- max(features_df$accuracy)

#Best feature df
best_feature_df <- data.frame(best_feature, best_accuracy)

##The best feature is drivin experience, with an accuracy of 0.78









