#Loading libraries
library(pacman)
p_load(tidyverse, ggfortify, glue, yardstick)

#loading and inspecting the df
insurance_df <- read_csv("car_insurance.csv")
glimpse(insurance_df)

insurance_df %>% select_if(~any(is.na(.))) %>% summarize_all(~sum(is.na(.)))

##credit score and annual mileage have null values. 

#removing customer id column
insurance_df <- insurance_df %>% select(-id)
