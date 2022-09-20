library(here) 
library(readr)

data <- read_csv("2018_Squirrel_Data.csv")

glimpse(data)

#fix variables
library(tidyverse)
library(tidylog) 

library(summarytools) # RESUMEN DE LAS VARIABLES (niveles, freq, graph, missing data points)

#create a subset with (possibly) interesting variables
data_sum <- data %>% select(X, 
                            Y,
                            Shift,
                            Date,
                            Age, 
                            Location, 
                            16,17,18,19,20,27,28,29)

dfSummary(data_sum)

#create var "activity" - what was the sq doing at moment of obs.?
data_sum2 <- data_sum %>% 
  mutate(activity = case_when (Running == 'TRUE' ~ 'running',
                               Chasing == "TRUE" ~ "chasing",
                               Climbing == "TRUE" ~ "climbing",
                               Eating == "TRUE" ~ "eating",
                               Foraging == "TRUE" ~ "foraging"))

glimpse(data_sum2)
data_sum2$activity<- as.factor(data_sum2$activity)

