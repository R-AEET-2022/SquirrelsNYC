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
                            `Unique Squirrel ID`,
                            Shift,
                            Date,
                            Age, 
                            Location, 
                            16,17,18,19,20,27,28,29)

dfSummary(data_sum)

#create var "activity" - what was the sq doing at moment of obs.?

#var for +1 activity
data_sum2 <- data_sum %>% 
  mutate(activity = case_when (Running == 'TRUE' ~ 'running',
                               Chasing == "TRUE" ~ "chasing",
                               Climbing == "TRUE" ~ "climbing",
                               Eating == "TRUE" ~ "eating",
                               Foraging == "TRUE" ~ "foraging")) 
  
data_sum2$activity<- as.factor(data_sum2$activity)

data_sum=NULL

data_sum2 <- data_sum2 %>% rename(data_sum2,
  id = "Unique Squirrel ID",
  runs = "Runs from"
)

variable.names(data_sum2)

