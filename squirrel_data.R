library(here) 
library(readr)
library(dplyr)

data <- read_csv("2018_Squirrel_Data.csv")

glimpse(data)

#fix variables
library(tidyverse)
library(tidylog) 

library(summarytools) # RESUMEN DE LAS VARIABLES (niveles, freq, graph, missing data points)

#create a subset with (possibly) interesting variables
data_sum <- data %>% select(X, 
                            Y,
                            `Unique_Squirrel_ID`,
                            Shift,
                            Date,
                            Age, 
                            Location, 
                            16,17,18,19,20,27,28,29)

dfSummary(data_sum)

#talnn
#kkkk

#create var "activity" - what was the sq doing at moment of obs.?

#var for +1 activity
data_sum2 <- data_sum %>% 
  mutate(activity = case_when (Running == 'TRUE' ~ 'running',
                               Chasing == "TRUE" ~ "chasing",
                               Climbing == "TRUE" ~ "climbing",
                               Eating == "TRUE" ~ "eating",
                               Foraging == "TRUE" ~ "foraging")) 
  
data_sum2$activity<- as.factor(data_sum2$activity)


data_sum3<-na.omit(data_sum)
data_sum3<-data_sum3[!(data_sum3$Age=="?"),]

data_sum=NULL
data_sum2=NULL

plot(factor(Running) ~ factor(Age), data = data_sum3)


#¿qué necesitan las ardillas para comer?
#varios modelos ---------
sq <- glm(Eating ~ Shift+Location,
          data = data_sum3,
          family = binomial)
summary(sq)
library(report)
report(sq)

sq4 <- glm(Eating ~ Shift,
          data = data_sum3,
          family = binomial)
summary(sq4)

library(report)
report(sq4)

sq1 <- glm(Eating ~ Location,
          data = data_sum3,
          family = binomial)
summary(sq1)

sq2 <- glm(Eating ~ Location*Age,
           data = data_sum3,
           family = binomial)
summary(sq2)


sq3 <- glm(Eating ~ Location*Shift,
          data = data_sum3,
          family = binomial)
summary(sq3)

AIC(sq, sq1, sq2, sq3, sq4)


library("effects")
allEffects(sq)
plot(allEffects(sq))

library(visreg)
visreg(sq, scale = "response", rug = FALSE)

#Es decir, ya sea tarde o temprano, las ardillas suelen estar
#comiendo en el suelo

#Model checking ----------

library("DHARMa")
simulateResiduals(sq, plot = TRUE)
library(performance)
check_predictions(sq)
