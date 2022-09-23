#Library ----
library(here) 
library(readr)
library(dplyr)
library(tidyverse)
library(tidylog) 
library(summarytools) # RESUMEN DE LAS VARIABLES (niveles, freq, graph, missing data points)

#Data----
data <- read_csv("2018_Squirrel_Data.csv")
glimpse(data)

#create a subset with (possibly) interesting variables
data_sum <- data %>% select(X, 
                            Y,
                            `Unique_Squirrel_ID`,
                            Shift,
                            Date,
                            Age, 
                            Location, 
                            16,17,18,19,20,27,28,29)

#create var "activity" - what was the sq doing at moment of obs.?
#var for +1 activity
data_sum2 <- data_sum %>% 
  mutate(activity = case_when (Running == 'TRUE' ~ 'running',
                               Chasing == "TRUE" ~ "chasing",
                               Climbing == "TRUE" ~ "climbing",
                               Eating == "TRUE" ~ "eating",
                               Foraging == "TRUE" ~ "foraging")) 
  
data_sum2$activity<- as.factor(data_sum2$activity)

#limpiar datos de NA y observaciones en duda
data_sum3<-na.omit(data_sum2)
data_sum3<-data_sum3[!(data_sum3$Age=="?"),]

plot(factor(Eating) ~ factor(Age), data = data_sum3)


#¿qué necesitan las ardillas para comer?
#varios modelos ---------
sq <- glm(Eating ~ Shift+Location,
          data = data_sum3,
          family = binomial)
summary(sq)
library(report)
report(sq)

residuals(sq)
plot(sq)

report(sq)

sq4 <- glm(Eating ~ Shift,
          data = data_sum3,
          family = binomial)
summary(sq4)

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

AIC(sq, sq1, sq2, sq3)


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

