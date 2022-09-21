#Data is categorical, so fit binomial GLM
sq.run <- glm(Running ~ Age,
              data = data_sum3,
              family = binomial)

summary(sq.run)

#We need to back-transform (apply inverse logit):
library(modelbased)

estimate_means(sq.run) #transform logit to probability scale
estimate_contrasts(sq.run) #all classes vs. all classes. Sign. differences?

##Interpreting logistic regression output (effects pkg)
library(effects)
summary(allEffects(sq.run))

### Model chack: visualize model results----
library(easystats)
check_model(sq.run)

library(DHARMa) #problems are marked in red
simulateResiduals(sq.run, plot= TRUE) #Interpretation = Q-Q plot

## CALIBRATION PLOT (obs. vs. pred.)----
library(predtools)

#store predictions in new variable
data_sum3$run.pred <- predict(sq.run, type = "response")

#Compares predicted vs observed probabilities (grouped by quantiles)
calibration_plot(data = as.data.frame(data_sum3), obs = "Running", pred = "run.pred",
                 x_lim = c(0,1), y_lim = c(0,1))
