
library(here) #permite hacer referencia siempre al directorio donde se encuentra el proyecto

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
