#Script to estimate the model parameters using a linear approximation
install.packages("dplyr")
library(dplyr)

growth_data <- read.csv("experiment1.csv")

#Case 1. K >> N0, t is small

data_subset1 <- growth_data %>% filter(t<1000) %>% mutate(N_log = log(N))
#When t is small- in the lag phase, so when t<1000
model1 <- lm(N_log ~ t, data_subset1)
summary(model1)
#This model is ln(N) = ln(N0) + rt
#So N0= e^(intercept), and r= slope

#Case 2. N(t) = K

data_subset2 <- growth_data %>% filter(t>3000)
#What t is when N reaches carrying capacity
model2 <- lm(N ~ 1, data_subset2)
summary(model2)
#This model is N (t) = K + 0 · t
#So K= intercept


#For experiment 1:
#N0= expt(6.883)
#r= 1.004e-02
#K= 6.000e+10