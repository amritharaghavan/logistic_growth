# logistic_growth
R scripts for a reproducible analysis of logistic growth

1) The analysis allowed  us to plot and model the logistic growth pattern of bacteria in an abundance of growth media. All we were provided with initially was the volume of bacteria, so, in order to be able to estimate the population size, both initially and as the population grew, we needed to model the growth pattern. The model led us to an estimate of the initial population of the bacteria suspended in the media. It also allowed us to plot the relationship between time and the change in abundance of bacteria, and from this we were able to estimate the growth rate in different phases of the logistic growth curve. Below, I have inserted the code, as well as annotations of the code, which show how this was modelled, both when t was small and when t was large:

I used the data for experiment 1.
```
#Script to estimate the model parameters using a linear approximation
install.packages("dplyr") 
library(dplyr) #the dplyr package is required to create this model

growth_data <- read.csv("experiment1.csv") #reading in the data that I used

#Case 1. K > N0, and t is small- so growth is exponential
#This is a model of exponential growth: so N = N0.exp(rt)- but it has been log-transformed, for linearity
#The model created is a linear model: log(N)= log(N0)+ rt
#So N0= e^(intercept), and r= slope

data_subset1 <- growth_data %>% filter(t<1000) %>% mutate(N_log = log(N)) #This filters the data to give us only the points from where t is small, and log transforms the data
model1 <- lm(N_log ~ t, data_subset1) #This creates the linear model based on the log transformations
summary(model1) #This gives the results table of this model: the results are below, under the second model

#Case 2. N(t) = K- so growth has plateaued at the carrying capacity
#This model is N (t) = K + 0 · t- N is the same, no matter what thhe time is: as the population has reached its capacity
#So K= intercept

data_subset2 <- growth_data %>% filter(t>3000) #Filtering the data to only give us when t is large: in this case, when t>3000, growth plateaued
model2 <- lm(N ~ 1, data_subset2) #Produces a linear model
summary(model2) #This gives the results table of this model: the results are below

```

   Results:
   
  Estimates of the initial population, the carrying capacity, and the growth rate:

  Results table from running model 1: 
  
  Coefficients:
  ```
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 6.883e+00  1.548e-02   444.8   <2e-16 
t           1.004e-02  2.749e-05   365.1   <2e-16
```
---

Residual standard error: 0.03332 on 15 degrees of freedom

Multiple R-squared:  0.9999,	Adjusted R-squared:  0.9999 

F-statistic: 1.333e+05 on 1 and 15 DF,  p-value: < 2.2e-16


Results table from running model 2:

Coefficients:
```
             Estimate Std. Error t value Pr(>|t|)
(Intercept) 6.000e+10  6.446e+03 9307484   <2e-16
```
---


Residual standard error: 37030 on 32 degrees of freedom

Both of these have very low p values, so we can take the results as significant- they are likely to be accurate estimates
  
Estimates for experiment 1:

  N0= exp(6.883)

    = 975.54 (2d.p)
   
  r= 1.004e-02
  
  K= 6.000e+10

Using these, models, we can plot the logistic growth curve: below is the code to do this, and the graph produced from this:

```
install.packages("ggplot2")
library(ggplot2) #Installing and loading packages
growth_data <- read.csv("experiment1.csv") #Reading in data

logistic_fun <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)
  
} #Integrating the estimates we found from the two models into a function for logistic growth

N0 <- exp(6.883) #Estimates taken from the linear models
  
r <- 1.004e-02 
  
K <- 6.000e+10 

ggplot(aes(t,N), data = growth_data) +
  
  geom_function(fun=logistic_fun, colour="red") +
  geom_point() #Produces the S curve from the estimates

#scale_y_continuous(trans='log10') would linearise this graph

```
The graph this produces is:
![image](https://github.com/oxstudent1/logistic_growth/assets/150163772/260e2536-f587-47b7-8a6c-2521ec8c5eaf)


2)For exponential growth:

N (t) = N0.exp(0.01004.4980)
      =exp(6.883).exp(0.01004.4980)
      =5.053887e+24

For logistic growth:

N(t)= K+0t
    =6e+10+ (0.4980)
    =6e+10, or the carrying capacity

In logistic growth, the population size would have reached the carrying capacity by t=4980, so N=6e+10: this is much smaller than the population size in an exponential model, as the population is able to keep growing with no cap in this model. The exponential growth result is much bigger than the logistic result.


3) R script: this is also included in the repository, as well as being copied below:

```
# Load necessary libraries
library(ggplot2)

# Function for exponential growth
exponential_growth <- function(t, N0, r) {
  N0 * exp(r * t)
}

# Function for logistic growth
logistic_growth <- function(t, N0, K, r) {
  N <- (N0 * K * exp(r * t)) / (K - N0 + N0 * exp(r * t))
  return(N)
}


# Set parameters
N0 <- 975.54  # Initial population
r <- 1.004e-02  # Growth rate
K <- 6.000e+10  # Carrying capacity (for logistic growth)

# Generate time values
time <- seq(0, 5000, by = 1)

# Calculate population values for both growth models
population_logistic <- logistic_growth(time, N0, K, r)
population_exponential <- exponential_growth(time, N0, r)

# Create a data frame for plotting
growth_data <- data.frame(t = rep(time, 2), 
                          population = c(population_logistic, population_exponential), 
                          type = rep(c("Logistic", "Exponential"), each = length(time)))

# Plot the results using ggplot2
Exponential_vs_logistic_plot<- ggplot(growth_data, aes(t, population, color = type, linetype = type)) +
  geom_line(lwd=0.75) +
  labs(x = "Time", y = "Population", title = "Logistic and Exponential Growth", legend="Model") +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal()

install.packages("ragg")

library(ragg)

#Saving the object as a high resolution .png file
agg_png("Exponential_vs_logistic_plot.png",  width = 4000, height = 3000, units = "px", res = 600)+
  print(Exponential_vs_logistic_plot)
dev.off()
```
The graph this produced:

![image](https://github.com/oxstudent1/logistic_growth/assets/150163772/460f7ed7-0222-4bec-8552-eb711bdc6df4)

This is also  saved as a .png file in the repository.
