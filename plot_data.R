#Script to plot the logistic growth data

growth_data <- read.csv("experiment1.csv")

install.packages("ggplot2")
library(ggplot2)

ggplot(aes(t,N), data = growth_data) +
  
  geom_point() +
  
  xlab("t") +
  
  ylab("N") +
  
  theme_bw()

#Draws an S curve showing abundance of bacteria against time in minutes

ggplot(aes(t,N), data = growth_data) +
  
  geom_point() +
  
  xlab("t") +
  
  ylab("log10(N)") +
  
  scale_y_continuous(trans='log10')

#Same graph but transformed to a log scale- makes the relationship look more linear
