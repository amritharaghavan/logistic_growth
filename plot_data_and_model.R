#Script to plot data and model

growth_data <- read.csv("experiment1.csv")

logistic_fun <- function(t) {
  
  N <- (N0*K*exp(r*t))/(K-N0+N0*exp(r*t))
  
  return(N)
  
}

N0 <- exp(6.883) #Estimates taken from the linear models
#Explained further in linear model code
  
r <- 1.004e-02 #
  
K <- 6.000e+10 #

ggplot(aes(t,N), data = growth_data) +
  
  geom_function(fun=logistic_fun, colour="red") +
  
  geom_point()

  scale_y_continuous(trans='log10')

#Can see logistic growth line through code, based on linear model

  #saving packages used:
  sink(file = "package-versions.txt")
  sessionInfo()
  sink()

  #Update email to make commits in my repository
  #git config --global user.email "<YOUR_EMAIL>"