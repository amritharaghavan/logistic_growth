

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
Exponential_vs_logistic_plot
install.packages("ragg")

library(ragg)

#Saving the object as a high resolution .png file
agg_png("Exponential_vs_logistic_plot.png",  width = 4000, height = 3000, units = "px", res = 600)+
  print(Exponential_vs_logistic_plot)
dev.off()
