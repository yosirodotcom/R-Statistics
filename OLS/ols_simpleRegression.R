model <- lm(dist ~ speed, data = cars)
summary(model)

# equation dist = -17.5791 + 3.9324(speed)
# r-squared = 65.11% variance of Y can be explained by variance of X
# p-value = 1.49e-12, three star: X caused Y siginficantly at less than 0.1%