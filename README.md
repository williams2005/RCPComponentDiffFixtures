# Step 1: Enter your data
glucose <- c(0, 20, 40, 60, 80, 100)          # X values: known glucose concentrations (µg/ml)
od      <- c(0.00, 0.304, 0.463, 0.488, 0.587, 0.596)  # Y values: corresponding Optical Density

unknown_od <- 0.473                           # Optical Density of the unknown sample
Step 2: Perform linear regression to get the best-fit line
model <- lm(od ~ glucose)                     # Fits O.D. = intercept + slope * concentration
# Print the equation and R-squared (shows how good the fit is)
summary(model)

# Step 3: Calculate the unknown glucose concentration using the line equation
slope     <- coef(model)["glucose"]           # Slope of the line
intercept <- coef(model)["(Intercept)"]       # Y-intercept
unknown_glucose <- (unknown_od - intercept) / slope

cat("Unknown glucose concentration =", round(unknown_glucose, 1), "µg/ml\n")

# Step 4: Plot the graph
# Create the base plot FIRST
plot(glucose, od,
     xlab = "Glucose Concentration (µg/ml)",
     ylab = "Optical Density (O.D.)",
     main = "A graph of Optical Density against Glucose Concentration (µg/ml)",
     pch  = 16,          # Solid circle points
     col  = "blue",      # Blue points for standards
     xlim = c(0, 120),   # Extend x-axis a bit for the unknown
     ylim = c(0, 0.7))   # Suitable y-axis range

# Add the regression line
abline(model, col = "red", lwd = 2)
# Add the unknown point
points(unknown_glucose, unknown_od, pch = 17, col = "green", cex = 1.5)
# Add dashed lines to trace the unknown
segments(0, unknown_od, unknown_glucose, unknown_od, col = "green", lty = 2)
segments(unknown_glucose, 0, unknown_glucose, unknown_od, col = "green", lty = 2)
# Add label
text(unknown_glucose + 8, unknown_od, 
     paste("Unknown ≈", round(unknown_glucose, 1), "µg/ml"), 
     col = "green")
