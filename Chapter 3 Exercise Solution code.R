###--- Chapter 3 Exercise Solution Code ---###

### Install and load the packages:

# TAM:
citation("TAM")
install.packages("TAM")
library("TAM")

# WrightMap:
citation("WrightMap")
install.packages("WrightMap")
library("WrightMap")

# eRm
citation("eRm")
install.packages("eRm")
library("eRm")

# psych
citation("psych")
install.packages("psych")
library("psych")

### Import the data:

#* Note: Update your working directory or include the complete file path to apply the read.csv() function
math_data <- read.csv("exercise_3.csv")

### Run the Dichotomous Rasch Model

# Remove the ID variable:
math_data.responses <- subset(math_data, select = -c(ids))

# Check descriptive statistics:
summary(math_data.responses)

# Run the model
dichot.math_data <- RM(math_data.responses)

# Request a summary of the model results
summary(dichot.math_data)

### Evaluate Unidimensionality

# Calculate person parameters
student.locations <- person.parameter(dichot.math_data)

# Calculate the model-predicted probabilities
model.prob <- pmat(student.locations)

# Calculate residuals using a modified response matrix that does not include the students with extreme scores
responses.without.extremes <- student.locations$X.ex

# Calculate residuals as the difference between the observed responses and model-predictions
resids <- responses.without.extremes - model.prob

### calculate the proportion of variance in the responses
## Variance of the observations: VO
observations.vector <- as.vector(responses.without.extremes)
VO <- var(observations.vector)

## Variance of the residuals: VR
residuals.vector <- as.vector(resids)
VR <- var(residuals.vector)

## Raw variance explained by Rasch measures: (VO - VR)/VO
(VO - VR)/VO

# Express the result as a percent:
((VO - VR)/VO) * 100

### Principal components analysis of standardized residual correlations

## Conduct a PCA of Standardized Residual Correlations
# Obtain a matrix with the standardized residuals from the model
item.fit <- itemfit(student.locations)
# Extract the standardized residuals
std.resids <- item.fit$st.res

# Conduct the PCA on the standardized residuals object
pca <- pca(std.resids, nfactors = ncol(math_data.responses), rotate = "none")

# Save the first five contrasts in a vector
contrasts <- c(pca$values[1], pca$values[2], pca$values[3], pca$values[4], pca$values[5])

# Plot the values using a simple graphical display
plot(contrasts, ylab = "Eigenvalues for Contrasts", xlab = "Contrast Number", main = "Contrasts from PCA of Standardized Residual Correlations")

### Summaries of Residuals: Infit & Outfit
# Examine the item.fit
item.fit

# Calculate numeric person fit statistics
person.fit <- personfit(student.locations)

# Explore the infit and outfit statistics
summary(person.fit$p.infitMSQ)
summary(person.fit$p.outfitMSQ)

### Graphical Displays for Evaluating Model-Data Fit
# Before constructing the plots, find the maximum and minimum values of the standardized residuals to set limits for the axes:
max.resid <- ceiling(max(std.resids))
min.resid <- ceiling(min(std.resids))

# The code below will produce standardized residual plots for each of the items:
for(item.number in 1:ncol(std.resids)){

  plot(std.resids[, item.number], ylim = c(min.resid, max.resid),
       main = paste("Standardized Residuals for Item ", item.number, sep = ""),
       ylab = "Standardized Residual", xlab = "Person Index")
  abline(h = 0, col = "blue")
  abline(h=2, lty = 2, col = "red")
  abline(h=-2, lty = 2, col = "red")

  legend("topright", c("Std. Residual", "Observed = Expected", "+/- 2 SD"), pch = c(1, NA, NA),
         lty = c(NA, 1, 2),
         col = c("black", "blue", "red"), cex = .8)

}

## Empirical item response functions
# Create plots for all of the items in our data
for(item.number in 1:ncol(std.resids)){
  plotICC(dichot.math_data, item.subset = item.number, empICC = list("raw"), empCI = list())
}

### Reliability Indices in Rasch Measurement
# Calculate the person separation reliability statistic
summary(SepRel(student.locations))

### Conduct Model-Data Fit Analyses using the TAM Package with Joint Maximum Likelihood Estimation

# Run the Rasch model
TAM_jmle_dichot.math_data <- tam.jml(math_data.responses)

## Evaluate unidimensionality
## Isolate the response matrix used in estimation:
resp <- TAM_jmle_dichot.math_data$resp

## Find the expected response probabilities based on the model:
resids <- IRT.residuals(TAM_jmle_dichot.math_data)

exp <- resids$X_exp

## Calculate raw (unstandardized) residuals:
resids.raw <- as.matrix(resp - exp)

## Calculate the variance in observations due to Rasch-model-estimated locations:

# Variance of the observations: VO
observations.vector <- as.vector(as.matrix(resp))
VO <- var(observations.vector)

# Variance of the residuals: VR
residuals.vector <- as.vector(resids.raw)
VR <- var(residuals.vector)

# Raw variance explained by Rasch measures: (VO - VR)/VO
(VO - VR)/VO

# Express the result as a percent:
((VO - VR)/VO) * 100

# Principal components analysis of standardized residual correlations
# Conduct the PCA on the standardized residuals object
pca <- pca(resids$stand_residuals, nfactors = ncol(math_data.responses), rotate = "none")

# Save the first five contrasts in a vector
contrasts <- c(pca$values[1], pca$values[2], pca$values[3], pca$values[4], pca$values[5])

# Plot the values using a simple graphical display
plot(contrasts, ylab = "Eigenvalues for Contrasts", xlab = "Contrast Number", main = "Contrasts from PCA of Standardized Residual Correlations")

# Numeric fit statistics
# Calculate numeric item and person fit statistics:
fit.results <- tam.fit(TAM_jmle_dichot.math_data)

item.fit <- fit.results$fit.item
person.fit <- fit.results$fit.person

# Generate plots of standardized residuals for individual items
std.resids <- resids$stand_residuals

# Find the maximum and minimum values of the standardized residuals to set limits for the axes:
max.resid <- ceiling(max(std.resids))
min.resid <- ceiling(min(std.resids))

# Produce standardized residual plots for each of the items
for(item.number in 1:ncol(std.resids)){

  plot(std.resids[, item.number], ylim = c(min.resid, max.resid),
       main = paste("Standardized Residuals for Item ", item.number, sep = ""),
       ylab = "Standardized Residual", xlab = "Person Index")
  abline(h = 0, col = "blue")
  abline(h=2, lty = 2, col = "red")
  abline(h=-2, lty = 2, col = "red")

  legend("topright", c("Std. Residual", "Observed = Expected", "+/- 2 SD"), pch = c(1, NA, NA),
         lty = c(NA, 1, 2),
         col = c("black", "blue", "red"), cex = .8)

}

# Reliability of separation statistics
## Person separation reliability:
TAM_jmle_dichot.math_data$WLEreliability

## Item separation reliability:
# Get Item scores
ItemScores <- colSums(math_data.responses)

# Get Item SD
ItemSD <- apply(math_data.responses,2,sd)

# Calculate the se of the Item
ItemSE <- ItemSD/sqrt(length(ItemSD))

# compute the Observed Variance (also known as Total Person Variability or Squared Standard Deviation)
SSD.ItemScores <- var(ItemScores)

# compute the Mean Square Measurement error (also known as Model Error variance)
Item.MSE <- sum((ItemSE)^2) / length(ItemSE)

# compute the Item Separation Reliability
item.separation.reliability <- (SSD.ItemScores-Item.MSE) / SSD.ItemScores
item.separation.reliability
