###--- Chapter 4 Code ---###

### Install and load the packages:

# eRm
citation("eRm")
install.packages("eRm")
library("eRm")

# TAM
citation("TAM")
install.packages("TAM")
library("TAM")

# WrightMap
citation("WrightMap")
install.packages("WrightMap")
library("WrightMap")

### Import the data:

#* Note: Update your working directory or include the complete file path to apply the read.csv() function]
science <- read.csv("liking_for_science.csv")

# Explore the data using descriptive statistics
summary(science)

### Run the Rating Scale Model

# Remove the Student ID variable:
science.responses <- subset(science_drop_i12, select = -student)

# Check descriptive statistics:
summary(science.responses)

# Run the RSM on the Liking for Science response data
RSM.science <- RSM(science.responses, se = TRUE)

# Request a summary of the model results
summary(RSM.science)

# Plot a Wright Map to visualize item and person locations:
plotPImap(RSM.science, main = "Liking for Science Rating Scale Model Wright Map")

## Examine item parameters

# Obtain the overall item location parameters
item.locations <- RSM.science$etapar
item.locations

# Calculate summary statistics
summary(item.locations)

# Calculate the location for item 1 by subtracting the sum of the item locations from zero
n.items <- ncol(science.responses)

# Find the location for item 1
i1 <- 0 - sum(item.locations[1:(n.items - 1)])

# Create a new object with all 24 item locations
item.locations.all <- c(i1, item.locations[c(1:(n.items - 1))])
item.locations.all

# Apply thresholds() function to the model object in order to obtain item locations (not centered at zero logits):
items.and.taus <- thresholds(RSM.science)
items.and.taus.table <- as.data.frame(items.and.taus$threshtable)
uncentered.item.locations <- items.and.taus.table$X1.Location

# set the mean of the item locations to zero logits:
centered.item.locations <- scale(uncentered.item.locations, scale = FALSE)
summary(centered.item.locations)

# Specify the number of thresholds as the maximum observed score in the response matrix (be sure the responses begin at category 0):
n.thresholds <- max(science.responses)

# Calculate adjacent-category threshold values:
tau.estimates <- NULL

tau <- 1
for(tau in 1:n.thresholds){
  tau.estimates[tau] <- (items.and.taus.table[, (1+tau)] - items.and.taus.table[,1])[1]
}

#SE for items + thresholds:
delta.tau.se <- items.and.taus$se.thresh
summary(delta.tau.se)

# SE for overall item:
delta.se <- RSM.science$se.eta
summary(delta.se)

### Plot item response functions (rating scale category probability plots)
plotICC(RSM.science, ask = FALSE)

# Calculate person parameters:
person.locations.estimate <- person.parameter(RSM.science)

# Store person parameters and their standard errors in a dataframe object:
person.locations <- cbind.data.frame(person.locations.estimate$thetapar,
                                     person.locations.estimate$se.theta)
names(person.locations) <- c("theta", "SE")

# View summary statistics for person parameters:
summary(person.locations)

### Examine item fit statistics
item.fit.results <- itemfit(person.locations.estimate)
item.fit <- cbind.data.frame(item.fit.results$i.infitMSQ,
                             item.fit.results$i.outfitMSQ,
                             item.fit.results$i.infitZ,
                             item.fit.results$i.outfitZ)
names(item.fit) <- c("infit_MSE", "outfit_MSE", "std_infit", "std_outfit")

# Request a summary of the numeric item fit statistics
summary(item.fit)


### Examine person fit statistics
person.fit.results <- personfit(person.locations.estimate)
person.fit <- cbind.data.frame(person.fit.results$p.infitMSQ,
                               person.fit.results$p.outfitMSQ,
                               person.fit.results$p.infitZ,
                               person.fit.results$p.outfitZ)
names(person.fit) <- c("infit_MSE", "outfit_MSE", "std_infit", "std_outfit")

# Request a summary of the numeric person fit statistics
summary(person.fit)

## Summarize the results in tables

### Model summary table:
RSM_summary.table.statistics <- c("Logit Scale Location Mean",
                                  "Logit Scale Location SD",
                                  "Standard Error Mean",
                                  "Standard Error SD",
                                  "Outfit MSE Mean",
                                  "Outfit MSE SD",
                                  "Infit MSE Mean",
                                  "Infit MSE SD",
                                  "Std. Outfit Mean",
                                  "Std. Outfit SD",
                                  "Std. Infit Mean",
                                  "Std. Infit SD",
                                  "Separation.reliability")

RSM_item.summary.results <- rbind(mean(centered.item.locations),
                                  sd(centered.item.locations),
                                  mean(delta.se),
                                  sd(delta.se),
                                  mean(item.fit.results$i.outfitMSQ),
                                  sd(item.fit.results$i.outfitMSQ),
                                  mean(item.fit.results$i.infitMSQ),
                                  sd(item.fit.results$i.infitMSQ),
                                  mean(item.fit.results$i.outfitZ),
                                  sd(item.fit.results$i.outfitZ),
                                  mean(item.fit.results$i.infitZ),
                                  sd(item.fit.results$i.infitZ),
                                  item.separation.reliability)


RSM_person.summary.results <- rbind(mean(person.locations$theta),
                                    sd(person.locations$theta),
                                    mean(person.locations$SE),
                                    sd(person.locations$SE),
                                    mean(person.fit$outfit_MSE),
                                    sd(person.fit$outfit_MSE),
                                    mean(person.fit$infit_MSE),
                                    sd(person.fit$infit_MSE),
                                    mean(person.fit$std_outfit),
                                    sd(person.fit$std_outfit),
                                    mean(person.fit$std_infit),
                                    sd(person.fit$std_infit),
                                    as.numeric(person.separation.reliability))

# Round the values for presentation in a table:
RSM_item.summary.results_rounded <- round(RSM_item.summary.results, digits = 2)

RSM_person.summary.results_rounded <- round(RSM_person.summary.results, digits = 2)

RSM_Table1 <- cbind.data.frame(RSM_summary.table.statistics,
                               RSM_item.summary.results_rounded,
                               RSM_person.summary.results_rounded)

# add descriptive column labels:
names(RSM_Table1) <- c("Statistic", "Items", "Persons")

### Item calibration table:
# Calculate the average rating for each item:
Avg_Rating <- apply(science.responses, 2, mean)

# Combine item calibration results in a table:

RSM_Table2 <- cbind.data.frame(c(1:ncol(science.responses)),
                               Avg_Rating,
                               centered.item.locations,
                               delta.se,
                               item.fit$outfit_MSE,
                               item.fit$std_outfit,
                               item.fit$infit_MSE,
                               item.fit$std_infit)
names(RSM_Table2) <- c("Task ID", "Average Rating", "Item Location","Item SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Sort Table 2 by Item difficulty:
RSM_Table2 <- RSM_Table2[order(-RSM_Table2$`Item Location`),]

# Round the numeric values (all columns except the first one) to 2 digits:
RSM_Table2[, -1] <- round(RSM_Table2[,-1], digits = 2)

### Person calibration table:
# Calculate average rating for persons who did not have extreme scores
Person_Avg_Rating <- apply(person.locations.estimate$X.ex,1, mean)

# Combine person calibration results in a table:
RSM_Table3 <- cbind.data.frame(rownames(person.locations),
                               Person_Avg_Rating,
                               person.locations$theta,
                               person.locations$SE,
                               person.fit$outfit_MSE,
                               person.fit$std_outfit,
                               person.fit$infit_MSE,
                               person.fit$std_infit)

names(RSM_Table3) <- c("Child ID", "Average Rating", "Person Location","Person SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Round the numeric values (all columns except the first one) to 2 digits:
RSM_Table3[, -1] <- round(RSM_Table3[,-1], digits = 2)

# RSM Application with Marginal Maximum Likelihood Estimation in TAM

## Prepare the data for analysis
#* Note: Update your working directory or include the complete file path to apply the read.csv() function]
science <- read.csv("liking_for_science.csv")

# Explore the data using descriptive statistics
summary(science)

# Remove the Student ID variable:
science.responses <- subset(science_drop_i12, select = -student)

# Check descriptive statistics:
summary(science.responses)

## Run the Rating Scale Model

# Generate a design matrix for the model
design.matrix <- designMatrices(resp=science.responses, modeltype="RSM", constraint = "items")$A

# Run the RSM with our design matrix
RSM.science_MMLE <- tam.mml(science.responses, irtmodel="RSM", A = design.matrix, constraint = "items")

# Request a summary of the model results
summary(RSM.science_MMLE)

# Extract the item location estimates from the model
items_MMLE <- RSM.science_MMLE$xsi[1:(ncol(science.responses)),]

# Center the item parameter estimates
uncentered.item.locations_MMLE <- items_MMLE$xsi
centered.item.locations_MMLE <- scale(uncentered.item.locations_MMLE, scale = FALSE)

# Request summary statistics for the estimates
summary(centered.item.locations_MMLE)

# Printing the overall item location parameter by calling xsi parameters
RSM.science_MMLE$xsi

# Specify the number of thresholds as the maximum observed score in the response matrix (be sure the responses begin at category 0):
n.thresholds <- max(science.responses)

## Calculate adjacent-category threshold values:
tau.estimates_MMLE <- NULL

# Find all but the final threshold estimate:
for(tau in 1: (n.thresholds - 1)){
  tau.estimates_MMLE[tau] <- RSM.science_MMLE$xsi[ncol(science.responses) + tau , 1]
}

# Calculate the final threshold estimate:
tau.estimates_MMLE[n.thresholds] <- -(sum(tau.estimates_MMLE))

# Print adjacent-categories threshold estimates to the console:
tau.estimates_MMLE

### Examine item response functions (rating scale category probability plots)
#graphics.off()
plot(RSM.science_MMLE, type="items")

## Evaluate item fit
# Examine numeric item fit indices
MMLE_fit <- tam.fit(RSM.science_MMLE)
item.fit_MMLE <- MMLE_fit$itemfit

# View summary statistics for the fit statistics
item.fit_MMLE <- MMLE_fit$itemfit
summary(item.fit_MMLE)

## Examine person parameters
# Use the tam.wle function to calculate person location parameters:
person.locations.estimate_MMLE <- tam.wle(RSM.science_MMLE)

# Store person parameters and their standard errors in a dataframe object:
person.locations_MMLE <- cbind.data.frame(person.locations.estimate_MMLE$theta,
                                          person.locations.estimate_MMLE$error)

names(person.locations_MMLE) <- c("theta", "SE")

# View summary statistics for person parameters:
summary(person.locations_MMLE)

# Subtract the original (uncentered) item mean location from the person locations:
person.locations_MMLE$theta_adjusted <- person.locations_MMLE$theta - mean(uncentered.item.locations_MMLE)

# Summary of person location estimates:
summary(person.locations_MMLE)

## Evaluate person fit
person.fit.results_MMLE <- tam.personfit(RSM.science_MMLE)
summary(person.fit.results_MMLE)

## Plot the Wright Map:

# Combine centered item estimates with thresholds:
n.items <- ncol(science.responses)

thresholds_MMLE <- matrix(data = NA, nrow = n.items, ncol = n.thresholds)

for(i in 1:n.thresholds){
  thresholds_MMLE[, i] <- centered.item.locations_MMLE + tau.estimates_MMLE[i]
}


#tau_labels <- paste("tau_", 1, sep = "")
thetas_MMLE <- person.locations_MMLE$theta_adjusted


# Plot the Variable Map
wrightMap(thetas = thetas_MMLE,
          thresholds = thresholds_MMLE,
          main.title = "Liking for Science Rating Scale Model Wright Map",
          show.thr.lab	= TRUE, dim.names = "",
          label.items.rows= 2)

# RSM Application with Joint Maximum Likelihood Estimation in TAM

## Run the Rating Scale Model
design.matrix <- designMatrices(resp=science.responses, modeltype="RSM", constraint = "items")$A

# Run the RSM with our design matrix
RSM.science_JMLE <- tam.jml(science.responses, A = design.matrix, constraint = "items",
                            control=list(maxiter=500), version=2 )

# Request a summary of the model results
summary(RSM.science_JMLE)

## Examine item parameters
# Extract the item location estimates from the model
items_JMLE <- RSM.science_JMLE$xsi[c(1:ncol(science.responses))]
items_JMLE

# Center the item parameter estimates
uncentered.item.locations_JMLE <- items_JMLE
centered.item.locations_JMLE <- scale(uncentered.item.locations_JMLE, scale = FALSE)

# Request summary statistics for the estimates
summary(centered.item.locations_JMLE)

# Printing the xsi parameters
RSM.science_JMLE$xsi[-c(1:ncol(science.responses))]

# Specify the number of thresholds as the maximum observed score in the response matrix (be sure the responses begin at category 0):
n.thresholds <- max(science.responses)

## Calculate adjacent-category threshold values:
tau.estimates_JMLE <- NULL

# Find all but the final threshold estimate:
for(tau in 1: (n.thresholds - 1)){
  tau.estimates_JMLE[tau] <- RSM.science_JMLE$xsi[ncol(science.responses) + tau ]
}

# Calculate the final threshold estimate:
tau.estimates_JMLE[n.thresholds] <- -(sum(tau.estimates_JMLE))

# Print adjacent-categories threshold estimates to the console:
tau.estimates_JMLE

## Examine item response functions (rating scale category probability plots)
#graphics.off()
plot(RSM.science_JMLE, type="items")

## Evaluate item fit
# Examine numeric item fit indices
JMLE_fit <- tam.fit(RSM.science_JMLE)
item.fit_JMLE <- JMLE_fit$fit.item

# View summary statistics
summary(item.fit_JMLE)

## Examine person parameters
# Store person parameters and their standard errors in a dataframe object:
person.locations_JMLE <- cbind.data.frame(RSM.science_JMLE$theta,
                                          RSM.science_JMLE$errorWLE)

names(person.locations_JMLE) <- c("theta", "SE")

# View summary statistics for person parameters:
summary(person.locations_JMLE)

# Subtract the original (uncentered) item mean location from the person locations:
person.locations_JMLE$theta_adjusted <- person.locations_JMLE$theta - mean(uncentered.item.locations_JMLE)

# Summary of person location estimates:
summary(person.locations_JMLE)

## Evaluate person fit
# Examine numeric person fit indices
JMLE_fit <- tam.fit(RSM.science_JMLE)

# View summary statistics for the fit statistics.
person.fit_JMLE <- JMLE_fit$fit.person
summary(person.fit_JMLE)

## Plot the Wright Map:'
# Combine centered item estimates with thresholds:

n.items <- ncol(science.responses)

thresholds_JMLE <- matrix(data = NA, nrow = n.items, ncol = n.thresholds)

for(i in 1:n.thresholds){
  thresholds_JMLE[, i] <- centered.item.locations_JMLE + tau.estimates_JMLE[i]

}

thetas_JMLE <- person.locations_JMLE$theta_adjusted


# Plot the Variable Map
wrightMap(thetas = thetas_JMLE,
          thresholds = thresholds_JMLE,
          main.title = "Liking for Science Rating Scale Model Wright Map: JMLE",
          show.thr.lab	= TRUE, dim.names = "",
          label.items.rows= 2)


# Example results section based on the eRm CMLE results:
# Print Table 1:
knitr::kable(
  RSM_Table1, booktabs = TRUE,
  caption = 'Model Summary Table'
)

# Print Table 2:
knitr::kable(
  RSM_Table2, booktabs = TRUE,
  caption = 'Item Calibration'
)

# Print Table 3:
knitr::kable(
  head(RSM_Table3,10), booktabs = TRUE,
  caption = 'Person Calibration'
  )

# Plot the variable-Map
graphics.off()
plotPImap(RSM.science, main = "Liking for Science Rating Scale Model Wright Map", sorted = TRUE, irug = FALSE)



