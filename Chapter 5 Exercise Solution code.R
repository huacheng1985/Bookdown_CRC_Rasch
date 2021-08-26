###--- Chapter 5 Exercise Solution Code ---###

### Install and load the packages:
# install.packages("eRm")
library("eRm")

### Import the data:

#* Note: Update your working directory or include the complete file path to apply the read.csv() function]
survey <- read.csv("survey_ratings.csv")

# explore the data using descriptive statistics
summary(survey)

### ***Run the Partial Credit Model***

# Remove the Person ID variable:
survey.responses <- subset(survey, select = -person)

# Check descriptive statistics:
summary(survey.responses)

# Run the PCM on the survey response data
PCM.survey <- PCM(survey.responses, se = TRUE)

# request a summary of the model results
summary(PCM.survey)

# Plot Wright Map
plotPImap(PCM.survey, main = "Partial Credit Model Wright Map")

## Examine item parameters

# Item Parameters
item.locations <- PCM.survey$etapar
item.locations

# Check the summary of Item Parameters
summary(item.locations)

# Find the location for item 1
i1 <- 0 - sum(item.locations[(1:length(item.locations)) - 1])

# create a new object with all of the item + threshold locations
item.locations.all <- c(i1, item.locations[(1:length(item.locations)) - 1])
item.locations.all[1]

# Apply thresholds() function to the model object in order to obtain item locations (not centered at zero logits):
items.and.taus <- thresholds(PCM.survey)
items.and.taus.table <- as.data.frame(items.and.taus$threshtable)
uncentered.item.locations <- items.and.taus.table$X1.Location

# Set the mean of the item locations to zero logits:
centered.item.locations <- scale(uncentered.item.locations, scale = FALSE)

# Summarize the results:
summary(centered.item.locations)

# Specify the number of items that were included in the analysis:
n.items <- ncol(survey.responses)

# Specify the number of thresholds as the maximum observed score in the response matrix (be sure the responses begin at category 0):
n.thresholds <- max(survey.responses)

# Create a matrix in which to store the adjacent-category threshold values for each item:
tau.matrix <- matrix(data = NA, ncol = n.thresholds, nrow = n.items)

# Calculate adjacent-category threshold values:
for(item.number in 1:n.items){
  for(tau in 1:n.thresholds){
    tau.matrix[item.number, tau] <- (items.and.taus.table[item.number, (1+tau)] -
                                       items.and.taus.table[item.number,1])[1]
  }
}

# compares the first threshold estimate to the second threshold estimate
tau.matrix[,1] <= tau.matrix[,2]

# calculate standard errors for each item + threshold location and store the
delta.tau.se <- items.and.taus$se.thresh
summary(delta.tau.se)

### Item Response Functions

# Plot ICC
plotICC(PCM.survey, ask = FALSE)

### Person Parameters

# Calculate person parameters:
person.locations.estimate <- person.parameter(PCM.survey)

# Store person parameters and their standard errors in a dataframe object:
person.locations <- cbind.data.frame(person.locations.estimate$thetapar,
                                     person.locations.estimate$se.theta)
names(person.locations) <- c("theta", "SE")

# View summary statistics for person parameters:
summary(person.locations)

### Item fit

# calculate numeric item fit statistics
item.fit.results <- itemfit(person.locations.estimate)

# format the item fit statistics as a data.frame for easy manipulation and exporting
item.fit <- cbind.data.frame(item.fit.results$i.infitMSQ,
                             item.fit.results$i.outfitMSQ,
                             item.fit.results$i.infitZ,
                             item.fit.results$i.outfitZ)

# name the data frame
names(item.fit) <- c("infit_MSE", "outfit_MSE", "std_infit", "std_outfit")

# request a summary of the numeric item fit statistics
summary(item.fit)

### Person fit

# calculate numeric person fit statistics
person.fit.results <- personfit(person.locations.estimate)

# format the person fit statistics as a data.frame for easy manipulation and exporting
person.fit <- cbind.data.frame(person.fit.results$p.infitMSQ,
                               person.fit.results$p.outfitMSQ,
                               person.fit.results$p.infitZ,
                               person.fit.results$p.outfitZ)

# name the data frame
names(person.fit) <- c("infit_MSE", "outfit_MSE", "std_infit", "std_outfit")

# request a summary of the numeric person fit statistics
summary(person.fit)

# Calculate the person separation reliability
person.separation.reliability <- SepRel(person.locations.estimate)
person.separation.reliability

## Item separation reliability:

# Get Item scores
ItemScores <- colSums(survey.responses)

# Get Item SD
ItemSD <- apply(survey.responses,2,sd)

# Calculate the SE of the Item
ItemSE <- ItemSD/sqrt(length(ItemSD))

# Compute the Observed Variance (also known as Total Person Variability or Squared Standard Deviation)
SSD.ItemScores <- var(ItemScores)

# Compute the Mean Square Measurement error (also known as Model Error variance)
Item.MSE <- sum((ItemSE)^2) / length(ItemSE)

# Compute the Item Separation Reliability
item.separation.reliability <- (SSD.ItemScores-Item.MSE) / SSD.ItemScores
item.separation.reliability

## Summarize the Results in Tables

# create tables that summarize the calibrations of the persons, items, and rating scale category thresholds
PCM_summary.table.statistics <- c("Logit Scale Location Mean",
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

PCM_item.summary.results <- rbind(mean(centered.item.locations),
                                  sd(centered.item.locations),
                                  mean(delta.tau.se),
                                  sd(delta.tau.se),
                                  mean(item.fit.results$i.outfitMSQ),
                                  sd(item.fit.results$i.outfitMSQ),
                                  mean(item.fit.results$i.infitMSQ),
                                  sd(item.fit.results$i.infitMSQ),
                                  mean(item.fit.results$i.outfitZ),
                                  sd(item.fit.results$i.outfitZ),
                                  mean(item.fit.results$i.infitZ),
                                  sd(item.fit.results$i.infitZ),
                                  item.separation.reliability)

PCM_person.summary.results <- rbind(mean(person.locations$theta),
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
PCM_item.summary.results_rounded <- round(PCM_item.summary.results, digits = 2)

PCM_person.summary.results_rounded <- round(PCM_person.summary.results, digits = 2)

PCM_Table1 <- cbind.data.frame(PCM_summary.table.statistics,
                               PCM_item.summary.results_rounded,
                               PCM_person.summary.results_rounded)

# Add descriptive column labels:
names(PCM_Table1) <- c("Statistic", "Items", "Persons")

# Calculate the average rating for each item:
Avg_Rating <- apply(survey.responses, 2, mean)

# Combine item calibration results in a table:

PCM_Table2 <- cbind.data.frame(c(1:ncol(survey.responses)),
                               Avg_Rating,
                               centered.item.locations,
                               tau.matrix,
                               item.fit$outfit_MSE,
                               item.fit$std_outfit,
                               item.fit$infit_MSE,
                               item.fit$std_infit)

# Add descriptive column labels:
names(PCM_Table2) <- c("Task ID", "Average Rating", "Item Location","Threshold 1", "Threshold 2", "Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Sort Table 2 by Item difficulty:
PCM_Table2 <- PCM_Table2[order(-PCM_Table2$`Item Location`),]

# Round the numeric values (all columns except the first one) to 2 digits:
PCM_Table2[, -1] <- round(PCM_Table2[,-1], digits = 2)

# Calculate average rating for persons who did not have extreme scores
Person_Avg_Rating <- apply(person.locations.estimate$X.ex,1, mean)

# Combine person calibration results in a table:
PCM_Table3 <- cbind.data.frame(rownames(person.locations),
                               Person_Avg_Rating,
                               person.locations$theta,
                               person.locations$SE,
                               person.fit$outfit_MSE,
                               person.fit$std_outfit,
                               person.fit$infit_MSE,
                               person.fit$std_infit)

# Add descriptive column labels:
names(PCM_Table3) <- c("Child ID", "Average Rating", "Person Location","Person SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Round the numeric values (all columns except the first one) to 2 digits:
PCM_Table3[, -1] <- round(PCM_Table3[,-1], digits = 2)

## PCM Application with MMLE in *TAM*

# Load the package
#install.packages("TAM")
library("TAM")

# install.packages("WrightMap")
library("WrightMap")

# generate a design matrix for the model that includes specifications for those parameters
design.matrix <- designMatrices(resp=survey.responses, modeltype="PCM", constraint = "items")$A

# Run the Partial Credit Model
PCM.survey_MMLE <- tam.mml(survey.responses, irtmodel="PCM", A = design.matrix, constraint = "items", verbose = FALSE)

# Request the summary
summary(PCM.survey_MMLE)

## Item Parameters

# extract the item location estimates from the model object
items_MMLE <- PCM.survey_MMLE$item_irt$beta

# center the item parameter estimates
uncentered.item.locations_MMLE <- items_MMLE
centered.item.locations_MMLE <- scale(uncentered.item.locations_MMLE, scale = FALSE)

# request summary statistics
summary(centered.item.locations_MMLE)

# View the item parameter table (note that the overall item location shown here is not centered):
PCM.survey_MMLE$item_irt

# Save only the threshold estimates, which begin in column 4:
tau.estimates_MMLE <- PCM.survey_MMLE$item_irt[, c(4 : (3+n.thresholds))]

# View the threshold estimates:
tau.estimates_MMLE

### Item Response Functions

# examine rating scale category probability plots
graphics.off()
plot(PCM.survey_MMLE, type="items")

### Item Fit

# examine numeric and graphical item fit indices
MMLE_fit <- msq.itemfit(PCM.survey_MMLE)
item.fit_MMLE <- MMLE_fit$itemfit

# view summary statistics
summary(item.fit_MMLE)

### Person Parameters
# Use the tam.wle function to calculate person location parameters:
person.locations.estimate_MMLE <- tam.wle(PCM.survey_MMLE)

# Store person parameters and their standard errors in a dataframe object:
person.locations_MMLE <- cbind.data.frame(person.locations.estimate_MMLE$theta,
                                          person.locations.estimate_MMLE$error)
# Add descriptive column labels:
names(person.locations_MMLE) <- c("theta", "SE")

# View summary statistics for person parameters:
summary(person.locations_MMLE)

# Subtract the original (uncentered) item mean location from the person locations:
person.locations_MMLE$theta_adjusted <- person.locations_MMLE$theta - mean(uncentered.item.locations_MMLE)

# Summary of person location estimates:
summary(person.locations_MMLE)

### Person fit

# evaluate person fit
person.fit.results_MMLE <- tam.personfit(PCM.survey_MMLE)
summary(person.fit.results_MMLE)

# plot Wright Map
# Combine centered item estimates with thresholds:
n.items <- ncol(survey.responses)

thresholds_MMLE <- matrix(data = NA, nrow = n.items, ncol = n.thresholds)

for(item.number in 1:n.items){
  for(tau in 1:n.thresholds){
    thresholds_MMLE[item.number, tau] <- centered.item.locations_MMLE[item.number] +
      tau.estimates_MMLE[item.number, tau]
  }
}

thetas_MMLE <- person.locations_MMLE$theta_adjusted

# Plot the Wright Map
wrightMap(thetas = thetas_MMLE,
          thresholds = thresholds_MMLE,
          main.title = "Partial Credit Model Wright Map (MMLE)",
          show.thr.lab	= TRUE, dim.names = "",
          label.items.rows= 2)


### PCM Application with JMLE in *TAM*

## Run the Partial Credit Model

# generate a design matrix for the model that includes specifications for those parameters
design.matrix <- designMatrices(resp=survey.responses, modeltype="PCM", constraint = "items")$A

# run the PCM with our design matrix
PCM.survey_JMLE <- tam.jml(survey.responses, A = design.matrix, constraint = "items", control=list(maxiter=500), version=2 , verbose = FALSE)

# request the summary
summary(PCM.survey_JMLE)

### Item Parameters

# examine the item difficulty location and rating scale category threshold estimates
items_JMLE <- PCM.survey_JMLE$item$xsi.item

# center the item parameter estimates
uncentered.item.locations_JMLE <- items_JMLE
centered.item.locations_JMLE <- scale(uncentered.item.locations_JMLE, scale = FALSE)
summary(centered.item.locations_JMLE)

# Specify the number of thresholds as the maximum observed score in the response matrix (be sure the responses begin at category 0):
n.thresholds <- max(survey.responses)

# Save the threshold estimates, which begin in column 5 of the item table:
tau.estimates_JMLE <- PCM.survey_JMLE$item[, c(5 : (4+n.thresholds))]

# Specify the number of items that were included in the analysis:
n.items <- ncol(survey.responses)

# Create a matrix in which to store the adjacent-category threshold values for each item:
tau.matrix_JMLE <- matrix(data = NA, ncol = n.thresholds, nrow = n.items)

# Calculate adjacent-category threshold values:

for(item.number in 1:n.items){
  for(tau in 1:n.thresholds){
    tau.matrix_JMLE[item.number, tau] <- ifelse(tau == 1,
                                                (tau.estimates_JMLE[item.number, tau] -
                                                   uncentered.item.locations_JMLE[item.number]),

                                                (tau.estimates_JMLE[item.number, tau] -
                                                   sum(tau.estimates_JMLE[item.number, c((tau-1))]) -
                                                   uncentered.item.locations_JMLE[item.number]))
  }
}

# View the threshold estimates:
tau.matrix_JMLE

### Item Response Functions

# examine rating scale category probability plots
graphics.off()
plot(PCM.survey_JMLE, type="items")

### Item Fit

# examine numeric item fit indices
JMLE_fit <- tam.fit(PCM.survey_JMLE)
item.fit_JMLE <- JMLE_fit$fit.item

# view summary statistics for the fit statistics
summary(item.fit_JMLE)

### Person Parameters
# Store person parameters and their standard errors in a data.frame object:
person.locations_JMLE <- cbind.data.frame(PCM.survey_JMLE$theta,
                                          PCM.survey_JMLE$errorWLE)

names(person.locations_JMLE) <- c("theta", "SE")

# View summary statistics for person parameters:
summary(person.locations_JMLE)

# Subtract the original (uncentered) item mean location from the person locations:
person.locations_JMLE$theta_adjusted <- person.locations_JMLE$theta - mean(uncentered.item.locations_JMLE)

# Summary of person location estimates:
summary(person.locations_JMLE)

### Person fit

# examine numeric person fit indices
JMLE_fit <- tam.fit(PCM.survey_JMLE)
person.fit_JMLE <- JMLE_fit$fit.person

# Request the summary
summary(person.fit_JMLE)

# plot the Wright Map
# Combine centered item estimates with thresholds:
n.items <- ncol(survey.responses)

thresholds_JMLE <- matrix(data = NA, nrow = n.items, ncol = n.thresholds)

for(item.number in 1:n.items){
  for(tau in 1:n.thresholds){
    thresholds_JMLE[item.number, tau] <- centered.item.locations_JMLE[item.number] +
      tau.matrix_JMLE[item.number, tau]
  }
}

thetas_JMLE <- person.locations_JMLE$theta_adjusted

# Plot the Wright Map
wrightMap(thetas = thetas_JMLE,
          thresholds = thresholds_JMLE,
          main.title = "Partial Credit Model Wright Map (JMLE)",
          show.thr.lab	= TRUE, dim.names = "",
          label.items.rows= 2)

## Tables for results section

# Print Table 1:
knitr::kable(
  PCM_Table1, booktabs = TRUE,
  caption = 'Model Summary Table'
)

# Print Table 2:
knitr::kable(
  PCM_Table2, booktabs = TRUE,
  caption = 'Item Calibrations'
)

# Print Table 3:
knitr::kable(
  head(PCM_Table3,10), booktabs = TRUE,
  caption = 'Person Calibration'
)

# Plot person-item map
graphics.off()
plotPImap(PCM.survey, main = "Partial Credit Model Wright Map", sorted = TRUE, irug = FALSE)
