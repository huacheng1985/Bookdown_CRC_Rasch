###--- Chapter 6 Exercise Solution Code ---###

### Install and load the packages:

# install.packages("TAM")
library("TAM")

# install.packages("WrightMap")
library("WrightMap")

# install.packages("psych")
library("psych")

### Import the data:

#* Note: Update your working directory or include the complete file path to apply the read.csv() function]
survey.ratings <- read.csv("exercise_6_data.csv")

# printing the first six rows of the data frame object
head(survey.ratings)

# explore the data using descriptive statistics
summary(survey.ratings)

### Specify Model Components

# items make up the first facet in our analysis. Our second facet will be subgroups. We specify this facet.
facets <- survey.ratings[ , "subgroup", drop = FALSE]

# identify the indicator variable
persons <- survey.ratings$person

# specify the response matrix
ratings <- subset(survey.ratings, select = -c(person, subgroup))

# specify the MFRM in an object
ratings_RS_MFRM <- ~ item + subgroup + step

### Run the RS-MFRM

# Now run our RS-MFRM
RS_MFR_model <- tam.mml.mfr(resp = ratings, facets = facets, formulaA = ratings_RS_MFRM, pid = persons, constraint = "items", verbose = FALSE)

### Overall Model Summary
summary(RS_MFR_model)

### Facet Results
facet.estimates <- RS_MFR_model$xsi.facets

# create objects in which we store the location estimates and standard errors for items, subgroups, and thresholds separately
item.estimates <- subset(facet.estimates, facet.estimates$facet == "item")
subgroup.estimates <- subset(facet.estimates, facet.estimates$facet == "subgroup")
threshold.estimates <- subset(facet.estimates, facet.estimates$facet == "step")

### Item Facet Results

# Request the summary
summary(item.estimates)

### Subgroup Facet Results

# examine the  subgroup estimates
summary(subgroup.estimates$xsi)

# print the locations to our console to inspect them
subgroup.estimates$xsi

### Person estimates

# examine the person location estimates from the RS-MFRM
person.locations <- tam.wle(RS_MFR_model)

# store the person identification numbers, location estimates, and standard errors in a new object
person.locations_RSMFR <- cbind.data.frame(person.locations$pid, person.locations$theta, person.locations$error)

# Name the object
names(person.locations_RSMFR) <- c("id", "theta", "se")

# request the summary
summary(person.locations_RSMFR)

### Threshold Estimates

# examine the threshold estimates
threshold.estimates$xsi

# store the item location estimates as a matrix that shows item-specific threshold locations
item_thresholds <- matrix(data = NA, nrow = nrow(item.estimates), ncol = nrow(threshold.estimates))

for(item in 1:nrow(item.estimates)){
  for(tau in 1:nrow(threshold.estimates)){
    item_thresholds[item,tau] <- (item.estimates$xsi[item] + threshold.estimates$xsi[tau])
  }
}

head(item_thresholds)

## plot the Wright Map
wrightMap(thetas = cbind(person.locations_RSMFR$theta, subgroup.estimates$xsi),
          axis.persons = "Persons",
          dim.names = c("Persons", "Subgroups"),
          thresholds = item_thresholds,
          show.thr.lab	= TRUE,
          label.items.rows= 2,
          label.items = item.estimates$parameter,
          axis.items = "Items",
          main.title = "Rating Scale Many-Facet Rasch Model Wright Map")

### Evaluate Model-Data Fit

# Unidimensionality
# extract the model residuals
resids <- IRT.residuals(RS_MFR_model)

# Extract the raw residuals from the residuals object:
r <- as.data.frame(resids$residuals)

# Save the residuals in a matrix:
resid.matrix <- matrix(data = NA, nrow = nrow(ratings), ncol = ncol(ratings))

ngroups <- nrow(subgroup.estimates)


for(item.number in 1:ncol(ratings)){
  group.items <- NULL

  for(group in 1:ngroups){
    group.items[group] <- paste("i", item.number, "-", "subgroup", group, sep = "")
  }

  item <- subset(r, select = group.items)

  resid.matrix[, item.number] <- rowSums(item, na.rm = TRUE)
}

# request a summary
summary(resid.matrix)

# Extract standardized residuals from the resids object:
s <- as.data.frame(resids$stand_residuals)

# Save the standardized residuals in a matrix:
std.resid.matrix <- matrix(data = NA, nrow = nrow(ratings), ncol = ncol(ratings))

ngroups <- nrow(subgroup.estimates)

for(item.number in 1:ncol(ratings)){
  group.items <- NULL

  for(group in 1:ngroups){
    group.items[group] <- paste("i", item.number, "-", "subgroup", group, sep = "")
  }

  item <- subset(r, select = group.items)

  std.resid.matrix[, item.number] <- rowSums(item, na.rm = TRUE)
}

# request the summary
summary(std.resid.matrix)

# Variance of the observations: VO
observations.vector <- as.vector(as.matrix(ratings))
VO <- var(observations.vector)

# Variance of the residuals: VR
residuals.vector <- as.vector(resid.matrix)
VR <- var(residuals.vector)

# Raw variance explained by Rasch measures: (VO - VR)/VO
(VO - VR)/VO

# Express the result as a percent:
((VO - VR)/VO) * 100

## Principal Components Analysis of Standardized Residual Correlations

# evaluate the MFRM requirement for unidimensionality using a principal components analysis (PCA) of standardized residual correlations
pca <- pca(std.resid.matrix, nfactors = ncol(ratings), rotate = "none")

contrasts <- c(pca$values[1], pca$values[2], pca$values[3], pca$values[4], pca$values[5])

graphics.off()
plot(contrasts, ylab = "Eigenvalues for Contrasts", xlab = "Contrast Number", main = "Contrasts from PCA of Standardized Residual Correlations", ylim = c(0, 2))

### Summaries of Residuals: Infit & Outfit Statistics

## Student fit
# examine person fit using numeric infit and outfit statistics
person.fit <- tam.personfit(RS_MFR_model)
summary(person.fit)

## Subgroup and Item Fit
# examine model-data fit related to the subgroup and item facets
item.subgroup.fit <- msq.itemfit(RS_MFR_model)
summary(item.subgroup.fit)

# examine model-data fit statistics specific to subgroups of examinees
fit_with_subgroups <- cbind.data.frame(survey.ratings$subgroup, person.fit)

# merges the person fit results with student subgroup identification numbers
fit_group_1 <- subset(fit_with_subgroups, fit_with_subgroups$`survey.ratings$subgroup` == 1)
summary(fit_group_1)
fit_group_2 <- subset(fit_with_subgroups, fit_with_subgroups$`survey.ratings$subgroup` == 2)
summary(fit_group_2)

# Boxplots for MSE fit statistics:
boxplot(fit_group_1$outfitPerson, fit_group_2$outfitPerson,
        fit_group_1$infitPerson, fit_group_2$infitPerson,
        names = c("Group 1 \nOutfit MSE", "Group 2 \nOutfit MSE",
                  "Group 1 \nInfit MSE", "Group 2 \nInfit MSE"),
        col = c("grey", "white", "grey", "white"),
        main = "MSE Fit Statistics for Group 1 \nand Group 2",
        cex.main = .8,
        ylab = "MSE Fit Statistic", xlab = "Subgroup")

# Boxplots for standardized fit statistics:
boxplot(fit_group_1$outfitPerson_t, fit_group_2$outfitPerson_t,
        fit_group_1$infitPerson_t, fit_group_2$infitPerson_t,
        names = c("Group 1 \nStd. Outfit", "Group 2 \nStd. Outfit",
                  "Group 1 \nStd. Infit", "Group 2 \nStd. Infit"),
        col = c("grey", "white", "grey", "white"),
        main = "Standardized Fit Statistics for Group 1 \nand Group 2",
        cex.main = .8,
        ylab = "MSE Fit Statistic", xlab = "Subgroup")

# examine fit statistics as they apply to individual raters
ngroups <- nrow(subgroup.estimates)

item.fit <- matrix(data = NA, nrow = ncol(ratings), ncol = (ngroups * 4) + 1 )


for(item.number in 1:ncol(ratings)){

  # calculate item-specific fit statistics:
  item.outfit <- item.subgroup.fit$itemfit$Outfit[((item.number*ngroups) - (ngroups - 1)) : (item.number*ngroups)]

  item.infit <- item.subgroup.fit$itemfit$Infit[((item.number*ngroups) - (ngroups - 1)) : (item.number*ngroups)]

  item.std.outfit <- item.subgroup.fit$itemfit$Outfit_t[((item.number*ngroups) - (ngroups - 1)) : (item.number*ngroups)]

  item.std.infit <- item.subgroup.fit$itemfit$Infit_t[((item.number*ngroups) - (ngroups - 1)) : (item.number*ngroups)]

  # add the fit statistics to the matrix:
  item.fit[item.number, ] <-  c(item.number, item.outfit, item.infit,
                                  item.std.outfit, item.std.infit)

  # Convert the item fit results to a dataframe object and add meaningful column names:
  item.fit_results <- as.data.frame(item.fit)

  infit_mse_labels <- NULL
  for(group in 1:ngroups){
    infit_mse_labels[group] <- paste("Infit_MSE_Group", group, sep = "")
  }

  outfit_mse_labels <- NULL
  for(group in 1:ngroups){
    outfit_mse_labels[group] <- paste("Outfit_MSE_Group", group, sep = "")
  }

  std_infit_mse_labels <- NULL
  for(group in 1:ngroups){
    std_infit_mse_labels[group] <- paste("Std.Infit_MSE_Group", group, sep = "")
  }

  std_outfit_mse_labels <- NULL
  for(group in 1:ngroups){
    std_outfit_mse_labels[group] <- paste("Std.Outfit_MSE_Group", group, sep = "")
  }
}

names(item.fit_results) <- c("Item", outfit_mse_labels, infit_mse_labels,
                                std_outfit_mse_labels, std_infit_mse_labels)

# Display the rater fit results for the first six raters using the head() function:
head(item.fit_results)

# Request the summary
summary(item.fit_results)

### Graphical Displays of Residuals

# Before constructing the plots, find the maximum and minimum values of the standardized residuals to set limits for the axes:
max.resid <- ceiling(max(std.resid.matrix))
min.resid <- ceiling(min(std.resid.matrix))

# The code below will produce plots of standardized residuals for selected raters as listed in raters.to.plot:
items.to.plot <- c(1:3)

for(item.number in items.to.plot){
    plot(std.resid.matrix[, item.number], ylim = c(min.resid, max.resid),
         main = paste("Standardized Residuals for item ", item.number, sep = ""),
         ylab = "Standardized Residual", xlab = "Person Index")
    abline(h = 0, col = "blue")
    abline(h=2, lty = 2, col = "red")
    abline(h=-2, lty = 2, col = "red")

    legend("topright", c("Std. Residual", "Observed = Expected", "+/- 2 SD"), pch = c(1, NA, NA),
           lty = c(NA, 1, 2),
           col = c("black", "blue", "red"), cex = .8)
}

### Expected and Observed Response Functions

# construct plots of expected and observed response functions
items.to.plot <- c(1:3)
plot(RS_MFR_model, type = "expected", items = items.to.plot)

# Summarize the Results in Tables
RS_MFRM_summary.table.statistics <- c("Logit Scale Location Mean",
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
                                        "Std. Infit SD")

RS_MFRM_person.summary.results <- rbind(mean(person.locations_RSMFR$theta),
                                           sd(person.locations_RSMFR$theta),
                                           mean(person.locations_RSMFR$se),
                                           sd(person.locations_RSMFR$se),
                                           mean(person.fit$outfitPerson),
                                           sd(person.fit$outfitPerson),
                                           mean(person.fit$infitPerson),
                                           sd(person.fit$infitPerson),
                                           mean(person.fit$outfitPerson_t),
                                           sd(person.fit$outfitPerson_t),
                                           mean(person.fit$infitPerson_t),
                                           sd(person.fit$infitPerson_t))

RS_MFRM_subgroup.summary.results <- rbind(mean(subgroup.estimates$xsi),
                                            sd(subgroup.estimates$xsi),
                                            mean(subgroup.estimates$se.xsi),
                                            sd(subgroup.estimates$se.xsi),
                                            mean(item.subgroup.fit$itemfit$Outfit),
                                            sd(item.subgroup.fit$itemfit$Outfit),
                                            mean(item.subgroup.fit$itemfit$Infit),
                                            sd(item.subgroup.fit$itemfit$Infit),
                                            mean(item.subgroup.fit$itemfit$Outfit_t),
                                            sd(item.subgroup.fit$itemfit$Outfit_t),
                                            mean(item.subgroup.fit$itemfit$Infit_t),
                                            sd(item.subgroup.fit$itemfit$Infit_t))

RS_MFRM_item.summary.results <- rbind(mean(item.estimates$xsi),
                                         sd(item.estimates$xsi),
                                         mean(item.estimates$se.xsi),
                                         sd(item.estimates$se.xsi),
                                         mean(item.subgroup.fit$itemfit$Outfit),
                                         sd(item.subgroup.fit$itemfit$Outfit),
                                         mean(item.subgroup.fit$itemfit$Infit),
                                         sd(item.subgroup.fit$itemfit$Infit),
                                         mean(item.subgroup.fit$itemfit$Outfit_t),
                                         sd(item.subgroup.fit$itemfit$Outfit_t),
                                         mean(item.subgroup.fit$itemfit$Infit_t),
                                         sd(item.subgroup.fit$itemfit$Infit_t))


# Round the values for presentation in a table:
RS_MFRM_person.summary.results_rounded <- round(RS_MFRM_person.summary.results, digits = 2)

RS_MFRM_subgroup.summary.results_rounded <- round(RS_MFRM_subgroup.summary.results, digits = 2)

RS_MFRM_item.summary.results_rounded <- round(RS_MFRM_item.summary.results, digits = 2)


RS_MFRM_Table1 <- cbind.data.frame(RS_MFRM_summary.table.statistics,
                                     RS_MFRM_person.summary.results_rounded,
                                     RS_MFRM_subgroup.summary.results_rounded,
                                     RS_MFRM_item.summary.results_rounded)


# add descriptive column labels:
names(RS_MFRM_Table1) <- c("Statistic", "Persons", "Subgroups", "Items")

# Print the table to the console
RS_MFRM_Table1

# Calculate the average rating for each rater:
Avg_Rating <- apply(ratings, 2, mean)

# Combine rater calibration results in a table:

RS_MFRM_Table2 <- cbind.data.frame(c(1:ncol(ratings)),
                                     Avg_Rating,
                                     item.estimates$xsi,
                                     item_thresholds,
                                     item.fit_results[, -1])

names(RS_MFRM_Table2) <- c("Item Number", "Average Rating", "Item Location","Threshold 1", "Threshold 2", "Threshold 3", names(item.fit_results[, -1]))

# Sort Table 2 by item difficulty:
RS_MFRM_Table2 <- RS_MFRM_Table2[order(-RS_MFRM_Table2$`Item Location`),]

# Round the numeric values (all columns except the first one) to 2 digits:
RS_MFRM_Table2[, -1] <- round(RS_MFRM_Table2[,-1], digits = 2)

# Print the table to the console
RS_MFRM_Table2

# Calculate average ratings for students:
Person_Avg_Rating <- apply(ratings, 1, mean)

# Combine person calibration results in a table:
RS_MFRM_Table3 <- cbind.data.frame(rownames(person.locations_RSMFR),
                                     Person_Avg_Rating,
                                     person.locations_RSMFR$theta,
                                     person.locations_RSMFR$se,
                                     person.fit$outfitPerson,
                                     person.fit$outfitPerson_t,
                                     person.fit$infitPerson,
                                     person.fit$infitPerson_t)

names(RS_MFRM_Table3) <- c("Person ID", "Average Rating", "Person Location","Person SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Sort Table 3 by person location:
RS_MFRM_Table3 <- RS_MFRM_Table3[order(-RS_MFRM_Table3$`Person Location`),]

# Round the numeric values (all columns except the first one) to 2 digits:
RS_MFRM_Table3[, -1] <- round(RS_MFRM_Table3[,-1], digits = 2)

# Print the first six rows of the table to the console
head(RS_MFRM_Table3)

# Calculate average ratings for person subgroups:
group.1.ratings <- subset(survey.ratings, ratings$subgroup == 1)
group.2.ratings <- subset(survey.ratings, ratings$subgroup == 2)

group.1_Avg_Rating <- mean(apply(group.1.ratings[, -c(1:2)], 1, mean))
group.2_Avg_Rating <- mean(apply(group.2.ratings[, -c(1:2)], 1, mean))

Subgroup_Avg_Rating <- c(group.1_Avg_Rating, group.2_Avg_Rating)

# Combine subgroup calibration results in a table:
RS_MFRM_Table4 <- cbind.data.frame(subgroup.estimates$parameter,
                                     Subgroup_Avg_Rating,
                                     subgroup.estimates$xsi,
                                     subgroup.estimates$se.xsi,
                                     c(mean(fit_group_1$outfitPerson), mean(fit_group_2$outfitPerson)),
                                     c(mean(fit_group_1$outfitPerson_t), mean(fit_group_2$outfitPerson_t)),
                                     c(mean(fit_group_1$infitPerson), mean(fit_group_2$infitPerson)),
                                     c(mean(fit_group_1$infitPerson_t), mean(fit_group_2$infitPerson_t)))


names(RS_MFRM_Table4) <- c("Subgroup", "Average Rating", "Subgroup Location","Subgroup Location SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Sort Table 4 by subgroup location:
RS_MFRM_Table4 <- RS_MFRM_Table4[order(-RS_MFRM_Table4$`Subgroup Location`),]

# Round the numeric values (all columns except the first one) to 2 digits:
RS_MFRM_Table4[, -1] <- round(RS_MFRM_Table4[,-1], digits = 2)

# Print the table to the console
head(RS_MFRM_Table4)
