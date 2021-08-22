###--- Chapter 6 Code ---###

### Install and load the packages:

# install.packages("TAM")
library("TAM")

# install.packages("WrightMap")
library("WrightMap")

# install.packages("psych")
library("psych")

### Import the data:

#* Note: Update your working directory or include the complete file path to apply the read.csv() function]
style <- read.csv("style_ratings.csv")

# printing the first six rows of the data frame object
head(style)

# explore the data using descriptive statistics
summary(style)

### Specify Model Components

# raters make up the first facet in our analysis. Our second facet will be student language subgroups. We specify this facet.
facets <- style[ , "language", drop = FALSE]

# identify the indicator variable
students <- style$student

# specify the response matrix
ratings <- subset(style, select = -c(student, language))

# specify the MFRM in an object
style_RS_MFRM <- ~ item + language + step

### Run the RS-MFRM

# Now run our RS-MFRM
RS_MFR_model <- tam.mml.mfr(resp = ratings, facets = facets, formulaA = style_RS_MFRM, pid = students, constraint = "items", verbose = FALSE)

### Overall Model Summary
summary(RS_MFR_model)

### Facet Results
facet.estimates <- RS_MFR_model$xsi.facets

# create objects in which we store the location estimates and standard errors for raters, subgroups, and thresholds separately
rater.estimates <- subset(facet.estimates, facet.estimates$facet == "item")
subgroup.estimates <- subset(facet.estimates, facet.estimates$facet == "language")
threshold.estimates <- subset(facet.estimates, facet.estimates$facet == "step")

### Rater Facet Results

# Request the summary
summary(rater.estimates)

### Student Subgroup Facet Results

# examine the language subgroup estimates
summary(subgroup.estimates$xsi)

# print the locations to our console to inspect them
subgroup.estimates$xsi

### Student estimates

# examine the student location estimates from the RS-MFRM
student.ach <- tam.wle(RS_MFR_model)

# store the student identification numbers, location estimates, and standard errors in a new object
student.locations_RSMFR <- cbind.data.frame(student.ach$pid, student.ach$theta, student.ach$error)

# Name the object
names(student.locations_RSMFR) <- c("id", "theta", "se")

# request the summary
summary(student.locations_RSMFR)

### Threshold Estimates

# examine the threshold estimates
threshold.estimates$xsi

# store the rater location estimates as a matrix that shows rater-specific threshold locations
rater_thresholds <- matrix(data = NA, nrow = nrow(rater.estimates), ncol = nrow(threshold.estimates))

for(rater in 1:nrow(rater.estimates)){
  for(tau in 1:nrow(threshold.estimates)){
    rater_thresholds[rater,tau] <- (rater.estimates$xsi[rater] + threshold.estimates$xsi[tau])
  }
}

head(rater_thresholds)

## plot the Wright Map
wrightMap(thetas = cbind(student.locations_RSMFR$theta, subgroup.estimates$xsi),
          axis.persons = "Students",
          dim.names = c("Students", "Subgroups"),
          thresholds = rater_thresholds,
          show.thr.lab	= TRUE,
          label.items.rows= 2,
          label.items = rater.estimates$parameter,
          axis.items = "Raters",
          main.title = "Rating Scale Many-Facet Rasch Model Wright Map:\n Style Ratings",
          cex.main = .6)

### Evaluate Model-Data Fit

# Unidimensionality
# extract the model residuals
resids <- IRT.residuals(RS_MFR_model)

# Extract the raw residuals from the residuals object:
r <- as.data.frame(resids$residuals)

# Save the residuals in a matrix:
resid.matrix <- matrix(data = NA, nrow = nrow(style), ncol = ncol(ratings))

ngroups <- nrow(subgroup.estimates)

for(rater.number in 1:ncol(ratings)){
  group.raters <- NULL

  for(group in 1:ngroups){
    group.raters[group] <- paste("rater_", rater.number, "-", "language", group, sep = "")
  }

  rater <- subset(r, select = group.raters)

  resid.matrix[, rater.number] <- rowSums(rater, na.rm = TRUE)
}

# request a summary
summary(resid.matrix)

# Extract standardized residuals from the resids object:
s <- as.data.frame(resids$stand_residuals)

# Save the standardized residuals in a matrix:
std.resid.matrix <- matrix(data = NA, nrow = nrow(style), ncol = ncol(ratings))

ngroups <- nrow(subgroup.estimates)

for(rater.number in 1:ncol(ratings)){
  group.raters <- NULL

  for(group in 1:ngroups){
    group.raters[group] <- paste("rater_", rater.number, "-", "language", group, sep = "")
  }

  rater <- subset(s, select = group.raters)

  std.resid.matrix[, rater.number] <- rowSums(rater, na.rm = TRUE)
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

plot(contrasts, ylab = "Eigenvalues for Contrasts", xlab = "Contrast Number", main = "Contrasts from PCA of Standardized Residual Correlations", ylim = c(0, 2))

### Summaries of Residuals: Infit & Outfit Statistics

## Student fit
# examine student fit using numeric infit and outfit statistics
student.fit <- tam.personfit(RS_MFR_model)
summary(student.fit)

## Subgroup and Rater Fit
# examine model-data fit related to the subgroup and rater facets
rater.subgroup.fit <- msq.itemfit(RS_MFR_model)
summary(rater.subgroup.fit)

# examine model-data fit statistics specific to subgroups of examinees
fit_with_subgroups <- cbind.data.frame(style$language, student.fit)

# merges the person fit results with student subgroup identification numbers
fit_group_1 <- subset(fit_with_subgroups, fit_with_subgroups$`style$language` == 1)
summary(fit_group_1)
fit_group_2 <- subset(fit_with_subgroups, fit_with_subgroups$`style$language` == 2)
summary(fit_group_2)

# Boxplots for MSE fit statistics:
boxplot(fit_group_1$outfitPerson, fit_group_2$outfitPerson,
        fit_group_1$infitPerson, fit_group_2$infitPerson,
        names = c("Group 1 \nOutfit MSE", "Group 2 \nOutfit MSE",
                  "Group 1 \nInfit MSE", "Group 2 \nInfit MSE"),
        col = c("grey", "white", "grey", "white"),
        main = "MSE Fit Statistics for English-not-Best-Language (Group 1) \nand English-Best-Language (Group 2) Students",
        cex.main = .8,
        ylab = "MSE Fit Statistic", xlab = "Student Subgroup")

# Boxplots for standardized fit statistics:
boxplot(fit_group_1$outfitPerson_t, fit_group_2$outfitPerson_t,
        fit_group_1$infitPerson_t, fit_group_2$infitPerson_t,
        names = c("Group 1 \nStd. Outfit", "Group 2 \nStd. Outfit",
                  "Group 1 \nStd. Infit", "Group 2 \nStd. Infit"),
        col = c("grey", "white", "grey", "white"),
        main = "Standardized Fit Statistics for English-not-Best-Language (Group 1) \nand English-Best-Language (Group 2) Students",
        cex.main = .8,
        ylab = "MSE Fit Statistic", xlab = "Student Subgroup")

# examine fit statistics as they apply to individual raters
ngroups <- nrow(subgroup.estimates)

rater.fit <- matrix(data = NA, nrow = ncol(ratings), ncol = (ngroups * 4) + 1 )

for(rater.number in 1:ncol(ratings)){

  # calculate rater-specific fit statistics:
  rater.outfit <- rater.subgroup.fit$itemfit$Outfit[((rater.number*ngroups) - (ngroups - 1)) : (rater.number*ngroups)]

  rater.infit <- rater.subgroup.fit$itemfit$Infit[((rater.number*ngroups) - (ngroups - 1)) : (rater.number*ngroups)]

  rater.std.outfit <- rater.subgroup.fit$itemfit$Outfit_t[((rater.number*ngroups) - (ngroups - 1)) : (rater.number*ngroups)]

  rater.std.infit <- rater.subgroup.fit$itemfit$Infit_t[((rater.number*ngroups) - (ngroups - 1)) : (rater.number*ngroups)]

  # add the fit statistics to the matrix:
  rater.fit[rater.number, ] <-  c(rater.number, rater.outfit, rater.infit,
                                  rater.std.outfit, rater.std.infit)

  # Convert the rater fit results to a dataframe object and add meaningful column names:

  rater.fit_results <- as.data.frame(rater.fit)

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

  names(rater.fit_results) <- c("Rater", outfit_mse_labels, infit_mse_labels,
                                std_outfit_mse_labels, std_infit_mse_labels)

# Display the rater fit results for the first six raters using the head() function:
head(rater.fit_results)

# Request the summary
summary(rater.fit_results)

### Graphical Displays of Residuals

# Before constructing the plots, find the maximum and minimum values of the standardized residuals to set limits for the axes:
max.resid <- ceiling(max(std.resid.matrix))
min.resid <- ceiling(min(std.resid.matrix))

# The code below will produce plots of standardized residuals for selected raters as listed in raters.to.plot:
raters.to.plot <- c(1:3)

for(rater.number in raters.to.plot){
  plot(std.resid.matrix[, rater.number], ylim = c(min.resid, max.resid),
       main = paste("Standardized Residuals for Rater ", rater.number, sep = ""),
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
raters.to.plot <- c(1:3)
plot(RS_MFR_model, type = "expected", items = raters.to.plot)

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

RS_MFRM_student.summary.results <- rbind(mean(student.locations_RSMFR$theta),
                                         sd(student.locations_RSMFR$theta),
                                         mean(student.locations_RSMFR$se),
                                         sd(student.locations_RSMFR$se),
                                         mean(student.fit$outfitPerson),
                                         sd(student.fit$outfitPerson),
                                         mean(student.fit$infitPerson),
                                         sd(student.fit$infitPerson),
                                         mean(student.fit$outfitPerson_t),
                                         sd(student.fit$outfitPerson_t),
                                         mean(student.fit$infitPerson_t),
                                         sd(student.fit$infitPerson_t))

RS_MFRM_subgroup.summary.results <- rbind(mean(subgroup.estimates$xsi),
                                          sd(subgroup.estimates$xsi),
                                          mean(subgroup.estimates$se.xsi),
                                          sd(subgroup.estimates$se.xsi),
                                          mean(rater.subgroup.fit$itemfit$Outfit),
                                          sd(rater.subgroup.fit$itemfit$Outfit),
                                          mean(rater.subgroup.fit$itemfit$Infit),
                                          sd(rater.subgroup.fit$itemfit$Infit),
                                          mean(rater.subgroup.fit$itemfit$Outfit_t),
                                          sd(rater.subgroup.fit$itemfit$Outfit_t),
                                          mean(rater.subgroup.fit$itemfit$Infit_t),
                                          sd(rater.subgroup.fit$itemfit$Infit_t))

RS_MFRM_rater.summary.results <- rbind(mean(rater.estimates$xsi),
                                       sd(rater.estimates$xsi),
                                       mean(rater.estimates$se.xsi),
                                       sd(rater.estimates$se.xsi),
                                       mean(rater.subgroup.fit$itemfit$Outfit),
                                       sd(rater.subgroup.fit$itemfit$Outfit),
                                       mean(rater.subgroup.fit$itemfit$Infit),
                                       sd(rater.subgroup.fit$itemfit$Infit),
                                       mean(rater.subgroup.fit$itemfit$Outfit_t),
                                       sd(rater.subgroup.fit$itemfit$Outfit_t),
                                       mean(rater.subgroup.fit$itemfit$Infit_t),
                                       sd(rater.subgroup.fit$itemfit$Infit_t))


# Round the values for presentation in a table:
RS_MFRM_student.summary.results_rounded <- round(RS_MFRM_student.summary.results, digits = 2)

RS_MFRM_subgroup.summary.results_rounded <- round(RS_MFRM_subgroup.summary.results, digits = 2)

RS_MFRM_rater.summary.results_rounded <- round(RS_MFRM_rater.summary.results, digits = 2)


RS_MFRM_Table1 <- cbind.data.frame(RS_MFRM_summary.table.statistics,
                                   RS_MFRM_student.summary.results_rounded,
                                   RS_MFRM_subgroup.summary.results_rounded,
                                   RS_MFRM_rater.summary.results_rounded)


# add descriptive column labels:
names(RS_MFRM_Table1) <- c("Statistic", "Students", "Subgroups", "Raters")

# Print the table to the console
RS_MFRM_Table1

# Calculate the average rating for each rater:
Avg_Rating <- apply(ratings, 2, mean)

# Combine rater calibration results in a table:

RS_MFRM_Table2 <- cbind.data.frame(c(1:ncol(ratings)),
                                   Avg_Rating,
                                   rater.estimates$xsi,
                                   rater_thresholds,
                                   rater.fit_results[, -1])

names(RS_MFRM_Table2) <- c("Rater ID", "Average Rating", "Rater Location","Threshold 1", "Threshold 2", "Threshold 3", names(rater.fit_results[, -1]))

# Sort Table 2 by rater severity:
RS_MFRM_Table2 <- RS_MFRM_Table2[order(-RS_MFRM_Table2$`Rater Location`),]

# Round the numeric values (all columns except the first one) to 2 digits:
RS_MFRM_Table2[, -1] <- round(RS_MFRM_Table2[,-1], digits = 2)

# Print the table to the console
RS_MFRM_Table2

# Calculate average ratings for students:
Person_Avg_Rating <- apply(ratings, 1, mean)

# Combine person calibration results in a table:
RS_MFRM_Table3 <- cbind.data.frame(rownames(student.locations_RSMFR),
                                   Person_Avg_Rating,
                                   student.locations_RSMFR$theta,
                                   student.locations_RSMFR$se,
                                   student.fit$outfitPerson,
                                   student.fit$outfitPerson_t,
                                   student.fit$infitPerson,
                                   student.fit$infitPerson_t)

names(RS_MFRM_Table3) <- c("Student ID", "Average Rating", "Student Location","Student SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Sort Table 3 by student location:
RS_MFRM_Table3 <- RS_MFRM_Table3[order(-RS_MFRM_Table3$`Student Location`),]

# Round the numeric values (all columns except the first one) to 2 digits:
RS_MFRM_Table3[, -1] <- round(RS_MFRM_Table3[,-1], digits = 2)

# Print the first six rows of the table to the console
head(RS_MFRM_Table3)

# Calculate average ratings for student subgroups:
group.1.style <- subset(style, style$language == 1)
group.2.style <- subset(style, style$language == 2)

group.1_Avg_Rating <- mean(apply(group.1.style[, -c(1:2)], 1, mean))
group.2_Avg_Rating <- mean(apply(group.2.style[, -c(1:2)], 1, mean))

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

### Another Example: Running PC-MFRM with Long-Format Data using the *TAM* Package

# import the data
writing <- read.csv("writing.csv")
head(writing)

# using descriptive statistics
summary(writing)

# Check the missing data
length(unique(writing$student))

## Specify the PC-MFRM

# With long format data, we need to ensure that the person identification numbers (in this case, student labels) are sorted from low to high before we can run the analysis.
writing <- writing[order(writing$student), ]

# specify the components of the PC-MFRM
writing.facets <- writing[, c("rater"), drop = FALSE]

# identify the object of measurement as students
writing.pid <- writing$student

# Specify the response matrix
writing.resp <- subset(writing, select = -c(student, language, rater))

## Specify the formula
PC.writing.formula <- ~ item + rater + item:step

## run the PC-MFRM model
writing_PC_MFRM.model <- tam.mml.mfr(resp=writing.resp, facets=writing.facets,
                                     formulaA=PC.writing.formula, pid=writing.pid, verbose = FALSE, constraint = "items")

# request the summary
summary(writing_PC_MFRM.model)

### Facets results

# Save the facet estimates:
facet.estimates <- writing_PC_MFRM.model$xsi.facets # all facets together

# Extract results for each facet separately:
domain.estimates <- subset(facet.estimates, facet.estimates$facet == "item")
rater.estimates <- subset(facet.estimates, facet.estimates$facet == "rater")

# Extract domain-specific threshold estimates:
threshold.estimates <- subset(facet.estimates, facet.estimates$facet == "item:step")

## Domain facet results

# examine the results for the domain facet
domain.estimates
summary(domain.estimates)

## Rater Facet Results

# examine the rater estimates
summary(rater.estimates)

## Student Facet Results

# estimate student locations and store the results
student.ach <- tam.wle(writing_PC_MFRM.model, progress = FALSE)

# store the student identification numbers, location estimates, and standard errors
student.locations_PCMFR <- cbind.data.frame(student.ach$pid, student.ach$theta, student.ach$error)

names(student.locations_PCMFR) <- c("id", "theta", "se")

# examine the student location estimates
summary(student.locations_PCMFR$theta)

### Threshold estimates

# used a PC formulation of the MFRM
n.domains <- nrow(domain.estimates)
n.thresholds <- max(writing.resp)

domain_taus <- matrix(data = NA, nrow = n.domains, ncol = n.thresholds)

for(domain.number in 1:n.domains){

  domain.threshold.labels <- NULL
  for(step.number in 1:n.thresholds){
    domain.threshold.labels[step.number] <- paste(domain.estimates$parameter[domain.number], ":step", step.number, sep = "")
  }

  domain.thresholds <- subset(threshold.estimates,
                              threshold.estimates$parameter %in% domain.threshold.labels)

  domain.thresholds.t <- t(domain.thresholds$xsi)

  domain_taus[domain.number,] <- domain.thresholds.t[1,]
}

domain_taus <- cbind.data.frame(c(1:n.domains), domain_taus)
names(domain_taus) <- c("domain", "t1", "t2", "t3")

domain_taus

# plot a Wright Map
wrightMap(thetas =cbind(student.locations_PCMFR$theta,
                        rater.estimates$xsi),
          axis.persons = "",
          dim.names = c("Students", "Raters"),
          thresholds = domain_taus[,-1],
          show.thr.lab	= TRUE,
          label.items.rows= 2,
          label.items = domain.estimates$parameter,
          axis.items = "Domains",
          main.title = "Partial Credit Many-Facet Rasch Model \nWright Map: Style Ratings",
          cex.main = .6)

### Evaluate Model-Data Fit

# construct a residual matrix
resids <- IRT.residuals(writing_PC_MFRM.model)

# Extract the raw residuals from the residuals object:
resid.matrix <- as.data.frame(resids$residuals)

# calculate standardized residuals and save them in a matrix
std.resid.matrix <- as.data.frame(resids$stand_residuals)

# Variance of the observations: VO
observations.vector <- as.vector(as.matrix(writing.resp))
VO <- var(observations.vector)

# Variance of the residuals: VR
residuals.vector <- as.vector(as.matrix(resid.matrix))
VR <- var(residuals.vector)

# Raw variance explained by Rasch measures: (VO - VR)/VO
(VO - VR)/VO

# Express the result as a percent:
((VO - VR)/VO) * 100

## Principal Components Analysis of Standardized Residual Correlations

# evaluate the MFRM requirement for unidimensionality using a principal components analysis (PCA) of standardized residual correlations
pca <- pca(as.matrix(std.resid.matrix), rotate = "none")

contrasts <- c(pca$values[1], pca$values[2], pca$values[3], pca$values[4], pca$values[5])

plot(contrasts, ylab = "Eigenvalues for Contrasts", xlab = "Contrast Number", main = "Contrasts from PCA of Standardized Residual Correlations \n(PC-MFRM)", cex.main = .8)

## Summaries of Residuals: Infit & Outfit Statistics

# Student Fit
student.fit <- tam.personfit(writing_PC_MFRM.model)
summary(student.fit)

## Domain and Rater Fit
# examine model-data fit related to the domain and rater facets
rater.domain.fit <- msq.itemfit(writing_PC_MFRM.model)
rater.domain.fit <- rater.domain.fit$itemfit
summary(rater.domain.fit)

# examine model-data fit statistics specific to levels of explanatory facets, such as domains in the current example
ngroups <- nrow(domain.estimates)

domain.fit <- matrix(data = NA, nrow = ncol(domain.estimates), ncol = 4 )

for(domain.number in 1:ncol(domain.estimates)){

  d <- domain.estimates$parameter[domain.number]

  rater.domain.labels <- NULL

  for(r in 1:nrow(rater.estimates)){
    rater_label <- rater.estimates$parameter[r]
    rater.domain.labels[r] <- paste(d, "-", rater_label, sep = "")
  }

  rater.domain.fit.subset <- subset(rater.domain.fit, rater.domain.fit$item %in% rater.domain.labels)

  # add the fit statistics to the matrix:
  domain.fit[domain.number, ] <-  c(mean(rater.domain.fit.subset$Infit),
                                    mean(rater.domain.fit.subset$Outfit),
                                    mean(rater.domain.fit.subset$Infit_t),
                                    mean(rater.domain.fit.subset$Outfit_t))
}

# add domain labels to the domain.fit object:

domain.fit <- cbind.data.frame(domain.estimates$parameter,
                               domain.fit)

# Convert the domain fit results to a data frame object and add meaningful column names:
domain.fit_results <- as.data.frame(domain.fit)

names(domain.fit_results) <- c("domain", "Mean_Infit_MSE", "Mean_Outfit_MSE",
                               "Mean_Std_Infit", "Mean_Std_Outfit")

# created a data frame with the domain-specific (average) fit statistics
summary(domain.fit_results)

## examine fit statistics as they apply to individual raters
n.domains <- nrow(domain.estimates)
n.raters <- nrow(rater.estimates)

rater.fit <- matrix(data = NA, nrow = n.raters, ncol = (n.domains * 4) + 1 )

for(rater.number in 1:nrow(rater.estimates)){

  if(rater.number < 10) r <- paste("rater_", rater.number, "_", sep = "")
  if(rater.number >= 10) r <- paste("rater_", rater.number, sep = "")

  rater.domain.labels <- NULL

  for(d in 1:nrow(domain.estimates)){
    domain_label <- domain.estimates$parameter[d]
    rater.domain.labels[d] <- paste(domain_label, "-", r, sep = "")
  }

  rater.domain.fit.subset <- subset(rater.domain.fit, rater.domain.fit$item %in% rater.domain.labels)

  # calculate rater-specific fit statistics:
  rater.outfit <- rater.domain.fit.subset$Outfit
  rater.infit <- rater.domain.fit.subset$Outfit
  rater.std.outfit <- rater.domain.fit.subset$Outfit_t
  rater.std.infit <- rater.domain.fit.subset$Infit_t

  # add the fit statistics to the matrix:
  rater.fit[rater.number, ] <-  c(rater.number, rater.outfit, rater.infit,
                                  rater.std.outfit, rater.std.infit)
}


# Convert the rater fit results to a dataframe object and add meaningful column names:

rater.fit_results <- as.data.frame(rater.fit)

infit_mse_labels <- NULL
for(domain in 1:n.domains){
  d <- domain.estimates$parameter[domain]
  infit_mse_labels[domain] <- paste("Infit_MSE_", d, sep = "")
}

outfit_mse_labels <- NULL
for(domain in 1:n.domains){
  d <- domain.estimates$parameter[domain]
  outfit_mse_labels[domain] <- paste("outfit_MSE_", d, sep = "")
}

std_infit_mse_labels <- NULL
for(domain in 1:n.domains){
  d <- domain.estimates$parameter[domain]
  std_infit_mse_labels[domain] <- paste("std_infit_MSE_", d, sep = "")
}

std_outfit_mse_labels <- NULL
for(domain in 1:n.domains){
  d <- domain.estimates$parameter[domain]
  std_outfit_mse_labels[domain] <- paste("std_outfit_MSE_", d, sep = "")
}

names(rater.fit_results) <- c("Rater", outfit_mse_labels, infit_mse_labels,
                              std_outfit_mse_labels, std_infit_mse_labels)

# Summarize the results
summary(rater.fit_results)

### Graphical Displays of Residuals

# Before constructing the plots, find the maximum and minimum values of the standardized residuals to set limits for the axes:
max.resid <- ceiling(max(std.resid.matrix))
min.resid <- ceiling(min(std.resid.matrix))

# The code below will produce plots of standardized residuals for selected raters as listed in raters.to.plot:
raters.to.plot <- c(1:2)

for(rater.number in raters.to.plot){

  if(rater.number < 10) r <- paste("rater_", rater.number, "_", sep = "")
  if(rater.number >= 10) r <- paste("rater_", rater.number, sep = "")

  rater.domain.labels <- NULL

  for(d in 1:nrow(domain.estimates)){
    domain_label <- domain.estimates$parameter[d]
    rater.domain.labels[d] <- paste(domain_label, "-", r, sep = "")
  }

  std.resid.subset <- subset(resids$stand_residuals, select = rater.domain.labels)

  for(domain.number in 1:n.domains){
    domain.name <- domain.estimates$parameter[domain.number]
    plot(std.resid.subset[, domain.number], ylim = c(min.resid, max.resid),
         main = paste("Standardized Residuals for Rater ", rater.number, " Domain = ", domain.name, sep = ""),
         ylab = "Standardized Residual", xlab = "Person Index")
    abline(h = 0, col = "blue")
    abline(h=2, lty = 2, col = "red")
    abline(h=-2, lty = 2, col = "red")

    legend("topright", c("Std. Residual", "Observed = Expected", "+/- 2 SD"), pch = c(1, NA, NA),
           lty = c(NA, 1, 2),
           col = c("black", "blue", "red"), cex = .8)
  }
}

## Expected and Observed Response Functions

# construct plots of expected and observed response functions
plot(writing_PC_MFRM.model, type = "expected", items = c(1:3))

## Summarize the Results in Tables
PC_MFRM_summary.table.statistics <- c("Logit Scale Location Mean",
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

PC_MFRM_student.summary.results <- rbind(mean(student.locations_PCMFR$theta),
                                         sd(student.locations_PCMFR$theta),
                                         mean(student.locations_PCMFR$se),
                                         sd(student.locations_PCMFR$se),
                                         mean(student.fit$outfitPerson),
                                         sd(student.fit$outfitPerson),
                                         mean(student.fit$infitPerson),
                                         sd(student.fit$infitPerson),
                                         mean(student.fit$outfitPerson_t),
                                         sd(student.fit$outfitPerson_t),
                                         mean(student.fit$infitPerson_t),
                                         sd(student.fit$infitPerson_t))


PC_MFRM_domain.summary.results <- rbind(mean(domain.estimates$xsi),
                                        sd(domain.estimates$xsi),
                                        mean(domain.estimates$se.xsi),
                                        sd(domain.estimates$se.xsi),
                                        mean(rater.domain.fit$Outfit),
                                        sd(rater.domain.fit$Outfit),
                                        mean(rater.domain.fit$Infit),
                                        sd(rater.domain.fit$Infit),
                                        mean(rater.domain.fit$Outfit_t),
                                        sd(rater.domain.fit$Outfit_t),
                                        mean(rater.domain.fit$Infit_t),
                                        sd(rater.domain.fit$Infit_t))

PC_MFRM_rater.summary.results <- rbind(mean(rater.estimates$xsi),
                                       sd(rater.estimates$xsi),
                                       mean(rater.estimates$se.xsi),
                                       sd(rater.estimates$se.xsi),
                                       mean(rater.domain.fit$Outfit),
                                       sd(rater.domain.fit$Outfit),
                                       mean(rater.domain.fit$Infit),
                                       sd(rater.domain.fit$Infit),
                                       mean(rater.domain.fit$Outfit_t),
                                       sd(rater.domain.fit$Outfit_t),
                                       mean(rater.domain.fit$Infit_t),
                                       sd(rater.domain.fit$Infit_t))


# Round the values for presentation in a table:
PC_MFRM_student.summary.results_rounded <- round(PC_MFRM_student.summary.results, digits = 2)

PC_MFRM_domain.summary.results_rounded <- round(PC_MFRM_domain.summary.results, digits = 2)

PC_MFRM_rater.summary.results_rounded <- round(PC_MFRM_rater.summary.results, digits = 2)


PC_MFRM_Table1 <- cbind.data.frame(PC_MFRM_summary.table.statistics,
                                   PC_MFRM_student.summary.results_rounded,
                                   PC_MFRM_domain.summary.results_rounded,
                                   PC_MFRM_rater.summary.results_rounded)

# add descriptive column labels:
names(PC_MFRM_Table1) <- c("Statistic", "Students", "Domains", "Raters")

# Print the table to the console
PC_MFRM_Table1

# Calculate the average rating for each rater:
n.raters <- nrow(rater.estimates)

Avg_Rating_rater.domains <- NULL

for(rater.number in 1:n.raters){
  rater.subset <- subset(writing, writing$rater == rater.number)
  rater.ratings <- as.vector(c(rater.subset$style,
                               rater.subset$org, rater.subset$conv,
                               rater.subset$sent_form))
  Avg_Rating_rater.domains[rater.number] <- mean(rater.ratings)
}



# Combine rater calibration results in a table:
PC_MFRM_Table2 <- cbind.data.frame(c(1:nrow(rater.estimates)),
                                   Avg_Rating_rater.domains,
                                   rater.estimates$xsi,
                                   rater.estimates$se.xsi,
                                   rater.fit_results[, -1])

names(PC_MFRM_Table2) <- c("Rater ID", "Average Rating", "Rater Location", "Rater SE",
                           names(rater.fit_results[, -1]))

# Sort Table 2 by rater severity:
PC_MFRM_Table2 <- PC_MFRM_Table2[order(-PC_MFRM_Table2$`Rater Location`),]

# Round the numeric values (all columns except the first one) to 2 digits:
PC_MFRM_Table2[, -1] <- round(PC_MFRM_Table2[,-1], digits = 2)

# Print the first six rows of the table to the console
head(PC_MFRM_Table2)

# Calculate the average rating for each domain:
Avg_Rating_domains <- colMeans(writing.resp)

# Combine domain calibration results in a table:
PC_MFRM_Table3 <- cbind.data.frame(domain.estimates$parameter,
                                   Avg_Rating_domains,
                                   domain.estimates$xsi,
                                   domain.estimates$se.xsi,
                                   domain_taus[, -1],
                                   domain.fit_results[, -1])

names(PC_MFRM_Table3) <- c("Domain", "Average Rating", "Domain Location", "Domain SE",
                           "Threshold 1", "Threshold 2", "Threshold 3",
                           names(domain.fit_results[, -1]))

# Sort Table 3 by domain difficulty:
PC_MFRM_Table3 <- PC_MFRM_Table3[order(-PC_MFRM_Table3$`Domain Location`),]

# Round the numeric values (all columns except the first one) to 2 digits:
PC_MFRM_Table3[, -1] <- round(PC_MFRM_Table3[,-1], digits = 2)

# Print the table to the console
PC_MFRM_Table3

# Calculate average ratings for students:
Person_Avg_Rating <- apply(writing.resp, 1, mean)

# Combine person calibration results in a table:
PC_MFRM_Table4 <- cbind.data.frame(rownames(student.locations_PCMFR),
                                   Person_Avg_Rating,
                                   student.locations_PCMFR$theta,
                                   student.locations_PCMFR$se,
                                   student.fit$outfitPerson,
                                   student.fit$outfitPerson_t,
                                   student.fit$infitPerson,
                                   student.fit$infitPerson_t)

names(PC_MFRM_Table4) <- c("Student ID", "Average Rating", "Student Location","Student SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Round the numeric values (all columns except the first one) to 2 digits:
PC_MFRM_Table4[, -1] <- round(PC_MFRM_Table4[,-1], digits = 2)

# Print the first six rows of the table to the console
head(PC_MFRM_Table4)

### Example Results Section
# Print Tables
knitr::kable(
  RS_MFRM_Table1 , booktabs = TRUE,
  caption = 'Model Summary Table'
)
knitr::kable(
  RS_MFRM_Table2, booktabs = TRUE,
  caption = 'Rater Calibrations'
)
knitr::kable(
  head(RS_MFRM_Table3,10), booktabs = TRUE,
  caption = 'Student Calibration'
)
knitr::kable(
  RS_MFRM_Table4, booktabs = TRUE,
  caption = 'Subgroup Calibration'
)

# Plot the Wright Map
wrightMap(thetas = cbind(student.locations_RSMFR$theta, subgroup.estimates$xsi),
          axis.persons = "Students",
          dim.names = c("Students", "Subgroups"),
          thresholds = rater_thresholds,
          show.thr.lab	= TRUE,
          label.items.rows= 2,
          label.items = rater.estimates$parameter,
          axis.items = "Raters",
          main.title = "Rating Scale Many-Facet Rasch Model \nWright Map: Style Ratings",
          cex.main = .6)

