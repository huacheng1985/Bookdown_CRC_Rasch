###--- Chapter 2 Code ---###

###### eRm analysis using CMLE #####

###--- Chapter 2 Code - eRm ---###

### Install and load the packages:

# eRm:
citation("eRm")
install.packages("eRm")
library("eRm")

### Import the data:

#* Note: Update your working directory or include the complete file path to apply the read.csv() function
transreas <- read.csv("transreas.csv")

# Explore the data using descriptive statistics
summary(transreas)

### Run the dichotomous Rasch model:

# Remove the Student ID and Grade level variables:
transreas.responses <- subset(transreas, select = -c(Student, Grade))

# Check descriptive statistics:
summary(transreas.responses)

# Run the Dichotomous Rasch Model
dichot.transreas <- RM(transreas.responses)

# Request a summary of the model results
summary(dichot.transreas)

# Plot the Wright Map
plotPImap(dichot.transreas, main = "Transitive Reasoning Assessment Wright Map", irug = FALSE)

### Examine Item Parameters:

# View individual item parameters:
difficulty <- dichot.transreas$etapar
difficulty

# Calculate the location for item 1:
n.items <- ncol(transreas.responses)
i1 <- 0 - sum(difficulty[1:(n.items - 1)])
difficulty.all <- c(i1, difficulty[c(1:(n.items - 1))])
difficulty.all

# Alternative method to calculate item difficulty (easiness * -1):
difficulty2 <- dichot.transreas$betapar * -1
difficulty2

# View standard errors for item parameters:
dichot.transreas$se.beta


# Examine descriptive statistics for item parameters:
summary(difficulty.all)
sd(difficulty.all)
summary(dichot.transreas$se.beta)
sd(dichot.transreas$se.beta)

hist(difficulty.all, main = "Histogram of Item Difficulty Estimates for the Transitive Reasoning Data",
     xlab = "Item Difficulty Estimates in Logits")

### Examine Person Parameters:
achievement <- student.locations$theta.table
achievement$id <- rownames(achievement)

# Add standard errors to person achievement object:
se <- as.data.frame(student.locations$se.theta$NAgroup1)
se$id <- rownames(se)
names(se) <- c("person_se", "id")

achievement.with.se <- merge(achievement, se, by = "id" )

# Examine descriptive statistics for person parameters:
summary(achievement)
hist(achievement$`Person Parameter`, main = "Histogram of Person Achievement Estimates \nfor the Transitive Reasoning Data",
     xlab = "Person Achievement Estimates in Logits")

# Examine item fit statistics:
student.locations <- person.parameter(dichot.transreas)
item.fit <- itemfit(student.locations)
item.fit

## Calculate reliability of item separation:
# Get Item scores
ItemScores <- colSums(transreas.responses)

# Get Item SD
ItemSD <- apply(transreas.responses,2,sd)

# Calculate the se of the Item
ItemSE <- ItemSD/sqrt(length(ItemSD))

# compute the Observed Variance (also known as Total Person Variability or Squared Standard Deviation)
SSD.ItemScores <- var(ItemScores)

# compute the Mean Square Measurement error (also known as Model Error variance)
Item.MSE <- sum((ItemSE)^2) / length(ItemSE)

# compute the Item Separation Reliability
item.separation.reliability <- (SSD.ItemScores-Item.MSE) / SSD.ItemScores
item.separation.reliability

## Examine person fit statistics:
person.fit <- personfit(student.locations)
summary(person.fit$p.infitMSQ)
summary(person.fit$p.outfitMSQ)

# Examine person reliability:
person_rel <- SepRel(student.locations)
person_rel$sep.rel

### Summarize the results in tables
summary.table.statistics <- c("Logit Scale Location Mean",
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
                              "Reliability of Separation")

item.summary.results <- rbind(mean(difficulty.all),
                              sd(difficulty.all),
                              mean(dichot.transreas$se.beta),
                              sd(dichot.transreas$se.beta),
                              mean(item.fit$i.outfitMSQ),
                              sd(item.fit$i.outfitMSQ),
                              mean(item.fit$i.infitMSQ),
                              sd(item.fit$i.infitMSQ),
                              mean(item.fit$i.outfitZ),
                              sd(item.fit$i.outfitZ),
                              mean(item.fit$i.infitMSQ),
                              sd(item.fit$i.infitZ),
                              item.separation.reliability)


person.summary.results <- rbind(mean(achievement.with.se$`Person Parameter`),
                                sd(achievement.with.se$`Person Parameter`),
                                mean(achievement.with.se$person_se, na.rm = TRUE),
                                sd(achievement.with.se$person_se, na.rm = TRUE),
                                mean(person.fit$p.outfitMSQ),
                                sd(person.fit$p.outfitMSQ),
                                mean(person.fit$p.infitMSQ),
                                sd(person.fit$p.infitMSQ),
                                mean(person.fit$p.outfitZ),
                                sd(person.fit$p.outfitZ),
                                mean(person.fit$p.infitZ),
                                sd(person.fit$p.infitZ),
                                person_rel$sep.rel)

# Round the values for presentation in a table:
item.summary.results_rounded <- round(item.summary.results, digits = 2)

person.summary.results_rounded <- round(person.summary.results, digits = 2)

Table1 <- cbind.data.frame(summary.table.statistics,
                           item.summary.results_rounded,
                           person.summary.results_rounded)

# add descriptive column labels:
names(Table1) <- c("Statistic", "Items", "Persons")

## Item calibration table:

# Calculate the proportion correct for each task:
PropCorrect <- apply(transreas.responses, 2, mean)

# Calculate the proportion correct for each task:
PropCorrect <- apply(transreas.responses, 2, mean)

# Combine item calibration results in a table:
Table2 <- cbind.data.frame(colnames(transreas.responses),
                           PropCorrect,
                           difficulty.all,
                           dichot.transreas$se.beta,
                           item.fit$i.outfitMSQ,
                           item.fit$i.outfitZ,
                           item.fit$i.infitMSQ,
                           item.fit$i.infitZ)
names(Table2) <- c("Task ID", "Proportion Correct", "Item Location","Item SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Sort Table 2 by Item difficulty:
Table2 <- Table2[order(-Table2$`Item Location`),]

# Round the numeric values (all columns except the first one) to 2 digits:
Table2[, -1] <- round(Table2[,-1], digits = 2)

## Person calibration table:

# Calculate proportion correct for persons:
PersonPropCorrect <- apply(student.locations$X.ex, 1, mean)

# Combine person calibration results in a table:
PersonPropCorrect <- apply(student.locations$X.ex, 1, mean)

# Combine person calibration results in a table:
Table3 <- cbind.data.frame(achievement.with.se$id,
                           PersonPropCorrect,
                           achievement.with.se$`Person Parameter`,
                           achievement.with.se$person_se,
                           person.fit$p.outfitMSQ,
                           person.fit$p.outfitZ,
                           person.fit$p.infitMSQ,
                           person.fit$p.infitZ)

names(Table3) <- c("Person ID", "Proportion Correct", "Person Location","Person SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Round the numeric values (all columns except the first one) to 2 digits:
Table3[, -1] <- round(Table3[,-1], digits = 2)



###### TAM analysis using MMLE #####

### Install and load the packages:

# TAM:
citation("TAM")
install.packages("TAM")
library("TAM")

# WrightMap:
citation("WrightMap")
install.packages("WrightMap")
library("WrightMap")

### Import the data:

#* Note: Update your working directory or include the complete file path to apply the read.csv() function
transreas <- read.csv("transreas.csv")

# Explore the data using descriptive statistics
summary(transreas)

### Run the dichotomous Rasch model:

# Remove the Student ID and Grade level variables:
transreas.responses <- subset(transreas, select = -c(Student, Grade))

# Check descriptive statistics:
summary(transreas.responses)

# Run the Dichotomous Rasch Model
dichot.transreas_MMLE <- tam.mml(transreas.responses, constraint = "items")

# Request a summary of the model results
summary(dichot.transreas_MMLE)

# Plot the Wright Map
IRT.WrightMap(dichot.transreas_MMLE, show.thr.lab=FALSE, main.title = "Transitive Reasoning Wright Map (MMLE)")

### Examine Item Parameters:

# View individual item parameters:
difficulty_MMLE <- as.data.frame(dichot.transreas_MMLE$item_irt$beta)
difficulty_MMLE$se <- dichot.transreas_MMLE$se.AXsi
names(difficulty_MMLE) <- c("item_difficulty", "item_se")

# Examine descriptive statistics for item parameters:
summary(difficulty_MMLE)
sd(difficulty_MMLE$item_difficulty)
sd(difficulty_MMLE$item_se)

hist(difficulty_MMLE$item_difficulty, main = "Histogram of Item Difficulty Estimates for the Transitive Reasoning Data \n (MMLE)",
     xlab = "Item Difficulty Estimates in Logits")

# Examine item fit statistics:
item.fit_MMLE <- tam.fit(dichot.transreas_MMLE)
item.fit_MMLE <- as.data.frame(item.fit_MMLE$itemfit)
summary(item.fit_MMLE)

# Fit statistics for all items:
dichot.transreas_MMLE2 <- tam(transreas.responses)
item.fit_MMLE2 <- tam.fit(dichot.transreas_MMLE2)
item.fit_MMLE2 <- as.data.frame(item.fit_MMLE2$itemfit)

### Examine Person Parameters:
achievement_MMLE <- as.data.frame(tam.wle(dichot.transreas_MMLE))

# Examine descriptive statistics for person parameters:
hist(achievement_MMLE$theta, main = "Histogram of Person Achievement Estimates \nfor the Transitive Reasoning Data \n (MMLE)",
     xlab = "Person Achievement Estimates in Logits", cex.main = .8)

# Examine person fit statistics:
person.fit_MMLE <- tam.personfit(dichot.transreas_MMLE)
summary(person.fit_MMLE)


######### TAM Analysis using JMLE ###########

## Estimate the Dichotomous Rasch Model using JMLE
dichot.transreas_JMLE <- tam.jml(transreas.responses, constraint = "items")

# Request a summary of the model results
summary(dichot.transreas_JMLE)

# Plot a Wright Map for the JMLE results:
difficulty_JMLE <- dichot.transreas_JMLE$item1$AXsi_.Cat1
theta_JMLE <- dichot.transreas_JMLE$theta
wrightMap(thetas = theta_JMLE, thresholds = difficulty_JMLE, show.thr.lab=FALSE, main.title = "Transitive Reasoning Wright Map: JMLE")

## JMLE Item Parameters:

# item standard errors:
jmle.item.se <- dichot.transreas_JMLE$errorP
jmle.item.se

### JMLE Person Parameters:

# person standard errors:
jmle.person.se <- dichot.transreas_JMLE$errorWLE

### JMLE Fit Statistics:

jmle.fit <- tam.jml.fit(dichot.transreas_JMLE)

## Examine item fit statistics:
jmle.item.fit <- as.data.frame(jmle.fit$fit.item)
jmle.item.fit
summary(jmle.item.fit)

## Examine person fit statistics:
jmle.person.fit <- as.data.frame(jmle.fit$fit.person)
summary(jmle.person.fit)
