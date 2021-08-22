###--- Chapter 2 Exercise Solution Code ---###

### Install and load the packages:

# TAM:
citation("TAM")
install.packages("TAM")
library("TAM")

# WrightMap:
citation("WrightMap")
install.packages("WrightMap")
library("WrightMap") 

### Import the mathematics assessment data:

#* Note: Update your working directory or include the complete file path to apply the read.csv() function
math_data <- read.csv("exercise_2.csv")

# Explore the data using descriptive statistics
summary(math_data)

### Run the dichotomous Rasch model:

# Remove the ID variable:
math_data.responses <- subset(math_data, select = -c(ids))

# Check descriptive statistics:
summary(math_data.responses)

# Run the Dichotomous Rasch Model
dichot.math_data <- tam(math_data.responses)

# Request a summary of the model results
summary(dichot.math_data)

# Plot the Wright Map 
IRT.WrightMap(dichot.math_data, show.thr.lab=FALSE, main.title = "Mathematics Assessment Wright Map")

### Examine Item Parameters:

# View individual item parameters:
difficulty <- dichot.math_data$xsi
difficulty

# Examine descriptive statistics for item parameters:
summary(difficulty)
sd(difficulty$xsi)
sd(difficulty$se.xsi)

hist(difficulty$xsi, main = "Histogram of Item Difficulty Estimates for the Chapter 2 Mathematics Assessment Data",
     xlab = "Item Difficulty Estimates in Logits") 

# Examine item fit statistics:
item.fit <- tam.fit(dichot.math_data) 
item.fit <- as.data.frame(item.fit$itemfit)
summary(item.fit)

### Examine Person Parameters:
achievement <- as.data.frame(tam.wle(dichot.math_data))

# Examine descriptive statistics for person parameters:
summary(achievement)
hist(achievement$theta, main = "Histogram of Person Achievement Estimates \nfor the Chapter 2 Mathematics Assessment Data",
     xlab = "Person Achievement Estimates in Logits") 

# Examine person fit statistics:
person.fit <- tam.personfit(dichot.math_data) 
summary(person.fit)

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

item.summary.results <- rbind(mean(difficulty$xsi),
                              sd(difficulty$xsi),
                              mean(difficulty$se.xsi),
                              sd(difficulty$se.xsi),
                              mean(item.fit$Outfit),
                              sd(item.fit$Outfit),
                              mean(item.fit$Infit),
                              sd(item.fit$Infit),
                              mean(item.fit$Outfit_t),
                              sd(item.fit$Outfit_t),
                              mean(item.fit$Infit_t),
                              sd(item.fit$Infit_t),
                              dichot.math_data$EAP.rel)  

person.summary.results <- rbind(mean(achievement$theta),
                                sd(achievement$theta),
                                mean(achievement$error),
                                sd(achievement$error),
                                mean(person.fit$outfitPerson),
                                sd(person.fit$outfitPerson),
                                mean(person.fit$infitPerson),
                                sd(person.fit$infitPerson),
                                mean(person.fit$outfitPerson_t),
                                sd(person.fit$outfitPerson_t),
                                mean(person.fit$infitPerson_t),
                                sd(person.fit$infitPerson_t),
                                mean(achievement$WLE.rel))

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
TaskCorrect <- apply(math_data.responses, 2, sum)
PropCorrect <- (TaskCorrect/nrow(math_data.responses))

# Combine item calibration results in a table:
Table2 <- cbind.data.frame(item.fit$parameter, 
                           PropCorrect,
                           difficulty$xsi,
                           difficulty$se.xsi,
                           item.fit$Outfit,
                           item.fit$Outfit_t,
                           item.fit$Infit,
                           item.fit$Infit_t)
names(Table2) <- c("Task ID", "Proportion Correct", "Item Location","Item SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")                           

# Sort Table 2 by Item difficulty:
Table2 <- Table2[order(-Table2$`Item Location`),]

# Round the numeric values (all columns except the first one) to 2 digits:
Table2[, -1] <- round(Table2[,-1], digits = 2)

## Person calibration table:

# Calculate proportion correct for persons:
PersonPropCorrect <- achievement$PersonScores / achievement$PersonMax

# Combine person calibration results in a table:
Table3 <- cbind.data.frame(achievement$pid,
                           PersonPropCorrect,
                           achievement$theta,
                           achievement$error,
                           person.fit$outfitPerson,
                           person.fit$outfitPerson_t,
                           person.fit$infitPerson,
                           person.fit$infitPerson_t)

names(Table3) <- c("Person ID", "Proportion Correct", "Person Location","Person SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Round the numeric values (all columns except the first one) to 2 digits:
Table3[, -1] <- round(Table3[,-1], digits = 2)


### Run the Dichotomous Rasch Model with Joint Maximum Likelihood Estimation

## Estimate the Dichotomous Rasch Model using JMLE
jmle.dichot.math_data <- tam.jml(math_data.responses)

# Request a summary of the model results
summary(jmle.dichot.math_data)

# Plot a Wright Map for the JMLE results:
jmle.difficulty <- jmle.dichot.math_data$xsi
theta <- jmle.dichot.math_data$theta
wrightMap(thetas = theta, thresholds = jmle.difficulty, show.thr.lab=FALSE, main.title = "Chapter 2 Mathematics Assessment Data Wright Map: JMLE")

### JMLE Item Parameters:

# item standard errors:
jmle.item.se <- jmle.dichot.math_data$errorP


# save item parameters in a data frame object:
jmle.item.estimates <- cbind.data.frame(c(1:35),
                                        jmle.difficulty,
                                        jmle.item.se)
names(jmle.item.estimates) <- c("Item ID", "Item Location", "Item Location SE")

# examine item parameters in more detail:
#View(jmle.item.estimates)
summary(jmle.item.estimates)
sd(jmle.item.estimates$`Item Location`)
hist(jmle.item.estimates$`Item Location`, main = "Histogram of Item Location Estimates \nfor the Chapter 2 Mathematics Assessment Data: JMLE", xlab = "Item Location Estimates in Logits") 

### JMLE Person Parameters:

# person standard errors:
jmle.person.se <- jmle.dichot.math_data$errorWLE

# save person parameters in a data frame object:

jmle.person.estimates <- cbind.data.frame(math_data$ids, theta, jmle.person.se)
names(jmle.person.estimates) <- c("Person ID", "Theta", "Theta SE")

# examine person parameters in more detail:
#View(jmle.person.estimates)
summary(jmle.person.estimates)
sd(jmle.person.estimates$Theta)
hist(jmle.person.estimates$Theta, main = "Histogram of Person Location Estimates \nfor the Chapter 2 Mathematics Assessment Data: JMLE", xlab = "Person Location Estimates in Logits") 

### JMLE Fit Statistics:

jmle.fit <- tam.jml.fit(jmle.dichot.math_data) 

## Examine item fit statistics:
jmle.item.fit <- as.data.frame(jmle.fit$fit.item)
jmle.item.fit
#View(jmle.item.fit)
summary(jmle.item.fit)

## Examine person fit statistics:
jmle.person.fit <- as.data.frame(jmle.fit$fit.person)
#View(jmle.person.fit)
summary(jmle.person.fit)

### Summarize the results from the JMLE dichotomous Rasch model analysis in tables
## Model summary table:

jmle.summary.table.statistics <- c("Logit Scale Location Mean",
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

jmle.item.summary.results <- rbind(mean(jmle.item.estimates$`Item Location`),
                                   sd(jmle.item.estimates$`Item Location`),
                                   mean(jmle.item.estimates$`Item Location SE`),
                                   sd(jmle.item.estimates$`Item Location SE`),
                                   mean(jmle.item.fit$outfitItem),
                                   sd(jmle.item.fit$outfitItem),
                                   mean(jmle.item.fit$infitItem),
                                   sd(jmle.item.fit$infitItem),
                                   mean(jmle.item.fit$outfitItem_t),
                                   sd(jmle.item.fit$outfitItem_t),
                                   mean(jmle.item.fit$infitItem_t),
                                   sd(jmle.item.fit$infitItem_t),
                                   jmle.dichot.math_data$WLEreliability)

#*#*# Item reliability???

jmle.person.summary.results <- rbind(mean(jmle.person.estimates$Theta),
                                     sd(jmle.person.estimates$Theta),
                                     mean(jmle.person.estimates$`Theta SE`),
                                     sd(jmle.person.estimates$`Theta SE`),
                                     mean(jmle.person.fit$outfitPerson),
                                     sd(jmle.person.fit$outfitPerson),
                                     mean(jmle.person.fit$infitPerson),
                                     sd(jmle.person.fit$infitPerson),
                                     mean(jmle.person.fit$outfitPerson_t),
                                     sd(jmle.person.fit$outfitPerson_t),
                                     mean(jmle.person.fit$infitPerson_t),
                                     sd(jmle.person.fit$infitPerson_t),
                                     mean(achievement$WLE.rel))

# Round the values for presentation in a table:
jmle.item.summary.results_rounded <- round(jmle.item.summary.results, digits = 2)

jmle.person.summary.results_rounded <- round(jmle.person.summary.results, digits = 2)

jmle.Table1 <- cbind.data.frame(jmle.summary.table.statistics,
                                jmle.item.summary.results_rounded, 
                                jmle.person.summary.results_rounded)

# Add descriptive column labels:
names(jmle.Table1) <- c("Statistic", "Items", "Persons")


## JMLE item calibration table:

# Calculate the proportion correct for each task:

TaskCorrect <- apply(math_data.responses, 2, sum)
PropCorrect <- (TaskCorrect/nrow(math_data.responses))

jmle.Table2 <- cbind.data.frame(jmle.item.fit$item, 
                                PropCorrect,
                                jmle.difficulty,
                                jmle.item.se,
                                item.fit$Outfit,
                                item.fit$Outfit_t,
                                item.fit$Infit,
                                item.fit$Infit_t)
names(jmle.Table2) <- c("Task ID", "Proportion Correct", "Item Location","Item SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")                           

# Sort the table by Item difficulty:
jmle.Table2 <- jmle.Table2[order(-jmle.Table2$`Item Location`),]

# Round the numeric values (all columns except the first one) for presentation in a table:
jmle.Table2[, -1] <- round(jmle.Table2[, -1], digits = 2)

## Person calibration table:

# calculate proportion correct for persons:
PersonPropCorrect <- achievement$PersonScores / achievement$PersonMax

jmle.Table3 <- cbind.data.frame(jmle.person.estimates$`Person ID`,
                                PersonPropCorrect,
                                jmle.person.estimates$Theta,
                                jmle.person.estimates$`Theta SE`,
                                jmle.person.fit$outfitPerson,
                                jmle.person.fit$outfitPerson_t,
                                jmle.person.fit$infitPerson,
                                jmle.person.fit$infitPerson_t)

names(jmle.Table3) <- c("Person ID", "Proportion Correct", "Person Location","Person SE","Outfit MSE","Std. Outfit", "Infit MSE","Std. Infit")

# Round the numeric values (all columns except the first one) for presentation in a table:
jmle.Table3[, -1] <- round(jmle.Table3[, -1], digits = 2)


### Compare the estimated parameters between JMLE and MMLE methods

## Compare item estimates:

plot(difficulty$xsi, jmle.difficulty,
     pch=16,
     xlab= "MMLE Estimate",
     ylab= "JMLE Estimate",
     main="Item Parameter Estimate Comparison")

cor(difficulty$xsi, jmle.difficulty)

## Compare person estimates:

plot(achievement$theta, jmle.person.estimates$Theta,
     pch=16,
     xlab= "MMLE Estimate",
     ylab= "JMLE Estimate",
     main="Person Parameter Estimate Comparison")

cor(achievement$theta, jmle.person.estimates$Theta)

