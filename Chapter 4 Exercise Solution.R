# Chapter 4 Exercise solution

## load eRm package:
citation("eRm")
# install.packages("eRm")
#install.packages("eRm")
library("eRm")

## input the data:
survey <- read.csv("survey_ratings.csv")
summary(survey)


## Remove the Student ID variable:
survey.responses <- subset(survey, select = -person)

## Check descriptive statistics:
summary(survey.responses)

## Run the Rating Scale Model:
RSM.survey <- RSM(survey.responses, se = TRUE)
summary(RSM.survey)

## Plot Wright Map:
plotPImap(RSM.survey, main = "Rating Scale Model Wright Map")

## Examine item parameters:
item.locations <- RSM.survey$etapar
item.locations
summary(item.locations)

### Extract item parameters only (no thresholds):
n.items <- ncol(survey.responses)
i1 <- 0 - sum(item.locations[1:(n.items - 1)])
item.locations.all <- c(i1, item.locations[c(1:(n.items - 1))])
item.locations.all

### Apply thresholds() function to the model object in order to obtain item locations (not centered at zero logits):
items.and.taus <- thresholds(RSM.survey)
items.and.taus.table <- as.data.frame(items.and.taus$threshtable)
uncentered.item.locations <- items.and.taus.table$X1.Location

#### set the mean of the item locations to zero logits:
centered.item.locations <- scale(uncentered.item.locations, scale = FALSE)
summary(centered.item.locations)


## Examine threshold estimates

### Specify the number of thresholds as the maximum observed score in the response matrix (be sure the responses begin at category 0):
n.thresholds <- max(survey.responses)

### Calculate adjacent-category threshold values:
tau.estimates <- NULL

for(tau in 1:n.thresholds){
  tau.estimates[tau] <- (items.and.taus.table[, (1+tau)] - items.and.taus.table[,1])[1]
}

## Find item standard errors:
### SE for items + thresholds:
delta.tau.se <- items.and.taus$se.thresh
summary(delta.tau.se)

### SE for overall item:
delta.se <- RSM.survey$se.eta
summary(delta.se)

## Plot item response functions (rating scale category probability plots):
plotICC(RSM.survey, ask = FALSE)

# Calculate person parameters:
person.locations.estimate <- person.parameter(RSM.survey)

# Store person parameters and their standard errors in a dataframe object:
person.locations <- cbind.data.frame(person.locations.estimate$thetapar,
                                     person.locations.estimate$se.theta)
names(person.locations) <- c("theta", "SE")

# View summary statistics for person parameters:
summary(person.locations)

## Examine item fit statistics

item.fit.results <- itemfit(person.locations.estimate)
item.fit <- cbind.data.frame(item.fit.results$i.infitMSQ,
                             item.fit.results$i.outfitMSQ,
                             item.fit.results$i.infitZ,
                             item.fit.results$i.outfitZ)

names(item.fit) <- c("infit_MSE", "outfit_MSE", "std_infit", "std_outfit")

summary(item.fit)

### Examine person fit statistics

person.fit.results <- personfit(person.locations.estimate)
person.fit <- cbind.data.frame(person.fit.results$p.infitMSQ,
                               person.fit.results$p.outfitMSQ,
                               person.fit.results$p.infitZ,
                               person.fit.results$p.outfitZ)
names(person.fit) <- c("infit_MSE", "outfit_MSE", "std_infit", "std_outfit")

summary(person.fit)

# Create model summary table
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
                                  "Std. Infit SD")

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
                                  sd(item.fit.results$i.infitZ))

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
                                    sd(person.fit$std_infit))

# Round the values for presentation in a table:
RSM_item.summary.results_rounded <- round(RSM_item.summary.results, digits = 2)

RSM_person.summary.results_rounded <- round(RSM_person.summary.results, digits = 2)

RSM_Table1 <- cbind.data.frame(RSM_summary.table.statistics,
                               RSM_item.summary.results_rounded, 
                               RSM_person.summary.results_rounded)

# add descriptive column labels:
names(RSM_Table1) <- c("Statistic", "Items", "Persons")  



