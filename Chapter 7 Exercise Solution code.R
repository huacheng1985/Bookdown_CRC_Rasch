###--- Chapter 7 Exercise Solution Code ---###

### Install and load the packages:

# install.packages("eRm")
library("eRm")

# install.packages("eRm")
library("TAM")

#################------ Exercise 7A ------###############

### Import the data:

#* Note: Update your working directory or include the complete file path to apply the read.csv() function]
mc <- read.csv("Exercise_7A_multiple_choice.csv")

# identify the variable that contains examinees' subgroup membership identifiers for our DIF analysis
mc.group <- mc$subgroup

# generate a frequency table for this variable
table(mc.group)

## DIF Method 1: Compare Item Locations between Subgroups

# Isolate the response matrix:
mc.responses <- subset(mc, select = -c(student, subgroup))

# Apply the model to the data
mc.RM <- RM(mc.responses)

# calculate subgroup-specific item difficulty values
subgroup_diffs <- Waldtest(mc.RM, splitcr = mc.group)

# create new objects with the group-specific item difficulties
subgroup_1_diffs <- subgroup_diffs$betapar1
subgroup_2_diffs <- subgroup_diffs$betapar2

# examine the differences between the subgroup-specific item difficulties by subtracting the two sets of values
subgroup_1_diffs - subgroup_2_diffs

# create an object called `comparisons` in which we store these results
comparisons <- as.data.frame(subgroup_diffs$coef.table)

# construct a table that includes subgroup-specific item difficulty values along with the results from the statistical hypothesis test
comparison.results <- cbind.data.frame(subgroup_1_diffs, subgroup_diffs$se.beta1,
                                       subgroup_2_diffs, subgroup_diffs$se.beta2,
                                       comparisons)
# Name the columns of the results
names(comparison.results) <- c("Subgroup_1_Difficulty", "Subgroup_1_SE",
                               "Subgroup_2_Difficulty", "Subgroup_2_SE",
                               "Z", "p_value")


## Calculate values for constructing the confidence bands:

mean.1.2 <- ((subgroup_1_diffs - mean(subgroup_1_diffs))/2*sd(subgroup_1_diffs) +
               (subgroup_2_diffs - mean(subgroup_2_diffs))/2*sd(subgroup_2_diffs))

joint.se <- sqrt((subgroup_diffs$se.beta1^2/sd(subgroup_1_diffs)) +
                   (subgroup_diffs$se.beta2^2/sd(subgroup_2_diffs)))

upper.group.1 <- mean(subgroup_1_diffs) + ((mean.1.2 - joint.se )*sd(subgroup_1_diffs))
upper.group.2 <- mean(subgroup_2_diffs) + ((mean.1.2 + joint.se )*sd(subgroup_2_diffs))

lower.group.1 <- mean(subgroup_1_diffs) + ((mean.1.2 + joint.se )*sd(subgroup_1_diffs))
lower.group.2 <- mean(subgroup_2_diffs) + ((mean.1.2 - joint.se )*sd(subgroup_2_diffs))

upper <- cbind.data.frame(upper.group.1, upper.group.2)
upper <- upper[order(upper$upper.group.1, decreasing = FALSE),]

lower <- cbind.data.frame(lower.group.1, lower.group.2)
lower <- lower[order(lower$lower.group.1, decreasing = FALSE),]

## Construct the scatterplot:

plot(subgroup_1_diffs, subgroup_2_diffs, xlim = c(-4, 4), ylim = c(-4, 4),
     xlab = "Lower Elementary", ylab = "Upper Elementary", main = "Lower Elementary Measures plotted against Upper Elementary Measures", cex.main = .8)
abline(a = 0, b = 1, col = "purple")

par(new = T)

lines(upper$upper.group.1, upper$upper.group.2, lty = 2, col = "red")

lines(lower$lower.group.1, lower$lower.group.2, lty = 2, col = "red")

legend("bottomright", c("Item Location", "Identity Line", "95% Confidence Band"),
       pch = c(1, NA, NA), lty = c(NA, 1, 2), col = c("black", "purple", "red"), cex = .8)

## DIF Method 2: Interaction Analysis

# Specify the facets in the model (grade-level subgroup) as a data.frame object
facets <- as.data.frame(mc.group)
facets <- facets[,"mc.group", drop=FALSE]

# Identify the object of measurement (students)
students <- mc$student

# Identify the response matrix:
ratings <- mc.responses

# Specify the model equation with the interaction term:
mc_MFRM_equation <- ~ item + mc.group + (item*mc.group)

# Apply the model to the responses:
mc_MFRM <- tam.mml.mfr(resp = ratings, facets = facets, formulaA = mc_MFRM_equation, pid = students, constraint = "items", verbose = FALSE)

# store all of the facet estimates in an object called `facet.estimates`
facet.estimates <- mc_MFRM$xsi.facets
item.estimates <- subset(facet.estimates, facet.estimates$facet == "item")
subgroup.estimates <- subset(facet.estimates, facet.estimates$facet == "mc.group")
interaction.estimates <- subset(facet.estimates, facet.estimates$facet == "item:mc.group")

## examine the relative magnitude of each interaction term by plotting the values in a simple scatterplot

# first twenty values show the interaction between each item and group 1
plot(interaction.estimates$xsi[1:20], main = "Interaction Effects for Group 1", ylab = "Interaction Estimate in Logits", xlab = "Item Number", ylim = c(-0.5, 0.5))

# second twenty values show the interaction between each item and group 2
plot(interaction.estimates$xsi[21:40], main = "Interaction Effects for Group 2", ylab = "Interaction Estimate in Logits", xlab = "Item Number", ylim = c(-0.5, 0.5))





#################------ Exercise 7B ------###############

## Import the data
survey.ratings <- read.csv("Exercise_7B_ratings.csv")

# isolate the response matrix and identify the subgroup variable
ratings <- subset(survey.ratings, select = -c(person, subgroup))

## DIF Method 1: Compare Item Locations between Subgroups

# Use the Partial Credit Model for our DIF analyses
PC_model <- PCM(ratings)

# identify subgroup classification variable
subgroups <- survey.ratings$subgroup

# calculate subgroup-specific item difficulty estimates
group1_item.diffs.overall <- NULL
group2_item.diffs.overall <- NULL

responses.g <- cbind.data.frame(subgroups, ratings)
responses.g1 <- subset(responses.g, responses.g$subgroups == 1)
responses.g2 <- subset(responses.g, responses.g$subgroups == 2)

subgroup_diffs <- Waldtest(PC_model, splitcr = subgroups)

for(item.number in 1:ncol(ratings)){

  n.thresholds.g1 <-  length(table(responses.g1[, item.number+1]))-1

  group1_item.diffs.overall[item.number] <- mean(subgroup_diffs$betapar1[((item.number*(n.thresholds.g1))-(n.thresholds.g1-1)):
                                                                           (item.number*(n.thresholds.g1))])*-1

  n.thresholds.g2 <-  length(table(responses.g2[, item.number+1]))-1

  group2_item.diffs.overall[item.number] <- mean(subgroup_diffs$betapar2[((item.number*(n.thresholds.g2))-(n.thresholds.g2-1)):
                                                                           (item.number*(n.thresholds.g2))])*-1
}

# view the group-specific item difficulty estimates
group1_item.diffs.overall
group2_item.diffs.overall

# calculate overall standard errors for each item
group1_item.se.overall <- NULL
group2_item.se.overall <- NULL

responses.g <- cbind.data.frame(subgroups, ratings)
responses.g1 <- subset(responses.g, responses.g$subgroups == 1)
responses.g2 <- subset(responses.g, responses.g$subgroups == 2)

subgroup_diffs <- Waldtest(PC_model, splitcr = subgroups)


for(item.number in 1:ncol(ratings)){

  n.thresholds.g1 <-  length(table(responses.g1[, item.number+1]))-1

  group1_item.se.overall[item.number] <- mean(subgroup_diffs$se.beta1[((item.number*(n.thresholds.g1))-(n.thresholds.g1-1)):
                                                                        (item.number*(n.thresholds.g1))])

  n.thresholds.g2 <-  length(table(responses.g2[, item.number+1]))-1

  group2_item.se.overall[item.number] <- mean(subgroup_diffs$se.beta2[((item.number*(n.thresholds.g2))-(n.thresholds.g2-1)):
                                                                        (item.number*(n.thresholds.g2))])
}

# calculate test statistics for the differences in overall item difficulties
z <- (group1_item.diffs.overall - group2_item.diffs.overall)/
  sqrt(group1_item.se.overall^2 + group2_item.se.overall^2)
z


### First, calculate values for constructing the confidence bands:

mean.1.2 <- ((group1_item.diffs.overall - mean(group1_item.diffs.overall))/2*sd(group1_item.diffs.overall) +
               (group2_item.diffs.overall - mean(group2_item.diffs.overall))/2*sd(group2_item.diffs.overall))

joint.se <- sqrt((group1_item.se.overall^2/sd(group1_item.diffs.overall)) +
                   (group2_item.se.overall^2/sd(group2_item.diffs.overall)))


upper.group.1 <- mean(group1_item.diffs.overall) + ((mean.1.2 - joint.se )*sd(group1_item.diffs.overall))
upper.group.2 <- mean(group2_item.diffs.overall) + ((mean.1.2 + joint.se )*sd(group2_item.diffs.overall))

lower.group.1 <- mean(group1_item.diffs.overall) + ((mean.1.2 + joint.se )*sd(group1_item.diffs.overall))
lower.group.2 <- mean(group2_item.diffs.overall) + ((mean.1.2 - joint.se )*sd(group1_item.diffs.overall))


upper <- cbind.data.frame(upper.group.1, upper.group.2)
upper <- upper[order(upper$upper.group.1, decreasing = FALSE),]


lower <- cbind.data.frame(lower.group.1, lower.group.2)
lower <- lower[order(lower$lower.group.1, decreasing = FALSE),]

## make the scatterplot:

plot(group1_item.diffs.overall, group2_item.diffs.overall, xlim = c(-4, 4), ylim = c(-4, 4),
     xlab = "Group 1", ylab = "English", main = "Group 1 Measures \n plotted against Group 2 Measures")
abline(a = 0, b = 1, col = "purple")

par(new = T)

lines(upper$upper.group.1, upper$upper.group.2, lty = 2, col = "red")

lines(lower$lower.group.1, lower$lower.group.2, lty = 2, col = "red")

legend("bottomright", c("Item Location", "Identity Line", "95% Confidence Band"),
       pch = c(1, NA, NA), lty = c(NA, 1, 2), col = c("black", "purple", "red"))


## DIF Method 2: Interaction Analysis

## Specify the facets in the model (subgroup) as a data.frame object
facets <- survey.ratings[,"subgroup", drop=FALSE]

## Identify the object of measurement (persons)
persons <- survey.ratings$person

## Identify the response matrix:
ratings <- survey.ratings[, -c(1:2)]

## Specify the model equation with the interaction term:
survey.ratings_PC_MFRM_equation <- ~ item + subgroup + item:step + (item*subgroup)

## Apply the model to the responses:
survey.ratings_MFRM <- tam.mml.mfr(resp = ratings, facets = facets, formulaA = survey.ratings_PC_MFRM_equation, pid = persons, constraint = "items", verbose = FALSE)

# store all of the facet estimates
facet.estimates <- survey.ratings_MFRM$xsi.facets

item.estimates <- subset(facet.estimates, facet.estimates$facet == "item")

subgroup.estimates <- subset(facet.estimates, facet.estimates$facet == "subgroup")

threshold.estimates <- subset(facet.estimates, facet.estimates$facet == "item:step")

interaction.estimates <- subset(facet.estimates, facet.estimates$facet == "item:subgroup")

## examine the relative magnitude of each interaction term by plotting the values in a simple scatterplot

# first 25 values show the interaction between each item and group 1
plot(interaction.estimates$xsi[1:25], main = "Interaction Effects for Group 1", ylab = "Interaction Estimate in Logits", xlab = "Item Number", ylim = c(-0.5, 0.5))

# second 25 values show the interaction between each item and group 2
plot(interaction.estimates$xsi[26:50], main = "Interaction Effects for Group 2", ylab = "Interaction Estimate in Logits", xlab = "Item Number", ylim = c(-0.5, 0.5))
