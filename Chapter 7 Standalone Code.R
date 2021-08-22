###--- Chapter 7 Code ---###

### Install and load the packages:

# install.packages("eRm")
library("eRm")

# install.packages("eRm")
library("TAM")

## DIF between two groups of examinees for a dichotomous item

# DIF illustration
IRF <- function(b_group1, b_group2){
  colors <- c("purple", "seagreen3")
  theta <- seq(-3, 3, .1)
  P1 <- 1 / (1 + exp(-(theta - b_group1)))
  P2 <- 1 / (1 + exp(-(theta - b_group2)))
  plot(theta, P1, type="l", xlim=c(-3,3), ylim=c(0,1),
       xlab="Theta", ylab="P(X=1)", col = colors[1], lty = 1,
       main = "Item i")
  lines(theta, P2, col = colors[2], lty = 2)
  legend("bottomright", c("Group 1", "Group 2"), lty = c(1, 2),
         col = c(colors[1], colors[2]))
}
IRF(b_group1 = 1, b_group2 = -1)

### Import the data:

#* Note: Update your working directory or include the complete file path to apply the read.csv() function]
transreas <- read.csv("transreas.csv")

# identify the variable that contains examinees' subgroup membership identifiers for our DIF analysis
transreas.grade.group <- ifelse(transreas$Grade <= 4, 1, 2)
# generate a frequency table for this variable
table(transreas.grade.group)

## DIF Method 1: Compare Item Locations between Subgroups

# Isolate the response matrix:
transreas.responses <- subset(transreas, select = -c(Student, Grade))

# Apply the model to the data
transreas.RM <- RM(transreas.responses)

# calculate subgroup-specific item difficulty values
subgroup_diffs <- Waldtest(transreas.RM, splitcr = transreas.grade.group)

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
comparison.results

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
facets <- as.data.frame(transreas.grade.group)
facets <- facets[,"transreas.grade.group", drop=FALSE]

# Identify the object of measurement (students)
students <- transreas$Student

# Identify the response matrix:
ratings <- transreas.responses

# Specify the model equation with the interaction term:
transreas_MFRM_equation <- ~ item + transreas.grade.group + (item*transreas.grade.group)

# Apply the model to the responses:
transreas_MFRM <- tam.mml.mfr(resp = ratings, facets = facets, formulaA = transreas_MFRM_equation, pid = students, constraint = "items", verbose = FALSE)

# store all of the facet estimates in an object called `facet.estimates`
facet.estimates <- transreas_MFRM$xsi.facets
item.estimates <- subset(facet.estimates, facet.estimates$facet == "item")
subgroup.estimates <- subset(facet.estimates, facet.estimates$facet == "transreas.grade.group")
interaction.estimates <- subset(facet.estimates, facet.estimates$facet == "item:transreas.grade.group")

## examine the relative magnitude of each interaction term by plotting the values in a simple scatterplot

# first ten values show the interaction between each item and group 1 (Lower Elementary)
plot(interaction.estimates$xsi[1:10], main = "Interaction Effects for Lower Elementary Students", ylab = "Interaction Estimate in Logits", xlab = "Item Number", ylim = c(-0.5, 0.5))

# second ten values show the interaction between each item and group 2 (Upper Elementary)
plot(interaction.estimates$xsi[11:20], main = "Interaction Effects for Upper Elementary Students", ylab = "Interaction Estimate in Logits", xlab = "Item Number", ylim = c(-0.5, 0.5))

## Detecting Differential Item Functioning in R for Polytomous Items

### Example Data: Style Ratings

## Import the data
style <- read.csv("style_ratings.csv")

# isolate the response matrix and identify the subgroup variable (language)
ratings <- subset(style, select = -c(student, language))

## DIF Method 1: Compare Item Locations between Subgroups

# Use the Partial Credit Model for our DIF analyses
PC_model <- PCM(ratings)

# identify subgroup classification variable
subgroups <- style$language

# calculate subgroup-specific item difficulty (rater severity) estimates
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

# view the group-specific item difficulty (rater severity) estimates
group1_item.diffs.overall
group2_item.diffs.overall

# calculate overall standard errors for each item (rater)
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
     xlab = "Language Other than English", ylab = "English", main = "Language-other-than-English Measures \n plotted against English Measures")
abline(a = 0, b = 1, col = "purple")

par(new = T)

lines(upper$upper.group.1, upper$upper.group.2, lty = 2, col = "red")

lines(lower$lower.group.1, lower$lower.group.2, lty = 2, col = "red")

legend("bottomright", c("Item Location", "Identity Line", "95% Confidence Band"),
       pch = c(1, NA, NA), lty = c(NA, 1, 2), col = c("black", "purple", "red"))


## DIF Method 2: Interaction Analysis

## Specify the facets in the model (language subgroup) as a data.frame object
facets <- style[,"language", drop=FALSE]

## Identify the object of measurement (students)
students <- style$student

## Identify the response matrix:
ratings <- style[, -c(1:2)]

## Specify the model equation with the interaction term:
style_PC_MFRM_equation <- ~ item + language + item:step + (item*language)

## Apply the model to the responses:
style_MFRM <- tam.mml.mfr(resp = ratings, facets = facets, formulaA = style_PC_MFRM_equation, pid = students, constraint = "items", verbose = FALSE)

# store all of the facet estimates
facet.estimates <- style_MFRM$xsi.facets

item.estimates <- subset(facet.estimates, facet.estimates$facet == "item")

subgroup.estimates <- subset(facet.estimates, facet.estimates$facet == "language")

threshold.estimates <- subset(facet.estimates, facet.estimates$facet == "item:step")

interaction.estimates <- subset(facet.estimates, facet.estimates$facet == "item:language")

## examine the relative magnitude of each interaction term by plotting the values in a simple scatterplot

# first 21 values show the interaction between each rater and group 1 (language other than English)
plot(interaction.estimates$xsi[1:21], main = "Interaction Effects for Language Other than English", ylab = "Interaction Estimate in Logits", xlab = "Item (Rater) Number", ylim = c(-0.5, 0.5))

# second 21 values show the interaction between each rater and group 2 (English)
plot(interaction.estimates$xsi[22:42], main = "Interaction Effects for English", ylab = "Interaction Estimate in Logits", xlab = "Item (Rater) Number", ylim = c(-0.5, 0.5))




