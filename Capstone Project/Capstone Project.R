library(dplyr)
library(tidyr)
library(caTools)
library(ROCR)

# Read in data and rename misspelled/inappropriately named columns. Remove negative from WaitTime
noshow <- read.csv("No-show-Issue-Comma-300k.csv")
noshow <- rename(noshow, Alcoholism = Alcoolism, Hypertension = HiperTension,
                 Disabled = Handcap, WaitTime = AwaitingTime, AppointmentDate = ApointmentData) %>%
  mutate(WaitTime= abs(WaitTime))

# Examine structure of data
str(noshow)

# Summary
summary(noshow)

# Print contrasts of factor variables
contrasts(noshow$Gender)
contrasts(noshow$Status)

# 2 value in SMS Reminder may be due to typo. Check how many rows there are with "2":
nrow(noshow$Sms_Reminder == 2)

# Print the unique variables of each column:
lapply(subset(noshow, select = -c(WaitTime, AppointmentDate, AppointmentRegistration)), unique)

# Determine number of rows that include strange Age values and filter them from main data source
nrow(subset(noshow, Age < 1))
noshow <- filter(noshow, Age >= 1)
nrow(subset(noshow, Age > 98))
noshow <- filter(noshow, Age <= 98)

## Chi square correlation tests between Status and independent variables
chisq.test(table(noshow$Status, noshow$Gender))
chisq.test(table(noshow$Status, noshow$Sms_Reminder))
chisq.test(table(noshow$Status, noshow$Alcoholism))
chisq.test(table(noshow$Status, noshow$Diabetes))
chisq.test(table(noshow$Status, noshow$Hypertension))
chisq.test(table(noshow$Status, noshow$Disabled))
chisq.test(table(noshow$Status, noshow$Smokes))
chisq.test(table(noshow$Status, noshow$Scholarship))
chisq.test(table(noshow$Status, noshow$DayOfTheWeek))
chisq.test(table(noshow$Status, noshow$Tuberculosis))

#Removing Sunday from DayOfTheWeek (and its levels)
noshow <- filter(noshow, DayOfTheWeek != "Sunday") %>%
  mutate(DayOfTheWeek = factor(DayOfTheWeek))

#Retry ChiSquared
chisq.test(table(noshow$Status, noshow$DayOfTheWeek))

# ANOVA test between Status and continuous numeric independent variables
summary(aov(Age ~ Status, data = noshow))
summary(aov(WaitTime ~ Status, data = noshow))

#Correlation table to test for multiple colinearity
cortable <- subset(noshow, select = -c(Status, Gender, DayOfTheWeek,
                                       AppointmentRegistration, AppointmentDate))
cor(cortable)

# Table of Status
table(noshow$Status)

#Baseline: Predict most frequent value for all observations (in this case, Show-Up)
# If we always predicted that people would always show up, we would have baseline accuracy of:
max(table(noshow$Status))/sum(table(noshow$Status))

#Randomly split data into test and training samples (set seed so that results are replicable)
# .70 chosen because in our data set, about 70% are showups, we want test and train to be representative
set.seed(90)
split = sample.split(noshow$Status, SplitRatio = .70)
noshowTrain = subset(noshow, split == TRUE)
noshowTest = subset(noshow, split == FALSE)

#Build first regression model using our chosen IVs
mod1 <- glm(Status ~ Gender + Alcoholism + Diabetes + Hypertension + Smokes
            + DayOfTheWeek + Tuberculosis + Scholarship + Age + WaitTime, data = noshowTrain, family = binomial)
summary(mod1)

# Diabetes and Gender not significant, create model 2
mod2 <- glm(Status ~ Alcoholism + Hypertension + Smokes + DayOfTheWeek
             + Scholarship + Age + WaitTime, data = noshowTrain, family = binomial)
summary(mod2)

# Test model for predictive accuracy, first using training data set then with test data set
predictTrain <- predict(mod2, type = "response")
table(noshowTrain$Status, predictTrain > .5)

predictTest <- predict(mod2, type = "response", newdata = noshowTest)
table(noshowTest$Status, predictTest > 0.5)

#Calculate AUC
ROCRpred = prediction(predictTest, noshowTest$Status)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Plot of effects
plot(allEffects(mod2))
