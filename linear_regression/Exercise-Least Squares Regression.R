# Read in states data
states.data <- readRDS("dataSets/states.rds")

# Subset energy and metro columns we are interested in for all rows
# Since we are calculating correlation later on, be sure to omit NA values.
sts.en.met <- subset(na.omit(states.data), select = c("energy", "metro"))

# View summary of subset. 
summary(sts.en.met)
# Examine correlation between energy consumed and percentage of residents in metropolitan areas.
cor(sts.en.met)

# Create scatterplot of data to check for outliers and linear/non-linear relationships
plot(sts.en.met)

# Fit regression model with energy consumption as dependent variable, 
# % of metro residents as independent variable.
en.mod <- lm(energy ~ metro, data = na.omit(states.data))
# Summary of regression model
summary(en.mod)
# Summary reveals that there is a significant negative relationship
# between energy consumption and metro residents with an alpha between .01 and .05.
# Since Multiple and Adjusted R-Squared are around .1, our model provides a slight
# improvement in prediction over baseline.

# Plot model to check for deviations from modeling assumptions
plot(en.mod)
# Normal Q-Q shows a few residuals that deviate from assumption of linearity,
# but overall relationship is linear.
# Across all plots, rows number 2 and 51 appear as outliers.
# It may be beneficial to exclude them from repeated analysis.

# Perform correlation analysis across all numeric columns of the dataset
# (be sure to exclude NA values)
cor(na.omit(states.data)[sapply(na.omit(states.data), is.numeric)])
# Correlations for energy consumption show greenhouse gas emission ("green")
# has a high correlation and may improve our predictive model.

# Subset new dataset that includes "green" column.
# Eliminate NA values and examine/plot data before fitting model.
sts.en.met.green <- subset(na.omit(states.data), select = c("energy", "metro", "green"))
summary(sts.en.met.green)
cor(sts.en.met.green)
plot(sts.en.met.green)

# Fitting a new model that includes "green" as a predictor
en.green.mod <- lm(energy ~ metro + green, data = na.omit(states.data))
# Summary of new model
summary(en.green.mod)
# Summary shows a higher R-squared value for this new model versus the previous model. 
# This indicates that including "green" improves predictive capability.
# Significance also shows green as highly significant predictor for energy consumption.

# Conduct ANOVA test to further compare models
anova(en.mod, en.green.mod)
# ANOVA shows that new model that includes "green" is significantly different from
# first model with metro as sole predictor. New model is better as ANOVA indicates SSE is reduced.
