# Read in states data
states.data <- readRDS("dataSets/states.rds")

# Rebuild initial regression equation using metro as predictor of energy consumption
en.mod <- lm(energy ~ metro, data = na.omit(states.data))

# Adding to initial equation by generating an interaction term. Does association between
# energy consumption and metro residents depend on population density?
en.mod.by.density <- lm(energy ~ metro * density, data = na.omit(states.data))
summary(en.mod.by.density)
# Summary indicates no significance

# Adding region to the model, are there differences across region?
en.mod.dense.region <- lm(energy ~ metro * density + region, data = na.omit(states.data))
anova(en.mod.dense.region)
# ANOVA does not show significant differences across region