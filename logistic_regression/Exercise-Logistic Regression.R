# Read in dataset
NH11 <- readRDS("dataSets/NatHealth2011.rds")

# Subset dataset with variables we are interested in exploring.
# In this case, we want to see if age or marital status can predict if an individual has ever worked.
nh11.work <- subset(NH11, select = c("everwrk", "age_p", "r_maritl"))
# View summary of our dataset
summary(nh11.work)

# The summary initially shows that "everwrk" is a factor with 5 levels.
# We want to convert this to a binary where a Yes response is 1 and a No response is 0.
# Summary also initially showed r_marital has 10 levels, but only 6 were used in the dataset.
# We can therefore drop unused levels so that our logistic regression model will fit.
NH11 <- transform(NH11,
                 everwrk = factor(everwrk,
                                         levels = c("1 Yes", "0 No")),
                 r_marital = droplevels(r_maritl))

# Create logistic regression model and summarize results
nh11.work.mod <- glm(everwrk ~ age_p + r_maritl, data = NH11, family = "binomial")
summary(nh11.work.mod)

# Use effects package to predict everwrk at each level of and r_maritl.
library(effects)
data.frame(Effect("r_maritl", nh11.work.mod))
# Probability of working is the same across all marital status levels. 
# Marital status does not predict work.
