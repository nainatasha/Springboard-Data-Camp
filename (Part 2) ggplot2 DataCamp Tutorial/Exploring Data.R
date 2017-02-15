# Explore the dataset with summary and str
summary(adult)
str(adult)

# Age histogram
ggplot(adult, aes(x = SRAGE_P)) +
  geom_histogram()

# BMI histogram
ggplot(adult, aes(x = BMI_P)) +
  geom_histogram()

# Age colored by BMI, default binwidth
ggplot(adult, aes(x = SRAGE_P, fill = factor(RBMI))) +
  geom_histogram( binwidth = 1)