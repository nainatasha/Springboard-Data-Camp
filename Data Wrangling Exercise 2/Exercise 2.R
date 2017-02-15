library(tidyr)
library(dplyr)
titanic <- read.csv("titanic_original.csv")

#Replace missing values in embarked column
titanic <- titanic %>%
  mutate(embarked = gsub("^$", "S", embarked)) 

#Calculate mean age and use it to replace missing values in age column
#I would personally keep these null. The range of ages in this data set varies greatly from .16 to 80 so the mean may not be the most accurate.
mean_age <- mean(titanic$age, na.rm = TRUE)

titanic <- mutate(replace_na(titanic, list(age = mean_age))) %>%
  #Replace missing values in boat column with "None."
  mutate(boat = gsub("^$", "None", boat)) %>%
  #Create binary has_cabin_number column.
  mutate(has_cabin_number = gsub("^[^$].+", "1", cabin)) %>%
  mutate(has_cabin_number = gsub("^$", "0", has_cabin_number))

write.csv(titanic, "titanic_clean.csv")
