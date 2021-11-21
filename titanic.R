options(digits = 3)  # report 3 significant digits
library(tidyverse)
library(titanic)

#
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
titanic

# age distribution with stacked gender
titanic %>% filter(!is.na(Age)) %>% group_by(Sex) %>%
  ggplot(aes(Age, color = Sex, fill = Sex)) + 
  geom_density(alpha = 0.2, bw = 0.75, position = "stack")


# QQ0-plot of passenger age
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

# bar plot of count and passengers survived grouped by gender
titanic %>% 
  ggplot(aes(Survived, color = Sex, fill = Sex)) +
  geom_bar(position = position_dodge())

# density plot of the age of surviving passengers
titanic %>% filter(!is.na(Age)) %>% group_by(Survived) %>%
  ggplot(aes(Age, color = Survived, fill = Survived)) + 
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(Survived~.)

# candlestick chart showing the fare paid versus survival
titanic %>% filter(!is.na(Fare == 0)) %>% group_by(Survived) %>%
  ggplot(aes(Survived, Fare, color = Survived)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.2)

# bar chart of survival relative to class
titanic %>% 
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_dodge())


titanic %>% 
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

titanic %>% 
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_dodge())

# density plot of survival relative to class, age and gender
titanic %>% filter(!is.na(Age)) %>% group_by(Survived) %>%
  ggplot(aes(Age, color = Survived, fill = Survived)) + 
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(Sex~Pclass)















