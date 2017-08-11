library(ggplot2)
library(dplyr)
library(ggmosaic)
library(GGally)

load("brfss2013.RData")

data <- brfss2013 %>%
  select(genhlth, physhlth, menthlth) %>%
  filter(!is.na(genhlth), !is.na(physhlth), !is.na(menthlth))

ggplot(data, aes(x = physhlth, y = menthlth)) +
  geom_point() +
  geom_smooth(method = "lm")

data <- data %>%
  arrange(desc(menthlth))

head(data) #before removed

data <- data %>%
  filter(row_number() != 1)

head(data) #after removed

data %>%
  select(physhlth, menthlth) %>%
  group_by(physhlth, menthlth) %>%
  add_tally() %>%
  ggplot(aes(x = physhlth, y = menthlth)) +
  geom_point(aes(size = n)) +
  geom_smooth(method = "lm")

cor(data$menthlth, data$physhlth)