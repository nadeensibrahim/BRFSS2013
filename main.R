library(ggplot2)
library(reshape2)
library(dplyr)
library(ggmosaic)
library(GGally)

load("brfss2013.RData")

####### Question 1

# Extract columns and filter NAs
health <- brfss2013 %>%
  select(genhlth, physhlth, menthlth) %>%
  filter(!is.na(genhlth), !is.na(physhlth), !is.na(menthlth))

# Plot physical health against mental health
ggplot(health, aes(x = physhlth, y = menthlth)) +
  geom_point() +
  geom_smooth(method = "lm")

# Remove outlier
health <- health %>%
  arrange(desc(menthlth)) %>%
  filter(row_number() != 1)
health %>%
  select(physhlth, menthlth) %>%
  group_by(physhlth, menthlth) %>%
  add_tally() %>%
  ggplot(aes(x = physhlth, y = menthlth)) +
  geom_point(aes(size = n)) +
  geom_smooth(method = "lm")

# Slightly positive correlation
cor(health$menthlth, health$physhlth)

# Graphs are right skewed so I used the median over the mean
ggplot(health, aes(x = physhlth)) + 
  geom_histogram(bins = 5)
ggplot(health, aes(x = menthlth)) + 
  geom_histogram(bins = 5)

# Get bad physical health days median
physical_medians <- health %>%
  select(genhlth, physhlth) %>% 
  group_by(genhlth) %>%
  filter(!is.na(genhlth)) %>%
  summarise(p_median = median(physhlth, na.rm = TRUE)) 

# Get bad mental health days median
mental_medians <- health %>%
  select(genhlth, menthlth) %>% 
  group_by(genhlth) %>%
  filter(!is.na(genhlth)) %>%
  summarise(m_median = median(menthlth, na.rm = TRUE)) 

# Join medians to health data to use as comparator (above and below median)
health <- health %>%
  left_join(physical_medians, by = 'genhlth') %>%
  left_join(mental_medians, by = 'genhlth') %>%
  mutate(bad_physhlth_days = ifelse(physhlth >= p_median, "above", "below")) %>%
  mutate(bad_menthlth_days = ifelse(menthlth >= m_median, "above", "below"))

health <- health %>%
  select(genhlth, bad_physhlth_days, bad_menthlth_days)

id <- rownames(health)
health <- cbind(id=id, health)

health_m <- melt(data = health, id.vars = c("id", "genhlth"), measure.vars = c("bad_physhlth_days", "bad_menthlth_days"))

ggplot(data=subset(health_m, !is.na(value))) +
  geom_mosaic(aes(x=product(genhlth), fill=value), divider=mosaic("h")) +
  theme(axis.text.x=element_text(angle=-25, hjust= .1)) +
  facet_grid(variable~.) +
  labs(x = "General Health", y = "Frequency", fill = "Average")

####### Question 2: Relationship between employement and general health
# At a glance it appears those who are unable to work have poor general health.
# Is this an indicator that health is keeping them from working?
employement <- brfss2013 %>%
  select(employ1, genhlth) %>%
  group_by(employ1, genhlth) %>%
  filter(!is.na(genhlth) & !is.na(employ1)) %>%
  tally() %>%
  ungroup() %>%
  group_by(employ1) %>%
  mutate(freq = n / sum(n))

ggplot(data = employement, aes(x = employ1, y = freq, fill = genhlth)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=-25, hjust= .1))
# stat = "identity" will make the heights of the bars the y value, "bin" will make it the count of Ys

# Lets check their physical ability..
disabled <- brfss2013 %>%
  select(employ1, qlactlm2) %>%
  group_by(employ1, qlactlm2) %>%
  filter(!is.na(qlactlm2) & !is.na(employ1)) %>%
  tally() %>%
  ungroup() %>%
  group_by(employ1) %>%
  mutate(freq = n / sum(n))

ggplot(data = disabled, aes(x = employ1, y = freq, fill = qlactlm2, label = paste(round(freq*100), "%", sep=""))) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=-25, hjust= .1)) +
  labs(x = "Employement Status", fill = "Physical Inhibitor", y = "Frequency") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))

# 82% of people who are unable to work have a physical inhibitor 
#   P(physical inhibitor = Yes|Unable to work)
# But being unable to work only accounts for 25% of people who are have a physical inhibitor. In fact most people who have a physical inhibitor appear to retire!
#   P(Unable to work|physical inhibitor = Yes)
# It is likely that not being able to work is due to a physical inhibitor, 
# but if they they have a physical inhibitor it does not likely mean they are just sitting around not working. In fact a large percentage of them are retired OR even employeed.

inhibitor_yes <- brfss2013 %>%
  select(employ1, qlactlm2) %>%
  group_by(employ1, qlactlm2) %>%
  filter(qlactlm2 == "Yes", !is.na(qlactlm2) & !is.na(employ1)) %>%
  tally() %>%
  ungroup() %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq))

inhibitor_yes$freq <- paste(round(inhibitor_yes$freq*100, digits = 2), "%", sep="")

inhibitor_yes[,c("employ1", "freq")]

 
####### Question 3: What's the effect of level of education on income?

unique(brfss2013$educa)

education_income <- brfss2013 %>%
  select(educa, income2) %>%
  filter(!is.na(educa), !is.na(income2)) %>%
  group_by(educa, income2) %>%
  tally() %>%
  ungroup() %>%
  group_by(educa) %>%
  mutate(freq = n / sum(n)) 

ggplot(data = education_income, aes(x = educa, y = freq, fill = income2)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=-25, hjust= .1)) +
  labs(x = "Level of Education", y = "Frequency", fill = "Income")

# People with more education make more money
