# RStudio API Code
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(readr)
library(tidyverse)
library(GGally)

# Data Import & Cleaning
week7_tbl <- read_delim("../data/week3.csv", ",", col_types = "ccccdddddddddd") %>% #from tidyverse, read_csv("../data/week3.csv") should also work
  mutate(timeStart = as.POSIXct(timeStart)) %>% #convert data types
  mutate(timeEnd = as.POSIXct(timeEnd)) %>%
  mutate(gender = factor(gender, labels = c("Female", "Male"))) %>%
  mutate(condition = factor(condition, labels = c("Block A", "Block B", "Control"))) %>%
  filter(q6 == 1) %>% #remove participants who failed attention check
  select(-q6) #remove attention check question altogether

# Visualization
ggpairs(week7_tbl[5:13]) #fig 1 god matrix
ggplot(week7_tbl, aes(x = timeStart, y = q1) ) + #fig 2 scatterplot q1 over time
  geom_point() +
  xlab("Date of Experiment") +
  ylab("Q1 Score")
ggplot(week7_tbl, aes(x = q1, y = q2, col = gender)) + #fig 3 q2 over q1 colored by gender
  geom_jitter()
ggplot(week7_tbl, aes(x = q1, y = q2)) +
  geom_jitter() +
  facet_grid(. ~ gender) +
  xlab("Score on Q1") +
  ylab("Score on Q2")
ggplot(week7_tbl, aes(x = gender, y = as.numeric((timeEnd - timeStart) * 60))) + #fig 4 experimental time by gender
  geom_boxplot() +
  xlab("Gender") +
  ylab("Time Elapsed (secs)")
ggplot(week7_tbl, aes(x = q5, y = q7, col = condition)) + #fig 5
  geom_jitter(width = .1) +
  geom_smooth(method = "lm", se = F) +
  labs(x = "Score on Q5", y = "Score on Q7", color = "Experimental Condition") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.line.x = element_blank(), axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "#DFDFDF"), legend.key = element_rect(fill = "#FFFFFF", color = "#FFFFFF"))
  






