# RStudio API Code
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(readr)
library(tidyverse)
library(GGally)
library(lubridate)
# Data Import & Cleaning
week7_tbl <- read_csv("../data/week3.csv") %>%
  mutate(timeStart = ymd_hms(timeStart), #convert data types
          timeEnd = ymd_hms(timeEnd),
          gender = factor(gender, labels = c("Female", "Male")), #labels MUST be alphabetical; else specify levels = c("M", "F"), labels = c("Male", "Female")
          condition = factor(condition, labels = c("Block A", "Block B", "Control"))) %>%
  filter(q6 == 1) %>% #remove participants who failed attention check
  select(-q6) #remove attention check question altogether

# Visualization
ggpairs(week7_tbl[,5:13]) #fig 1 god matrix
ggplot(week7_tbl, aes(x = timeStart, y = q1) ) + #fig 2 scatterplot q1 over time
  geom_point() +
  scale_x_datetime("Date of Experiment") +
  scale_y_continuous("Q1 Score")
ggplot(week7_tbl, aes(x = q1, y = q2, col = gender)) + #fig 3 q2 over q1 colored by gender
  geom_jitter()
ggplot(week7_tbl, aes(x = q1, y = q2)) +
  geom_jitter() +
  facet_grid(. ~ gender) +
  xlab("Score on Q1") +
  ylab("Score on Q2")
week7_tbl %>% mutate(timediff = difftime(timeEnd, timeStart, units = c("secs"))) %>%
  ggplot(aes(x = gender, y = timediff)) + #fig 4 experimental time by gender
  geom_boxplot() +
  scale_x_discrete("Gender") +
  scale_y_continuous("Time Elapsed (secs)") # + coord_cartesian(ylim = c(0, 1250)) to start y axis at 0
ggplot(week7_tbl, aes(x = q5, y = q7, col = condition)) + #fig 5
  geom_jitter(width = .1) +
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous("Score on Q5") +
  scale_y_continuous("Score on Q7") +
  scale_color_discrete("Experimental Condition") +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(axis.line.x = element_blank(), axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "#DFDFDF"), legend.key = element_rect(fill = "#FFFFFF", color = "#FFFFFF"))
  






