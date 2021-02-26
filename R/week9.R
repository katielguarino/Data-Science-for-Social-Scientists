# RStudio API Code
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(GGally) #ggpairs()
library(broom) #tidy()




# Data Import & Cleaning
health_tbl <- read_tsv("../data/w4inhome_dvn.tab") %>%
  select(mh1 = H4MH3, mh2 = H4MH4, mh3 = H4MH5, mh4 = H4MH6, 
         jailage = H4CJ20, gender = BIO_SEX4
  ) %>%
  mutate(mh1 = as.numeric(recode(mh1, "0" = "4", "1" = "3", "2" = "2", "3" = "1", "4" = "0")),
         mh2 = replace(mh2, mh2 > 4, NA),
         mh3 = replace(mh3, mh3 > 4, NA),
         mh4 = as.numeric(recode(mh4, "0" = "4", "1" = "3", "2" = "2", "3" = "1", "4" = "0")),
         jailage = replace(jailage, jailage >= 97, NA),
         gender = factor(gender, levels = c("1", "2"), labels = c("Male", "Female"))
  ) %>%
  # alt way (nontidy, from library "psych") to reverse score mh1 and mh3: higher score = better mental health:
  # health_tbl[ , 1:4] <- reverse.code(c(-1, 1, 1, -1), health_tbl[ , 1:4], mini = 0, maxi = 4)
  rowwise() %>%                                             #group by rows in order to
  mutate(mh = mean(c(mh1, mh2, mh3, mh4), na.rm = T)) %>%   #create scale scores (means)
  ungroup() %>%
  filter(jailage, !is.na(jailage))
    
# Visualization
ggpairs(health_tbl[5:7]) #god plots of jailage, gender, mh
cor(health_tbl[-6], method = "pearson", use = "pairwise.complete") #exclude col 6 (gender): nonnumeric

# Analysis: Mental Health on Age of First Jailing and Gender
model1 <- lm(mh ~ jailage + gender, data = health_tbl) #main effects only
par(mfrow = c(2, 2)) #sets base R plot settings to display 2x2 matrix; par(mfrow = c(1, 1)) will return to default
plot(model1)
# Plot infereneces: no assumptions drastically violated
  # Residuals vs. fitted values shows no correlation, indicating independent residuals 
  # Q-Q plot shows residuals are mostly normal with slight nonnormality at extremes
  # Scale-Location (sqrt of residuals vs fitted) shows homoskedasticity: model predicts 
    # equally well regardless of x values
  # Resdiauls vs. Leverage shows little influence of outliers; model parameters don't change
    # much when any given observation is removed
summary(model1)
ggplot(augment(model1), aes(x = jailage, y = mh, col = gender)) +
  geom_line(aes(y = .fitted))
model2 <- lm(mh ~ jailage * gender, data = health_tbl)
plot(model2)
# Plot inferences: slightly more problematic assumptions than main effects model
  # especially re: homoskedasticity (Scale-Location); more variance in residuals for higher fitted values
summary(model2)
ggplot(health_tbl, aes(x = jailage, y = mh, col = gender, group = gender)) +
  geom_smooth(method = "lm")
hlm <- tidy(anova(model1, model2))
hlm
tibble(delta_rsquared = summary(model2)$r.squared - summary(model1)$r.squared, 
       F_statistic = hlm$statistic[2], 
       p_value = hlm$p.value[2])
# HLM conclusions: the nonsignifcant F statistic for the comparison of the interaction
  # model against the main effects model indicates there is no significant interaction. 
  # Therefore the more parsimonious model (model1) should be retained.

