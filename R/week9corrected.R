# RStudio API Code
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(GGally) #ggpairs()
library(broom) #augment(), tidy()




# Data Import & Cleaning
  # create function to replace nonscale values (values over 4):
replace_over_4 <- function(x) replace(x, x > 4, NA)
health_tbl <- read_tsv("../data/w4inhome_dvn.tab") %>%
  select(mh1 = H4MH3, mh2 = H4MH4, mh3 = H4MH5, mh4 = H4MH6, 
         jailage = H4CJ20, gender = BIO_SEX4
  ) %>%
  mutate_at(vars(mh1:mh4), replace_over_4) %>%
  mutate(mh1 = 4 - mh1,
         mh4 = 4 - mh4,
         jailage = replace(jailage, jailage >= 97, NA),
         gender = factor(gender, levels = c("1", "2"), labels = c("Male", "Female"))
  ) %>%
  # alt: (nontidy, from library "psych") reverse score mh1 and mh3: higher score = better mental health:
  # health_tbl[ , 1:4] <- reverse.code(c(-1, 1, 1, -1), health_tbl[ , 1:4], mini = 0, maxi = 4)
  rowwise() %>%                                             #group by rows in order to
  mutate(mh = mean(c(mh1, mh2, mh3, mh4), na.rm = T)) %>%   #create scale scores (means)
  ungroup() %>%
  filter(jailage, !is.na(jailage))

# Visualization
health_tbl %>% 
  select(jailage, gender, mh) %>%
  ggpairs() #god plots of jailage, gender, mh
health_tbl %>% 
  select(-gender) %>% 
  cor(method = "pearson", use = "pairwise") #exclude col 6 (gender): nonnumeric

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
  # especially re: homoskedasticity (Scale-Location); more variance in residuals for higher 
  # fitted values
summary(model2)
ggplot(health_tbl, aes(x = jailage, y = mh, col = gender, group = gender)) +
  geom_smooth(method = "lm", se = F) +
  coord_cartesian(ylim = c(0, 4)) #revise y axis to full scale range of response options
hlm <- tidy(anova(model1, model2))
hlm
  # use glimpse in console to find r.squared: 
  # e.g., try glimpse(model1) (it's not there), then try glimpse(summary(model1)) (success)
tibble(delta_rsquared = summary(model2)$r.squared - summary(model1)$r.squared, 
       F_statistic = hlm$statistic[2], 
       p_value = hlm$p.value[2])
  # HLM conclusions: the nonsignifcant F statistic for the comparison of the interaction
  # model against the main effects model indicates there is no significant interaction. 
  # Therefore the more parsimonious model (model1) should be retained.

