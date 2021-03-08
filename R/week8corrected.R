# RStudio API Code
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(GGally) #ggpairs god matrix
library(car) #Anova wrapper (necessary for Type III SS ANOVA)
library(yarrr) #RDI pirate plot
library(emmeans) #creating marginal means, replaces lsmeans
library(broom) #tidy for marginal means plot
library(magrittr)
library(multcomp)
library(lsr) #etaSquared
library(apaTables)

# Data Import & Cleaning
health_tbl <- read_tsv("../data/w4inhome_dvn.tab") %>%
  select(admin_month = IMONTH4, 
         gender = BIO_SEX4, 
         living_mother = H4WP1, fiw = H4LM29) %>% # fiw = family influencing work
  mutate(gender = factor(gender, levels = c("1", "2"), labels = c("Male", "Female")), 
         living_mother = factor(living_mother, levels = c("0", "1", "8"), labels = c("No", "Yes", "Don't Know"))) %>%
  mutate(fiw = replace(fiw, fiw >= 6 , NA)) #replace values outside of likert scale with NA

# Visualization
ggpairs(health_tbl)

# Analysis: FIW on Gender and Living MOther
options(contrasts = c("contr.sum", "contr.poly")) # set up for using type III SS
linear_model <- lm(fiw ~ admin_month + living_mother * gender, data = health_tbl) #covariate being controlled doesn't need to come first in type III, but does for other types
anova_model <- Anova(linear_model, type="III") 
anova_model #print ANOVA table
plot(linear_model)  #diagnostic plots
  #raw data, description, and inference (RDI) plot
pirateplot(formula = fiw ~ living_mother * gender, data = health_tbl, inf.method = "ci") 
  #marginal means plot
emmeans(linear_model, "living_mother", by = "gender") %>%
  tidy(ls) %>%
  ggplot(aes(x = gender, y = estimate, group = living_mother, col = living_mother)) + 
  geom_line()
  #post hoc analysis
health_tbl %<>% mutate(condition = interaction(gender, living_mother, sep = "x")) # creates interaction term as factor
posthoc_model <- lm(fiw ~ admin_month + condition, data = health_tbl) # model with new interaction term
posthocs <- glht(posthoc_model, linfct = mcp(condition = "Tukey")) # Tukey test on all crossings (individual contrast tests)
summary(posthocs)                                                  # TukeyHSD() won't work with more than one IV
etaSquared(linear_model, type=3) 
  #write APA style table to Word doc
apa.aov.table(linear_model, type = 3, filename = "../output/analysis.doc", conf.level=.95)
  #planned contrast
    #contrast first two groups of living_mother (i.e., "no" and "yes") with last group ("Don't Know")
    #creates contrast of means for those who know whether their mother is alive vs. those who don't
ls <- emmeans(linear_model, "living_mother")
contrast(ls, list(known_vs_unknown = c(-.5, -.5, 1))) 

# Conclusions: 
# Hypothesis 1 (that living mother and gender interact to predict family 
# interference on work) was not supported. The test of the interaction term, 
# living_mother x gender, in the ANOVA model was not significant (F = .304, p = .738).

# Hypothesis 2 (that those who don’t know if their mother is alive or not have greater 
# family interference on work than those who do while controlling for admin_month) was also
# not supported. The estimated difference in marginal means in the planned contrast analysis
# was not significant (t = .908, p = .363).
