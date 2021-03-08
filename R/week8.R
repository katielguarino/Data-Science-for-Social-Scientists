# RStudio API Code
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(GGally) #ggpairs god matrix
library(car) #Anova wrapper (necessary for Type III SS ANOVA)
library(emmeans) #creating marginal means, replaces lsmeans
library(broom) #tidy for marginal means plot
library(yarrr) #RDI pirate plot
library(mosaic) #Tukey will throw an error without this loaded; it wraps itself
library(lsr) #etaSquared
library(apaTables)



# Data Import & Cleaning
health_tbl <- read_tsv("../data/w4inhome_dvn.tab") %>%
  select(admin_month = IMONTH4, gender = BIO_SEX4, living_mother = H4WP1, fiw = H4LM29) %>% # fiw = family influencing work
  mutate(gender = factor(gender, levels = c("1", "2"), labels = c("Male", "Female")), 
         living_mother = factor(living_mother, levels = c("0", "1", "8"), labels = c("No", "Yes", "Don't Know"))) %>%
  mutate(fiw = replace(fiw, !(fiw < 9 & fiw > 0) , NA)) #replace values outside of likert scale with NA



# Visualization
ggpairs(health_tbl)

# Analysis: FIW on Gender and Living MOther
options(contrasts = c("contr.sum", "contr.poly"))
linear_model <- lm(fiw ~ admin_month + living_mother * gender, data = health_tbl)
anova_model <- Anova(linear_model, type="III") 
anova_model #print ANOVA table
plot(linear_model)  #diagnostic plots
  #raw data, description, and inference (RDI) plot
pirateplot(formula = fiw ~ living_mother * gender, data = health_tbl, inf.method = "ci") 
  #marginal means plot
ls <- emmeans(linear_model, "living_mother", by = "gender") 
tidy(ls) %>%
  ggplot(aes(x = gender, y = estimate, group = living_mother, col = living_mother)) + 
  geom_line()
  #post hoc analysis
TukeyHSD(linear_model, which = c("living_mother", "gender"))
etaSquared(linear_model, type=3) 
  #write APA style table to Word doc
apa.aov.table(linear_model, type = 3, filename = "../output/analysis.doc", conf.level=.95)
  #planned contrast
    #contrast first two groups of living_mother (i.e., "no" and "yes") with last group ("Don't Know")
    #creates contrast of means for those who know whether their mother is alive vs. those who don't
contrast(ls, list(known_vs_unknown = c(-.5, -.5, 1))) 


# Conclusions: 
# Hypothesis 1 (that living mother and gender interact to predict family 
# interference on work) was not supported. The coefficient estimate for the interaction 
# term, living_mother x gender, in the ANOVA model was not significant (p = .738).
# Hypothesis 2 (that those who don’t know if their mother is alive or not have greater 
# family interference on work than those who do while controlling for admin_month) was also
# not supported. The p-value for the estimated difference in marginal means for both males 
# (p = .815) and females (p = .281) in the planned contrast analysis was greater than .05.
