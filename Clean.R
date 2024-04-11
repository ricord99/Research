library(lavaan) #package used to test indirect effects
library(lavaanPlot) #plotting
library(psych) #descriptives
library(semTools) #additional stats tools
library(tidyverse) #data wranglings
library(car)
library(modelr)
library(psyntur)
library(mediation)
library(ggplot2)
library(dplyr)
library(lavaanPlot)
library(lm.beta)


# Load in Data ------------------------------------------------------------

df<-read.csv("https://raw.githubusercontent.com/ricord99/Research/main/Feb_19.csv")

glimpse(df)

# Data structures ---------------------------------------------------------

# Going through each variable to change from integer to dbl
df$Computer.experience_1 <- as.numeric(df$Computer.experience_1)
df$Age_1 <- as.numeric(df$Age_1)
df$Gender <- as.factor(df$Gender)
df$PO <- as.numeric(df$PO)
df$PerceivedSurveil1 <- as.numeric(df$PerceivedSurveil1)
df$PerceivedSurveil2 <- as.numeric(df$PerceivedSurveil2)
df$PerceivedSurveil3 <- as.numeric(df$PerceivedSurveil3)
df$PerceivedSurveil4 <- as.numeric(df$PerceivedSurveil4)
df$PrivacyConcerns1 <- as.numeric(df$PrivacyConcerns1)
df$PrivacyConcerns2 <- as.numeric(df$PrivacyConcerns2)
df$PrivacyConcerns3 <- as.numeric(df$PrivacyConcerns3)
df$PrivacyConcerns4 <- as.numeric(df$PrivacyConcerns4)
df$TrustRS1 <- as.numeric(df$TrustRS1)
df$TrustRS2 <- as.numeric(df$TrustRS2)
df$TrustRS3 <- as.numeric(df$TrustRS3)
df$TrustRS4 <- as.numeric(df$TrustRS4)
df$TrustRS5 <- as.numeric(df$TrustRS5)
df$CyberParanoia1 <- as.numeric(df$CyberParanoia1)
df$CyberParanoia2 <- as.numeric(df$CyberParanoia2)
df$CyberParanoia3 <- as.numeric(df$CyberParanoia3)
df$CyberParanoia4 <- as.numeric(df$CyberParanoia4)
df$CyberParanoia5 <- as.numeric(df$CyberParanoia5)
df$CyberParanoia6 <- as.numeric(df$CyberParanoia6)


# Variable formatting -----------------------------------------------------


mean_scores1 <- dplyr::select(df, PO) %>% bind_cols(
  total_scores(df,
               PS = starts_with('PerceivedSurveil'),
               PPC = starts_with('PrivacyConcerns'),
               TrustRS = starts_with('TrustRS'),
               CP = starts_with('CyberParanoia'),
               .method = 'sum_like'))


# Now each question has been summed together, for each P in order to represent the variables for 
# analysis

# Demonstration 
glimpse(mean_scores1)



# Descriptive ------------------------------------------------------------

# Table format for all questions (before variables have been constructed)
df %>%psych::describe(.)

# Descriptors across each variable
mean_scores1 %>% psych::describe(.)

mean_scores1 %>%  pairs.panels(.)

# Easier to see the correlations between vars
scatterplot_matrix(mean_scores1, PO, PS, PPC, TrustRS, CP)
# PS and CP strongest correlation
# PPC and PS strongly correlated
# => general conceptual support



# Viz + simple regressions ---------------------------------------------------------------------

scatterplot(mean_scores1, x=CP,y= TrustRS, best_fit_line = TRUE)


scatterplot(mean_scores1, x=PO, y = TrustRS, best_fit_line = TRUE)
# Trend suggests as there is a more conservative orientation that
# trust in RS reduces

i <- lm(TrustRS~PO, mean_scores1)
summary(i)
# Perhaps there is a non-linear relationship???
# Distribution is skewed towards lower bound residuals
# = no there is a fairly normal distribution of residuals 

# plot of residuals
ggplot(data = mean_scores1, aes(x = i$residuals)) + 
  geom_histogram(bins = 20, fill = 'steelblue', color = 'black') + 
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

# Internal reliability ----------------------------------------------------

cronbach(df,
         PS = starts_with('PerceivedSurveil'),
         PPC = starts_with('PrivacyConcerns'),
         TrustRS = starts_with('TrustRS'),
         CP = starts_with('CyberParanoia'))
# Decent alpha across the board
# CP is slightly lacking

# Multiple Linear Regression ----------------------------------------------

laf <- lm(TrustRS~PS+PPC+PO, mean_scores1)
summary(laf)

# VIF ---------------------------------------------------------------------

vif(laf)
# All variables underneath threshold

# Model w/ lavaan ---------------------------------------------------------


lav <- '
# direct path
TrustRS~c*PO

# path a
CP~a*PO

# path d1
PS~d1*CP
# path d2
PPC~d2*CP

# path b1
TrustRS~b1*PS

#path b2
TrustRS~b2*PPC

direct:=c
indirectPS:= a*b1*d1
indirectPPC:= a*b2*d2
total:=c+(a*b1*d1)+(a*b2*d2)
'

fit <- sem(lav, data=mean_scores1, se ="bootstrap", bootstrap= 1000)

summary(fit, fit.measures=TRUE, standardized = TRUE, ci = TRUE, rsquare=TRUE)

# ANALYSIS

# No paths have stat significance 
# indirect mediating paths are close to 0 but still straddle it
# Limited explanatory power in the mediating effect of CP, PS, PPC in describing trust in RS

# Partial support for the hypotheses: std.all coefficient path for PS~CP is slightly larger than
# coefficient path for PPC~CP

#################################################################


lav2 <- '
# removed paths
TrustRS~0*PO

CP~0*PO

# direct path
TrustRS~c*CP

# path a1
PS~a1*CP

# path a2
PPC~a2*CP

# path b1
TrustRS~b1*PS

#path b2
TrustRS~b2*PPC

direct:=c
indirectPS:= a1*b1
indirectPPC:= a2*b2
total:=c+(a1*b1)+(a2*b2)
'

fit2 <- sem(lav2, data=mean_scores1, se ="bootstrap", bootstrap= 1000)

summary(fit2, fit.measures=TRUE, standardized = TRUE, ci = TRUE, rsquare=TRUE)


# This is identical to the model below, except the PO path has been partitioned
# Results here are identical to the model below EG PPC is a sig mediator


# Model Comparisons -------------------------------------------------------------

summary(compareFit(fit,fit2))

# No sig difference in models
