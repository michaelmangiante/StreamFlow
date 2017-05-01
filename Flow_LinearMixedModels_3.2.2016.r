# Michael Mangiante
# 3.3.2016
# This script is to analyze the relationship between Flow and anthropogenic/natural variables.
# We first analyzed the best model including all potential "Natural" variables
# Second, we compared the best model of natural variables to that of anthrpogenic variables to see which explained
#	the variability in the flashiness dataset better.

# To compair the datasets I used a linear mixed model.
# the fixed effects were my test statistics
# the random effects were either site ID (to account for variability in time) or Time
# Specifically, i want to remove the random effect of temporal auto-correlation.  There is also spatial auto-correlation
#	but I do not address that in this script. 


# Load data
setwd("K:/Streamflow/WorkingFolder_4.14.2015/FinalData/Figures")
data <- read.csv(file="CombinedData_1.6.2016.csv", header = TRUE, sep = ",")
colnames(data)

attach(data)

require(lme4)

# Initial plot with just Slope

envdata.1 <- lmer(log(Mean.Flow) ~ Mean.Slope  + (1|Site.ID),
data = data, REML = FALSE) # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(envdata.1))
qqline(resid(envdata.1))

summary(envdata.1) # AIC = 236.8


# Add in time
envdata.2 <- lmer(log(Mean.Flow) ~ Mean.Slope  + (1|Site.ID) + (1|Year),
data = data, REML = FALSE) # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(envdata.2))
qqline(resid(envdata.2))

summary(envdata.2) # AIC 153.4

anova(envdata.1, envdata.2) # very significant with time.. apparently time matters?
#################################

envdata.3 <- lmer(log(Mean.Flow) ~ Mean.Slope  + Drainage.Area +  (1|Site.ID) + (1|Year),
data = data, REML = FALSE)
qqnorm(resid(envdata.3))
qqline(resid(envdata.3))

summary(envdata.3) # AIC = 152.2

anova(envdata.2, envdata.3) # pvalue = 0.07732 ### Might be a little close to tell...
#################################

envdata.4 <- lmer(log(Mean.Flow) ~ Mean.Slope  + Drainage.Area + Ecoregion + (1|Site.ID) + (1|Year),
data = data, REML = FALSE)
qqnorm(resid(envdata.4))
qqline(resid(envdata.4))

summary(envdata.4) # AIC = 129.1

anova(envdata.3, envdata.4) # pvalue = 4.35e-06
#################################

envdata.5 <- lmer(log(Mean.Flow) ~ Mean.Slope  + Ecoregion + (1|Site.ID) + (1|Year),
data = data, REML = FALSE)
qqnorm(resid(envdata.5))
qqline(resid(envdata.5))

summary(envdata.5) # AIC = 131.4

anova(envdata.4, envdata.5) # pvalue = 0.03614 ### Might be a little close
#################################

envdata.6 <- lmer(log(Mean.Flow) ~ Mean.Slope  + Drainage.Area + Ecoregion + Percent.Sand + (1|Site.ID) + (1|Year),
data = data, REML = FALSE)
qqnorm(resid(envdata.6))
qqline(resid(envdata.6))

summary(envdata.6) # AIC = 126.2

anova(envdata.5, envdata.6) # pvalue = .009645

anova(envdata.4, envdata.6) # pval = 0.02698

# Best model is envdata.6 for log(Flow)



###############################################################################################


# Run for Anthropogenic variables

urbdata.1 <- lmer(log(Mean.Flow) ~ Percent.Developed  + (1|Site.ID),
data = data, REML = FALSE) # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(urbdata.1))
qqline(resid(urbdata.1))
summary(urbdata.1) # AIC 164.2

# add in time

urbdata.2 <- lmer(log(Mean.Flow) ~ Percent.Developed  + (1|Site.ID) + (1|Year),
data = data, REML = FALSE) # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(urbdata.2))
qqline(resid(urbdata.2))
summary(urbdata.2) # AIC = 90.9


anova(urbdata.1, urbdata.2) # pvalue = 2.2e-16 ---- should use urbdata.2

anova(urbdata.2, envdata.6) # pvalue = 1 ---- means the urban data is much better at explaining variability than the environmental model
#####################

urbdata.3 <- lmer(log(Mean.Flow) ~ Percent.Developed  + Disturbance.Value + (1|Site.ID) + (1|Year),
data = data, REML = FALSE) # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(urbdata.3))
qqline(resid(urbdata.3))
summary(urbdata.3) # AIC = 117.5


anova(urbdata.2, urbdata.3) # pvalue = 0.2671
#####################


