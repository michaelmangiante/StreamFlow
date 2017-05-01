# Michael Mangiante
# 3.3.2016
# This script is to analyze the relationship between Flashiness and anthropogenic/natural variables.
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
library(ggplot2)
library(multcomp)
library(fitdistrplus)
library(logspline)




# Determine my distrobution

# Reset between 0 and 1

bin.Flash <- tanh(Mean.Flashiness)

plot(Mean.Flashiness~ Percent.Developed)
plot(bin.Flash~ Percent.Developed)

descdist(Mean.Flashiness, discrete = FALSE) # Shows a beta distribution
descdist(bin.Flash, discrete = FALSE)
fit.beta <- fitdist(bin.Flash, "beta")
fit.weibull <- fitdist(Mean.Flashiness, "weibull")
fit.norm <- fitdist(Mean.Flashiness, "norm")


plot(fit.norm)
plot(fit.weibull)
plot(fit.beta)


# Initial plot with just Slope

envdata.1 <- glmer(bin.Flash ~ Mean.Slope  + (1|Site.ID),
data = data, family = "quasibinomial") # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(envdata.1))
qqline(resid(envdata.1))

summary(envdata.1) # AIC = -894.7


# Add in time
envdata.2 <- lmer(Mean.Flashiness ~ Mean.Slope  + (1|Site.ID) + (1|Year),
data = data, REML = FALSE) # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(envdata.2))
qqline(resid(envdata.2))

summary(envdata.2) # AIC -913.2

anova(envdata.1, envdata.2) # very significant with time.. apparently time matters?
#################################

envdata.3 <- lmer(Mean.Flashiness ~ Mean.Slope  + Drainage.Area +  (1|Site.ID) + (1|Year),
data = data, REML = FALSE)
qqnorm(resid(envdata.3))
qqline(resid(envdata.3))

summary(envdata.3) # AIC = -919.4

anova(envdata.2, envdata.3) # pvalue = 0.00426
#################################

envdata.4 <- lmer(Mean.Flashiness ~ Mean.Slope  + Drainage.Area + Ecoregion + (1|Site.ID) + (1|Year),
data = data, REML = FALSE)
qqnorm(resid(envdata.4))
qqline(resid(envdata.4))

summary(envdata.4) # AIC = -969.9

anova(envdata.3, envdata.4) # pvalue = 1.864 e -11
#################################

envdata.5 <- lmer(Mean.Flashiness ~ Mean.Slope  + Drainage.Area + Ecoregion + Percent.Sand + (1|Site.ID) + (1|Year),
data = data, REML = FALSE)
qqnorm(resid(envdata.5))
qqline(resid(envdata.5))

summary(envdata.5) # AIC = -984.4

anova(envdata.4, envdata.5) # pvalue = 4.834e-05
#################################

envdata.6 <- lmer(Mean.Flashiness ~ Mean.Slope  + Drainage.Area + Ecoregion + Percent.Sand + MaximumDistancefromPourPoint + (1|Site.ID) + (1|Year),
data = data, REML = FALSE)
qqnorm(resid(envdata.6))
qqline(resid(envdata.6))

summary(envdata.6) # AIC = -984.6

anova(envdata.5, envdata.6) # pvalue = .13

# Best model is envdata.5



###############################################################################################


# Run for Anthropogenic variables

urbdata.1 <- lmer(Mean.Flashiness ~ Percent.Developed  + (1|Site.ID),
data = data, REML = FALSE) # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(urbdata.1))
qqline(resid(urbdata.1))
summary(urbdata.1) # AIC -992.8

# add in time

urbdata.2 <- lmer(Mean.Flashiness ~ Percent.Developed  + (1|Site.ID) + (1|Year),
data = data, REML = FALSE) # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(urbdata.2))
qqline(resid(urbdata.2))
summary(urbdata.2) # AIC = -993.3


anova(urbdata.1, urbdata.2) # pvalue = .1108 ---- should use urbdata.1

anova(urbdata.1, envdata.5) # pvalue = .3094 ---- means the urban data is much better at explaining variability than the environmental model
#####################

urbdata.3 <- lmer(Mean.Flashiness ~ Percent.Developed  + Distance.to.Dam..m. + (1|Site.ID),
data = data, REML = FALSE) # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(urbdata.3))
qqline(resid(urbdata.3))
summary(urbdata.3) # AIC = ##### On a different scale so does not effect it


anova(urbdata.1, urbdata.3) # pvalue = 1
#####################

urbdata.4 <- lmer(Mean.Flashiness ~ Percent.Developed  + Disturbance.Value + (1|Site.ID),
data = data, REML = FALSE) # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(urbdata.4))
qqline(resid(urbdata.4))
summary(urbdata.4) # AIC = -984.8


anova(urbdata.1, urbdata.4) # pvalue = 0.006369 ### AIC goes up but p value is significant?
#####################

##########################################################
##########################################################

# Compair Road Area to Percent Developed


road <- subset(data, Year == "Y2011")



road.mod1 <- glm(road$Mean.Flashiness ~ road$Percent.Developed,
data = road) # 

qqnorm(resid(road.mod1))
qqline(resid(road.mod1))
summary(road.mod1) # r2 = .6641, AIC = -70.516


road.mod2 <- glm(road$Mean.Flashiness ~ road$WS.Normalized.Road.Area..Sq.km.,
data = road) #

qqnorm(resid(road.mod2))
qqline(resid(road.mod2))
summary(road.mod2) # r2 = .702, AIC = -80.352


anova(road.mod1, road.mod2, test = "F") #
#####################

road.mod3 <- glm(road$Mean.Flashiness ~ road$WS.Normalized.Road.Area..Sq.km. + road$Disturbance.Value,
data = road) # need to add REML if you are comparing models (use likelihood ratio test)

qqnorm(resid(road.mod3))
qqline(resid(road.mod3))
summary(road.mod3) # r2 = .7212, AIC = -65.569


########################################
# Plot

# Urban to use: urbdata.1 or urbdata.4
# Natural to use: envdata.5


tmp <- as.data.frame(confint(glht(urbdata.1))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()
