# Michael Mangiante
# 3/7/2016

# In this I look at the relationship between vlashiness and urban between years
# THis is similart to the glm code.
# I need a truncated regression because flashiness is between 0 and 2
# Trunkate at 2 and to the right. or left at 0

# Also, the data must be standardized.  Then You can compair which value
# is having the greatest effect on your model.



# Load data
setwd("K:/Streamflow/WorkingFolder_4.14.2015/FinalData/Figures")
datas <- read.csv(file="CombinedData_1.6.2016.csv", header = TRUE, sep = ",")
colnames(data)

attach(data)

require(lme4)
library(ggplot2)
require(truncreg)

datas$Mean.Slope <- scale(datas$Mean.Slope, center = TRUE, scale = TRUE)
datas$Drainage.Area <- scale(datas$Drainage.Area, center = TRUE, scale = TRUE)
datas$Percent.Sand <- scale(datas$Percent.Sand, center = TRUE, scale = TRUE)

datas$MaximumDistancefromPourPoint <- scale(datas$MaximumDistancefromPourPoint, center = TRUE, scale = TRUE)
datas$Disturbance.Value <- scale(datas$Diturbance.Value, center = TRUE, scale = TRUE)


# Subset Data

Y2011 <- subset(datas, Year == "Y2011")
Y2011$Percent.Developed <- scale(Y2011$Percent.Developed, center = TRUE, scale = TRUE)
Y2006 <- subset(datas, Year == "Y2006")
Y2006$Percent.Developed <- scale(Y2006$Percent.Developed, center = TRUE, scale = TRUE)
Y2001 <- subset(datas, Year == "Y2001")
Y2001$Percent.Developed <- scale(Y2001$Percent.Developed, center = TRUE, scale = TRUE)
Y1992 <- subset(datas, Year == "Y1992")
Y1992$Percent.Developed <- scale(Y1992$Percent.Developed, center = TRUE, scale = TRUE)
Y1984 <- subset(datas, Year == "Y1984")
Y1984$Percent.Developed <- scale(Y1984$Percent.Developed, center = TRUE, scale = TRUE)

#############################################################################
# Truncated Regression for 1984

#mod1984.1 <- truncreg(Mean.Flashiness ~ Mean.Slope, point = 2, direction = "right",  data = Y1984)
#summary(mod1984.1)  # Shows the same exact thing as LM

mod1984.1 <- lm(Mean.Flashiness ~ Mean.Slope, data = Y1984)
summary(mod1984.1) # adjusted r2 = 0.04188

mod1984.2 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area, data = Y1984)
summary(mod1984.2) # adjusted r2 = 0.09284

anova(mod1984.1, mod1984.2) # pr(>F): 0.01274

mod1984.3 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Percent.Sand, data = Y1984)
summary(mod1984.3) # adjusted r2 = 0.1809

anova(mod1984.2, mod1984.3) # pr(>F): .001928

mod1984.4 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area  + Percent.Sand + MaximumDistancefromPourPoint, data = Y1984)
summary(mod1984.4) # adjusted r2 = 0.1837

anova(mod1984.3, mod1984.4) # pr(>F): 0.2585

mod1984.5 <- lm(Mean.Flashiness ~ Mean.Slope  + Drainage.Area + Percent.Sand + Ecoregion, data = Y1984)
summary(mod1984.5) # adjusted r2 = 0.5038

anova(mod1984.3, mod1984.5) # pr(>F): 1.025-e08

mod1984.6 <- lm(Mean.Flashiness ~ Mean.Slope  + Drainage.Area + Percent.Sand + Ecoregion + Percent.Developed, data = Y1984)
summary(mod1984.6) # adjusted r2 = 0.7508

anova(mod1984.5, mod1984.6) # pr(>F): 1.165e-13

mod1984.7 <- lm(Mean.Flashiness ~ Mean.Slope + Percent.Sand + Ecoregion + Percent.Developed, data = Y1984)
summary(mod1984.7) # adjusted r2 = 0.719

anova(mod1984.6, mod1984.7) # pr(>F): 0.001252

mod1984.8 <- lm(Mean.Flashiness ~  Mean.Slope + Ecoregion + Percent.Developed, data = Y1984)
summary(mod1984.8) # adjusted r2 = 0.6996

anova(mod1984.7, mod1984.8) # pr(>F): 0.01211

mod1984.9 <- lm(Mean.Flashiness ~ Mean.Slope + Percent.Developed, data = Y1984)
summary(mod1984.9) # adjusted r2 = 0.6381

anova(mod1984.8, mod1984.9) # pr(>F): .001102

mod1984.10 <- lm(Mean.Flashiness ~ Percent.Developed, data = Y1984)
summary(mod1984.10) # adjusted r2 = 0.6422

anova(mod1984.9, mod1984.10) # pr(>F): .9129

anova(mod1984.8, mod1984.10)

mod1984.11 <- lm(Mean.Flashiness ~  Percent.Developed + Mean.Slope, data = Y1984)
summary(mod1984.11) # adjusted r2 = 0.6381

anova(mod1984.10, mod1984.11) # pr(>F): 1.165e-13

########################## ## Best Model for 1984 is Model 6: 
# Includes all variables.  If you want to take out Ecoregion it is still all variables. but mod 11
summary(mod1984.6)
summary(mod1984.11)

##################################################################################
###################################################################################

# 1992


mod1992.1 <- lm(Mean.Flashiness ~ Mean.Slope, data = Y1992)
summary(mod1992.1) # adjusted r2 = 0.1446

mod1992.2 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area, data = Y1992)
summary(mod1992.2) # adjusted r2 = 0.0006715

anova(mod1992.1, mod1992.2) # pr(>F): 0.5216

mod1992.3 <- lm(Mean.Flashiness ~ Mean.Slope + Percent.Sand, data = Y1992)
summary(mod1992.3) # adjusted r2 = 0.04876

anova(mod1992.1, mod1992.3) # pr(>F): 0.04726

mod1992.4 <- lm(Mean.Flashiness ~ Mean.Slope + Percent.Sand + MaximumDistancefromPourPoint, data = Y1992)
summary(mod1992.4) # adjusted r2 = 0.03935

anova(mod1992.3, mod1992.4) # pr(>F): .6585

mod1992.5 <- lm(Mean.Flashiness ~ Mean.Slope + Percent.Sand + Ecoregion, data = Y1992)
summary(mod1992.5) # adjusted r2 = 0.5734

anova(mod1992.3, mod1992.5) # pr(>F): 9.012e-13

mod1992.6 <- lm(Mean.Flashiness ~ Mean.Slope + Percent.Sand + Ecoregion + Percent.Developed, data = Y1992)
summary(mod1992.6) # adjusted r2 = 0.5972

anova(mod1992.5, mod1992.6) # pr(>F): 0.02174

mod1992.7 <- lm(Mean.Flashiness ~ Mean.Slope + Ecoregion + Percent.Developed, data = Y1992)
summary(mod1992.7) # adjusted r2 = 0.5861

anova(mod1992.6, mod1992.7) # pr(>F): 0.08303

mod1992.8 <- lm(Mean.Flashiness ~  Drainage.Area + Percent.Developed, data = Y1992)
summary(mod1992.8) # adjusted r2 = 0.3616

anova(mod1992.6, mod1992.8) # pr(>F): 0.08303

mod1992.9 <- lm(Mean.Flashiness ~  Percent.Developed, data = Y1992)
summary(mod1992.9) # adjusted r2 = 0.3443

nrow(Y1992$Mean.Flashiness) # 85
nrow(Y1992$Percent.Developed)
plot(Y1992$Mean.Flashiness ~Y1992$Percent.Developed)

######################### 
# 1992 : Really bad models.  Best model is with Ecoregion
summary(mod1992.6)
# Without Ecoregion best r2 = .3616
summary(mod1992.8)


#########################################################################################################
########################################################################################################
#########################################################################################################
# 2001


mod2001.1 <- lm(Mean.Flashiness ~ Mean.Slope, data = Y2001)
summary(mod2001.1) # adjusted r2 = 0.07829

mod2001.2 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area, data = Y2001)
summary(mod2001.2) # adjusted r2 = 0.1494

anova(mod2001.1, mod2001.2) # pr(>F): 0.007248

mod2001.3 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Percent.Sand, data = Y2001)
summary(mod2001.3) # adjusted r2 = 0.1738

anova(mod2001.2, mod2001.3) # pr(>F): 0.07292

mod2001.4 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion, data = Y2001)
summary(mod2001.4) # adjusted r2 = 0.5899

anova(mod2001.2, mod2001.4) # pr(>F): 4.9e-11

mod2001.5 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion + Percent.Developed, data = Y2001)
summary(mod2001.5) # adjusted r2 = 0.7859

anova(mod2001.4, mod2001.5) # pr(>F): 7.75e-12

mod2001.6 <- lm(Mean.Flashiness ~ Percent.Sand + Mean.Slope + Drainage.Area + Ecoregion + Percent.Developed, data = Y2001)
summary(mod2001.6) # adjusted r2 = 0.7922

anova(mod2001.5, mod2001.6) # pr(>F): .079

mod2001.7 <- lm(Mean.Flashiness ~  Mean.Slope + Drainage.Area + Percent.Developed, data = Y2001)
summary(mod2001.7) # adjusted r2 = 0.7114

anova(mod2001.5, mod2001.7) # pr(>F): .0001606

mod2001.8 <- lm(Mean.Flashiness ~ Percent.Developed, data = Y2001)
summary(mod2001.8) # adjusted r2 = 0.6702

###################
# For 2001:  Best model is Mod5
summary(mod2001.5)
# Without ecoregion it is mod 7 with r2 of .7114
summary(mod2001.7)

#################################################################################################
#####################################################################################################
#####################################################################################################

# 2006


mod2006.1 <- lm(Mean.Flashiness ~ Mean.Slope, data = Y2006)
summary(mod2006.1) # adjusted r2 = 0.05095

mod2006.2 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area, data = Y2006)
summary(mod2006.2) # adjusted r2 = 0.1403

anova(mod2006.1, mod2006.2) # pr(>F): 0.003

mod2006.3 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Percent.Sand, data = Y2006)
summary(mod2006.3) # adjusted r2 = 0.183

anova(mod2006.2, mod2006.3) # pr(>F): 0.02962

mod2006.4 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Percent.Sand + MaximumDistancefromPourPoint, data = Y2006)
summary(mod2006.4) # adjusted r2 = 0.1719

anova(mod2006.3, mod2006.4) # pr(>F): 0.9

mod2006.5 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Percent.Sand + Percent.Developed, data = Y2006)
summary(mod2006.5) # adjusted r2 = 0.7016

anova(mod2006.3, mod2006.5) # pr(>F): 2.2e-16

mod2006.6 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area +  Percent.Developed, data = Y2006)
summary(mod2006.6) # adjusted r2 = 0.7047

anova(mod2006.5, mod2006.6) # pr(>F): .6247

mod2006.7 <- lm(Mean.Flashiness ~ Percent.Developed, data = Y2006)
summary(mod2006.7) # adjusted r2 = 0.6577




#################
# For 2006, best model is model 6:
summary(mod2006.6)
# WE NO LONGER INCLUDE ECOREGION.	
# Ecoregion is highly correlated with Percent.Developed.  Most development happens in the plains and therefore 
# it is suggesting the high flashiness in the plains is due to that ecoregion, and its actually development. 

#############################################################################################################
########################################################################################################
##########################################################################################################

# 2011

mod2011.1 <- lm(Mean.Flashiness ~ Mean.Slope, data = Y2011)
summary(mod2011.1) # adjusted r2 = 0.0598

mod2011.2 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area, data = Y2011)
summary(mod2011.2) # adjusted r2 = 0.1362

anova(mod2011.1, mod2011.2) # pr(>F): 0.006

mod2011.3 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Percent.Sand, data = Y2011)
summary(mod2011.3) # adjusted r2 = 0.2235

anova(mod2011.2, mod2011.3) # pr(>F): 0.002

mod2011.4 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Percent.Sand + Percent.Developed, data = Y2011)
summary(mod2011.4) # adjusted r2 = 0.7268

anova(mod2011.3, mod2011.4) # pr(>F): 2.2e-16

mod2011.5 <- lm(Mean.Flashiness ~ Mean.Slope + Drainage.Area +  Percent.Developed, data = Y2011)
summary(mod2011.5) # adjusted r2 = 0.7287

anova(mod2011.4, mod2011.5) # pr(>F): .4922

mod2011.6 <- lm(Mean.Flashiness ~ Percent.Developed, data = Y2011)
summary(mod2011.6) # adjusted r2 = 0.683

#################
# For 2011, best model is mod 5
summary(mod2011.5)

##########################################################################
## SUMMARY -- SUMMARY -- SUMMARY -- SUMMARY ###################################
############################################################################
##########################################################################

summary(mod1984.11) # r^2 = .6675	Drainage.Area + Percent.Developed 
summary(mod1992.8)  # r^2 = .3616	Drainage.Area + Percent.Developed
summary(mod2001.7)  # r^2 = .7151	Mean.Slope + Drainage.Area + Percent.Developed
summary(mod2006.6)  # r^2 = .7047	Mean.Slope + Drainage.Area + Percent.Developed
summary(mod2011.5)  # r^2 = .7287	Mean.Slope + Drainage.Area + Percent.Developed


# Just Urbanization
par(mfrow=c(3,2))
summary(mod1984.10) # 0.6463
nrow(Y1984) # n = 89
plot(Y1984$Mean.Flashiness ~ Y1984$Percent.Developed)
summary(mod1992.9) # adjusted r2 = 0.3521
nrow(Y1992) # n = 85
plot(Y1992$Mean.Flashiness ~ Y1992$Percent.Developed)
summary(mod2001.8) # adjusted r2 = 0.6743
nrow(Y2001) n = 81
plot(Y2001$Mean.Flashiness ~ Y2001$Percent.Developed)
summary(mod2006.7) # adjusted r2 = 0.6621
nrow(Y2006) # n = 78
plot(Y2006$Mean.Flashiness ~ Y2006$Percent.Developed)
summary(mod2011.6) # adjusted r2 = 0.687
nrow(Y2011) # n = 80
plot(Y2011$Mean.Flashiness ~ Y2011$Percent.Developed)
#