
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


# Subset data

Y2011 <- subset(data, Year == "Y2011")
Y2006 <- subset(data, Year == "Y2006")
Y2001 <- subset(data, Year == "Y2001")
Y1992 <- subset(data, Year == "Y1992")
Y1984 <- subset(data, Year == "Y1984")

###################################################################
# GLM for 1984


mod.1984.1 <- glm(Mean.Flashiness ~ Mean.Slope, data=Y1984)
summary(mod.1984.1) # AIC = -8.9838

mod.1984.2 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area, data=Y1984)
summary(mod.1984.2) # AIC = -13.353
anova(mod.1984.1, mod.1984.2, test = "F") # 0.01336

mod.1984.3 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion, data=Y1984)
summary(mod.1984.3) # AIC = -41.813
anova(mod.1984.2, mod.1984.3, test = "F") # 1.113e-06

mod.1984.4 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y1984)
summary(mod.1984.4) # AIC = -59.249
anova(mod.1984.3, mod.1984.4, test = "F") # 3.049e-05

mod.1984.5 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand + MaximumDistancefromPourPoint, data=Y1984)
summary(mod.1984.5) # AIC = -58.555
anova(mod.1984.4, mod.1984.5, test = "F") # 0.2829

## Best model is mod4

mod.1984.6 <- glm(Mean.Flashiness ~ Percent.Developed + Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y1984)
summary(mod.1984.6) # AIC = -118.71
anova(mod.1984.4, mod.1984.6, test = "F") # 1.711e-13

mod.1984.7 <- glm(Mean.Flashiness ~ Percent.Developed + Disturbance.Value + Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y1984)
summary(mod.1984.7) # AIC = -120.96
anova(mod.1984.6, mod.1984.7, test = "F") # 0.0547


## Best total model is mod7

###################################################################
# GLM for 1992


mod.1992.1 <- glm(Mean.Flashiness ~ Mean.Slope, data=Y1992)
summary(mod.1992.1) # AIC = -29.763

mod.1992.2 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area, data=Y1992)
summary(mod.1992.2) # AIC = -28.147
anova(mod.1992.1, mod.1992.2, test = "F") # 0.5442

mod.1992.3 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area  + Ecoregion, data=Y1992)
summary(mod.1992.3) # AIC = -90.863 
anova(mod.1992.2, mod.1992.3, test = "F") # 1.037e-12

mod.1992.4 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y1992)
summary(mod.1992.4) # AIC = -92.685
anova(mod.1992.3, mod.1992.4, test = "F") # 0.02984

mod.1992.5 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand + MaximumDistancefromPourPoint, data=Y1992)
summary(mod.1992.5) # AIC = -91.951
anova(mod.1992.4, mod.1992.5, test = "F") # 0.295

## Best model is mod4

mod.1992.6 <- glm(Mean.Flashiness ~ Percent.Developed + Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y1992)
summary(mod.1992.6) # AIC = -98.333
anova(mod.1992.4, mod.1992.6, test = "F") # 0.01005

mod.1992.7 <- glm(Mean.Flashiness ~ Percent.Developed + Disturbance.Value + Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y1992)
summary(mod.1992.7) # AIC = -103.27
anova(mod.1992.6, mod.1992.7, test = "F") # 0.01486


## Best total model is mod7

###################################################################
# GLM for 2001


mod.2001.1 <- glm(Mean.Flashiness ~ Mean.Slope, data=Y2001)
summary(mod.2001.1) # AIC = -27.731

mod.2001.2 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area, data=Y2001)
summary(mod.2001.2) # AIC = -23.395
anova(mod.2001.1, mod.2001.2, test = "F") # 0.01375

mod.2001.3 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area   + Ecoregion, data=Y2001)
summary(mod.2001.3) # AIC = -27.856
anova(mod.2001.2, mod.2001.3, test = "F") # 1.254e-10

mod.2001.4 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y2001)
summary(mod.2001.4) # AIC = -34.793
anova(mod.2001.3, mod.2001.4, test = "F") # 0.0052

mod.2001.5 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand + MaximumDistancefromPourPoint, data=Y2001)
summary(mod.2001.5) # AIC = -35.112
anova(mod.2001.4, mod.2001.5, test = "F") # 0.1575

## Best model is mod4

mod.2001.6 <- glm(Mean.Flashiness ~ Percent.Developed + Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y2001)
summary(mod.2001.6) # AIC = -79.317
anova(mod.2001.4, mod.2001.6, test = "F") # 2.422e-10

mod.2001.7 <- glm(Mean.Flashiness ~ Percent.Developed + Disturbance.Value + Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y2001)
summary(mod.2001.7) # AIC = -81.193
anova(mod.2001.6, mod.2001.7, test = "F") # 0.06962


## Best total model is mod7

#######################################################################
# GLM for 2006


mod.2006.1 <- glm(Mean.Flashiness ~ Mean.Slope, data=Y2006)
summary(mod.2006.1) # AIC = 30.106

mod.2006.2 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area, data=Y2006)
summary(mod.2006.2) # AIC = 24.714
anova(mod.2006.1, mod.2006.2, test = "F") # .0078

mod.2006.3 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area   + Ecoregion, data=Y2006)
summary(mod.2006.3) # AIC = -16.032
anova(mod.2006.2, mod.2006.3, test = "F") # 1.812e-08

mod.2006.4 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y2006)
summary(mod.2006.4) # AIC = -22.235, r2 = .55
anova(mod.2006.3, mod.2006.4, test = "F") # .008037

mod.2006.5 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand + MaximumDistancefromPourPoint, data=Y2006)
summary(mod.2006.5) # AIC = -21.737
anova(mod.2006.4, mod.2006.5, test = "F") # .2604

## Best model is mod4

mod.2006.6 <- glm(Mean.Flashiness ~ Percent.Developed + Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y2006)
summary(mod.2006.6) # AIC = -65.444
anova(mod.2006.4, mod.2006.6, test = "F") # 6.52e-10


mod.2006.7 <- glm(Mean.Flashiness ~ Percent.Developed + Disturbance.Value + Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y2006)
summary(mod.2006.7) # AIC = -74.174, r2 = .77
anova(mod.2006.6, mod.2006.7, test = "F") # .002821


## Best total model is mod7

#######################################################################
# GLM for 2011


mod.2011.1 <- glm(Mean.Flashiness ~ Mean.Slope, data=Y2011)
summary(mod.2011.1) # AIC = 14.214

mod.2011.2 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area, data=Y2011)
summary(mod.2011.2) # AIC = 9.6528
anova(mod.2011.1, mod.2011.2, test = "F") # 0.0122

mod.2011.3 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area   + Ecoregion, data=Y2011)
summary(mod.2011.3) # AIC = -38.803
anova(mod.2011.2, mod.2011.3, test = "F") # 7.16e-10

mod.2011.4 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y2011)
summary(mod.2011.4) # AIC = -43.899
anova(mod.2011.3, mod.2011.4, test = "F") # 0.01351

mod.2011.5 <- glm(Mean.Flashiness ~ Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand + MaximumDistancefromPourPoint, data=Y2011)
summary(mod.2011.5) # AIC = -44.409
anova(mod.2011.4, mod.2011.5, test = "F") # 0.1447

## Best model is mod4

mod.2011.6 <- glm(Mean.Flashiness ~ Percent.Developed + Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y2011)
summary(mod.2011.6) # AIC = -85.857
anova(mod.2011.4, mod.2011.6, test = "F") # 1.028e-09

mod.2011.7 <- glm(Mean.Flashiness ~ Percent.Developed + Disturbance.Value + Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y2011)
summary(mod.2011.7) # AIC = -91.471
anova(mod.2011.6, mod.2011.7, test = "F") # 0.01167

modsingle <- glm(Mean.Flashiness ~ Percent.Developed, data = Y2011)
plot(modsingle)
anova(modsingle, mod.2011.7,  test = "F") # 6.787e-05
summary(modsingle) # AIC = -70.516

## Best total model is mod7



###### Test Road Area

mod.2011.8 <- glm(Mean.Flashiness ~ WS.Normalized.Road.Area..Sq.km. + Disturbance.Value + Mean.Slope + Drainage.Area + Ecoregion + Percent.Sand, data=Y2011)
summary(mod.2011.8) # AIC = -103.22
anova(mod.2011.7, mod.2011.8, test = "F") # 0.01167

summary(glm(Mean.Flashiness ~ WS.Normalized.Road.Area..Sq.km. + Disturbance.Value  + Mean.Slope + Ecoregion + Percent.Sand, data=Y2011))

plot(Y2011$Mean.Flashiness ~ Y2011$WS.Normalized.Road.Area..Sq.km.)
plot(Y2011$Mean.Flashiness ~ Y2011$Percent.Developed)

test2011 <- Y2011[c("Percent.Developed", "Disturbance.Value", "Mean.Slope", "Drainage.Area", "Ecoregion", "Percent.Sand", "WS.Normalized.Road.Area..Sq.km.")]
pairs(test2011)

############## 
summary(mod.1984.7)
summary(mod.1992.7)
summary(mod.2001.7)
summary(mod.2006.7)
summary(mod.2011.7)
summary(mod.2011.8)

summary(glm(Mean.Flashiness ~ Percent.Developed + Disturbance.Value, data=Y2011))
