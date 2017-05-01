# Michael Mangiante
# 1/30/2017

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

#yearlist = (Y1984, Y1992, Y2001, Y2006, Y2011)# could automate with this

# Truncated Regression for 1984
#mod1 <- lm(Mean.Flashiness ~ Percent.Developed + Mean.Flow + Drainage.Area + MaximumDistancefromPourPoint
# 	+ Mean.Slope + Percent.Sand + Distance.to.Dam..m. + Distance.to.Obstruction..m. + Disturbance.Value + 
#	Stream.Length..m., data = Y1984)
mod1 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope + Percent.Sand+ MaximumDistancefromPourPoint, data = Y1984)
summary(mod1)

mod2 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope + Percent.Sand, data = Y1984)
summary(mod2)

anova(mod1,mod2)

mod3 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope, data = Y1984)
summary(mod3)

anova(mod2,mod3)
anova(mod1,mod3)

mod4 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area, data = Y1984)
summary(mod4)

anova(mod3,mod4)
anova(mod2,mod4)
anova(mod1, mod4)

mod5 <- lm(Mean.Flashiness ~ Percent.Developed, data = Y1984)
summary(mod5)

anova(mod4,mod5)
anova(mod3,mod5)
anova(mod2,mod5)
anova(mod1, mod5)

mod6 <- lm(Mean.Flashiness ~ Percent.Developed + Mean.Slope, data = Y1984)
summary(mod6)

anova(mod3,mod6)

################# 1992 #####################################
year = Y1992
mod1 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope + Percent.Sand+ MaximumDistancefromPourPoint, data = year)
summary(mod1)

mod2 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope + Percent.Sand, data = year)
summary(mod2)

anova(mod1,mod2)

mod3 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope, data = year)
summary(mod3)

anova(mod2,mod3)
anova(mod1,mod3)

mod4 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area, data = year)
summary(mod4)

anova(mod3,mod4)
anova(mod2,mod4)
anova(mod1, mod4)

mod5 <- lm(Mean.Flashiness ~ Percent.Developed, data = year)
summary(mod5)

anova(mod4,mod5)
anova(mod3,mod5)
anova(mod2,mod5)
anova(mod1, mod5)

mod6 <- lm(Mean.Flashiness ~ Percent.Developed + Mean.Slope, data = year)
summary(mod6)

anova(mod3,mod6)

################# 2001 #####################################
year = Y2001
mod1 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope + Percent.Sand+ MaximumDistancefromPourPoint, data = year)
summary(mod1)

mod2 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope + Percent.Sand, data = year)
summary(mod2)

anova(mod1,mod2)

mod3 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope, data = year)
summary(mod3)

anova(mod2,mod3)
anova(mod1,mod3)

mod4 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area, data = year)
summary(mod4)

anova(mod3,mod4)
anova(mod2,mod4)
anova(mod1, mod4)

mod5 <- lm(Mean.Flashiness ~ Percent.Developed, data = year)
summary(mod5)

anova(mod4,mod5)
anova(mod3,mod5)
anova(mod2,mod5)
anova(mod1, mod5)

mod6 <- lm(Mean.Flashiness ~ Percent.Developed + Mean.Slope, data = year)
summary(mod6)

anova(mod3,mod6)
################# 2006 #####################################
year = Y2006
mod1 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope + Percent.Sand+ MaximumDistancefromPourPoint, data = year)
summary(mod1)

mod2 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope + Percent.Sand, data = year)
summary(mod2)

anova(mod1,mod2)

mod3 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope, data = year)
summary(mod3)

anova(mod2,mod3)
anova(mod1,mod3)

mod4 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area, data = year)
summary(mod4)

anova(mod3,mod4)
anova(mod2,mod4)
anova(mod1, mod4)

mod5 <- lm(Mean.Flashiness ~ Percent.Developed, data = year)
summary(mod5)

anova(mod4,mod5)
anova(mod3,mod5)
anova(mod2,mod5)
anova(mod1, mod5)

mod6 <- lm(Mean.Flashiness ~ Percent.Developed + Mean.Slope, data = year)
summary(mod6)

anova(mod3,mod6)
################# 2011 #####################################
year = Y2011
mod1 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope + Percent.Sand+ MaximumDistancefromPourPoint, data = year)
summary(mod1)

mod2 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope + Percent.Sand, data = year)
summary(mod2)

anova(mod1,mod2)

mod3 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area + Mean.Slope, data = year)
summary(mod3)

anova(mod2,mod3)
anova(mod1,mod3)

mod4 <- lm(Mean.Flashiness ~ Percent.Developed + Drainage.Area, data = year)
summary(mod4)

anova(mod3,mod4)
anova(mod2,mod4)
anova(mod1, mod4)

mod5 <- lm(Mean.Flashiness ~ Percent.Developed, data = year)
summary(mod5)

anova(mod4,mod5)
anova(mod3,mod5)
anova(mod2,mod5)
anova(mod1, mod5)

mod6 <- lm(Mean.Flashiness ~ Percent.Developed + Mean.Slope, data = year)
summary(mod6)

anova(mod3,mod6)