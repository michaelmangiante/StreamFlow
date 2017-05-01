# Mciahel Mangiante
# Break Point Analysis using linear models only to the right of the line.
# Subset the data to the right of the line through a loop


# Load data
setwd("K:/Streamflow/WorkingFolder_4.14.2015/FinalData/Figures")
data <- read.csv(file="CombinedData_1.6.2016.csv", header = TRUE, sep = ",")
colnames(data)

attach(data)

range_x <- .50
intervals = 1000
coef1 = c()
coef2 = c()
r2 = c() 


for (i in 1:intervals)
{
Cx <- min(Percent.Developed) + (i -1) * (range_x/intervals)
info <- subset(data, Percent.Developed >= Cx)

fit <- lm(info$Mean.Flashiness ~ info$Percent.Developed)

coef1[i] <- summary(fit)$coef[1] # intercept estimate
coef2[i] <- summary(fit)$coef[2] # estimate
r2[i] <- summary(fit)$r.squared
}

best_R2 <- max(r2)
best_R2 # .69
pos <- which.max(r2)
coef1[pos] # 0.16
coef2[pos] # 1.009
best_Cx <- min(Percent.Developed) + (pos - 1)* (range_x/intervals)
best_Cx # 0.134
plot(Mean.Flashiness ~ Percent.Developed)
segments(best_Cx, best_Cx * coef2[pos] + coef1[pos], 100, 100 * coef2[pos] + coef1[pos], col = "green") 

above37 <- subset(data, Percent.Developed >= .37)
above24 <- subset(data, Percent.Developed >= .24)
summary(lm(above37$Mean.Flashiness ~ above37$Percent.Developed)) # .52
summary(lm(above24$Mean.Flashiness ~ above24$Percent.Developed)) # .59
summary(lm(Mean.Flashiness ~ Percent.Developed)) # .62

boxplot(r2)
qqnorm(r2)
hist(r2, breaks = 500)
r2.sub <- subset(r2, r2 >= 0.685)
length(r2.sub)

Cx_list = c()

for (i in r2.sub)
{
mod.pos <- which(r2 == i)
value <- min(Percent.Developed) + (mod.pos - 1)* (range_x/intervals)
Cx_list <- c(Cx_list, value)
}

Cx_list


plot(Percent.Developed, Mean.Flashiness)
LeftY.Inflection <- (coef1[pos]+best_Cx*coef2[pos])+ (-coef2[pos]*best_Cx)
RightY.Inflection <- (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*best_Cx)
segments(0, coef1[pos]+best_Cx*coef2[pos], best_Cx, LeftY.Inflection,  col="red")
segments(best_Cx, RightY.Inflection, max(Percent.Developed), (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*max(Percent.Developed)), col="green")
abline(v = best_Cx)

abline(v = min(Cx_list), col = "red", lty = "dashed")
abline(v = max(Cx_list), col = "red", lty = "dashed")










##########################################################################################################
##########################################################################################################
##########################################################################################################

range_x <- max(Percent.Developed) - min(Percent.Developed)
intervals = 1000
coef1 = c()
coef2 = c()
coef3 = c()
r2 = c()

for (i in 1:intervals)
{
Cx <- min(Percent.Developed) + (i -1) * (range_x/intervals)
rhs <- function(x) ifelse(x < Cx, 0, x - Cx)

fit <- lm(Mean.Flashiness ~ rhs(Percent.Developed))
coef1[i] <- summary(fit)$coef[1] # intercept estimate
coef2[i] <- summary(fit)$coef[2] # lhs Percent.Developed estimate
r2[i] <- summary(fit)$r.squared
}

best_r2<-max(r2)
best_r2 # .67
pos <- which.max(r2)
coef1[pos] # 0.38622
coef2[pos] # 1.323727
best_Cx <- min(Percent.Developed) + (pos - 1)* (range_x/intervals)
best_Cx # 0.3496485

plot(Mean.Flashiness ~ Percent.Developed)
#abline(coef1[pos]-best_Cx*coef2[pos],coef2[pos])
RightY.Inflection <- (coef1[pos]-best_Cx*coef2[pos]) + (coef2[pos]*best_Cx)
segments(best_Cx, RightY.Inflection, max(Percent.Developed), (coef1[pos]-best_Cx*coef2[pos]) + (coef2[pos]*max(Percent.Developed)), col="green")
segments(best_Cx, RightY.Inflection, 0, RightY.Inflection, col = "red")
abline(v = best_Cx)
#segments(best_Cx, best_Cx * coef2[pos] + coef1[pos], 100, 100 * coef2[pos] + coef1[pos], col = "green") 
#


###################################################################################################################
####################################################################################################################
################################### Compute with GLM and find best model using AIC#########################
#####################################################################################################################

range_x <- max(Percent.Developed) - min(Percent.Developed)
intervals = 1000
coef1 = c()
coef2 = c()
coef3 = c()
AIC = c()

for (i in 1:intervals)
{
Cx <- min(Percent.Developed) + (i -1) * (range_x/intervals)
lhs <- function(Percent.Developed) ifelse(Percent.Developed < Cx, Cx - Percent.Developed, 0)
rhs <- function(Percent.Developed) ifelse(Percent.Developed < Cx, 0, Percent.Developed - Cx)

fit <- glm(Mean.Flashiness ~ lhs(Percent.Developed) + rhs(Percent.Developed))

coef1[i] <- summary(fit)$coef[1] # intercept estimate
coef2[i] <- summary(fit)$coef[2] # lhs Percent.Developed estimate
coef3[i] <- summary(fit)$coef[3] # rhs Percent.Developed estimate
AIC[i] <- extractAIC(fit)[2]
}
best_AIC<-min(AIC)
pos <- which.min(AIC)
bestEst <- coef1[pos]
bestInt <- coef2[pos]
best_Cx <- min(Percent.Developed) + (pos - 1)* (range_x/intervals)

plot(Percent.Developed, Mean.Flashiness)
abline(coef1[pos]+best_Cx*coef2[pos], -coef2[pos])
abline(coef1[pos]-best_Cx*coef3[pos],coef3[pos])

best_Cx # 0.3717
best_AIC # -411.2194
bestEst # LHS = 0.4288
bestInt # -0.1542
coef3[pos] # RHS r= 1.29

boxplot(AIC)
hist(AIC, breaks = 300)

AIC.pos <- subset(AIC, AIC <= -410.55 ) #-410.68 = n of 45; right now its the to 5% of AICs
length(AIC.pos) # 97

Cx_list = c()

for (i in AIC.pos)
{
mod.pos <- which(AIC == i)
value <- min(Percent.Developed) + (mod.pos - 1)* (range_x/intervals)
Cx_list <- c(Cx_list, value)
}

Cx_list
min(Cx_list) # 0.352
max(Cx_list) # 0.399


plot(Percent.Developed, Mean.Flashiness)
LeftY.Inflection <- (coef1[pos]+best_Cx*coef2[pos])+ (-coef2[pos]*best_Cx)
RightY.Inflection <- (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*best_Cx)
segments(0, coef1[pos]+best_Cx*coef2[pos], best_Cx, LeftY.Inflection,  col="red")
segments(best_Cx, RightY.Inflection, max(Percent.Developed), (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*max(Percent.Developed)), col="green")
abline(v = best_Cx)

abline(v = min(Cx_list), col = "red", lty = "dashed")
abline(v = max(Cx_list), col = "red", lty = "dashed")

#
summary(glm(Mean.Flashiness ~ Percent.Developed)) # r2 = 0.6278, AIC = -350.24










###################################################################################################################
####################################################################################################################
################################### Compute with GLM and find best model using AIC for one side #########################
#####################################################################################################################

range_x <- .6
intervals = 1000
coef1 = c()
coef2 = c()
AIC = c()

for (i in 1:intervals)
{
Cx <- min(Percent.Developed) + (i -1) * (range_x/intervals)
info <- subset(data, Percent.Developed >= Cx)

fit <- glm(info$Mean.Flashiness ~ info$Percent.Developed)

coef1[i] <- summary(fit)$coef[1] # intercept estimate
coef2[i] <- summary(fit)$coef[2] # Percent.Developed estimate
AIC[i] <- extractAIC(fit)[2]
}
best_AIC<-min(AIC)
pos <- which.min(AIC)
bestEst <- coef1[pos]
bestInt <- coef2[pos]
best_Cx <- min(Percent.Developed) + (pos - 1)* (range_x/intervals)

plot(Percent.Developed, Mean.Flashiness)
abline(coef1[pos]+best_Cx*coef2[pos], -coef2[pos])

plot(Mean.Flashiness ~ Percent.Developed)
segments(best_Cx, best_Cx * coef2[pos] + coef1[pos], 100, 100 * coef2[pos] + coef1[pos], col = "green") 



best_Cx # 0.3717
best_AIC # -411.2194
bestEst # LHS = 0.4288
bestInt # -0.1542
coef3[pos] # RHS r= 1.29

boxplot(AIC)
hist(AIC, breaks = 500)

AIC.pos <- subset(AIC, AIC <= -200 )
length(AIC.pos)

Cx_list = c()

for (i in AIC.pos)
{
mod.pos <- which(AIC == i)
value <- min(Percent.Developed) + (mod.pos - 1)* (range_x/intervals)
Cx_list <- c(Cx_list, value)
}

Cx_list


plot(Percent.Developed, Mean.Flashiness)
LeftY.Inflection <- (coef1[pos]+best_Cx*coef2[pos])+ (-coef2[pos]*best_Cx)
RightY.Inflection <- (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*best_Cx)
segments(0, coef1[pos]+best_Cx*coef2[pos], best_Cx, LeftY.Inflection,  col="red")
segments(best_Cx, RightY.Inflection, max(Percent.Developed), (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*max(Percent.Developed)), col="green")
abline(v = best_Cx)

abline(v = min(Cx_list), col = "red", lty = "dashed")
abline(v = max(Cx_list), col = "red", lty = "dashed")

# fills to the end. Not a good model

