# Michael Mangiante
# Piecewise Linear Regression
# Broken Stick Model
# 3/8/2016

# http://people.stat.sfu.ca/~cschwarz/Stat-650/Notes/PDFbigbook-R/R-part018.pdf
#http://stats.stackexchange.com/questions/19772/estimating-the-break-point-in-a-broken-stick-piecewise-linear-model-with-rando/19777
# yi = B0 + B1min(CP, Dayi)
# CP is change point, must use different values for cp to obtain best fit.

# Package SiZeR


require(lme4)
library(ggplot2)
library(multcomp)
library(fitdistrplus)
library(logspline)

plot(Mean.Flashiness ~ Percent.Developed)

### Break Point Analysis




##################################################################################
# http://stackoverflow.com/questions/15874214/piecewise-function-fitting-with-nls-in-r
# Load data
setwd("K:/Streamflow/WorkingFolder_4.14.2015/FinalData/Figures")
data <- read.csv(file="CombinedData_1.6.2016.csv", header = TRUE, sep = ",")
colnames(data)

attach(data)

range_x <- max(Percent.Developed) - min(Percent.Developed)
intervals = 1000
coef1 = c()
coef2 = c()
coef3 = c()
r2 = c()

for (i in 1:intervals)
{
Cx <- min(Percent.Developed) + (i -1) * (range_x/intervals)
lhs <- function(Percent.Developed) ifelse(Percent.Developed < Cx, Cx - Percent.Developed, 0)
rhs <- function(Percent.Developed) ifelse(Percent.Developed < Cx, 0, Percent.Developed - Cx)

fit <- lm(Mean.Flashiness ~ lhs(Percent.Developed) + rhs(Percent.Developed))
coef1[i] <- summary(fit)$coef[1] # intercept estimate
coef2[i] <- summary(fit)$coef[2] # lhs Percent.Developed estimate
coef3[i] <- summary(fit)$coef[3] # rhs Percent.Developed estimate
r2[i] <- summary(fit)$r.squared
}
best_r2<-max(r2)
pos <- which.max(r2)
bestEst <- coef1[pos]
bestInt <- coef2[pos]
best_Cx <- min(Percent.Developed) + (pos - 1)* (range_x/intervals)

plot(Percent.Developed, Mean.Flashiness)
abline(coef1[pos]+best_Cx*coef2[pos], -coef2[pos])
abline(coef1[pos]-best_Cx*coef3[pos],coef3[pos])

best_Cx # .3717
best_r2 # .68
bestEst # LHS = 0.4288
bestInt # -1.2916
coef3[pos] # RHS = 1.29




boxplot(r2)
hist(r2, breaks = 500)

r2.68 <- subset(r2, r2 >= 0.68)
length(r2.68) # 45

r2.68


Cx_max <- min(Percent.Developed) + (pos.max - 1)* (range_x/intervals)
Cx_max # .3717
Cx_min <- min(Percent.Developed) + (pos.min - 1)* (range_x/intervals)
Cx_min # .355

Cx_list = c()

for (i in r2.68)
{
mod.pos <- which(r2 == i)
value <- min(Percent.Developed) + (mod.pos - 1)* (range_x/intervals)
Cx_list <- c(Cx_list, value)
}

Cx_list
min(Cx_list) # 0.355
max(Cx_list) # 0.3976


plot(Percent.Developed, Mean.Flashiness)
LeftY.Inflection <- (coef1[pos]+best_Cx*coef2[pos])+ (-coef2[pos]*best_Cx)
RightY.Inflection <- (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*best_Cx)
segments(0, coef1[pos]+best_Cx*coef2[pos], best_Cx, LeftY.Inflection,  col="red")
segments(best_Cx, RightY.Inflection, max(Percent.Developed), (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*max(Percent.Developed)), col="green")
abline(v = best_Cx)

abline(v = min(Cx_list), col = "red", lty = "dashed")
abline(v = max(Cx_list), col = "red", lty = "dashed")










#############################################
# Remove Outliers

data.out <- subset(data, FlashDevl_Outliers == "")
plot(data.out$Mean.Flashiness ~ data.out$Percent.Developed)
attach(data.out)


range_x <- max(Percent.Developed) - min(Percent.Developed)
intervals = 1000
coef1 = c()
coef2 = c()
coef3 = c()
r2 = c()

for (i in 1:intervals)
{
Cx <- min(Percent.Developed) + (i -1) * (range_x/intervals)
lhs <- function(Percent.Developed) ifelse(Percent.Developed < Cx, Cx - Percent.Developed, 0)
rhs <- function(Percent.Developed) ifelse(Percent.Developed < Cx, 0, Percent.Developed - Cx)

fit <- lm(Mean.Flashiness ~ lhs(Percent.Developed) + rhs(Percent.Developed))
coef1[i] <- summary(fit)$coef[1]
coef2[i] <- summary(fit)$coef[2]
coef3[i] <- summary(fit)$coef[3]
r2[i] <- summary(fit)$r.squared
}
best_r2<-max(r2)
pos <- which.max(r2)
bestEst <- coef1[pos]
bestInt <- coef2[pos]
best_Cx <- min(Percent.Developed) + (pos - 1)* (range_x/intervals)

plot(Percent.Developed, Mean.Flashiness)
abline(coef1[pos]+best_Cx*coef2[pos], -coef2[pos]) # lhs
abline(coef1[pos]-best_Cx*coef3[pos],coef3[pos]) # rhs

best_Cx # .219
best_r2 # .803
bestEst # LHS= 0.44966
bestInt # -0.4392
coef3[pos] # RHS = 0.9586

testing <- subset(data.out, Percent.Developed >= best_Cx)

testmod <- lm(testing$Mean.Flashiness ~ testing$Percent.Developed)
summary(testmod)
summary(testmod)$r.squared
abline(testmod, col="red")

plot(testing$Mean.Flashiness ~ testing$Percent.Developed)

plot(Percent.Developed, Mean.Flashiness)
LeftY.Inflection <- (coef1[pos]+best_Cx*coef2[pos])+ (-coef2[pos]*best_Cx)
RightY.Inflection <- (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*best_Cx)
segments(0, coef1[pos]+best_Cx*coef2[pos], best_Cx, LeftY.Inflection,  col="red")
segments(best_Cx, RightY.Inflection, max(testing$Percent.Developed), (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*max(testing$Percent.Developed)), col="green")
abline(v = best_Cx)


#############################################
# For only year 2011

data.2011 <- subset(data, Year == "Y2011")
plot(data.2011$Mean.Flashiness ~ data.2011$Percent.Developed)
attach(data.2011)


range_x <- max(Percent.Developed) - min(Percent.Developed)
intervals = 1000
coef1 = c()
coef2 = c()
coef3 = c()
r2 = c()

for (i in 1:intervals)
{
Cx <- min(Percent.Developed) + (i -1) * (range_x/intervals)
lhs <- function(Percent.Developed) ifelse(Percent.Developed < Cx, Cx - Percent.Developed, 0)
rhs <- function(Percent.Developed) ifelse(Percent.Developed < Cx, 0, Percent.Developed - Cx)

fit <- lm(Mean.Flashiness ~ lhs(Percent.Developed) + rhs(Percent.Developed))
coef1[i] <- summary(fit)$coef[1]
coef2[i] <- summary(fit)$coef[2]
coef3[i] <- summary(fit)$coef[3]
r2[i] <- summary(fit)$r.squared
}
best_r2<-max(r2)
pos <- which.max(r2)
bestEst <- coef1[pos]
bestInt <- coef2[pos]
best_Cx <- min(Percent.Developed) + (pos - 1)* (range_x/intervals)

plot(Percent.Developed, Mean.Flashiness)
abline(coef1[pos]+best_Cx*coef2[pos], -coef2[pos])
abline(coef1[pos]-best_Cx*coef3[pos],coef3[pos])

best_Cx # .71
best_r2 # .72
bestEst # 0.711
bestInt # -2.09

plot(Percent.Developed, Mean.Flashiness)
LeftY.Inflection <- (coef1[pos]+best_Cx*coef2[pos])+ (-coef2[pos]*best_Cx)
RightY.Inflection <- (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*best_Cx)
segments(0, coef1[pos]+best_Cx*coef2[pos], best_Cx, LeftY.Inflection,  col="red")
segments(best_Cx, RightY.Inflection, max(Percent.Developed), (coef1[pos]-best_Cx*coef3[pos]) + (coef3[pos]*max(Percent.Developed)), col="green")
abline(v = best_Cx)







#################################################################################
##################################################################################################






bp = 10
b1 <- function(x, bp) ifelse(x < bp, bp - x, 0)
b2 <- function(x, bp) ifelse(x < bp, 0, x - bp)

foo <- function(bp)
{
mod <- lm(Mean.Flashiness ~ b1(Percent.Developed, bp) + b2(Percent.Developed, bp), data = data)
deviance(mod)
}

search.range <- c(min(Percent.Developed), max(Percent.Developed))
foo.opt <- optimize(foo, interval = search.range)
bp <- foo.opt$minimum
bp # 0.3714

mod <- lmer(Mean.Flashiness ~ b2(Percent.Developed, bp) + (1|Year), data = data)
summary(mod)

res2 <- c(search.range$minimum, f(search.range$minimum))

best_Cx <- res2[1]
coef1 <- res2[3]
coef2 <- res2[4]
coef3 <- res2[5]
plot(Percent.Developed,Mean.Flashiness)
abline(coef1 + best_Cx*coef2, -coef2) # lhs
abline(coef1-best_Cx*coef3, coef3) #rs


##







# Confidence Interval for the Breakpoint: profile Likelihood (qchisq(0.95,1)) to the minimum deviance

foo.root <- function(bp, tgt)
{
foo(bp) - tgt
}
tgt <- foo.opt$objective + qchisq(0.95, 1)
lb95 <- uniroot(foo.root, lower=search.range[2], upper = bp, tgt = tgt)
ub95 <- uniroot(foo.root, lower = bp, upper = search.range[1], tgt=tgt)
lb95$root
ub95$root




