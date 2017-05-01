# Michael Mangiante
# 3/7/2016

# Figured for IALE Presentation
# Figure 1.  Raw Data with over all trendline

# Figure 2. Break point with two trend lines.  Maybe add in SD

# Figure 3. Break piont with two trend lines and potential outliers.  Include 
#	SD from line to pinpoint outliers.


# Load data
setwd("K:/Streamflow/WorkingFolder_4.14.2015/FinalData/Figures")
data <- read.csv(file="CombinedData_1.6.2016.csv", header = TRUE, sep = ",")
colnames(data)



# Subest Data
	# above 24% points

percentage = .134

standarderror = 1.64

highurb <- subset(data, Percent.Developed >= percentage)
lowurb <- subset(data, Percent.Developed < percentage)



####################### Plot 1 ######################################################

nrow(data)

lm1 <- lm(data$Mean.Flashiness ~ data$Percent.Developed)
summary(lm1)


# Set Margins
par(lheight=1)
par(mar=c(5.1, 5.1, 2.1, 2.1))


plot1 <- plot(NULL, xlim=c(0,1), ylim = c(0,1.6), ylab="Mean Richard-Baker Flashiness Index", xlab="Percent Urban Development (%)", cex.axis = 1.4, cex.lab = 1.65)

# Estimate (m)
#m.high <- lm1$coefficients[2]
#m.high
	# Intercept (b)
#b.high <- lm1$coefficients[1]
	# Residual Standard Error
#require(lme4)
#resid.high <- summary(lm1)$sigma
#resid.high

### Standard Deviation Lines:
# Include 

#hmin.2 <- (((m.high)* min(data$Percent.Developed))+b.high)+(resid.high*2)
#hmax.2 <- (((m.high)* max(data$Percent.Developed))+b.high)+(resid.high*2)
#hmin.1.2 <- (((m.high)* min(data$Percent.Developed))+b.high)-(resid.high*2)
#hmax.1.2 <- (((m.high)* max(data$Percent.Developed))+b.high)-(resid.high*2)

#polygon(c(min(data$Percent.Developed),max(data$Percent.Developed),max(data$Percent.Developed), min(data$Percent.Developed)),
# c(hmin.2,hmax.2, hmax.1.2,hmin.1.2), col ="grey95", border= NA)

#segments(min(data$Percent.Developed), hmin.2, max(data$Percent.Developed), hmax.2, col="orange", lty="dashed")
#segments(min(data$Percent.Developed), hmin.1.2, max(data$Percent.Developed), hmax.1.2, col="orange", lty="dashed")

##### One Standard Error


#hmin <- (((m.high)* min(data$Percent.Developed))+b.high)+resid.high
#hmax <- (((m.high)* max(data$Percent.Developed))+b.high)+resid.high
#hmin.1 <- (((m.high)* min(data$Percent.Developed))+b.high)-resid.high
#hmax.1 <- (((m.high)* max(data$Percent.Developed))+b.high)-resid.high

#polygon(c(min(data$Percent.Developed),max(data$Percent.Developed),max(data$Percent.Developed), min(data$Percent.Developed)),
# c(hmin,hmax, hmax.1,hmin.1), col ="grey88", border= NA)

#segments(min(data$Percent.Developed), hmin, max(data$Percent.Developed), hmax, col="red", lty="dashed")
#segments(min(data$Percent.Developed), hmin.1, max(data$Percent.Developed), hmax.1, col="red", lty="dashed")



##################################################

# Add data to plot

alldata <- points(data$Percent.Developed, data$Mean.Flashiness, col = 1, pch = 19)

arrows(data$Percent.Developed, data$Mean.Flashiness - data$MeanFlashines95CI, data$Percent.Developed, data$Mean.Flashiness + data$MeanFlashines95CI,
	 length=0.05, angle =90, code = 3)

### Trendline ###

#high.trend.min <- (m.high* min(data$Percent.Developed))+b.high
#high.trend.max <- (m.high* max(data$Percent.Developed))+b.high
#segments(min(data$Percent.Developed), high.trend.min, max(data$Percent.Developed), high.trend.max, col="blue", lwd=2)

# Legend

par(lheight=.8)

vall.m.all <- format(m.high, digits = 3)
vall.b.all <- format(b.high, digits = 3)
vall.r2.all <- format(summary(lm1)$r.squared, digits = 4)
all.n <- nrow(data)

#text(.12, 1.5, labels= "Richard-Backer Flashiness vs \nPercent Urban Development", font = 7, cex = 1.35)
text(.285, 1.5, labels= "Richard-Backer Flashiness vs Percent Urban Development", font = 7, cex = 1.8)

row.1.all <- substitute(y == m * x + b, list(m = vall.m.all, b = vall.b.all)) 
#text(.12, 1.35, labels = row.1.all, cex = 1.25)

row.2.all <- bquote(R^2 == .(vall.r2.all))
#text(.12, 1.29, labels = row.2.all, cex = 1.25)

row.3.all <- substitute(Count == x, list(x = all.n))
#text(.12, 1.23, labels = row.3.all, cex = 1.25)


vall.se.all <- format(resid.high, digits = 4)
row.4.all <- substitute(SE == s, list(s = vall.se.all))
#text(.12, 1.17, labels = row.4.all, cex = 1.25)


par(lheight=1)
#polygon(c(.8,.82,.82,.8),c(.35,.35,.3,.3), col ="grey88", border= NA)
#segments(.8, .35, .82, .35, col = "red", lty="dashed")
#segments(.8, .3,.82,.3, col="red", lty="dashed") 

#vall.se.h <- format(resid.high, digits = 4)
#se.text.1 <- bquote(bold("+/- 1 SE: "==.(vall.se.h)))
#text(.895, .325, labels = se.text.1, col="red", cex = 1.25)

#polygon(c(.8,.82,.82,.8),c(.27,.27,.22,.22), col ="grey95", border= NA)
#segments(.8, .27, .82, .27, col = "orange", lty="dashed")
#segments(.8, .22,.82,.22, col="orange", lty="dashed") 

#vall.se.2.h <- format(resid.high*2, digits = 4)
#se.2.text.1 <- bquote(bold("+/- 2 SE: "==.(vall.se.2.h)))
#text(.895, .245, labels = se.2.text.1, col="orange", cex = 1.25)

text(.895, .245, labels = "95% CI Error Bars", cex = 1.4)
text(.895, .18, labels = row.3.all, cex = 1.4)

########################################################################################################
########################################################################################################
########################## End of Graph 1 ##############################################################
##########################################################################################################
########################################################################################################

# Set Margins
par(lheight=1)
par(mar=c(5.1, 5.1, 2.1, 2.1))


percentage = .37
standarderror = 1.64 # 1.44 = 85%, 1.64 = 90%
high.percentage = 0.399 
low.percentage = 0.352


highurb <- subset(data, Percent.Developed >= percentage)
lowurb <- subset(data, Percent.Developed < percentage)



# Linear Model

above25.fit <- lm(highurb$Mean.Flashiness ~ highurb$Percent.Developed)
summary(above25.fit)

# Open empty plot
	# ylim = 1.6
	# xlim = 1

flashurb <- plot(NULL, xlim=c(0,1), ylim = c(0,1.6), ylab="Mean Richard-Baker Flashiness Index", xlab="Percent Urban Development (%)", cex.axis = 1.4, cex.lab = 1.65)


################ Standard Error Lines/Polygon###################
# Add error box around plot: SE = (Estimate * Percent Developed)+ Intercept) +/- Residual Standard Error
# SE >25%

#prd <- predict(above25.fit, newdata = data.frame(x = highurb$Percent.Developed), interval = c("confidence"),
#	level = 0.90, type ="response")
#lines(highurb$Percent.Developed, prd[,2], col="red", lty=2)
#lines(highurb$Percent.Developed, prd[,3], col="red", lty=2)

	# Estimate (m)
m.high <- above25.fit$coefficients[2]
m.high
	# Intercept (b)
b.high <- above25.fit$coefficients[1]
	# Residual Standard Error
require(lme4)
resid.high <- summary(above25.fit)$sigma
resid.high

### Two Standard Deviation Line
# Residual Standard error in R I think actually refers to the estimated standard error of the model...

hmin.2 <- (((m.high)* min(highurb$Percent.Developed))+b.high)+(resid.high*standarderror)
hmax.2 <- (((m.high)* max(highurb$Percent.Developed))+b.high)+(resid.high*standarderror)
hmin.1.2 <- (((m.high)* min(highurb$Percent.Developed))+b.high)-(resid.high*standarderror)
hmax.1.2 <- (((m.high)* max(highurb$Percent.Developed))+b.high)-(resid.high*standarderror)

polygon(c(high.percentage,max(highurb$Percent.Developed),max(highurb$Percent.Developed), high.percentage),
 c(hmin.2,hmax.2, hmax.1.2,hmin.1.2), col ="grey95", border= NA)

segments(high.percentage, hmin.2, max(highurb$Percent.Developed), hmax.2, col="orange", lty="dashed")
segments(high.percentage, hmin.1.2, max(highurb$Percent.Developed), hmax.1.2, col="orange", lty="dashed")

##### One Standard Error


hmin <- (((m.high)* min(highurb$Percent.Developed))+b.high)+resid.high
hmax <- (((m.high)* max(highurb$Percent.Developed))+b.high)+resid.high
hmin.1 <- (((m.high)* min(highurb$Percent.Developed))+b.high)-resid.high
hmax.1 <- (((m.high)* max(highurb$Percent.Developed))+b.high)-resid.high

#polygon(c(min(highurb$Percent.Developed),max(highurb$Percent.Developed),max(highurb$Percent.Developed), min(highurb$Percent.Developed)),
# c(hmin,hmax, hmax.1,hmin.1), col ="grey88", border= NA)

#segments(min(highurb$Percent.Developed), hmin, max(highurb$Percent.Developed), hmax, col="red", lty="dashed")
#segments(min(highurb$Percent.Developed), hmin.1, max(highurb$Percent.Developed), hmax.1, col="red", lty="dashed")



##################################################

# Add data to plot

above25 <- points(highurb$Percent.Developed, highurb$Mean.Flashiness, col =1, pch=19)

arrows(highurb$Percent.Developed, highurb$Mean.Flashiness - highurb$MeanFlashines95CI, highurb$Percent.Developed, highurb$Mean.Flashiness + highurb$MeanFlashines95CI,
	 length=0.05, angle =90, code = 3)
	# Above 25% trendline


### Trendline ###

high.trend.min <- (m.high* high.percentage)+b.high
high.trend.max <- (m.high* max(highurb$Percent.Developed))+b.high
segments(high.percentage, high.trend.min, max(highurb$Percent.Developed), high.trend.max, col="blue", lwd=2)




############################## Bellow 25% points#########
# Linear Model

bellow25.fit <- lm(lowurb$Mean.Flashiness ~ lowurb$Percent.Developed)
summary(bellow25.fit)



################ Standard Error Lines/Polygon###################
# Add error box around plot: SE = (Estimate * Percent Developed)+ Intercept) +/- Residual Standard Error
# SE < 25%

	# Estimate (m)
m.low <- bellow25.fit$coefficients[2]
m.low
	# Intercept (b)
b.low <- bellow25.fit$coefficients[1]
	# Residual Standard Error

resid.low <- summary(bellow25.fit)$sigma
resid.low


### Two Standard Deviation Line
# Residual Standard error in R I think actually refers to the estimated standard error of the model...

lmin.2 <- (((m.low)* min(lowurb$Percent.Developed))+b.low)+(resid.low*standarderror)
lmax.2 <- (((m.low)* max(lowurb$Percent.Developed))+b.low)+(resid.low*standarderror)
lmin.1.2 <- (((m.low)* min(lowurb$Percent.Developed))+b.low)-(resid.low*standarderror)
lmax.1.2 <- (((m.low)* max(lowurb$Percent.Developed))+b.low)-(resid.low*standarderror)

polygon(c(min(lowurb$Percent.Developed),low.percentage,low.percentage, min(lowurb$Percent.Developed)),
 c(lmin.2,lmax.2, lmax.1.2,lmin.1.2), col ="grey95", border= NA)

segments(min(lowurb$Percent.Developed), lmin.2, low.percentage, lmax.2, col="orange", lty="dashed")
segments(min(lowurb$Percent.Developed), lmin.1.2, low.percentage, lmax.1.2, col="orange", lty="dashed")

####### 1 Standard Deviation

lmin <- (((m.low)* min(lowurb$Percent.Developed))+b.low)+resid.low
lmax <- (((m.low)* max(lowurb$Percent.Developed))+b.low)+resid.low
lmin.1 <- (((m.low)* min(lowurb$Percent.Developed))+b.low)-resid.low
lmax.1 <- (((m.low)* max(lowurb$Percent.Developed))+b.low)-resid.low

#polygon(c(min(lowurb$Percent.Developed),max(lowurb$Percent.Developed),max(lowurb$Percent.Developed), min(lowurb$Percent.Developed)),
# c(lmin,lmax, lmax.1,lmin.1), col ="grey88", border= NA)

#segments(min(lowurb$Percent.Developed), lmin, max(lowurb$Percent.Developed), lmax, col="red", lty="dashed")
#segments(min(lowurb$Percent.Developed), lmin.1, max(lowurb$Percent.Developed), lmax.1, col="red", lty="dashed")
##################################################

# Add data to plot

bellow25 <- points(lowurb$Percent.Developed, lowurb$Mean.Flashiness, col =1, pch=19)

arrows(lowurb$Percent.Developed, lowurb$Mean.Flashiness - lowurb$MeanFlashines95CI, lowurb$Percent.Developed, lowurb$Mean.Flashiness + lowurb$MeanFlashines95CI,
	 length=0.05, angle =90, code = 3)

	# Bellow 25% trendline


### Trendline ###

low.trend.min <- (m.low* min(lowurb$Percent.Developed))+b.low
low.trend.max <- (m.low* low.percentage)+b.low
segments(min(lowurb$Percent.Developed), low.trend.min, low.percentage, low.trend.max, col="blue", lwd=2)

# breakpoint


polygon(c(low.percentage, low.percentage, high.percentage, high.percentage), c(-.1, 1.7, 1.7, -.1), density = c(10,20), angle = c(-45, 54), col = "grey50")


#segments(low.percentage, -.1, low.percentage, 1.7, col = "red", lty = "dashed", lwd = 1)
#segments(high.percentage, -.1, high.percentage, 1.7, col = "red", lty = "dashed", lwd = 1)


segments(percentage, -.1 , percentage, 1.7, col="green", lty="solid", lwd=2)

# Labels

par(lheight=1)
#polygon(c(.8,.82,.82,.8),c(.35,.35,.3,.3), col ="grey88", border= NA)
#segments(.8, .35, .82, .35, col = "red", lty="dashed")
#segments(.8, .3,.82,.3, col="red", lty="dashed") 

#vall.se.h <- format(resid.high, digits = 4)
#se.text.1 <- bquote(bold("+/- 1 SE: "==.(vall.se.h)))
#text(.895, .325, labels = se.text.1, col="red")

polygon(c(.8,.82,.82,.8),c(.27,.27,.22,.22), col ="grey95", border= NA)
segments(.8, .27, .82, .27, col = "orange", lty="dashed")
segments(.8, .22,.82,.22, col="orange", lty="dashed") 

vall.se.2.h <- format(resid.high*standarderror, digits = 4)
se.2.text.1 <- bquote("+/- 1.64 SE: "==.(vall.se.2.h)) ###########
text(.91, .245, labels = se.2.text.1, cex = 1.4) # , col="orange"

segments(.8, .16, .82, .16, col = "green", lwd=2)
text(.91, .16, labels = " Breakpoint Estimate", cex = 1.4)

polygon(c(.77, .79, .79, .77), c(.11, .11, .06, .06), density = c(10,20), angle = c(-45, 54), col = "grey50")
text(.91, .08, labels = " Estimated Breakpoint Area", cex = 1.4)
par(lheight=.5)

vall.m.l <- format(m.low, digits = 3)
vall.b.l <- format(b.low, digits = 3)
vall.r2.l <- format(summary(bellow25.fit)$r.squared, digits = 4)
low.n <- nrow(lowurb)

text(.14, 1.5, labels= "For Sites Where Percent Urban < 37%", cex = 1.4)

row.1.tx <- substitute(y == m * x + b, list(m = vall.m.l, b = vall.b.l)) 
text(.14, 1.45, labels = row.1.tx, cex = 1.4)

row.2.tx <- bquote(R^2 == .(vall.r2.l))
text(.14, 1.4, labels = row.2.tx, cex = 1.4)

row.3.tx <- substitute(count == x, list(x = low.n))
text(.14, 1.35, labels = row.3.tx, cex = 1.4)

vall.se.l <- format(resid.low, digits = 4)
row.4.tx <- substitute(SE == s, list(s = vall.se.l))
text(.14, 1.3, labels = row.4.tx, cex = 1.4)

# Above 

vall.m.h <- format(m.high, digits = 3)
vall.b.h <- format(b.high, digits = 3)
vall.r2.h <- format(summary(above25.fit)$r.squared, digits = 4)
high.n <- nrow(highurb)

text(.6, 1.5, labels= "For Sites Where Percent Urban >= 37%", cex = 1.4)

row.up.1.tx <- substitute(y == m * x + b, list(m = vall.m.h, b = vall.b.h)) 
text(.6, 1.45, labels = row.up.1.tx, cex = 1.4)

row.up.2.tx <- bquote(R^2 == .(vall.r2.h))
text(.6, 1.4, labels = row.up.2.tx, cex = 1.4)

row.up.3.tx <- substitute(count == x, list(x = high.n))
text(.6, 1.35, labels = row.up.3.tx, cex = 1.4)

row.up.4.tx <- substitute(SE == s, list(s = vall.se.h))
text(.6, 1.3, labels = row.up.4.tx, cex = 1.4)

#######################################################################################################################
################################# END PLOT 2 #########################################################################
########################################################################################################################



