setwd("K:/Streamflow/WorkingFolder_4.14.2015/FinalData")


data <- read.csv(file = "AggregatedData_SitesWithDevl_11_23_2015.csv", header = TRUE, sep = ",")

attach(data)
colnames(data)
plot(Percent.Developed, Mean.Flashiness)
plot(Percent.Developed, Mean.Flow)

plot(Percent.Developed, WS.Normalized.Road.Area..Sq.km.)
test <- glm(Percent.Developed~WS.Normalized.Road.Area..Sq.km.)
summary(test)

plot(WS.Normalized.Road.Area..Sq.km., Mean.Flashiness)
plot(WS.Normalized.Road.Area..Sq.km., Mean.Flow)
plot(WS.Normalized.Road.Area..Sq.km., Q5..Average.Annual)


pairs(data)
summary(data)
colnames(data)

myvars2 <- c(3,7,10,12,13,14,15,16,17,18,20,21,22,23,24,25,26,27,28,29,30,31)

myvars <- c("Year", "Mean.Flashiness", "Ecoregion.Gradient..", "Ecoregion", "Percent.Developed", "L1.Urban.Gradient..", 
"Mean.Flow", "Drainage.Area", "Mean.Slope", "Percent.Sand", "Percent.Silt", "Percent.Clay", 
"Distance.To.Dam..m.", "Distance.to.Obstruction..m.", "Number.of.Dams..from.other.dataset.", 
"Disturbance.Value", "Q5..Average.Annual", "Q95.. Average.Annual", "X7Q2..non.normalized", 
"X7Q2..Normalized.to.Watershed.Size", "Stream.Area..sq.KM.", "Stream.Length..m.",
"Road.Area..sq.Km.", "WS.Normalized.Stream.Area..Sq.KM.", "WS.Normalized.Road.Area..Sq.km.",
"WS.Normalized.Stream.Length..km.")

statdata <- data[myvars2]

pairs(statdata)

attach(statdata)

mod1 <- glm(Mean.Flashiness ~ Percent.Developed * Drainage.Area * Mean.Slope *  
Distance.to.Dam..m. * Distance.to.Obstruction..m. * Number.of.Dams..from.other.dataset. * WS.Normalized.Road.Area..Sq.km.)

mod2 <- glm(Mean.Flashiness ~ Percent.Developed)
summary(mod2)
plot(mod2)

mod3 <- glm(Mean.Flashiness ~ WS.Normalized.Road.Area..Sq.km.)
summary(mod3)
plot(mod3)

mod4 <- glm(Mean.Flashiness ~ WS.Normalized.Road.Area..Sq.km. * Percent.Developed)
summary(mod4)
plot(mod4)

mod5 <- glm(Mean.Flashiness ~ WS.Normalized.Road.Area..Sq.km. * Percent.Sand)
summary(mod5)
plot(mod5)

mod6 <- glm(Mean.Flashiness ~ WS.Normalized.Road.Area..Sq.km. * Percent.Sand * Stream.Area..sq.KM.)
summary(mod6)
plot(mod6)