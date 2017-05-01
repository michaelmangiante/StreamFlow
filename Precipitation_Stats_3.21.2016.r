# Michael Mangiante
# 3/16/2016

# This script is to evaluate precipitation datasets


# Load data

setwd("K:/Streamflow/WorkingFolder_4.14.2015/FinalData")
prc <- read.csv(file = "PrecipData.csv", header = TRUE, sep = ",")
colnames(prc)


require(lme4)
library(ggplot2)
library(sets)

attach(prc)


# subset data to years

y84<- subset(prc, Year >= 1982 & Year <= 1986)
y92<- subset(prc, Year >= 1990 & Year <= 1994)
y01<- subset(prc, Year >= 1999 & Year <= 2003)
y06<- subset(prc, Year >= 2004 & Year <= 2008)
y11<- subset(prc, Year >= 2009 & Year <= 2013)

##############################################################################
# non-Parametric Test between Each of the divisions.
# The only issue is that N

detach(y06)
attach(y84) # no differences in 84
attach(y92) # no differences in 92
attach(y01) # no differences in 01
attach(y06)
attach(y11)

test <- kruskal.test(DE_00, DE_01) # .4309
t.test(DE_00, DE_01) # 
t.test(DE_00, DE_02) #
t.test(DE_00, MD_00) #
t.test(DE_00, MD_01) #
t.test(DE_00, MD_02) #
t.test(DE_00, MD_03) #
t.test(DE_00, MD_04) #
t.test(DE_00, MD_05) #
t.test(DE_00, MD_06) #
t.test(DE_00, MD_07) #
t.test(DE_00, MD_08) # 0.001251
t.test(DE_00, PA_00) #
t.test(DE_00, PA_01) #
t.test(DE_00, PA_02) # 0.005
t.test(DE_00, PA_03) #
t.test(DE_00, PA_04) #
t.test(DE_00, PA_05) #
t.test(DE_00, PA_06) # 0.017
t.test(DE_00, PA_07) #
t.test(DE_00, PA_08) # 0.0285
t.test(DE_00, PA_09) #
t.test(DE_00, PA_10) #
t.test(DE_00, VA_00) #
t.test(DE_00, VA_01) #
t.test(DE_00, VA_02) #
t.test(DE_00, VA_03) #
t.test(DE_00, VA_04) #
t.test(DE_00, VA_05) #
t.test(DE_00, VA_06) #
t.test(DE_00, WV_00) #
t.test(DE_00, WV_01) #
t.test(DE_00, WV_02) #
t.test(DE_00, WV_03) #
t.test(DE_00, WV_04) # 0.0006
t.test(DE_00, WV_05) #
t.test(DE_00, WV_06) # 0.000106


divisions = cbind(DE_00, DE_01, DE_02, MD_00, MD_01, MD_02, MD_03, MD_04, MD_05, MD_06, MD_07, MD_08, 
PA_00, PA_01, PA_02, PA_03, PA_04, PA_05, PA_06, PA_07, PA_08, PA_09, PA_10, VA_00, VA_01, VA_02,
VA_03, VA_04, VA_05, VA_06, WV_00, WV_01, WV_02, WV_03, WV_04, WV_05, WV_06) # 37

divisions2 = c(DE_01, DE_02, MD_00, MD_01, MD_02, MD_03, MD_04, MD_05, MD_06, MD_07, MD_08, 
PA_00, PA_01, PA_02, PA_03, PA_04, PA_05, PA_06, PA_07, PA_08, PA_09, PA_10, VA_00, VA_01, VA_02,
VA_03, VA_04, VA_05, VA_06, WV_00, WV_01, WV_02, WV_03, WV_04, WV_05, WV_06)

vals <- vector(mode = "list", length = length(divisions))

names(vals) <- paste("kruskal_df[[", 1:length(divisions), "]]_df[[", (1:length(divisions))+1, "]]", sep = "")

for (i in 1:(length(divisions)-1)){
vals[[i]] <- t.test(divisions[i], divisions[i+1])[1:3]
}



vals <- c()

mat <- matrix(, nrow = _____, ncol = n.columns)

for (i in divisions)
{
for (k in divisions2)
{
test <- kruskal.test(i, k)
vals <- test$p.value
rbind(i,k, vals)

}
}


###################################################################################################


# Set Margins
par(lheight=1)
par(mar=c(5.1, 5.1, 2.1, 2.1))



precipitation <- plot(NULL, xlim = c(


