######################################################################
#The intent of this project to to take a list of breakdown voltages
#and calculate the breakdown voltages, create a wiebull plot, calculate
#the weibull parameters, and construct a Weibull Distribution
#
#The first line of each file should be the material thickness in microns
#
#The rest of the numbers are the breakdown voltages
######################################################################

library("ggplot2")
library("plyr")
library("ggthemes")
library("reshape2")
library("MASS")
library("car")
library("survival")

filename <- "SampleBreakdownSet.txt"

d <- read.table(filename, header = FALSE, sep = "")

#Extract thickness from the dataset; set to variable t
t = d[1,1]

#Extract the breakdown voltages and toss into dataset, BDV
BDV = tail(d,(nrow(d)-1))

#Calculates the breakdown field from the thickness and BDV
BDF = (BDV*10000) / t


# Estimate k and c using mean and standard deviation.

k <- (sd(BDF$V1)/mean(BDF$V1))^(-1.086)
c <- mean(BDF$V1)/(gamma(1+1/k))
k
c

#Plots a QQ plot to show the goodness of fit
qqPlot(BDF, distribution="weibull", scale = c, shape= k, 
       las=1, pch=19)

#Constructs the Weibull plot from the shape and scale parameter
plot(dweibull(x = BDF$V1, shape = k, scale = 1.97e6, log = FALSE))
pweibull(q = BDF, shape = 34, scale = 1, lower.tail = TRUE, log BDF= FALSE)



