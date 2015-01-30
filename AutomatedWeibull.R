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


filename <- "SampleBreakdownSet.txt"

d <- read.table(filename, header = FALSE, sep = "")
head(d)
tail(d)
summary(d)
nrow(d)

#Extract thickness from the dataset; set to variable t
t = head(d,1)
t
t[1,1]

#Extract the breakdown voltages and toss into dataset, BDV
BDV = tail(d,(nrow(d)-1))
BDV

#Calculates the breakdown field from the thickness and BDV
BDF = BDV / t[1,1]
BDF

