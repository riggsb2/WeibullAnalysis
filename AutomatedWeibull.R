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
library("scales")

filename <- "SampleBreakdownSet.txt"

d <- read.table(filename, header = FALSE, sep = "")

#Extract thickness from the dataset; set to variable t
t = d[1,1]

#Extract the breakdown voltages and toss into dataset, BDV
BDV = tail(d,(nrow(d)-1))

#Calculates the breakdown field from the thickness and BDV
BDF = (BDV) / (t*100)

# Estimate k and c using mean and standard deviation.

k <- (sd(BDF$V1)/mean(BDF$V1))^(-1.086)
c <- mean(BDF$V1)/(gamma(1+1/k))

#Plots a QQ plot to show the goodness of fit
qqPlot(BDF, distribution="weibull", scale = c, shape= k, 
       las=1, pch=19)

#Cacluates the failure probability for a given set of breakdown fields
p <- pweibull(q = BDF$V1, shape = k, scale = c, 
              lower.tail = TRUE, log.p = FALSE)
#Puts breakdown voltages and probabilities of failure in a datafram
df <- data.frame(BDF$V1,p)

#Plots the dataframe
ggplot(df, aes(BDF.V1, p)) +
  geom_smooth() +
  ggtitle(paste0("Weibull Distributions for\n", filename)) +
  scale_y_continuous("Probability of Failure", labels = percent) +
  scale_x_continuous("Breakdown Field / MV*cm^-1", breaks = 1:3) +
  theme(plot.title = element_text(size = 20, face = "bold", color = "black"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black"),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16, face = "bold", color = "black"),
        axis.text.y = element_text(size = 14))
  





