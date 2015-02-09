######################################################################
#The intent of this project to to take a list of breakdown voltages
#and calculate the breakdown voltages, create a wiebull plot, calculate
#the weibull parameters, and construct a Weibull Distribution
#
#The first line of each file should be the material thickness in microns
#
#The rest of the numbers are the breakdown voltages
######################################################################

library("ggplot2") #for general plotting
library("ggthemes") #for advanced plotting
library("scales") #for more plotting
library("plyr")
library("reshape2") 
library("MASS")
library("car") #for applied regressions

BDfile <- "SampleBreakdownSet.txt"
Cpfile <- "SampleDielectricSet.csv"

BD <- read.table(BDfile, header = FALSE, sep = "")
Cp <- read.table(Cpfile, header = TRUE, sep = ",")

#Extract thickness from the dataset; set to variable t
t = BD[1,1]

#Set a function Calck to calculate the dielectric constant
#from the capacitance measurement
Calck <- function(x) {10*(x*t/1000)/(0.0019*8.85*10^-12)}
  
#Extract the breakdown voltages and toss into dataset, BDV
BDV = tail(BD,(nrow(BD)-1))

#Calculates the breakdown field from the thickness and BDV
BDF = (BDV) / (t*100)

#Calculates the dielectric constant and from k. Sets into dataframe and names columns
k = data.frame(Cp$Frequency, Calck(Cp$Cp.Avg), Calck(Cp$Cp.Stdev), Cp$Loss.Avg, Cp$Loss.Stdev)
names(k) <- c('Freq', 'k.avg', 'k.stdev', 'Loss.avg', 'Loss.stdev')
k[101,2]

#Plots the Dielectric constant and loss
ggplot(k, aes(Freq, k.avg)) +
  geom_smooth(size = 1) +
  ggtitle(paste0("Dielectric data for\n", Cpfile)) +
  scale_y_continuous("Dielectric Constant", limits = c(0,max(k$k.avg)+5)) +
  scale_x_continuous("Frequency / Hz", trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "black"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black"),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16, face = "bold", color = "black"),
        axis.text.y = element_text(size = 14))

ggplot(k, aes(Freq, Loss.avg)) +
  geom_smooth(size = 1) +
  ggtitle(paste0("Dielectric data for\n", Cpfile)) +
  scale_y_continuous("Dielectric Loss", limits = c(0,1), breaks = seq(0,1,0.1)) +
  scale_x_continuous("Frequency / Hz", trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "black"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black"),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16, face = "bold", color = "black"),
        axis.text.y = element_text(size = 14))

# Estimate alpha (scale) and beta (shape) using mean and standard deviation.
beta <- (sd(BDF$V1)/mean(BDF$V1))^(-1.086)
alpha <- mean(BDF$V1)/(gamma(1+1/beta))

#Plots a QQ plot to show the goodness of fit
qqPlot(BDF, distribution="weibull", scale = alpha, shape = beta, 
       las=1, pch=19)

#Cacluates the failure probability for the given breakdown fields, puts the results
#into a dataframe, and plots the result
p <- pweibull(q = BDF$V1, shape = beta, scale = alpha, 
              lower.tail = TRUE, log.p = FALSE)

BDFdf <- data.frame(BDF$V1,p)

ggplot(BDFdf, aes(BDF.V1, p)) +
  geom_smooth(size = 1) +
  ggtitle(paste0("Weibull Distributions for\n", BDfile)) +
  scale_y_continuous("Probability of Failure", labels = percent) +
  scale_x_continuous("Breakdown Field / MV*cm^-1", breaks = seq(0,10,0.5), limits = c(0,max(BDFdf$BDF.V1)+1)) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "black"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black"),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16, face = "bold", color = "black"),
        axis.text.y = element_text(size = 14))

#Makes an energy density function, energy density dataframe and plots the result
EDV <-function (x,y) {8.85E-2*0.5*y*x^2}
  
Energydf <- data.frame(EDV(BDF$V1,k[101,2]),p)
names(Energydf) <- c("EDV","Prob")

ggplot(df, aes(Energydf$EDV, Energydf$Prob)) +
  geom_line(size = 1) +
  ggtitle(paste0("Energy Density for\n", BDfile)) +
  scale_y_continuous("Probability of Failure", labels = percent) +
  scale_x_continuous("Energy Density / J*cm-3", limits=c(0,max(Energydf$EDV)+5)) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold", color = "black"),
        axis.title.x = element_text(size = 16, face = "bold", color = "black"),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16, face = "bold", color = "black"),
        axis.text.y = element_text(size = 14))




