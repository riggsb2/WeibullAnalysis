# Load packages

library(MASS)
library(car)


# First, we generate 1000 random numbers from a Weibull distribution with
# scale = 1 and shape = 1.5

rw <- rweibull(1000, scale=1, shape=1.5)

# We can calculate a kernel density estimation to inspect the distribution
# Because the Weibull distribution has support [0,+Infinity), we are truncate
# the density at 0

par(bg="white", las=1, cex=1.1)
plot(density(rw, bw=0.5, cut=0), las=1, lwd=2,
     xlim=c(0,5),col="steelblue")

# Now, we can use fitdistr to calculate the parameters by MLE
# The option "lower = 0" is added because the parameters of the Weibull distribution need to be >= 0

fitdistr(rw, densfun="weibull", lower = 0)


#Compare the input scale and shape parameters to fitted with a qqplot
qqPlot(rw, distribution="weibull", scale = 1, shape= 1.5, las=1, pch=19)
