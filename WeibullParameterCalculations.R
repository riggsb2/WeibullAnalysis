library(MASS)  # for "fitdistr"

set.seed(123)
#-----------------------------------------------------------------------------
# Generate 10000 random numbers from a Weibull distribution
# with shape = 1.5 and scale = 1
#-----------------------------------------------------------------------------

rw <- rweibull(10000, shape=1.5, scale=1)

#-----------------------------------------------------------------------------
# 1. Estimate k and c by MLE
#-----------------------------------------------------------------------------

fitdistr(rw, densfun="weibull", lower = 0)
shape         scale   
1.515380298   1.005562356 

#-----------------------------------------------------------------------------
# 2. Estimate k and c using the leas square fit
#-----------------------------------------------------------------------------

n <- 100 # number of bins
breaks <- seq(0, max(rw), length.out=n)

freqs <- as.vector(prop.table(table(cut(rw, breaks = breaks))))
cum.freqs <- c(0, cumsum(freqs)) 

xi <- log(breaks)
yi <- log(-log(1-cum.freqs))

# Fit the linear regression
least.squares <- lm(yi[is.finite(yi) & is.finite(xi)]~xi[is.finite(yi) & is.finite(xi)])
lin.mod.coef <- coefficients(least.squares)

k <- lin.mod.coef[2]
k
1.515115
c <- exp(-lin.mod.coef[1]/lin.mod.coef[2])
c
1.006004

#-----------------------------------------------------------------------------
# 3. Estimate k and c using the median and quartiles
#-----------------------------------------------------------------------------

med <- median(rw)
quarts <- quantile(rw, c(0.25, 0.75))

k <- log(log(0.25)/log(0.75))/log(quarts[2]/quarts[1])
k
1.537766
c <- med/log(2)^(1/k)
c
1.004434

#-----------------------------------------------------------------------------
# 4. Estimate k and c using mean and standard deviation.
#-----------------------------------------------------------------------------

k <- (sd(rw)/mean(rw))^(-1.086)
c <- mean(rw)/(gamma(1+1/k))
k
1.535481
c
1.006938