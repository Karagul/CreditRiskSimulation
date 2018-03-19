require(plyr)
require(MASS)
require(fitdistrplus)
require(logspline)
library(foreach)
library(rootSolve)
library(doParallel)
registerDoParallel(detectCores()-1)


df = read.table("/home/jh0iku/Documents/q2/2df30/assignment2.txt")

colnames(df) <- c("exposure")
head(df)

#working directory problems
getwd()
setwd()

#Question 1
nrow(df)
summary(df$exposure)
outlier <- boxplot.stats(df$exposure)$out
index <- which(df$exposure %in% outlier)

#scatter plot as overview
plot(df$exposure, main="overview", ylab='exposure')

#Outliers
n_outliers <- 0
for (i in index){
    print( df[i,])
    n_outliers <- n_outliers + 1
}

df2 <- df
for (i in index){
    df2[i,] <- NA
}
df2<-na.omit(df2)
hist(df2$exposure, breaks=100)
boxplot(df2$exposure)
#summary without outliers
summary(df2$exposure)


#using fitdistrplus
plot(df2n$exposure, pch=20)
plotdist(df2n$exposure, histo = TRUE, demp = TRUE)
descdist(df2n$exposure, discrete = FALSE, boot=5000)
fit_ln <-fitdist(df2n$exposure, "lnorm")
fit_g <-fitdist(df2n$exposure, "gamma")
fit_b <- fitdist(df2n$exposure, "beta")

par(mfrow=c(2,2))
plot.legend <- c("beta", "lognormal", "gamma")
denscomp(list(fit_b, fit_g, fit_ln), legendtext = plot.legend, breaks=100)
cdfcomp (list(fit_b, fit_g, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_b, fit_g, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_b, fit_g, fit_ln), legendtext = plot.legend)

#Goodness of fit test
gofstat(list(fit_b, fit_g, fit_ln), fitnames = c("beta", "gamma", "lognormal"))
#conclusion is gamma

#Question 2
gammaval <- 0.995/0.005

#Question 3
#Mixed poisson model
run <- 100000
n <- length(df2$exposure)
EAD <- df2$exposure
LGD <- 0.62
mPoisson <- function(LAMBDA,n){
  Xi <- rpois(length(df2$exposure), LAMBDA) >= 1
  return(sum(Xi))
}

LAMBDAS <- rexp(run, gammaval)

ndefaults <- foreach(LAMBDA = LAMBDAS,.combine='c')%do%{
  mPoisson(LAMBDA)
}

ndeffac <- as.factor(ndefaults)
summary(ndeffac)[1:21]/run
hist(ndefaults, main = "number of defaults")
summary(ndefaults)
#Question 4
#Question 5
lossrealisation <- function(LAMBDA,n){
  Xi <- rpois(length(df2$exposure), LAMBDA) >= 1
  Ln <- Xi * EAD * LGD
  return(sum(Ln))
}
LAMBDAS <- rexp(run, gammaval)
SLn <- foreach(LAMBDA = LAMBDAS,.combine='c')%do%{
  lossrealisation(LAMBDA)
}


#expected loss
EL <- mean(SLn)
#unexpected loss
UL <- sd(SLn)

# empirical distribution
losses <- c(0, sort(sigLOSS))
y <- (0:run)/run
#value at risk
a <- 0.95
j <- floor(a*run)
VaR <- losses[j]
#expected shortfall (tail conditional expectation)
TCE <- mean(SLn[SLn>VaR])
TCE

# Histogram of sigLOSS
hist(SLn, freq=FALSE, col="blue", breaks=100)

# empirical distribution graph
plot(losses, y, type="l", col="blue")

# show Value-at-Risk in distribution
abline(h=a, col="red")
abline(v=VaR, col="red")

#Question 6
Yi <- rgamma(1,shape = 7.65 ,rate = 15.34)


#Question 7
#step1:question3
mPoissonUni <- function(unilamb, n){
  Xi_n <- rpois(length(df2$exposure), unilamb) >= 1
  return(sum(Xi_n))
}
b <- 0.0100335
unilambs <- runif(run, 0, b) 
unidefaults <- foreach(unilamb = unilambs,.combine = c)%do%{
  mPoissonUni(unilamb)
}
unidefaults_fac <- as.factor(unidefaults)
summary(unidefaults_fac)[1:21]/run

#step2:question5
lossrealisation2 <- function(unilamb, n){
  Xi_n <- rpois(length(df2$exposure), unilamb) >= 1
  Ln <- Xi_n*EAD*LGD
  return(sum(Ln))
}
unilambs <- runif(run, 0, b)
SLn <- foreach(unilamb = unilambs,.combine = c)%do%{
  lossrealisation2(unilamb)
}

#expected loss
EL2 <- mean(SLn)
#unexpected loss
UL2 <- sd(SLn)

# empirical distribution
losses2 <- c(0, sort(SLn))
y2 <- (0:run)/run

#value at risk
a <- 0.95
j2 <- floor(a*run)
VaR2 <- losses2[j2]
# show Value-at-Risk in distribution
plot(losses2, y, type="l", col="blue")
abline(h=a, col="red")
abline(v=VaR2, col="red")
#expected shortfall (tail conditional expectation)
TCE2 <- mean(SLn[SLn>VaR2])
TCE2
# Histogram of sigLOSS
hist(SLn, freq=FALSE, col="blue", breaks=100)


