### Code that was used for the Chandler Lake OP in 2017
### Annotations in 2024 as best as I can figure out why I did what I did!!


## Chandler lake area
newlake_la <- 3717  # Crosswind
# newlake_la <- 1300+317  # Chandler


## all lake areas reported
la <- c(3582.1,97.8,71.7,633.8,126.3,2014.6,195.1,2226.3,3798.5,39.1,87.4,124,169,365.2,196.2,1174.5,6479.2,172.2,204,101.9,195.1,42.9,67.7,62.4,28.6,23.5,716.4,36.2,143.3,189.3,244.1,291.4,221,278.2,167.9,144.1,1388.5,2519.1,115.7,22.4,167.3,1730.5,137.6,90.3,441.9,483.2,93.8,189.7,26.1,26.4,402.3,1061.5,165.9,80.9,2079.9,975.4,229.5,152.8,1002.7,234.7,945.8,197.1,615.1,65.6,711.5,113.4,336.1,256,951,228.7,194.8,317.6,417.9,87.4,271.7,226.4,74.9,3060.3,93.6,162,995.8,81.6,1012.9,543.2,178.3,289.8,73.7,224.8,1788,46.1,42.1,168.8,158,155.6,113.6,2197.5,817.9,1172,2821,102,1374.6,212.5,36.6,181.7,409.1,1374.6,639.5,178.4,85,2427.6,744.9,52.9,69,21.9,325.6,344.3,229.6,25.3,56.7,95.5,63.3,200.3,31.5,34.5,738.2,489.3,767,16.3,145.6,157.8,5310,324.9,175.6,230.2,1066.3,183.1,35.6,620.3,151,157.8,263.8,507.5,654.5,76.6,289.7,137.6,25,12215.3,383.4,83.1,50.1,115.7,97.2,5154.2,305.6,1077,562.8,754.8,1077,864.8,1197.1,75.8,1320.6,132.8,56.7,136.8,45.2,560.7,210.5,140.3,383.9,287.6,105.2,421.2,6374.4,2071.1,192.2,29.7,171.9,267.5,52.9,252,118.3,246.1,25.1,1256.1,51.7,607.5,96.7,46.9,384.4,118.3,234.7,63.7,48.3,78.1,150,111.7,196,1123,167.9,27.2,157.6,137.2,336.8,1363,44.6,199.1,1955.5,574.8,37.9,363.3,109.4)
hist(la)
abline(v=newlake_la)
hist(log10(la))
abline(v=log10(newlake_la))
length(la)

## these could POSSIBLY have been used in the LA model
whichreg <- c(4,6,8,9,11,10,13,14,17,18,27,28,35,37,38,52,55,56,59,61,63,64,66,69,73,75,81,83,84,94,97,99,101,102,105,108,110,120,123,138,141,143,147,148,152,154,175,182,183,188,198,201,205,210,212)
lareg <- la[whichreg]
hist(log10(lareg))
abline(v=log10(newlake_la))


## comparable weights from PAXSON lake
pwts1 <- c(1.36,1.38,2.58,1.1,1.12,3.02,1.1,1.18,1.46,1.1,1.24,1.38,1.54,1.66,2,0.88,0.94,1.06,1.28,0.86,0.96,1.6,2.56,2.64,1.36,2.34,3.02,3.74,2.58)
pl <- c(410,420,440,440,510,445,485,530,468,470,435,490,484,446,460,440,360,436,403,505,500,410,485,504,398,545,500,600,530)
pwts <- pwts1[pl>=400]
hist(pwts)
sd(pwts)
mean(pwts)
length(pwts)

## This was a trial and error routine to figure out what the error standard
## deviation would have been (arriving at epssd = 0.305)
## The reason lakes are sampled without replacement is because I didn't know
## which 43 of the 50-some lakes were used to inform the LAM
epssd <- 0.305
n <- 10000
b0 <- b1 <- r2 <- predvar <- rep(NA,n)

## would really like to visualize this relationship
all_la <- all_log10_la <- all_log10_yp <- all_yp <- matrix(nrow=n, ncol=43)

for(i in 1:n) {
  x <- sample(lareg, 43)
  # x <- sample(la, 43)
  log10x <- log10(x)
  log10YPpred <- 0.6 + 0.72*log10(x)
  eps <- rnorm(43, 0, epssd)
  log10YPsim <- log10YPpred + eps
  mod <- lm(log10YPsim~log10x)
  b0[i] <- unname(mod$coefficients[1])
  b1[i] <- unname(mod$coefficients[2])
  r2[i] <- summary(mod)$r.squared
  predvar[i] <- epssd^2 + predict(mod,newdata=data.frame(log10x=log10(newlake_la)),se.fit=T)$se.fit^2

  # storing stuff to plot
  all_la[i,] <- x
  all_log10_la[i,] <- log10(x)
  all_log10_yp[i,] <- log10YPsim
  all_yp[i,] <- 10^log10YPsim
}
par(mfrow=c(2,2))
hist(b0)
abline(v=.6)
abline(v=median(b0),lty=2)
hist(b1)
abline(v=.72)
abline(v=median(b1),lty=2)
hist(r2)
abline(v=.69)
abline(v=median(r2),lty=2)
hist(predvar)
abline(v=median(predvar),lty=2)


#### this is the best estimate of the prediction variance for a NEW LAKE
predvarsim <- median(predvar)

## vector of simulated YP of new lake, on the log scale
predlog10YP <- rnorm(n, 0.6 + 0.72*log10(newlake_la), sqrt(predvarsim))
hist(predlog10YP)

-(mean(predlog10YP) - quantile(predlog10YP,0.975))/mean(predlog10YP)  # not sure what this does

## back to natural scale
predYP <- 10^predlog10YP
hist(predYP)

## prediction interval on the natural scale, relative to median
quantile(predYP,c(0.025,0.975))/median(predYP)

## simulating a vector of weights ... and doing something with it
wts <- rnorm(n, 1, 1/sqrt(100))
predYPn <- predYP/wts
hist(predYPn)
sd(predYPn)/mean(predYPn)



# or maybe would be better to do a prediction interval on the log scale and backtransform
CIlog <- 0.6 + 0.72*log10(newlake_la) + c(-1,1)*qnorm(0.95)*sqrt(predvarsim)
10^CIlog


par(mfrow=c(1,2))
plot(as.numeric(all_log10_la)[1:200], as.numeric(all_log10_yp)[1:200],
     xlab="log10(lake area)", ylab="log10(YP)", main="Pred Intvl for Crosswind (log scale)")
for(i in 1:100) abline(b0[i], b1[i], col=adjustcolor(4,alpha.f=.2))
abline(.6,.72, lwd=2)
# abline(v=log10(newlake_la))
lines(x=rep(log10(newlake_la),2), y=CIlog, lwd=4, col=4, lend=2)

plot(as.numeric(all_la)[1:200], as.numeric(all_yp)[1:200],
     xlab="lake area", ylab="YP", main="Pred Intvl for Crosswind (natural scale)")
for(i in 1:100) curve(10^(b0[i]+b1[i]*log10(x)), add=TRUE, col=adjustcolor(4,alpha.f=.2))
curve(10^(.6+.72*log10(x)), add=TRUE, lwd=2)
# abline(v=newlake_la)
lines(x=rep(newlake_la,2), y=10^CIlog, lwd=4, col=4, lend=2)


### I think this section was estimating YPn assuming YP to be nonrandom??


## calculation of point estimates
YPhat <- 10^(0.6 + 0.72*log10(newlake_la))       # estimated YP in kg
What <- mean(pwts)                           # est mn weight
VWhat <- var(pwts)/length(pwts)              # est var of the mean weight
YPnhat <- YPhat/What + YPhat*VWhat/(What^3)  # unbiased est of YP in number
VYPnhat <- (YPhat^2)*VWhat/(What^4)          # est var of YP in number
SEYPnhat <- sqrt(VYPnhat)                    # se of YP in number

# normal CI  ... assuming YPn is normal which will be wrong
YPnhat + c(-1,1)*qnorm(0.975)*SEYPnhat
(YPnhat + c(-1,1)*qnorm(0.975)*SEYPnhat)/YPnhat

# simulating from resampling
n <- 10000
YPnB <- YPnB2 <- rep(NA,n)
nsample <- 100
for(i in 1:n) {
  Bsamp <- sample(pwts,nsample,replace=T)
  Whati <- mean(Bsamp)
  VWhati <- var(Bsamp)/length(pwts)
  YPnB[i] <- YPhat/Whati + YPhat*VWhati/(Whati^3)
  YPnB2[i] <- predYP[i]/Whati + predYP[i]*VWhati/(Whati^3)   ### incorporating prediction error in YP
}
quantile(YPnB,c(0.025,0.975))
quantile(YPnB,c(0.025,0.975))/YPnhat
hist(YPnB)


### now looking at intervals incorporating prediction error from YP

quantile(predYP,c(0.025,0.975))/median(predYP)   # original error in YP prediction

quantile(YPnB2,c(0.025,0.975))
quantile(YPnB2,c(0.025,0.975))/median(YPnB2)   # also incorporating est error in mn weight
hist(YPnB2)

qnorm(0.975)*sd(pwts)/(.075*mean(pwts))^2  # no idea what this is




## I really want to make sure the "unbiased" delta method thing is unbiased
## in the correct direction!
## THIS IS ONLY A SIMULATION
# wts <- rnorm(n=100, mean=mean(pwts), sd=sd(pwts))
nsim <- 10000
n <- 100  # sample size to draw
YPn1 <- YPn2 <-
  YPn1_unb <- YPn2_unb <-
  vYPn1 <- vYPn2 <-
  mnwts1 <- mnwts2 <-
  vmnwts1 <- vmnwts2 <-rep(NA, nsim)
for(isim in 1:nsim) {
  simwts1 <- rnorm(n=n, mean=mean(pwts), sd=sd(pwts))
  simwts2 <- sample(pwts, size=n, replace=TRUE)
  mnwts1[isim] <- mean(simwts1)
  mnwts2[isim] <- mean(simwts2)
  vmnwts1[isim] <- var(simwts1)/n
  vmnwts2[isim] <- var(simwts2)/n
  YPn1[isim] <- YPhat/mnwts1[isim]
  YPn2[isim] <- YPhat/mnwts2[isim]
  YPn1_unb[isim] <- YPhat/mnwts1[isim] + YPhat/(mnwts1[isim]^3)*vmnwts1[isim]
  YPn2_unb[isim] <- YPhat/mnwts2[isim] + YPhat/(mnwts2[isim]^3)*vmnwts2[isim]
  ## from this, it looks like it should be minus, not plus.  But from below
  ## it should actually be plus like in the OP.

  vYPn1[isim] <- (YPhat^2)/(mnwts1[isim]^4)*vmnwts1[isim]
  vYPn2[isim] <- (YPhat^2)/(mnwts2[isim]^4)*vmnwts2[isim]
}
hist1 <- function(x,true,...) {
  hist(x, ...=...)
  abline(v=mean(x))
  abline(v=true, lty=2, col=4)
}
par(mfrow=c(3,2))
# hist1(YPn1, YPhat/mean(pwts), main="YP naive Norm")
# hist1(YPn2, YPhat/mean(pwts), main="YP naive resampled")
# hist1(YPn1_unb, YPhat/mean(pwts), main="YP unbiased Norm")
# hist1(YPn2_unb, YPhat/mean(pwts), main="YP unbiased resampled")
hist1(YPn1, mean(YPhat/mnwts1), main="YP naive Norm")
hist1(YPn2, mean(YPhat/mnwts2), main="YP naive resampled")
hist1(YPn1_unb, mean(YPhat/mnwts1), main="YP unbiased Norm")
hist1(YPn2_unb, mean(YPhat/mnwts2), main="YP unbiased resampled")
hist1(vYPn1, var(YPn1), main="variance Norm")
hist1(vYPn2, var(YPn2), main="variance resampled")


## or maybe another way
x <- rnorm(n=10000, mean=10, sd=2.5)
hist(x)
hist(1/x)
abline(v=mean(1/x), col=2)
abline(v=1/mean(x), lty=2, col=3)
abline(v=1/mean(x) + 1/((mean(x)^3))*var(x), lty=3, col=4)  # it should actually be plus



## testing & editing Jordy's function
#' Calculate Maximum Sustainable Yield (MSY) as defined by the Lester model
#' @param lake (character) Name of the lake
#' @param temp (numeric) Mean annual temperature (in C)
#' @param area (numeric) Lake surface area (in ha)
#' @param mean_depth (numeric) Mean lake depth (in m)
#' @param max_depth (numeric) Maximum lake depth (in m)
#' @param return_df (logical) Whether to return a data.frame including lake parameters (TRUE)
#' @param or just a vector of MSY in kg/(ha-yr)
#' @return vector of MSY in kg/(ha-yr), or data.frame with lake
#' @export
#' @examples
#' lake <- c("Crosswind", "Fielding", "Glacier Gap", "Louise", "Paxson", "Little Sevenmile", "Summit", "Susitna", "Round Tangle", "Shallow Tangle", "Combined Tangle")
#' temp <- c(-3.04, -5.89, -7.13, -3.29, -4.15, -5.89, -5.89, -3.29, -7.13, -7.13, -7.13)
#' area <- c(3716.55, 561.96, 178.06, 5913.07, 1569.92, 35.13, 1770.12, 3635.16, 156.15, 129.5, 285.65)
#' mean_depth <- c(15.9, 8.7, 7.1, 13, 8.4, 4.4, 15.6, 9, 10, 2, 6.4)
#' max_depth <- c(36.6, 23.1, 24.4, 51, 29.7, 14.1, 63.4, 36.6, 27.3, 19.8, 27.3)
#' lester_msy(lake, temp, area, mean_depth, max_depth)
#' lester_msy(lake, temp, area, mean_depth, max_depth, return_df=TRUE)
lester_msy <- function(lake=NULL, temp, area, mean_depth, max_depth, return_df=FALSE){
  Temp <- temp
  A <- area
  D_max <- max_depth
  D_mn <- mean_depth
  DR <- D_max/D_mn
  D_th <- 3.26*A^0.109*D_mn^0.213*exp(-0.0263*Temp)
  pV_hy <- (1-D_th/D_max)^DR
  pV_eb <- exp(-4.63*pV_hy)
  L_inf <- 957*(1-exp(-0.14*(1+log(A))))
  W_inf <- (L_inf/451)^3.2
  S <- 1/(1+exp(2.47+0.386*Temp-16.8*pV_hy))
  B_msy <- 8.47*(D_mn*pV_eb*S)/W_inf^1.33
  M <- 0.26*(exp(0.021*Temp+0.0004*Temp^2))/W_inf^0.30
  msy_ha <- B_msy*M
  msy <- msy_ha*area   # original version was rounded to two decimal points
  if(return_df) {
    if(is.null(lake)) stop("Please supply a vector of lake names to the lake= argument")
    return(data.frame(lake=lake, temp=temp, area=area, mean_depth=mean_depth, max_depth=max_depth, msy=msy))
  } else {
    return(msy)
  }
}

lester_msy <- function(lake=NULL, temp, area, mean_depth, max_depth, return_df=FALSE){
  Temp <- temp
  A <- area
  D_max <- max_depth
  D_mn <- mean_depth
  DR <- D_max/D_mn
  D_th <- 3.26*(A^0.109)*(D_mn^0.213)*exp(-0.0263*Temp)
  pV_hy <- (1-(D_th/D_max))^DR
  pV_eb <- exp(-4.63*pV_hy)
  L_inf <- 957*(1-exp(-0.14*(1+log(A))))
  W_inf <- (L_inf/451)^3.2
  S <- 1/(1+exp(2.47+(0.386*Temp)-(16.8*pV_hy)))
  B_msy <- 8.47*(D_mn*pV_eb*S)/(W_inf^1.33)
  M <- 0.26*(exp((0.021*Temp)+(0.0004*(Temp^2))))/(W_inf^0.30)
  msy_ha <- B_msy*M
  msy <- msy_ha*area   # original version was rounded to two decimal points
  if(return_df) {
    if(is.null(lake)) stop("Please supply a vector of lake names to the lake= argument")
    return(data.frame(lake=lake, temp=temp, area=area, mean_depth=mean_depth, max_depth=max_depth, msy=msy))
  } else {
    return(msy)
  }
}

lake <- c("Crosswind", "Fielding", "Glacier Gap", "Louise", "Paxson", "Little Sevenmile", "Summit", "Susitna", "Round Tangle", "Shallow Tangle", "Combined Tangle")
temp <- c(-3.04, -5.89, -7.13, -3.29, -4.15, -5.89, -5.89, -3.29, -7.13, -7.13, -7.13)
area <- c(3716.55, 561.96, 178.06, 5913.07, 1569.92, 35.13, 1770.12, 3635.16, 156.15, 129.5, 285.65)
mean_depth <- c(15.9, 8.7, 7.1, 13, 8.4, 4.4, 15.6, 9, 10, 2, 6.4)
max_depth <- c(36.6, 23.1, 24.4, 51, 29.7, 14.1, 63.4, 36.6, 27.3, 19.8, 27.3)
aaa<-lester_msy(lake, temp, area, mean_depth, max_depth, return_df = T)

