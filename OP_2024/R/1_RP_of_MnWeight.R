## need a header

library(tidyverse)
library(dsftools)


## (1) Look in the Master Laketrout database to see if there are comparable
## samples of weights to use in sample size calculations


## look for Crosswind samples in master LT database
morphometry <- read_csv("OP_2024/flat_data/all_lakes/lake_morphometry2.csv", skip=2)
laketrout_all <- read_csv("OP_2024/flat_data/all_lakes/length_weight2.csv", skip=2) %>%
  left_join(morphometry)

justCrosswind <- laketrout_all %>% filter(LakeName=="Crosswind Lake" & Year < 2024 & !is.na(Weight_g))
nrow(justCrosswind)  # 0

justFielding <- laketrout_all %>% filter(LakeName=="Fielding Lake" & !is.na(Weight_g))
nrow(justFielding)  # 625

hist(justFielding$Weight_g)
# understandably right-skewed

mean(justFielding$Weight_g)  # 2277.138
sd(justFielding$Weight_g)  # 1766.362
se(justFielding$Weight_g)  # 70.65448

table(justFielding$ProjectTitle)
boxplot(justFielding$Weight_g ~ justFielding$ProjectTitle)

# renaming the vector of weights to use later
F_wts <- justFielding$Weight_g



## (2) Simulate the relative precision of estimating mean weight from stratified sample
n1 <- 100
n2 <- 100
strat_wts <- c(1, 1)  ## relative weights for each stratum
wt_adj <- c(.5, 1.5)  ## (multiplicative) weight adjustment for each stratum?

nsim <- 10000
av_wt1 <- av_wt2 <- est_mn <- rep(NA, nsim)  # initializing vectors
for(i_sim in 1:nsim) {
  av_wt1[i_sim] <- mean(sample(F_wts*wt_adj[1], size=n1, replace=TRUE))
  av_wt2[i_sim] <- mean(sample(F_wts*wt_adj[2], size=n2, replace=TRUE))
  est_mn[i_sim] <- sum(c(av_wt1[i_sim], av_wt2[i_sim])*strat_wts/sum(strat_wts))
}
hist(est_mn)

mn_true <- sum(strat_wts*wt_adj*mean(F_wts)/sum(strat_wts))
abline(v=mn_true, col=2, lty=2, lwd=2)

###### think through the syntax of pulling confidence out for desired RP (or vice versa)
###### turn this into a dsftools function!!

# or not
quantile(abs(est_mn-mn_true)/mn_true, .95)


## let's try a bunch of versions of weight adjustment..
adj_amount <- seq(0, .99, by=.01)
quantile_vec <- rep(NA, length(adj_amount))
for(i_amount in seq_along(adj_amount)) {
  strat_wts <- c(1, 1)  ## relative weights for each stratum
  wt_adj <- 1 + c(-1, 1)*adj_amount[i_amount]  ## (multiplicative) weight adjustment for each stratum?

  nsim <- 10000
  av_wt1 <- av_wt2 <- est_mn <- rep(NA, nsim)  # initializing vectors
  for(i_sim in 1:nsim) {
    av_wt1[i_sim] <- mean(sample(F_wts*wt_adj[1], size=n1, replace=TRUE))
    av_wt2[i_sim] <- mean(sample(F_wts*wt_adj[2], size=n2, replace=TRUE))
    est_mn[i_sim] <- sum(c(av_wt1[i_sim], av_wt2[i_sim])*strat_wts/sum(strat_wts))
  }
  mn_true <- sum(strat_wts*wt_adj*mean(F_wts)/sum(strat_wts))
  quantile_vec[i_amount] <- quantile(abs(est_mn-mn_true)/mn_true, .95)
}
plot(adj_amount, quantile_vec)


## now let's try a bunch of versions of TRUE strat_wts...
betap <- 1:20
quantile_vec <- rep(NA, length(betap))
for(i_amount in seq_along(betap)) {
  strat_wts <- c(1, 1)  ## relative weights for each stratum
  wt_adj <- 1 + c(-1, 1)*.5  ## (multiplicative) weight adjustment for each stratum?

  nsim <- 100000
  av_wt1 <- av_wt2 <- est_mn <- rep(NA, nsim)  # initializing vectors
  for(i_sim in 1:nsim) {
    av_wt1[i_sim] <- mean(sample(F_wts*wt_adj[1], size=n1, replace=TRUE))
    av_wt2[i_sim] <- mean(sample(F_wts*wt_adj[2], size=n2, replace=TRUE))
    est_mn[i_sim] <- sum(c(av_wt1[i_sim], av_wt2[i_sim])*strat_wts/sum(strat_wts))
  }
  strat_wts_true <- rep(NA, 2)
  strat_wts_true[1] <- rbeta(1, betap[i_amount], betap[i_amount])
  strat_wts_true[2] <- 1-strat_wts_true[1]
  mn_true <- sum(strat_wts_true*wt_adj*mean(F_wts)/sum(strat_wts_true))
  quantile_vec[i_amount] <- quantile(abs(est_mn-mn_true)/mn_true, .95)
}
plot(betap, quantile_vec)




## (3) incorporating the variance within the Lake Area Model
newlake_la <- 3717  # Crosswind

## all lake areas reported in Evans et al (LA model)
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


## This was a trial and error routine to figure out what the error standard
## deviation would have been (arriving at epssd = 0.305)
## The reason lakes are sampled without replacement is because I didn't know
## which 43 of the 50-some lakes were used to inform the LAM
epssd <- 0.305
n <- 10000
b0 <- b1 <- r2 <- predvar <- rep(NA,n)

# ## would really like to visualize this relationship
# all_la <- all_log10_la <- all_log10_yp <- all_yp <- matrix(nrow=n, ncol=43)

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

  # # storing stuff to plot
  # all_la[i,] <- x
  # all_log10_la[i,] <- log10(x)
  # all_log10_yp[i,] <- log10YPsim
  # all_yp[i,] <- 10^log10YPsim
}
par(mfrow=c(2,2))
hist(b0)
abline(v=.6)
abline(v=median(b0), lty=2, col=2, lwd=2)
hist(b1)
abline(v=.72)
abline(v=median(b1), lty=2, col=2, lwd=2)
hist(r2)
abline(v=.69)
abline(v=median(r2), lty=2, col=2, lwd=2)
hist(predvar)
abline(v=median(predvar), lty=2, col=2, lwd=2)


#### this is the best estimate of the prediction variance for a NEW LAKE
predvarsim <- median(predvar)   # [1] 0.09958763

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
