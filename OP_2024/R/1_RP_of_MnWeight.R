## Relative Precision of Mean Weight
##
## The purpose of this script is to estimate the relative precision of mean weight
## of lake trout in Crosswind Lake, given the proposed sample sizes in the
## operational plan.
##
## Sampling will consist of two events (March and June), and there is potential
## for differing size selectivity between the two events.
##
## The basic outline of this script is to
## (1) load a comparable weight dataset to sample from
## (2) simulate the relative precision by resampling from this sample
##     - investigate the effect of a difference in mean weights between the two samples
##     - investigate the effect of non-equal fishing mortality (winter vs summer)


# load packages
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
# including this will hopefully account for the additional uncertainty due to
# differences in mean weight by sampling event (March vs June)

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
quantile(abs(est_mn-mn_true)/mn_true, .95)    # 12%


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

# slight increase in uncertainty (decrease in precision) when the adjustment
# amount goes up (that is, samples have greater difference in means)
par(mfrow=c(1,1))
plot(adj_amount, quantile_vec)
abline(v=0.5)
abline(h=0.12)


## now let's try a bunch of versions of TRUE strat_wts...
# this is done by drawing random samples from a Beta distribution centered at 50%
# Which Beta distribution you ask?  Let's plot a few.  Beta(20,20) looks conservative enough.
curve(dbeta(x,50,50))
curve(dbeta(x,20,20), add=TRUE, col=2)
curve(dbeta(x,10,10), add=TRUE, col=3)
curve(dbeta(x,5,5), add=TRUE, col=4)
bp <- c(50,20,10,5)
legend("topleft", col=1:4, lty=1,
       legend=paste0("beta(", bp, ",", bp, ")"))

## ok let's actually do it now
betap <- 1:50 # candidate values for beta parameters
quantile_vec <- rep(NA, length(betap))
for(i_amount in seq_along(betap)) {
  strat_wts <- c(1, 1)  ## relative weights for each stratum
  wt_adj <- 1 + c(-1, 1)*.5  ## (multiplicative) weight adjustment for each stratum?

  nsim <- 10000
  av_wt1 <- av_wt2 <- est_mn <- rep(NA, nsim)  # initializing vectors
  for(i_sim in 1:nsim) {
    av_wt1[i_sim] <- mean(sample(F_wts*wt_adj[1], size=n1, replace=TRUE))
    av_wt2[i_sim] <- mean(sample(F_wts*wt_adj[2], size=n2, replace=TRUE))
    est_mn[i_sim] <- sum(c(av_wt1[i_sim], av_wt2[i_sim])*strat_wts/sum(strat_wts))
  }
  # strat_wts_true <- rep(NA, 2)
  # strat_wts_true[1] <- rbeta(1, betap[i_amount], betap[i_amount])
  # strat_wts_true[2] <- 1-strat_wts_true[1]
  beta1 <- rbeta(nsim, betap[i_amount], betap[i_amount])
  beta2 <- 1-beta1
  mn_true <- rep(NA, nsim)
  for(i in 1:nsim) {
    strat_wts_true <- c(beta1[i], beta2[i])
    mn_true[i] <- sum(strat_wts_true*wt_adj*mean(F_wts)/sum(strat_wts_true))
  }
  # mn_true <- sum(strat_wts_true*wt_adj*mean(F_wts)/sum(strat_wts_true))
  quantile_vec[i_amount] <- quantile(abs(est_mn-mn_true)/mn_true, .95)
}
plot(betap, quantile_vec)
## just as expected, precision gets better as beta parameters get bigger


#### FINAL WORST-CASE SCENARIO TO PUT IN THE OP PLAN:
#### - weight adjustment of 67% & 133%  (mean weight of March sample is twice as big as June)
#### - beta parameters of 20 (80% chance that March harvest is between 40% and 60% of total)

strat_wts <- c(1, 1)  ## relative weights for each stratum
wt_adj <- 1 + c(-1, 1)*.33  ## (multiplicative) weight adjustment for each stratum?
betap_fixed <- 20
nsim <- 10000
av_wt1 <- av_wt2 <- est_mn <- rep(NA, nsim)  # initializing vectors
for(i_sim in 1:nsim) {
  av_wt1[i_sim] <- mean(sample(F_wts*wt_adj[1], size=n1, replace=TRUE))
  av_wt2[i_sim] <- mean(sample(F_wts*wt_adj[2], size=n2, replace=TRUE))
  est_mn[i_sim] <- sum(c(av_wt1[i_sim], av_wt2[i_sim])*strat_wts/sum(strat_wts))
}
# strat_wts_true <- rep(NA, 2)
# strat_wts_true[1] <- rbeta(1, betap[i_amount], betap[i_amount])
# strat_wts_true[2] <- 1-strat_wts_true[1]
beta1 <- rbeta(nsim, betap_fixed, betap_fixed)
beta2 <- 1-beta1
mn_true <- rep(NA, nsim)
for(i in 1:nsim) {
  strat_wts_true <- c(beta1[i], beta2[i])
  mn_true[i] <- sum(strat_wts_true*wt_adj*mean(F_wts)/sum(strat_wts_true))
}
# mn_true <- sum(strat_wts_true*wt_adj*mean(F_wts)/sum(strat_wts_true))

quantile(abs(est_mn-mn_true)/mn_true, .95)   # 15%
