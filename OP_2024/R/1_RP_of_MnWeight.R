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
betap <- 1:100
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



