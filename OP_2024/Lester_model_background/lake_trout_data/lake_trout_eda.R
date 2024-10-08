library(tidyverse)
library(jagsUI)
library(jagshelper)

morphometry <- read_csv("C:\\Users\\mbtyers\\Documents\\Current Projects\\Crosswind_Lake_trout\\OP_2024\\Lester_model_background\\lake_trout_data\\lake_morphometry.csv", skip=2)

# # is lake name unique?  YES
# sum(!is.na(morphometry$LakeName))
# length(unique(morphometry$LakeName))

laketrout_all <- read_csv("C:\\Users\\mbtyers\\Documents\\Current Projects\\Crosswind_Lake_trout\\OP_2024\\Lester_model_background\\lake_trout_data\\length_weight.csv", skip=2) %>%
  left_join(morphometry)

sum(!is.na(laketrout_all$ForkLength_mm))  # 34107
sum(!is.na(laketrout_all$Weight_g))  # 3894
sum(!is.na(laketrout_all$Weight_g) & !is.na(laketrout_all$ForkLength_mm))  # 3887

boxplot(laketrout_all$ForkLength_mm ~ laketrout_all$LakeName, las=2, xlab="")

# we have way more data for length than weight!  But a decent amount for weight too


## subsetting for length-weight relationship
laketrout_lw_filter1 <- laketrout_all %>%
  # mutate(Weight_g = ifelse(Weight_g > 100000, Weight_g/1000, Weight_g)) %>%
  mutate(Weight_g = ifelse(Weight_g < 50, Weight_g*1000, Weight_g)) %>%
  filter(Weight_g < 100000) %>%
  filter(ForkLength_mm > 150)



laketrout_lw_filter1 %>%
  # filter(!ProjectTitle %in% c("Mark-Recapture Event 1 - (September - 2002)",
  #                             "Mark-Recapture Event 1 - (September - 2003)",
  #                             "Mark-Recapture Event 1 - (September - 2004)",
  #                             "Mark-Recapture Event 2 - (May - 2003)")) %>%
  ggplot(aes(x=ForkLength_mm, y=Weight_g, colour=LakeName)) +
           geom_point(alpha=.3) +
  # facet_wrap(facets=~LakeName) +
  scale_y_log10() +
  scale_x_log10()

laketrout_lw_filter2 <- laketrout_lw_filter1 %>%
  filter(!ProjectTitle %in% c("Mark-Recapture Event 1 - (September - 2002)",
                              "Mark-Recapture Event 1 - (September - 2003)",
                              "Mark-Recapture Event 1 - (September - 2004)",
                              "Mark-Recapture Event 2 - (May - 2003)"))

laketrout_lw_filter2 %>%
  ggplot(aes(x=ForkLength_mm, y=Weight_g, colour=LakeName)) +
  geom_point(alpha=.3) +
  # facet_wrap(facets=~LakeName) +
  scale_y_log10() +
  scale_x_log10()

# laketrout_lw_filter2 %>%
#   ggplot(aes(x=ForkLength_mm, y=Weight_g, colour=LakeName)) +
#   geom_point(alpha=.3) +
#   facet_wrap(facets=~LakeName) +
#   scale_y_log10() +
#   scale_x_log10()

lm_filter2 <- lm(log(Weight_g/1000)~log(ForkLength_mm), data=laketrout_lw_filter2)
plot(lm_filter2$residuals/sd(lm_filter2$residuals))

laketrout_lw <- laketrout_lw_filter2 %>%
  filter(lm_filter2$residuals/sd(lm_filter2$residuals) > -4 &
         lm_filter2$residuals/sd(lm_filter2$residuals) < 4)

laketrout_lw %>%
  ggplot(aes(x=ForkLength_mm, y=Weight_g)) +#, colour=LakeName
  geom_point(alpha=.3) +
  facet_wrap(facets=~LakeName) +
  scale_y_log10() +
  scale_x_log10()


lm1 <- lm(log(Weight_g/1000)~log(ForkLength_mm), data=laketrout_lw)
summary(lm1)
# lm1$residuals %>% plot(col=laketrout_lw$LakeID)
# lm1$residuals %>% plot(col=as.factor(paste(laketrout_lw$LakeID,laketrout_lw$Year)))
# plot(lm1$residuals[laketrout_lw$LakeName=="Paxson Lake"], pch=16,
#      col=as.factor(laketrout_lw$ProjectTitle[laketrout_lw$LakeName=="Paxson Lake"]))
AIC(lm1)

lm2 <- lm(log(Weight_g/1000)~log(ForkLength_mm)*LakeName, data=laketrout_lw)
summary(lm2)
AIC(lm2)

lm3 <- lm(log(Weight_g/1000)~log(ForkLength_mm)*LakeName + log(ForkLength_mm)*Year, data=laketrout_lw)
summary(lm3)
AIC(lm3)

# lm3 <- lm(log(Weight_g/1000)~log(ForkLength_mm)*LakeName + Year, data=laketrout_lw)
# summary(lm3)
# AIC(lm3)

anova(lm1, lm2, lm3)




# specify model, which is written to a temporary file
lt_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    ypp[i] ~ dnorm(mu[i], tau)
    mu[i] <- b0[lake[i]] + b1[lake[i]]*x[i]
  }

  for(j in 1:nlake) {
    b0[j] ~ dnorm(mu_b0, tau_b0)
    b1[j] ~ dnorm(mu_b1, tau_b1)
  }

  tau <- pow(sig, -2)
  sig ~ dunif(0, 10)

  mu_b0 ~ dnorm(0, 0.001)
  sig_b0 ~ dunif(0, 10)
  tau_b0 <- pow(sig_b0, -2)

  mu_b1 ~ dnorm(0, 0.001)
  sig_b1 ~ dunif(0, 10)
  tau_b1 <- pow(sig_b1, -2)

  for(ipred in 1:npred) {
    for(jpred in 1:nlake) {
      fit[ipred,jpred] <- b0[jpred] + b1[jpred]*xpred[ipred]
      # pred[ipred,jpred] ~ dnorm(fit[ipred,jpred], tau)
    }
  }
}', file=lt_jags)


# bundle data to pass into JAGS
lt_data <- list(x=log(laketrout_lw$ForkLength_mm) - mean(log(laketrout_lw$ForkLength_mm)),
                y=log(laketrout_lw$Weight_g/1000),
                n=nrow(laketrout_lw),
                lake=as.numeric(as.factor(laketrout_lw$LakeName)),
                nlake=length(unique(laketrout_lw$LakeName)))
lt_data$xpred <- seq(from=min(lt_data$x), to=max(lt_data$x), length.out=30)
lt_data$npred <- length(lt_data$xpred)

# JAGS controls
niter <- 2000   # 2 minutes for 2k on lappy, or 3 with predictions
ncores <- min(10, parallel::detectCores()-1)

{
  tstart <- Sys.time()
  print(tstart)
  lt_jags_out <- jagsUI::jags(model.file=lt_jags, data=lt_data,
                              parameters.to.save=c("b0","b1","sig",
                                                   "mu_b0","sig_b0","mu_b1","sig_b1",
                                                   "fit","pred","ypp"),
                              n.chains=ncores, parallel=T, n.iter=niter,
                              n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)
}
plotRhats(lt_jags_out)
traceworstRhat(lt_jags_out, parmfrow=c(3,3))

par(mfrow=c(2,1))
caterpillar(lt_jags_out, p="b0")
caterpillar(lt_jags_out, p="b1")

par(mfrow=c(1,1))
qq_postpred(lt_jags_out, p="ypp", y=lt_data$y)
ts_postpred(lt_jags_out, p="ypp", y=lt_data$y, x=lt_data$x)
ts_postpred(lt_jags_out, p="ypp", y=lt_data$y, x=lt_data$y)

par(mfrow=c(3,3))
for(i in 1:lt_data$nlake) {
  envelope(lt_jags_out$sims.list$fit[,,i], x=lt_data$xpred)
  points(lt_data$x[lt_data$lake==i], lt_data$y[lt_data$lake==i])
  abline(a=lt_jags_out$q50$mu_b0, b=lt_jags_out$q50$mu_b1, lty=3)
}

lm1 %>% summary
lt_jags_out$q50$mu_b0
lt_jags_out$sd$mu_b0
lt_jags_out$q50$mu_b1
lt_jags_out$sd$mu_b1

# # this might be obsolete
# laketrout %>%
#   lm(log(Weight_g/1000)~log(ForkLength_mm)*Year*LakeName, data=.) %>%
#   summary
# 
# laketrout %>%
#   lm(log(Weight_g/1000)~log(ForkLength_mm), data=.) %>%
#   summary



# visualize relationship separating by lake
# weight distribution by lake

# fit (Bayes) with random slope & intercept by lake

# figure out how to approximate Linf & Winf
# Linf & Winf by lake area (even just visualize)
# would latitude matter?


## filter to lakes with at least (20?) observations
nlengths <- table(laketrout_all$LakeName[!is.na(laketrout_all$ForkLength_mm)])
nweights <- table(laketrout_all$LakeName[!is.na(laketrout_all$Weight_g)])
hist(nlengths)
hist(nweights)
length(nlengths)
length(nweights)
length(nlengths[nlengths>20])
length(nweights[nweights>20])

minn <- 20  # see if there's a logical corner
laketrout_lengtharea <- filter(laketrout_all,
                               LakeName %in% names(nlengths[nlengths >= minn]))
laketrout_weightarea <- filter(laketrout_all,
                               LakeName %in% names(nweights[nweights >= minn]))

ni <- rep(NA, 100)
for(i in 1:100) {
  ni[i] <- sum(nlengths >= i)
}
plot(ni)

q_inf <- 0.95
wt_q <- with(laketrout_weightarea, 
             tapply(Weight_g, LakeName, quantile, q_inf, na.rm=TRUE))
ln_q <- with(laketrout_lengtharea, 
             tapply(ForkLength_mm, LakeName, quantile, q_inf, na.rm=TRUE))

# Lester did "the mean fork length of the largest 10% in our sample, 
# after removing fish smaller than 300 mm."
length_lester <- function(x) {
  x1 <- x[x >= 300]
  x2 <- x1[x1 >= quantile(x1, 0.9, na.rm=TRUE)]
  return(mean(x2, na.rm=TRUE))
}
ln_q <- with(laketrout_lengtharea, 
             tapply(ForkLength_mm, LakeName, length_lester))

area_wt_q <- with(laketrout_weightarea, 
                  tapply(SurfaceArea_h, LakeName, median, na.rm=TRUE))
area_ln_q <- with(laketrout_lengtharea, 
                  tapply(SurfaceArea_h, LakeName, median, na.rm=TRUE))

par(mfrow=c(2,2))
plot(area_wt_q, wt_q)
plot(area_wt_q, wt_q, log="x")
plot(area_ln_q, ln_q)
plot(area_ln_q, ln_q, log="x")

## trying to re-fit Lester's L_inf from lake area model
# specify model, which is written to a temporary file
linf_jags <- tempfile()
cat('model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    ypp[i] ~ dnorm(mu[i], tau)
    mu[i] <- b0 * (1 - exp(-b1 * (1 + log(x[i]))))
  }

  tau <- pow(sig, -2)
  sig ~ dunif(0, 300)
  b0 ~ dnorm(957, pow(0.3*957, -2))
  b0_prior ~ dnorm(957, pow(0.3*957, -2))
  b1 ~ dnorm(0.14, pow(1*0.14, -2))
  b1_prior ~ dnorm(0.14, pow(1*0.14, -2))
}', file=linf_jags)


# bundle data to pass into JAGS
xx <- area_ln_q[!is.na(area_ln_q) & !is.na(ln_q)]
yy <- ln_q[!is.na(area_ln_q) & !is.na(ln_q)]
linf_data <- list(x=xx[order(xx)],
                  y=yy[order(xx)],
                  n=length(xx))

# JAGS controls
niter <- 10000
# ncores <- 3
ncores <- min(10, parallel::detectCores()-1)

{
  tstart <- Sys.time()
  print(tstart)
  linf_jags_out <- jagsUI::jags(model.file=linf_jags, data=linf_data,
                                parameters.to.save=c("b0","b1",
                                                     "b0_prior","b1_prior",
                                                     "sig", "mu","ypp"),
                                n.chains=ncores, parallel=T, n.iter=niter,
                                n.burnin=niter/2, n.thin=niter/2000)
  print(Sys.time() - tstart)
}
# nbyname(linf_jags_out)
plotRhats(linf_jags_out)
traceworstRhat(linf_jags_out, parmfrow=c(3,3))

par(mfrow=c(3,2))
comparepriors(linf_jags_out)
plot(linf_data$x, linf_data$y, log="x", 
     xlab="Lake Area (ha)", ylab="L_inf (mm)",
     main="Trend")
envelope(linf_jags_out, p="mu", x=linf_data$x, add=TRUE)
curve(957*(1-exp(-0.14*(1+log(x)))), lty=3, add=TRUE)

envelope(linf_jags_out, p="ypp", x=linf_data$x, log="x", 
         xlab="Lake Area (ha)", ylab="L_inf (mm)",
         main="Post Predictive")
points(linf_data$x, linf_data$y)
curve(957*(1-exp(-0.14*(1+log(x)))), lty=3, add=TRUE)

qq_postpred(linf_jags_out, p="ypp", y=linf_data$y)
ts_postpred(linf_jags_out, p="ypp", y=linf_data$y, x=linf_data$x, log="x")
cor(linf_jags_out$q50$mu, linf_data$y)^2
