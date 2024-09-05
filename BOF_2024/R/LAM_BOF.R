library(tidyverse)

########## reading data

## per Corey email 9/4/2024
legal_nobait <- read_csv("BOF_2024/flat_data/legal_nobait.csv")

## the data I was sent before
march_data <- read_csv("Analysis_2024/flat_data/March_Catch_Data.csv")[,-(14:21)]
june_data <- read_csv("Analysis_2024/flat_data/June_Catch_Data.csv")

all_data <- data.frame(event = c(rep("March", nrow(march_data)),
                                 rep("June", nrow(june_data))),
                       FL = c(march_data$FL, june_data$FL),
                       TL = c(march_data$FL, june_data$TL),
                       weight_kg = c(march_data$kg, june_data$Kg),
                       bait = c(march_data$Bait, june_data$Bait),
                       gear = c(march_data$Gear, rep(NA, nrow(june_data))),
                       technique = c(march_data$Technique, rep(NA, nrow(june_data))))

## reconstructing legal_nobait from previous data - does it match?
legal_nobait1 <- filter(all_data, (event == "March") | (event == "June" & bait == "N"))

(sort(legal_nobait$kg) == sort(legal_nobait1$weight_kg)) # %>% all
## it does match!



########### LAM inputs

area <- 3717
log10_area <- log10(area)
log10_YP <- 0.72*log10_area + 0.6
YP_kg <- 10^log10_YP




########### estimating mean weight of harvest
season_weights <- c(0.5, 0.5)   # summer, then winter

# by season
season_Wbar <- tapply(legal_nobait$kg, legal_nobait$Season, mean)
season_n <- tapply(legal_nobait$kg, legal_nobait$Season, length)
season_varWbar <- tapply(legal_nobait$kg, legal_nobait$Season, var)/season_n
season_seWbar <- sqrt(season_varWbar)

# combined
Wbar <- sum(season_weights*season_Wbar)/sum(season_weights)
varWbar <- sum((season_weights^2)*season_varWbar)/(sum(season_weights)^2)
seWbar <- sqrt(varWbar)

# LAM assuming YP is known without error
YP_n <- YP_kg/Wbar + YP_kg/(Wbar^3)*varWbar
var_YP_n <- (YP_kg^2)/(Wbar^4)*varWbar
se_YP_n <- sqrt(var_YP_n)
ci_YP_n <- YP_n + c(-1,1)*1.96*se_YP_n


## investigating the effect of different weightings
possible_wts <- seq(.01, .99, by=.01)
ci_YP_n_all <- matrix(nrow=length(possible_wts), ncol=2)
YP_n_all <- rep(NA, length(possible_wts))
for(i in seq_along(possible_wts)) {
  season_weights <- c(possible_wts[i], 1-possible_wts[i])   # summer, then winter

  # by season
  season_Wbar <- tapply(legal_nobait$kg, legal_nobait$Season, mean)
  season_n <- tapply(legal_nobait$kg, legal_nobait$Season, length)
  season_varWbar <- tapply(legal_nobait$kg, legal_nobait$Season, var)/season_n
  season_seWbar <- sqrt(season_varWbar)

  # combined
  Wbar <- sum(season_weights*season_Wbar)/sum(season_weights)
  varWbar <- sum((season_weights^2)*season_varWbar)/(sum(season_weights)^2)
  seWbar <- sqrt(varWbar)

  # LAM assuming YP is known without error
  YP_n <- YP_kg/Wbar + YP_kg/(Wbar^3)*varWbar
  var_YP_n <- (YP_kg^2)/(Wbar^4)*varWbar
  se_YP_n <- sqrt(var_YP_n)
  ci_YP_n <- YP_n + c(-1,1)*1.96*se_YP_n

  YP_n_all[i] <- YP_n
  ci_YP_n_all[i,] <- ci_YP_n
}
plot(NA, xlim=0:1, ylim=range(ci_YP_n_all),
     ylab="YP_n", xlab="Seasonal Weights",
     xaxt="n")
lines(possible_wts, YP_n_all)
lines(possible_wts, ci_YP_n_all[,1], lty=2)
lines(possible_wts, ci_YP_n_all[,2], lty=2)
legend("topleft", lty=1:2, legend=c("Estimate", "95% CI"))

axis(side=1, at=seq(0, 1, by=0.2),
     labels = paste0(seq(0, 100, 20), "% / ",
                    seq(100, 0, -20), "%"))


## seeing what happens when we resample the weights and draw season weightings from a Beta(5,5)
curve(dbeta(x, 5, 5))

## the sampling variances of the weight samples are incorporated using bootstrapping!
nsim <- 10000
YP_sim <- rep(NA, nsim)
for(i in 1:nsim) {
  winter_boot <- sample(legal_nobait$kg[legal_nobait$Season == "Winter"], replace=TRUE)
  summer_boot <- sample(legal_nobait$kg[legal_nobait$Season == "Summer"], replace=TRUE)

  # weight_winter <- 0.5
  weight_winter <- rbeta(1, 5, 5)
  weight_summer <- 1 - weight_winter

  Wbar <- (weight_winter * mean(winter_boot)) + (weight_summer * mean(summer_boot))
  YP_sim[i] <- YP_kg/Wbar
}
hist(YP_sim)
mean(YP_sim)
median(YP_sim)
sd(YP_sim)

quantile(YP_sim, c(0.025, 0.975))
