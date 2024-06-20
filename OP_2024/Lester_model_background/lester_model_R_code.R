#' Calculate Maximum Sustainable Yield (MSY) as defined by the Lester model
#' @param lake (character) Name of the lake
#' @param temp (numeric) Mean annual temperature (in C)
#' @param area (numeric) Lake surface area (in ha)
#' @param mean_depth (numeric) Mean lake depth (in m)
#' @param max_depth (numeric) Maximum lake depth (in m)
#' @return Returns MSY in kg/(ha-yr)
#' @export 
#' @examples  
#' lake <- c("Crosswind", "Fielding", "Glacier Gap", "Louise", "Paxson", "Little Sevenmile", "Summit", "Susitna", "Round Tangle", "Shallow Tangle", "Combined Tangle")
#' temp <- c(-3.04, -5.89, -7.13, -3.29, -4.15, -5.89, -5.89, -3.29, -7.13, -7.13, -7.13)
#' area <- c(3716.55, 561.96, 178.06, 5913.07, 1569.92, 35.13, 1770.12, 3635.16, 156.15, 129.5, 285.65)
#' mean_depth <- c(15.9, 8.7, 7.1, 13, 8.4, 4.4, 15.6, 9, 10, 2, 6.4)
#' max_depth <- c(36.6, 23.1, 24.4, 51, 29.7, 14.1, 63.4, 36.6, 27.3, 19.8, 27.3)
#' lester_msy(lake, temp, area, mean_depth, max_depth)
lester_msy <- function(lake, temp, area, mean_depth, max_depth){
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
  msy <- round(msy_ha*area, 2)
  return(data.frame(lake=lake, temp=temp, area=area, mean_depth=mean_depth, max_depth=max_depth, msy=msy))
}
