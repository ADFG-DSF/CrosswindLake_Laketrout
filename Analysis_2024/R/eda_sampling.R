library(tidyverse)

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

par(mfrow=c(2,2))
with(all_data, boxplot(FL ~ bait))
with(all_data, boxplot(FL ~ event))
with(all_data, boxplot(FL ~ paste(event, bait)))
with(all_data, boxplot(FL ~ paste(bait, event)))

with(all_data, table(bait, event))
with(subset(all_data, bait=="Y"), t.test(FL ~ event))
with(subset(all_data, bait=="N"), t.test(FL ~ event))

lm1 <- lm(FL ~ bait*event, data=all_data)
lm1 %>% AIC
lm1 %>% summary

lm2 <- lm(FL ~ bait+event, data=all_data)
lm2 %>% AIC
lm2 %>% summary


par(mfrow=c(2,2))
with(all_data, boxplot(weight_kg ~ bait))
with(all_data, boxplot(weight_kg ~ event))
with(all_data, boxplot(weight_kg ~ paste(event, bait)))
with(all_data, boxplot(weight_kg ~ paste(bait, event)))

with(all_data, table(bait, event))
with(subset(all_data, bait=="Y"), t.test(weight_kg ~ event))
with(subset(all_data, bait=="N"), t.test(weight_kg ~ event))

lm1 <- lm(weight_kg ~ bait*event, data=all_data)
lm1 %>% AIC
lm1 %>% summary

lm2 <- lm(weight_kg ~ bait+event, data=all_data)
lm2 %>% AIC
lm2 %>% summary

## SIMPSON'S PARADOX!!
