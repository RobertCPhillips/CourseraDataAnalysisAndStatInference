load(url("http://www.openintro.org/stat/data/mlb11.RData"))

#--------------------------------------------------
# at bats
#--------------------------------------------------
cor_at_bats <- cor(mlb11$runs, mlb11$at_bats)
plot(runs~at_bats,data=mlb11)

fit1 <- lm(runs ~ at_bats, data = mlb11)
abline(fit1)
summary(fit1)

mlb11$runs_at_bats <- predict(fit1,mlb11)
mlb11$runs_at_bats_res <- mlb11$runs - mlb11$runs_at_bats

#check linearity
plot(fit1$residuals ~ mlb11$at_bats)
abline(h = 0, lty = 3)  # adds a horizontal dashed line at y = 0

#check if residuals are nearly normal
hist(fit1$residuals)
qqnorm(fit1$residuals)
qqline(fit1$residuals)  # adds diagonal line to the normal prob plot


#--------------------------------------------------
# homeruns
#--------------------------------------------------
cor_homeruns <- cor(mlb11$runs, mlb11$homeruns)
plot(runs~homeruns,data=mlb11)

fit2 <- lm(runs ~ homeruns, data = mlb11)
abline(fit2)
summary(fit2)

mlb11$runs_homeruns <- predict(fit2,mlb11)
mlb11$runs_homeruns_res <- mlb11$runs - mlb11$runs_homeruns

#check linearity
plot(fit2$residuals ~ mlb11$homeruns)
abline(h = 0, lty = 3)  # adds a horizontal dashed line at y = 0

#check if residuals are nearly normal
hist(fit2$residuals)
qqnorm(fit2$residuals)
qqline(fit2$residuals)  # adds diagonal line to the normal prob plot

#--------------------------------------------------
# hits
#--------------------------------------------------
cor_hits <- cor(mlb11$runs, mlb11$hits)
plot(runs~hits,data=mlb11)

fit3 <- lm(runs ~ hits, data = mlb11)
abline(fit3)
summary(fit3)

mlb11$runs_hits <- predict(fit3,mlb11)
mlb11$runs_hits_res <- mlb11$runs - mlb11$runs_hits

#check linearity
plot(fit3$residuals ~ mlb11$hits)
abline(h = 0, lty = 3)  # adds a horizontal dashed line at y = 0

#check if residuals are nearly normal
hist(fit3$residuals)
qqnorm(fit3$residuals)
qqline(fit3$residuals)  # adds diagonal line to the normal prob plot

#--------------------------------------------------
# batting average
#--------------------------------------------------
cor_bat_avg <- cor(mlb11$runs, mlb11$bat_avg)
plot(runs~bat_avg,data=mlb11)

fit4 <- lm(runs ~ bat_avg, data = mlb11)
abline(fit4)
summary(fit4)

mlb11$runs_bat_avg <- predict(fit4,mlb11)
mlb11$runs_bat_avg_res <- mlb11$runs - mlb11$runs_bat_avg

#check linearity
plot(fit4$residuals ~ mlb11$bat_avg)
abline(h = 0, lty = 3)  # adds a horizontal dashed line at y = 0

#check if residuals are nearly normal
hist(fit4$residuals)
qqnorm(fit4$residuals)
qqline(fit4$residuals)  # adds diagonal line to the normal prob plot

#--------------------------------------------------
# strikeouts
#--------------------------------------------------
cor_strikeouts <- cor(mlb11$runs, mlb11$strikeouts)
plot(runs~strikeouts,data=mlb11)

fit5 <- lm(runs ~ strikeouts, data = mlb11)
abline(fit5)
summary(fit5)

mlb11$runs_strikeouts <- predict(fit5,mlb11)
mlb11$runs_strikeouts_res <- mlb11$runs - mlb11$runs_strikeouts

#check linearity
plot(fit5$residuals ~ mlb11$strikeouts)
abline(h = 0, lty = 3)  # adds a horizontal dashed line at y = 0

#check if residuals are nearly normal
hist(fit5$residuals)
qqnorm(fit5$residuals)
qqline(fit5$residuals)  # adds diagonal line to the normal prob plot

#--------------------------------------------------
# wins
#--------------------------------------------------
cor_wins <- cor(mlb11$runs, mlb11$wins)
plot(runs~wins,data=mlb11)

fit6 <- lm(runs ~ wins, data = mlb11)
abline(fit6)
summary(fit6)

mlb11$runs_wins <- predict(fit6,mlb11)
mlb11$runs_wins_res <- mlb11$runs - mlb11$runs_wins

#check linearity
plot(fit6$residuals ~ mlb11$wins)
abline(h = 0, lty = 3)  # adds a horizontal dashed line at y = 0

#check if residuals are nearly normal
hist(fit6$residuals)
qqnorm(fit6$residuals)
qqline(fit6$residuals)  # adds diagonal line to the normal prob plot

#--------------------------------------------------
# new_onbase
#--------------------------------------------------
cor_onbase <- cor(mlb11$runs, mlb11$new_onbase)
plot(runs~new_onbase,data=mlb11)

fit7 <- lm(runs ~ new_onbase, data = mlb11)
abline(fit7)
summary(fit7)

#--------------------------------------------------
# new_slug
#--------------------------------------------------
cor_slug <- cor(mlb11$runs, mlb11$new_slug)
plot(runs~new_slug,data=mlb11)

fit8 <- lm(runs ~ new_slug, data = mlb11)
abline(fit8)
summary(fit8)

#--------------------------------------------------
# new_obs
#--------------------------------------------------
cor_obs <- cor(mlb11$runs, mlb11$new_obs)
plot(runs~new_obs,data=mlb11)

fit9 <- lm(runs ~ new_obs, data = mlb11)
abline(fit9)
summary(fit9)
