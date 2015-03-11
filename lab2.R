load(url("http://www.openintro.org/stat/data/kobe.RData"))
head(kobe)

kobe$basket[1:9]

kobe_streak <- calc_streak(kobe$basket)
barplot(table(kobe_streak))

mean(kobe_streak)
median(kobe_streak)
boxplot(kobe_streak)

table(kobe$basket)

#--example simulation
outcomes <- c("heads", "tails")
sim_fair_coin <- sample(outcomes, size = 100, replace = TRUE)
table(sim_fair_coin)

sim_unfair_coin <- sample(outcomes, size = 100, replace = TRUE, prob=c(.2,.8))
table(sim_unfair_coin)

#--simulated shooter
outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 133, replace = TRUE, prob=c(.45,.55))
table(sim_basket)

sim_streak <- calc_streak(sim_basket)
barplot(table(sim_streak))
