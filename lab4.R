
#fage: father's age in years.
#mage: mother's age in years.
#mature: maturity status of mother.
#weeks: length of pregnancy in weeks.
#premie: whether the birth was classified as premature (premie) or full-term.
#visits: number of hospital visits during pregnancy.
#marital: whether mother is married or not married at birth.
#gained: weight gained by mother during pregnancy in pounds.
#weight: weight of the baby at birth in pounds.
#lowbirthweight: whether baby was classified as low birthweight (low) or not (not low).
#gender: gender of the baby, female or male.
#habit: status of the mother as a nonsmoker or a smoker.
#whitemom: whether mom is white or not white.

load(url("http://bit.ly/dasi_nc"))
summary(nc)
gained_clean <- na.omit(nc$gained)
n <- length(gained_clean)

#2
no.weight <- sum(is.na(nc$gained))


boot_means = rep(NA, 100)

for(i in 1:100){
  boot_sample = sample(gained_clean, n, replace = TRUE)
  boot_means[i] = mean(boot_sample)                       
}

hist(boot_means)

############################################

source("http://bit.ly/dasi_inference")

#5
inference(nc$gained, type = "ci", method = "simulation", 
          conflevel = 0.95, est = "median", boot_method = "se")

#6
plot(weight~habit,data=nc)


#7
inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci", 
          null = 0, alternative = "twosided", method = "theoretical", 
          order = c("smoker","nonsmoker"))

#8
young_mom <- nc[nc$mature == 'younger mom',]
young_int <- c(min(young_mom$mage),max(young_mom$mage))
mat_mom <- nc[nc$mature == 'mature mom',]
mat_int <- c(min(mat_mom$mage),max(mat_mom$mage))

############################################

load(url("http://bit.ly/dasi_gss_ws_cl"))
plot(wordsum~class,data=gss)

inference(y = gss$wordsum, x = gss$class, est = "mean", 
          type = "ht", alternative = "greater", method = "theoretical")
