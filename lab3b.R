load(url("http://www.openintro.org/stat/data/ames.RData"))
population <- ames$Gr.Liv.Area

n <- 60
samp <- sample(population, n)

sample_mean <- mean(samp)

se <- sd(samp)/sqrt(n)
upper_lower <- sample_mean + c(-1,1)* 1.96 * se
upper_lower

#q5
samples <- 50
samp_mean <- rep(NA, samples)
samp_sd <- rep(NA, samples)

for(i in 1:samples){
  samp <- sample(population, n) # obtain a sample of size n = 60 from the population
  samp_mean[i] <- mean(samp)    # save sample mean in ith element of samp_mean
  samp_sd[i] <- sd(samp)        # save sample sd in ith element of samp_sd
}

z <- qnorm(.975)
lower <- samp_mean - z * samp_sd / sqrt(n) 
upper <- samp_mean + z * samp_sd / sqrt(n)

plot_ci(lower, upper, mean(population))
