load(url("http://www.openintro.org/stat/data/ames.RData"))

area <- ames$Gr.Liv.Area
price <- ames$SalePrice

summary(area)
hist(area)

#q2
samp50 <- sample(area, 50)
summary(samp50)
hist(samp50)

samp100 <- sample(area, 100)
summary(samp100)
samp1000 <- sample(area, 1000)
summary(samp1000)

sampler <- function(size){
  samples <- rep(NA, size)
  
  for(i in 1:size) {
    samp <- sample(area, 50)
    samples[i] <- mean(samp)
  }
  samples
}

#q3
sample_means5000 <- sampler(5000)
hist(sample_means5000)

sample_means10 <- sampler(10)
sample_means50 <- sampler(50)
sample_means100 <- sampler(100)

#q4

par(mfrow = c(3, 1))

xlimits = range(sample_means10)

hist(sample_means10, breaks = 20, xlim = xlimits)
hist(sample_means50, breaks = 20, xlim = xlimits)
hist(sample_means100, breaks = 20, xlim = xlimits)
