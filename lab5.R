source("http://bit.ly/dasi_inference")
load(url("http://www.openintro.org/stat/data/atheism.RData"))

us12 <- subset(atheism, atheism$nationality == "United States" & atheism$year == "2012")

#8
p.us <- dim(us12)[1]
p.us.atheism <- sum(us12$response == 'atheist')/p.us
p.us.se <- sqrt(p.us.atheism*(1-p.us.atheism)/p.us)
p.us.me <- p.us.se * 1.96 

inference(us12$response, est = "proportion", type = "ci", 
          method = "theoretical", success = "atheist")


#9
n9 <- 1000
p9 <- seq(0, 1, 0.01)
me9 <- 2*sqrt(p9*(1 - p9)/n9)
plot(me9 ~ p9)

#10
spain <- subset(atheism, atheism$nationality == "Spain" & 
                        (atheism$year == "2012" | atheism$year == "2005"))

inference(spain$response, spain$year, est = "proportion", type = "ci", 
          method = "theoretical", success = "atheist")

#11
us <- subset(atheism, atheism$nationality == "United States" & 
                  (atheism$year == "2012" | atheism$year == "2005"))

inference(us$response, us$year, est = "proportion", type = "ci", 
          method = "theoretical", success = "atheist")


