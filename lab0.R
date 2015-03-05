#Refers to the number of male and female births in the United States. 
#The data set contains the data for all years from 1940 to 2002. 
source("http://www.openintro.org/stat/data/present.R")
present

#q1
dim(present)
names(present)

#q2
girls <- present$girls

#q3
plot(present$year,present$girls)

#q4
present$total <- present$girls+present$boys
plot(present$year,present$total)
imax <- which.max(present$total)
present[imax,]

#q5
present$prop.boys <- present$boys / present$total
plot(present$year, present$prop.boys)

#q6
present$more.boys <- present$prop.boys > .5
sum(present$more.boys)/dim(present)[1]

#q7
plot(present$year, present$boys/present$girls)

#q8
present$diff <- abs(present$boys-present$girls)
imaxdiff <- which.max(present$diff)
present[imaxdiff,]

