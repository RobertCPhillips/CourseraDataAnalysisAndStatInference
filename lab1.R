#genhlth - respondents were asked to evaluate their general health, 
#          responding either excellent, very good, good, fair or poor. 
#exerany - indicates whether the respondent exercised in the past month (1) 
#          or did not (0). 
#hlthplan - indicates whether the respondent had some form of health 
#           coverage (1) or did not (0). 
#smoke100 -  indicates whether the respondent had smoked at least 100 
#            cigarettes in her lifetime. 

#The other variables record the respondent's height in inches, 
#weight in pounds as well as their desired weight, wtdesire, age in years, 
#and gender.

source("http://www.openintro.org/stat/data/cdc.R")
colnames(cdc)

#2,3,4
str(cdc)

#5
genders <- table(cdc$gender)

#6
hlths <- table(cdc$genhlth)
prop.table(hlths)

#7
mosaicplot(smoke100~gender, data=cdc)
smoking <- table(cdc[,c("smoke100","gender")])
prop.table(smoking)

#8
cdc$under23 <- cdc$age < 23
table(cdc$under23,cdc$smoke100)

#9
cdc$bmi <- cdc$weight * 703 / (cdc$height)^2
boxplot(bmi~genhlth, data=cdc)

#10
plot(wtdesire~weight, data=cdc)
cor(cdc$weight, cdc$wtdesire)


