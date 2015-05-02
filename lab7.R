load(url("http://www.openintro.org/stat/data/evals.RData"))


plot(evals$score ~ evals$bty_avg)
xj <- jitter(evals$score)
plot(xj ~ evals$bty_avg)

m_bty <- lm(evals$score ~ evals$bty_avg)
abline(m_bty)

plot(m_bty$residuals~evals$bty_avg)
abline(0,0)
hist(m_bty$residuals)

#-------------------------------
plot(evals$bty_avg ~ evals$bty_f1lower)
cor(evals$bty_avg, evals$bty_f1lower)
plot(evals[,13:19])

m_bty_gen <- lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)

plot(m_bty_gen$residuals~evals$bty_avg)
abline(0,0)
hist(m_bty_gen$residuals)

multiLines(m_bty_gen)

#-------------------------------
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals)
summary(m_bty_rank)


#-------------------------------
m_full <- lm(score ~ rank + ethnicity + gender + language + 
                     age + cls_perc_eval  + cls_students + 
                     cls_level + cls_profs + cls_credits + 
                     bty_avg, data = evals)
summary(m_full)

#full - .1412
#no rank - .1418
#no bty - .1174
#no cls_profs - .1431
#no cls_students - .1402





