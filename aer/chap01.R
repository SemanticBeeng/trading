
data("Journals", package = "AER")

dim(Journals)

names(Journals)

plot(log(subs) ~ log(price/citations), data = Journals)

j_lm <- lm(log(subs) ~ log(price/citations), data = Journals)
abline(j_lm)

summary(j_lm)

# experiment
plot(Journals$subs)
plot(log(Journals$subs))

plot(Journals$price / Journals$citations)
plot(log(Journals$price / Journals$citations))

# elasticity of the demand with respect to the price per citation is −0.5331, which
# is signiﬁcantly diﬀerent from 0 at all conventional levels. 
j_lm$coefficients[2]

## 
data("CPS1985", package = "AER")
cps <- CPS1985

library("quantreg")

cps_lm <- lm(formula = log(wage) ~ experience + I(experience^2) + education, 
             data = cps)

# quantile regression
cps_rq <- rq(formula = log(wage) ~ experience + I(experience^2) + education, 
             data = cps, 
             tau = seq(0.2, 0.8, by = 0.15))

cps2 <- data.frame(education = mean(cps$education), 
                   experience = min(cps$experience):max(cps$experience))

cps2 <- cbind(cps2, predict(cps_lm, newdata = cps2,  interval = "prediction"))

cps2 <- cbind(cps2, predict(cps_rq, newdata = cps2, type = ""))

plot(log(wage) ~ experience, data = cps)

for(i in 6:10) 
  lines(cps2[,i] ~ experience, data = cps2, col = "red")

plot(summary(cps_rq))

