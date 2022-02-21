#Codio Ch 7

#####Q1#####

library(ISLR2)
attach(Wage)
set.seed(1)

fit1 <- lm(wage~age)
summary(fit1)$coef #Q01-1
summary(fit1)$df #Q01-2
summary(fit1)$adj.r.squared #Q01-3

fit4 <- lm(wage~poly(age,4))
summary(fit4)$coef #Q01-4
summary(fit4)$df #Q01-5
summary(fit4)$adj.r.squared #Q01-6


####Q2####
fit4.manual <- lm(wage~age+I(age^2)+I(age^3)+I(age^4))
summary(fit4.manual)$coef #Q02-1
summary(fit4.manual)$df #Q02-2
summary(fit4.manual)$adj.r.squared #Q02-3

fit4.cbind <- lm(wage~cbind(age,age^2,age^3,age^4))
summary(fit4.cbind)$coef #Q02-4

fit4.raw <- lm(wage~poly(age,4,raw=T))
summary(fit4.raw)$coef #Q02-5

####Q3####
age.bounds <- range(age)
print(age.bounds) #Q03-1

age.grid <- seq(age.bounds[1],age.bounds[2])
age.grid
length(age.grid)

####Q4####
age.x <- list(age = age.grid)
print(age.x)
age.grid

fit4.pred <- predict(fit4, age.x, se = TRUE)
print(fit4.pred$fit)

fit4manual.pred <- predict(fit4.manual, age.x, se = TRUE)
print(fit4manual.pred$fit) #Q04-4

####Q5####
print(fit4.pred$se.fit) #Q05-1

se.ubound <- fit4.pred$fit + 2*fit4.pred$se.fit
print(se.ubound) #Q05-2

se.lbound <- fit4.pred$fit - 2*fit4.pred$se.fit
print(se.lbound) #Q05-3 

se.bands <- cbind(se.ubound,se.lbound)
print(se.bands) #Q05-4

####Q6####
#create plot
####Q7####
fit1 <- lm(wage~age)
fit2 <- lm(wage~poly(age,2))
fit3 <- lm(wage~poly(age,3))
fit4 <- lm(wage ~ poly(age,4))
fit5 <- lm(wage ~ poly(age,5))

poly.anova <- anova(fit1,fit2,fit3,fit4,fit5)
poly.anova

print(poly.anova$`Pr(>F)`) #Q07-1
poly.anova$RSS[1]-poly.anova$RSS[2]

####Q8####
print(summary(fit5)$coef) #Q08-1
3.1446392^2 #Q08-2


####Q9####
fit.classification <- glm(I(wage > 250) ~ poly(age,4), family = binomial)

predicted_fit <- predict(fit.classification, newdata = age.x, type = "link")
summary(predicted_fit) #Q09-2
        
prob_fit <- predict(fit.classification, newdata = age.x, type = "response")
summary(prob_fit) #Q09-3 

####Q10####
step_fit <- cut(age,4)
table(step_fit) #Q10-1

fit.step <- lm(wage~cut(age,4))
summary(fit.step)$coef #Q10-2

####Q11####
library(splines)

fit.3knots <- lm(wage~bs(age, knots = c(25,40,60)))
print(summary(fit.3knots)$coef) #Q11-1
fit.3knots$df

wage_hat <- predict(fit.3knots, newdata = age.x, se = T)
print(wage_hat$se.fit) #Q11-2

se.ubound <- wage_hat$fit + 2*wage_hat$se.fit
se.lbound <- wage_hat$fit - 2*wage_hat$se.fit
se.bands <- cbind(se.ubound, se.lbound)
print(se.bands) #Q11-3

#plot the 3-knot splines model
plot(age, wage, col="gray")
lines(age.grid, wage_hat$fit, lwd=2, col="red")
lines(age.grid, se.ubound, lty="dashed")
lines(age.grid, se.lbound, lty="dashed")
title("3-Knot Splines")

####Q12####
bs.3knots <- bs(age , knots = c(25, 40, 60))

class(bs.3knots) #Q12-1
dim(bs.3knots) #Q12-2
print(attr(bs.3knots, "degree")) #Q12-3

bs.6knots <- bs(age, df = 6)
print(attr(bs.6knots, "degree")) #Q12-4
print(attr(bs.6knots, "knots")) #Q12-5

bs.degree4 <- bs(age, degree = 4, df = 6)
print(attr(bs.degree4, "knots")) #Q12-6

print(attr(bs(age, degree = 2, df =6), "knots")) #Q12-7

####Q13####
ns.4df <- ns(age, df = 4)
print(attr(ns.4df, "knots")) #Q13-1

fit.ns <- lm(wage~ns.4df)
summary(fit.ns)$coef #Q13-2

pred.ns <- predict(fit.ns, newdata = age.x, se = T)
print(pred.ns$fit) #Q13-3

####Q14####
fit.smooth <- smooth.spline(age, wage, df = 16)
fit.smooth$df #Q14-1

fit.smooth.cv <- smooth.spline(age, wage, cv = TRUE)
fit.smooth.cv$df #Q14-2

plot(age, wage, xlim=age.bounds, cex=.5, color="darkgrey")
lines(fit.smooth , col = "red", lwd = 2)
lines(fit.smooth.cv , col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

####Q15####
fit.loess2<-loess(wage~ age, span=.2)
pred.loess2<-predict(fit.loess2, data.frame(age.x))
pred.loess2 #Q15-1

fit.loess5<-loess(wage~ age, span=.5)
pred.loess5<-predict(fit.loess5, data.frame(age.x))
summary(pred.loess5) #Q15-2

plot(age, wage, xlim=age.bounds, cex=.5, color="darkgrey")
title("Local Regression")
lines(age.grid, pred.loess2, col="red", lwd=2)
lines(age.grid, pred.loess5, col="blue", lwd=2)

legend("topright", legend=c("span = .2", "span = .5"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)

####Q16####
gam1 <- lm(wage ~ ns(year,4) + ns(age,5) + education)
summary(gam1) #Q16-1

#install.packages("gam")
library(gam)

plot.Gam(gam1, se=TRUE, col="red")



####Q17####

gam.m3 <- gam(wage ~ s(year,4) + s(age,5) + education)
summary(gam.m3) #Q17-1

plot(gam.m3, se=TRUE, col="red")

####Q18####
gam.m1 <- gam(wage~ s(age,5) + education)
gam.m2 <- gam(wage~ year + s(age,5) + education)
gam.m3 <- gam(wage~ s(year,4) + s(age,5) + education)

anova(gam.m1, gam.m2, gam.m3, test = "F") #Q18-1

####Q19####
pred_gam.m2 <- predict(gam.m2, newdata = Wage)
summary(pred_gam.m2) #Q19-1

mean((summary(gam.m2)$residuals)^2) #Q19-2

####Q20####
gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = .7) + education)
summary (gam.lo) #Q20-1

plot.Gam(gam.lo, se = TRUE , col = "green")

####Q21####
gam.lo.i <- gam(wage~ lo(year,age,span = 0.5) + education)
summary(gam.lo.i) 

#install.packages("akima")
library(akima)

plot(gam.lo.i)

####Q22####
gam.lr <- gam(I(wage > 250) ~ year + s(age,df = 5) + education, family = binomial)
summary(gam.lr) #Q22-1
plot(gam.lr, se=TRUE, col="red")

####Q23####
table(education, I(wage > 250)) #Q23-1

gam.lr.s <- gam(I(wage > 250) ~ year + s(age,df = 5) + education, subset = education != "1. HS Grad")
summary(gam.lr.s) #Q23-2

plot(gam.lr.s, se=TRUE, col="red")
