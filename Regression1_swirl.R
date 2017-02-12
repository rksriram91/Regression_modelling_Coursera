plot(jitter(child,4)~parent,galton)
regrline<-lm(child~parent , galton)
regrline<-lm(child~parent , galton)

#2 Residuals

#Residuals are balanced among mean points

fit=lm(child~parent , galton)
summary(fit)
mean(fit$residuals)
cov(fit$residuals,galton$parent)
fit$coef[1]
ols.ic <- fit$coef[1]
ols.slope <- fit$coef[2]
#Here are the vectors of variations or tweaks
sltweak <- c(.01, .02, .03, -.01, -.02, -.03) #one for the slope
ictweak <- c(.1, .2, .3, -.1, -.2, -.3)  #one for the intercept
lhs <- numeric()
rhs <- numeric()
#left side of eqn is the sum of squares of residuals of the tweaked regression line
for (n in 1:6) lhs[n] <- sqe(ols.slope+sltweak[n],ols.ic+ictweak[n])
#right side of eqn is the sum of squares of original residuals + sum of squares of two tweaks
for (n in 1:6) rhs[n] <- sqe(ols.slope,ols.ic) + sum(est(sltweak[n],ictweak[n])^2)

lhs-rhs
all.equal(lhs,rhs)
varChild <- var(galton$child)
varRes <- var(fit$residuals)
varEst <- var(est(ols.slope, ols.ic))
all.equal(varChild,varRes+varEst)

#
efit <- lm(accel ~ mag+dist, attenu)
cov(efit$residuals, attenu$mag)
cov(efit$residuals, attenu$dist)


#Regession Swirl 3 Least Squares estimation


