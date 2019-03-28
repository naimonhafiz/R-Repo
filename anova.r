install.packages("faraway")
require(faraway)
data(savings)
head(savings)
summary(savings)

# Fit model
m1 <- lm(sr ~ log(dpi), data = savings)
summary(m1)
summary(m1,corr=T)
# Plot
plot(sr~log(dpi),data = savings)
abline(m1)

# Estimates of beta_0 and beta_1
confint(m1)

# Estimate sigma
sqrt(deviance(m1)/df.residual(m1))
summary(m1)$sigma

# R2
1 - deviance(m1)/sum((savings$sr-mean(savings$sr))^2)
summary(m1)$r.square

SSTO <- sum((savings$sr-mean(savings$sr))^2)
SSE <- deviance(m1)
SSR <- SSTO - SSE

# F test
(tss <- sum((savings$sr-mean(savings$sr))^2))
(rss <- deviance(m1))
df.residual(m1)
(fstat <- ((tss-rss)/1)/(rss/df.residual(m1)))
1-pf(fstat,1,df.residual(m1))

# T test
sqrt(fstat)
(tstat <- summary(m1)$coef[2,3])
2*(1-pt(sqrt(fstat),df.residual(m1)))

# General Linear F test
m2 <- lm(sr ~ 1, data = savings) # Reduced Model, beta_1 = 0
summary(m2)
(rss2 <- deviance(m2))
(fstat <- (deviance(m2) - deviance(m1))/(deviance(m1)/df.residual(m1)))
1-pf(fstat,1,df.residual(m1))

# Another way to compare nested models
anova(m2,m1)
