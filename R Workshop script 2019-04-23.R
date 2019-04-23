#Generating data
set.seed(1655317)
X <- rnorm(100, mean=0, sd=1)
X1 <- rnorm(100, mean=10, sd=2)
X2 <- rnorm(100, mean=11, sd=2)
X1.alt <- X1
X1.alt[13] <- 50
Z <- rexp(100, rate=1/10)
Z1 <- as.factor(rbinom(100, 2, 0.5))
Y <- 0.5*X1 + as.numeric(Z1) + rnorm(100)
Y1 <- as.factor(rbinom(100,1,(Y-min(Y))/(max(Y)-min(Y))))
eg.data <- data.frame(X,X1,X1.alt,X2,Z,Z1,Y,Y1)


#Looking at the first few observations
head(eg.data)

#Plotting each pair of variables
pairs(data.frame(X,X1,X2,Z,Z1,Y,Y1))
#removing categorical variables
pairs(data.frame(X,X1,X2,Z,Y))

#Basic decriptive statistics
mean(X)
sd(X)
median(X)
min(X)
max(X)

quantile(X)
quantile(X, probs=c(0.025,0.975)) #c() combines values into a vector

summary(X)
summary(eg.data)

###t-tests
?t.test

#1 sample
t.test(X)

#2 sample
t.test(X1,X2)
t.test(X1,Z) #this isn't a valid test!
hist(X1, col="grey")
hist(Z, col="grey")

#Paired t-test
t.test(X1,X2, paired=TRUE)

#QQ-plots
qqnorm(X)
qqline(X)

qqnorm(X1)
qqline(X1)

qqnorm(Z)
qqline(Z)

###Chi-square test
chisq.test(Z1,Y1) #works

table(Z1,Y1)
chisq.test(table(Z1,Y1)) #also works

chisq.test(Z1,Y1)$expected

###ANOVA
ANOVA.mod <- aov(Y~Z1) #models can be saved as R objects
summary(ANOVA.mod)

#Pairwise differences
TukeyHSD(ANOVA.mod, which="Z1")

#Boxplot
boxplot(Y~Z1)

###Regression
#SLR
SLR.mod <- lm(Y~X1, data=eg.data) 
summary(SLR.mod)

plot(SLR.mod) #a bit annoying to look at

par(mfrow=c(2,2)) #make 2x2 panel for viewing plots
plot(SLR.mod)
par(mfrow=c(1,1)) #return R to the default 1x1 plot panel

#Multiple Linear Regression
Reg.mod <- lm(Y~X1+X2+Z1)
summary(Reg.mod) #order of variables doesn't matter

#ANOVA fit to a regression model
anova(Reg.mod) #order of variables matters!

#Example of the same regression model with a different order
summary(lm(Y~X1+Z1+X2))
anova(lm(Y~X1+Z1+X2))

par(mfrow=c(2,2))
plot(Reg.mod)
par(mfrow=c(1,1))

plot(X1,resid(Reg.mod))

#Model omitting Z1 (nested model)
Reg.mod2 <- lm(Y~X1+X2)
summary(Reg.mod2)
#ANOVA to test if Z1 is necessary
anova(Reg.mod2,Reg.mod)

#Model omitting X2
Reg.mod3 <- lm(Y~X1+Z1)
#ANOVA to test if X2 is necessary (redundant test, same as original summary)
anova(Reg.mod3,Reg.mod)

#Confidence intervals
summary(Reg.mod)$coefficients

Reg.est <- summary(Reg.mod)$coefficients[,1] 
Reg.SE <- summary(Reg.mod)$coefficients[,2]
Reg.df <- Reg.mod$df.residual

#theoretical t-stat
tstat <- qt(0.975,df=Reg.df)

cbind(Est=Reg.est, 
	 Lower=Reg.est-tstat*Reg.SE,
	 Upper=Reg.est+tstat*Reg.SE)



#outlier
Reg.mod.alt <- lm(Y~X1.alt+X2+Z1)
summary(Reg.mod.alt)
anova(Reg.mod.alt)

par(mfrow=c(2,2))
plot(Reg.mod.alt)
par(mfrow=c(1,1))

hist(X1.alt, col="grey")
summary(X1.alt)
X1.alt[13]

###Logistic Regression
Logit.mod <- glm(Y1~Y, family = binomial(link="logit"))
summary(Logit.mod)

summary(Logit.mod)$coefficients

Logit.est <- summary(Logit.mod)$coefficients[2,1] 
Logit.SE <- summary(Logit.mod)$coefficients[2,2]


exp(Logit.est)

cbind(`Est OR`=exp(Logit.est),
	 	`Lower OR`=exp(Logit.est-1.96*Logit.SE),
		`Upper OR`=exp(Logit.est+1.96*Logit.SE))

#Finding and using other packages
#install.packages("survival")
library(survival)

?coxph
Surv.mod <- coxph(Surv(stop, event) ~ (rx + size + number) + cluster(id), bladder)

summary(Surv.mod)


