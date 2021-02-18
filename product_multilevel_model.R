# Jame Fanjul
# Multilevel Model
# Nested hierarchies


# Clean memory
rm(list=ls())
dev.off()

library(ggplot2)

# Working directory
#\""~
setwd("C:/YOUR PATH")

## F Distribution
# Quantiles
q_95=qf(0.05,2,2,lower.tail=F)
q_95

#Percentiles
pf(q_95,df1 = 2,df2 = 2)

# Figure
# When the degrees of freedom tend to infinity 
# the distribution function of f becomes 1
x = seq(0,5,0.001)
plot(x,df(x,df1=20,df2=20),xlim=c(0,5),ylim=c(0,3),"l",
     col="green",xlab = "Valor F")

# Random model versus mixed model
# We want to estimate the variability of the counts of bacteria in food samples in a
# supermarket, for this purpose 20 types of packaging are chosen, 3 samples are taken per package
# And the sample is repeated twice.

num=read.csv("num.csv",sep=";")

# What particularity do these boxplots have?
boxplot(c~emp,num)


aov0=aov(c~emp+mue,num)
# Can you identify the error in this ANOVA?
summary(aov0)

# Normality assessment
num$emp_f=factor(num$emp)
num$mue_f=factor(num$mue)

aov1=aov(c~emp_f+mue_f,num)
summary(aov1)
shapiro.test(aov1$residuals)


aov2=aov(log10(c)~emp_f+mue_f,num)
summary(aov2)
shapiro.test(aov2$residuals)


# Nested structure
# Against what error term are the F values being calculated?
aov3=lm(log10(c)~emp_f + emp_f/mue_f,num)
anova(aov3)


# Correction to the model: unconditional nesting = hierarchical nesting
if(!require(EMSaov)){install.packages("EMSaov")}
library(EMSaov)
num$lc<-log10(num$c)
aov4=EMSanova(lc~emp_f + mue_f,
              data = num,
              type = c("R","R"),
              nested = c(NA, "emp_f"))
aov4