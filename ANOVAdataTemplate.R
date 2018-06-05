#RA measurements
#Author: Janice Love
#Date: June 2, 2018


myData <- read.csv("//home/janicelove/Desktop/ra.slc12a1.csv",header=FALSE)

plot(V2 ~ as.factor(V1), data= myData, ylab = "absolute length (micrometers)", xlab="treatment")

#to reorder x-axis 
x1 = factor (myData$V1, levels=c("DMSO", "RA", "DEAB"))
plot(V2 ~ x1, data= myData, ylab = "absolute length of irx1b+ domain (micrometers)", xlab="treatment", 
     col = c("#999999", "#7D05fc", "#f9ea04"))

#The following is from: http://www.sthda.com/english/wiki/one-way-anova-test-in-r

#Compute analysis of variance
res.aov <- aov(V2 ~ V1, data = myData)

#Summary of the analysis
summary(res.aov)

#Tukey
#diff: difference between means of the two groups, making multiple pair-wise comparisons
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

TukeyHSD(res.aov)

#---------------------------------------
#Check ANOVA assumptions
#---------------------------------------

#Check the homogeneity of variance assumption
#The residuals versus fits plot can be used to check the homogeneity of variances

# 1. Homogeneity of variances
plot(res.aov, 1)

# 2. Normality: check the normality assumption
#Normality plot of residuals. In this plot, the quantiles of the residuals are plotted #against the quantiles of the normal distribution. 

#The normal probability plot of residuals is used to check the assumption that the residuals are normally distributed. It should approximately follow a straight line.
plot(res.aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test: non parametric alternative to one-way ANOVA 
shapiro.test(x = aov_residuals )

#---------------------------------------
#Non-parametric alternative to one-way ANOVA test
#---------------------------------------
#Note that, a non-parametric alternative to one-way ANOVA is Kruskal-Wallis rank sum test, 
#which can be used when ANOVA assumptions are not met.

kruskal.test(V2 ~ V1, data = myData)
