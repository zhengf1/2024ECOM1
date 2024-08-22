##---------------------------------------------------------------------------------
############### ECOM20001 Econometrics 1 Tutorial 1
# by Zheng Fan; fan.z@unimelb.edu.au
# Contact me if you have any questions.

# please refer to Dave's R code for detailed explanations (for your self learning)
# it contains much more than the tutorial questions with extra examples,
# so please be patient when go through the code

rm(list=ls()) # remove everything in the environment to start a new project

# --------- set the working directory; yours will be different
setwd( dirname(rstudioapi::getActiveDocumentContext()$path) )
# or
setwd("~/Dropbox/01 UoM-Teaching/2024-S2-Ecom1/Week5") # your path should be different 

# --------- Load Stargazer package for summary statistics and regression tables
library(stargazer)

#**********************************************************************************************
# HEIGHT AND EARNINGS: lm(), confint(), coef()

## Load dataset on income and height
mydata1=read.csv(file="tute5_height.csv")

## Regression of earnings on height
earn_reg1=lm(earnings~height,data=mydata1)  # run the single linear regression
summary(earn_reg1)                          # summarize the regression results

## 95% CI for the OLS slope coefficient on height from earn_reg1 regression
confint(earn_reg1, 'height', level=0.95)

## 95% CI for the OLS intercept from earn_reg1 regression
confint(earn_reg1, '(Intercept)', level=0.95)


## Obtain regression coefficients from earn_reg1 regression
beta = coef(summary(earn_reg1))[, "Estimate"] 
# or 
beta = earn_reg1$coefficients
# beta[1] is OLS estimate of intercept
# beta[2] is OLS estimate of slope

## Obtain standard errors from coefficients earn_reg1 regression
se = coef(summary(earn_reg1))[, "Std. Error"] 
# se[1] is standard error of OLS estimate of intercept
# se[2] is standard error of OLS estimate of slope

## Compute 95% CI of the regression slope coefficient by hand
CI95_low=beta[2]-1.96*se[2]    # lower bound of 95% CI
CI95_upp=beta[2]+1.96*se[2]    # upper bound of 95% CI

# ------------------------- Q2
## 95% CI for increasing height by 100cm on earnings
CI95_low_100=100*(beta[2]-1.96*se[2])    # lower bound of 95% CI
CI95_upp_100=100*(beta[2]+1.96*se[2])    # upper bound of 95% CI
paste("95% CI lower bound for 100cm increase in earnings is: ", CI95_low_100)
paste("95% CI upper bound for 100cm increase in earnings is: ", CI95_upp_100)

# ------------------------- Q3
## t-statistic and p-value for null that slope=0.03
tstat2=(beta[2]-0.03)/se[2]
(critical_value = qnorm(0.975)) # 5%; two tail.
pval2=2*pnorm(-abs(tstat2))
paste("pvalue for 2-sided test of null that slope=0.03 is:", pval2)
# Fail to reject null, p-value (pval2)=0.280



#**********************************************************************************************
# REGRESSION: HOMICIDES AND POLICE

## Load dataset on income and height
mydata2=read.csv(file="tute5_crime.csv")

## Summary Statistics
stargazer(mydata2, 
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),  
          type="text", title="Descriptive Statistics",
          out="sumstats2.txt")
# So a typical county had 3066 police offices and 13 homicides in 2012
# The range is considerable: min police and homicides is 809 and 1
# The max police and homicides is 31435 and 111 (!)

## Scatter plot of homicides and police numbers
pdf("q2_scat_homicides_police1.pdf")
plot(mydata2$police,mydata2$homicides,
     main="Homicides and Police Force Across England and Wales Counties",
     xlab="Number of Police Officers in 2012",
     ylab="Number of Homicides in 2012",
     col="forestgreen",
     pch=16)
dev.off()

## identify the outlier
mydata2$county[which(mydata2$homicides>100)]

plot(mydata2$police[-25],mydata2$homicides[-25])

## Scatter plot removing potential outlier
pdf("q2_scat_homicides_police2.pdf")
plot(mydata2$police[mydata2$homicides<100],mydata2$homicides[mydata2$homicides<100],
     main="Homicides and Police Force Across England and Wales Counties",
     sub="(Outlier Removed)",
     xlab="Number of Police Officers in 2012",
     ylab="Number of Homicides in 2012",
     col="forestgreen",
     pch=16)
dev.off()



# ------------------------- Q3
crime_reg1=lm(homicides~police, data=mydata2)
summary(crime_reg1)
confint(crime_reg1, 'police', level=0.95)


# ------------------------- Q4
## Construct re-scalled police independent variable in terms of 1000's of police
mydata2$police_1000=mydata2$police/1000     # save the re-scaled police variable in mydata2
summary(mydata2)
# Notice how with the summary statistics police_1000 now shows up with a mean of 
# 3.066 (which means 3066 police on average) which has a similar scale as the mean 
# of homicides in the sample of 12.93

## Re-run our homicides and police regression with our re-scaled police_1000 regressor
## and compute the 95% confidence interval
crime_reg2=lm(homicides~police_1000, data=mydata2)
summary(crime_reg2)
confint(crime_reg2, 'police_1000', level=0.95)


# ------------------------- Q5
# Outliers

# Recall from our scatter plot above "q2_scat_homicides_police1.pdf" that 'Metropolitan Police'
# is a potential outlier. Let's look at the influence of the outlier on our results using 
# re-scaled police numbers throughout

## Regression results without potential outlier
crime_reg3=lm(homicides[homicides<100]~police_1000[homicides<100], data=mydata2)
summary(crime_reg3)
confint(crime_reg3, 'police_1000[homicides < 100]', level=0.95)


## Plotting the impact of the oulier on regression results, highlighting the outlier's impact
pdf("q2_scat_homicides_police3.pdf")
plot(mydata2$police_1000,mydata2$homicides,
     main="Homicides and Police Force Across England and Wales Counties",
     xlab="Number of Police Officers in 2012 (1000s)",
     ylab="Number of Homicides in 2012",
     col="forestgreen",
     pch=16)
abline(crime_reg2, col="blue", lwd=2)
abline(crime_reg3, col="red", lwd=2)
legend("bottomright", c("regression 2 (include outlier)","regression 3 (omit outlier)"), col = c("blue","red"),pch=16)
dev.off()







