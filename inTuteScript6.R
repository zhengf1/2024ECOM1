##---------------------------------------------------------------------------------
############### ECOM20001 Econometrics 1 Tutorial 6
# by Zheng Fan; fan.z@unimelb.edu.au
# Contact me if you have any questions.

# please refer to Dave's R code for detailed explanations (for your self learning)
# it contains much more than the tutorial questions with extra examples,
# so please be patient when go through the code
# 
# THIS R.script has(will) been uploaded to
# https://github.com/zhengf1/2024ECOM1
# Please either write your own code, or follow the scripts provieded.

## install the package if you have not done this yet
# install.packages("stargazer")
# install.packages("AER")

rm(list=ls()) # remove everything in the environment to start a new project

# --------- set the working directory; yours will be different
setwd( dirname(rstudioapi::getActiveDocumentContext()$path) )
# or
setwd("~/Dropbox/01 UoM-Teaching/2024-S2-Ecom1/Week6") # your path should be different 

## Load Stargazer package for summary statistics and regression tables
library(stargazer)
## Load Applied Econometrics package (see above for installing AER package)
library(AER)


# ------------------ Q0 ------------------
## Load dataset on income and height
mydata1=read.csv(file="tute6_smoke.csv")

## List the variables in the dataset named mydata1
names(mydata1)

## Dimension of the dataset
# 3000 observations (babies and their mothers)
# 13 variables: id, birthweight, smoker, alcohol, drinks, nprevisit, tripre1, tripre2, tripre3,
#               tripre0, unmarried, educ, age            
dim(mydata1)


# ------------------ Q1 ------------------
# Understand the data: SUMMARY STATISTICS

# Compute summary statistics to get a sense of a typical (baby,mother) pair in the data
## Summary Statistics
stargazer(mydata1, 
          summary.stat = c("n", "mean", "sd", "median", "min", "max"),  
          type="text", title="Descriptive Statistics",
          out="sumstats1.txt")

# ------------------ Q2 ------------------
# density plot and Hypothesis testing

# Birthweight probability densities among smokers and non-smokers
plot(density(mydata1$birthweight[mydata1$smoker==1]), 
     col="red",lty=1,main="Birthweight Among Smoking and Non-Smoking Mothers", xlab="Birthweight")
lines(density(mydata1$birthweight[mydata1$smoker==0]), col="blue",lty=2)
legend("topright", legend=c("Smoker", "Non-Smoker"), 
       col=c("red","blue"), lty=c(1,2))

# T-test of difference in birthweight for smokers and non-smokers
mean(mydata1$birthweight[mydata1$smoker==1])
mean(mydata1$birthweight[mydata1$smoker==0])
mean(mydata1$birthweight[mydata1$smoker==1])-mean(mydata1$birthweight[mydata1$smoker==0])
t.test(mydata1$birthweight[mydata1$smoker==1],mydata1$birthweight[mydata1$smoker==0])

# ------------------ Q3 ------------------
# POTENTIAL FOR OMITTED VARIABLE BIAS AND IMPORTANCE OF CONTROLS

# 1. Birth weight, smoking, alcohol

# Birth weight densities, drinkers and non-drinkers
plot(density(mydata1$birthweight[mydata1$alcohol==1]), 
     col="red",lty=1,main="Birthweight Among Drinking and Non-Drinking Mothers", xlab="Birthweight")
lines(density(mydata1$birthweight[mydata1$alcohol==0]), col="blue",lty=2)
legend("topright", legend=c("Drinker", "Non-Drinker"), 
       col=c("red","blue"), lty=c(1,2))

# T-test of difference in drinking for smokers and non-smokers
mean(mydata1$alcohol[mydata1$smoker==1])
mean(mydata1$alcohol[mydata1$smoker==0])
mean(mydata1$alcohol[mydata1$smoker==1])-mean(mydata1$alcohol[mydata1$smoker==0])
t.test(mydata1$alcohol[mydata1$smoker==1],mydata1$alcohol[mydata1$smoker==0])


# 2. Birth weight, smoking, prenatal care

# Birth weight densities, with and without pre-natal care
plot(density(mydata1$birthweight[mydata1$tripre0==0]), 
     col="red",lty=1,main="Birthweight Among Babies With and Without Prenatal Care", xlab="Birthweight")
lines(density(mydata1$birthweight[mydata1$tripre0==1]), col="blue",lty=2)
legend("topright", legend=c("Pre-Natal Care", "No Pre-Natal Care"), 
       col=c("red","blue"), lty=c(1,2))

# T-test of difference in prenatal care for smokers and non-smokers
mean(mydata1$tripre0[mydata1$smoker==1])
mean(mydata1$tripre0[mydata1$smoker==0])
mean(mydata1$tripre0[mydata1$smoker==1])-mean(mydata1$tripre0[mydata1$smoker==0])
t.test(mydata1$tripre0[mydata1$smoker==1],mydata1$tripre0[mydata1$smoker==0])


# 3. Birth weight, smoking, education

# Scatterplot between birth weight and education with line of best fit
birthweight_educ_reg=lm(birthweight~educ,data=mydata1)
plot(mydata1$educ,mydata1$birthweight,
     main="Birth Weight and Years of Education",
     xlab="Years of Education",
     ylab="Birth Weight",
     col="grey",
     pch=16)
abline(birthweight_educ_reg, col="red", lwd=2)

# T-test of difference in education for smokers and non-smokers
mean(mydata1$educ[mydata1$smoker==1])
mean(mydata1$educ[mydata1$smoker==0])
mean(mydata1$educ[mydata1$smoker==1])-mean(mydata1$educ[mydata1$smoker==0])
t.test(mydata1$educ[mydata1$smoker==1],mydata1$educ[mydata1$smoker==0])

# --------- Omitted variable bias:
# Does missing alcohol/Pre-Natal Care/education matter?
#
# Whether beta1 is likely to be biased, and if so, 
# what is the direction of the bias in each case? 
#
# eq1: -- birthweight = beta0 + beta1 * smoker + u
# eq2: -- birthweight = beta0 + beta1 * smoker + beta2 * alcohol + u
# 
# e.g. if alcohol is included(excluded) would the parameter 
# estimate for smoker (beta1) be positively or negatively biased - 
# Do this in turn for each variable?
#
# Put it simple:
# Is beta1 from eq1 larger or smaller than beta1 from eq2.
# 
# Discuss!


# ----- Alcohol:
# Alcohol is likely to be negative related to birthweight.
# Alcohol is likely to be positive related to smoker.
# consider increase in smoker, increase in alcohol, decrease birthweight
#
# Missing alcohol, leading downward bias on beta1,
# as smoker captures the channel from alcohol

# ----- Pre-Natal Care:
# Pre-Natal Care is likely to be positive related to birthweight.
# Pre-Natal Care is likely to be negative related to smoker.
# consider increase in smoker, decrease in Pre-Natal Care, decrease birthweight

# ----- Education:
# Education is likely to be positive related to birthweight.
# Education is likely to be negative related to smoker.
# consider increase in smoker, decrease in Pre-Natal Care, decrease birthweight


# ------------------ Q4 ------------------
# REGRESSIONS: CHECKING HOMOSKEDASTICITY AND HETEROSKEDASTICITY

# Comparing single linear regression results of birth weight on smoking status, accounting
# and not accounting for heteroskedasticity

# The coeftest() command is used to compute heteroskedasticity-robust standard errors
# To run the command you take two steps, similar to how you output regression results
# reg=lm(y~x,data=mydata)                     # run the regression
# coeftest(reg, vcov = vcovHC(reg, "HC1"))    # print regression results with robust standard errors
#
# default summary print regression results with homoskedasticity
# summary(reg)              

# Single linear regression results relating birthweight and smoking, assuming homoskedasticity
reg1=lm(birthweight~smoker,data=mydata1)
summary(reg1)

# Single linear regression results relating birthweight and smoking, allowing for heteroskedasticity
coeftest(reg1, vcov = vcovHC(reg1, "HC1")) 
summary(reg1)$adj.r.squared   # report the adjusted R-Squared from the regression

# Notice how the regression coefficients from summary(reg1) and coeftest(reg1, vcov = vcovHC(reg1, "HC1")) 
# are exactly the same, but the standard errors and t-statistics are different. The "HC" in HC1" stands for 
# Heteroskedasticity-Consistent standard errors, and the "1" stands for the type of Variance-Covariance
# matrix (e.g., vcov) that is used to compute the standard errors (there's other types of matrices,
# but you do not have to worry about this at all for this subject).


# ------------------ Q5
# Building up the regression model by progressively adding control variables to evaluate 
# their impact on the coefficient of interest (smoker) and model fit 

# As we build up the model, we will use new R command called stargazer() to allow us 
# to output results from our regressions in an easily readable way. For each regression
# we use the lm() command to get the regression coefficient estimates, and the 
# vcovHC() + sqrt() commands to get the heteroskedasticity robust standard errors

# Single linear regression with no other controls
reg1=lm(birthweight~smoker,data=mydata1)  # save regression results for reg1
cov1=vcovHC(reg1, type = "HC1")           # next 2 lines save the robust standard errors for reg1
se1=sqrt(diag(cov1))

# Controlling for alcohol
reg2=lm(birthweight~smoker+alcohol+drinks,data=mydata1) # save regression results for reg2
cov2=vcovHC(reg2, type = "HC1")                         # next 2 lines save the robust standard errors for reg2
se2=sqrt(diag(cov2))                    

# Controlling for alcohol, prenatal care
reg3=lm(birthweight~smoker+alcohol+drinks+nprevisit+tripre1+tripre2+tripre3,data=mydata1) # save regression results for reg3
cov3=vcovHC(reg3, type = "HC1")                         # next 2 lines save the robust standard errors for reg3
se3=sqrt(diag(cov3))  

# Controlling for alcohol, prenatal care, demographics
reg4=lm(birthweight~smoker+alcohol+drinks+nprevisit+tripre1+tripre2+tripre3+age+educ+unmarried,data=mydata1) # save regression results for reg4
cov4=vcovHC(reg4, type = "HC1")                         # next 2 lines save the robust standard errors for reg4
se4=sqrt(diag(cov4))  

# Controlling for alcohol, prenatal care, demographics
# Assuming homoskedastic standard errors for comparison to reg4
reg5=lm(birthweight~smoker+alcohol+drinks+nprevisit+tripre1+tripre2+tripre3+age+educ+unmarried,data=mydata1) # save regression results for reg5
cov5=vcovHC(reg5, type = "const")                         # next 2 lines save the standard errors for reg5
se5=sqrt(diag(cov5))  

## Regression output table for the 4 regressions reg1 reg2 reg3 reg4
# Reporting with 2 digits after decimal with 'digits=2' command
# First line lists the regression coefficient estimates we saved
# Second line se() lists the standard errors we saved
# Third line digits=2 says to report 2 digits after the decimal
# Fourth line dep.var.labels=c() creates the label for the dependent variable
# Fifth line covariate.labels= (and the lines after) create the labels for the independent variables
# The last line with out= creates the .txt file with the regression output
stargazer(reg1, reg2, reg3, reg4, reg5, type="text",
          se=list(se1, se2, se3, se4, se5),
          digits=2, 
          dep.var.labels=c("Baby Birthweight in Grams"),
          covariate.labels=
            c("Smoker",
              "Drinks Alcohol During Pregnancy",
              "Drinks per Week During Pregnancy",
              "Prenatal Visits",
              "Prenatal Care in 1st Trimester",
              "Prenatal Care in 2nd Trimester",
              "Prenatal Care in 3rd Trimester",
              "Age",
              "Years of Education",
              "Unmarried",
              "Constant"),
          out="reg_output.txt")   # Output results to your director in a text file

# What does Note *p<0.1; **p<0.05; ***p<0.01 in the table mean?
# This is a legend for hypothesis test results reported in the table for individual regression
# coefficients. Specifically, a regression coefficient in the table with either a "*", "**" or "***"
# is statistically significantly different from 0 at the 10%, 5%, and 1% level depending on 
# how many stars are next to the coefficient (hence the name of the command 'stargazer()')
# More precisely, these stars "*" indicate results from a hypothesis test that a given regression 
# coefficient equals 0 against the alternative that the coefficient does not equal 0 
# (e.g., the stars correspond to results from a 2-tailed test of the null a coefficient equals 0)

############### Notes ############### 
# this R.script will be uploaded to
# https://github.com/zhengf1/2024ECOM1

# This inTute code is provided only for the tutorial sessions.

# please refer to Dave's R code for detailed explanations
# it contains much more than the tutorial questions required



