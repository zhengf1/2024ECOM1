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
setwd("~/Dropbox/01 UoM-Teaching/2024-S2-Ecom1/Week3") # your path should be different 

## Load Stargazer package for summary statistics and regression tables
library(stargazer)

## Load the dataset from a comma separate value
data=read.csv(file="tute3_cps.csv")

## List the variables in the dataset named data
names(data)

## Dimension of the dataset
# 15052 observations (individuals)
# 5 variables: year, ahe, bachelor, female, age
dim(data)

# --------- INDEXING 
## Index observation 1 for ahe
data$ahe[1]    # observation 1 for ahe

## Index observation 37 for female 
data$female[37]   # observation 37 for female

## Index observations 1 to 5 for ahe
data$ahe[1:5]

## Index observations 37 to 47 for female
data$female[37:47]


# --------- SUBSETS OF DATA
# Suppose we want to just look at ahe for females
# We can get all the observations that corresponds to females with female==1
# data$ahe[data$female==1] is ahe for females
# data$ahe[data$female==0] is ahe for males

# --------- Question C.1. --------- 
## Mean and standard deviation of earnings for females
mean(data$ahe[data$female==1])
sd(data$ahe[data$female==1])

## Mean and standard deviation of earnings for males
mean(data$ahe[data$female==0])
sd(data$ahe[data$female==0])

# compare the distributions on earnings from each groups (gender)
pdf("ahe_female.pdf")
plot(density(data$ahe[data$female==1]), col="red",lty=1,
     xlab="AHE",main="Gender and Earnings")
lines(density(data$ahe[data$female==0]), col="blue",lty=1)
legend("topright", legend=c("Female", "Male"), col=c("red", "blue"), lty=c(1,1))

# --------- Question C.2. --------- 
## Mean and standard deviation of earnings for bachelor degree
mean(data$ahe[data$bachelor==1])
sd(data$ahe[data$bachelor==1])

## Mean and standard deviation of earnings for no bachelor degree
mean(data$ahe[data$bachelor==0])
sd(data$ahe[data$bachelor==0])

# compare the distributions on earnings from each groups (bachelor)
pdf("ahe_bachelor.pdf")
plot(density(data$ahe[data$bachelor==0]), col="red",lty=1,xlab="AHE",main="Education and Earnings")
lines(density(data$ahe[data$bachelor==1]), col="blue",lty=1)
legend("topright", legend=c("No Bachelor Degree", "Bachelor Degree"), col=c("red", "blue"), lty=c(1,1))
dev.off()

# --------- Question C.3. --------- 
# Confidence interval and Hypothesis testing
ahe_mu=mean(data$ahe)                 # Sample mean of ahe
ahe_nobs=length(data$ahe)             # Number of observations; length() returns the number of obs in ahe
ahe_sd=sd(data$ahe)                   # Sample standard deviation of ahe
ahe_se=ahe_sd/sqrt(ahe_nobs)          # Standard error of the sample mean
critial_value = qnorm(0.975)
ahe_CI95_low=ahe_mu - critial_value * ahe_se       # Lower bound of the 95% CI
ahe_CI95_high=ahe_mu + critial_value * ahe_se      # Upper bound of the 95% CI
c(ahe_CI95_low, ahe_CI95_high)

## Test the null that the true value of the mean of ahe is 19.5 (example)
t.test(data$ahe, mu=19.5 ) # default is a two sided test
t.test(data$ahe, mu=19.5, alternative = "two.sided" )
t.test(data$ahe, mu=19.5, alternative = "greater" )
t.test(data$ahe, mu=19.5, alternative = "less" )

# manual calculate p-value for right-tail test
t_act=(ahe_mu-19.5)/ahe_se  # t-statistic
pvalue1 = 1-pnorm(t_act)      # compute p-value
paste("One-sided p-value for greater than (>) alternative:",pvalue1)

## Test the null that the true value of the mean of ahe among females is 18 (example)
t.test(data$ahe[data$female==1],mu=18)

# --------- Question C.4. --------- 
## Test difference of means in ahe for male and female without bachelor in 2012
# Gender wage gap in 2012 among people without bachelor degree
mean(data$ahe[data$female==1 & data$year==2012 & data$bachelor==0])
mean(data$ahe[data$female==0 & data$year==2012 & data$bachelor==0])
diff1=mean(data$ahe[data$female==1 & data$year==2012 & data$bachelor==0])-mean(data$ahe[data$female==0 & data$year==2012 & data$bachelor==0])

t.test(data$ahe[data$female==1 & data$year==2012 & data$bachelor==0],
       data$ahe[data$female==0 & data$year==2012 & data$bachelor==0])

sub1 = data$ahe[data$female==1 & data$year==2012 & data$bachelor==0]
sub2 = data$ahe[data$female==0 & data$year==2012 & data$bachelor==0]
mu_sub1 = mean(sub1)
sd_sub1 = sd(sub1)
se_sub1 = sd_sub1 / sqrt(length(sub1))
mu_sub2 = mean(sub2)
sd_sub2 = sd(sub2)
se_sub2 = sd_sub2 / sqrt(length(sub2))
test_statistics = (mu_sub1 - mu_sub2) / sqrt(se_sub1^2+se_sub2^2)

crt_val =  qnorm(0.975)
p_value2 = pnorm( test_statistics )

# --------- Question C.5. --------- 
## Test difference of means in ahe for male and female with bachelor in 2012
# Gender wage gap in 2012 among people with bachelor degree
mean(data$ahe[data$female==1 & data$year==2012 & data$bachelor==1])
mean(data$ahe[data$female==0 & data$year==2012 & data$bachelor==1])
diff2=mean(data$ahe[data$female==1 & data$year==2012 & data$bachelor==1])-mean(data$ahe[data$female==0 & data$year==2012 & data$bachelor==1])

t.test(data$ahe[data$female==1 & data$year==2012 & data$bachelor==1],
       data$ahe[data$female==0 & data$year==2012 & data$bachelor==1])


# Graphically: difference in gender wage gap depending on education in 2012
pdf("ahe_female_bachelor_2012.pdf")
plot(density(data$ahe[data$female==1 & data$year==2012 & data$bachelor==0]), col="red",lty=1,main="Gender, Education, and Earnings in 2012", xlab="AHE")
lines(density(data$ahe[data$female==0 & data$year==2012 & data$bachelor==0]), col="blue",lty=1)
lines(density(data$ahe[data$female==1 & data$year==2012 & data$bachelor==1]), col="red",lty=2)
lines(density(data$ahe[data$female==0 & data$year==2012 & data$bachelor==1]), col="blue",lty=2)
legend("topright", legend=c("Female, No Degree", "Male, No Degree", "Female Degree", "Male Degree"), 
       col=c("red","blue","red","blue"), lty=c(1,1,2,2))
dev.off()

############### Notes ############### 
# this R.script will be uploaded to
# https://github.com/zhengf1/2024ECOM1

# please refer to Dave's R code for detailed explanations
# it contains much more than the tutorial questions required

