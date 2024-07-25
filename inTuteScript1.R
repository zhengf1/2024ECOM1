##---------------------------------------------------------------------------------
############### ECOM20001 Econometrics 1 Tutorial 1
# by Zheng Fan; fan.z@unimelb.edu.au
# Contact me if you have any questions.

############### Comment ############### 
# Everything after "#" is a comment, which won't be executed 
# Please always remember to us "#" to make notes 
# so that you know what you have done

rm(list=ls()) # remove everything in the environment to start a new project

############### Set working directories ############### 
# It is very very important to set the working directories.
# Make sure your data set is in the folder.

# - method 1: manually find the folder and set working directories.
# manually search by: clicking Files -> navigate to the folder -> set

# - method 2: select the path
setwd("~/Dropbox/01 UoM-Teaching/2024-S2-Ecom1/Week1") # your path should be different 

# - method 3: set the working directories the same as where you save your R.script.
setwd( dirname(rstudioapi::getActiveDocumentContext()$path) )

# check your working directory
getwd()

############### Print ############### 
print(2001)
print("Econometrics")
test_number = 3001

(test_number = 3001) # bracket will produce the output
print(test_number)

test_vector = c(1.3, 2.1, 3.4, 4.6, 5.2) # "c" combines number into a vector
print(test_vector)

# index: how to get access to the 3rd component in test_vector?
test_vector[4] # use square bracket
sum(test_vector)
summary(test_vector) # some summary statistics can be obtained with one command

############### Load the data ############### 

rm(list=ls()) # remove everything in the environment to start a new project

# Only when you set the working directories properly, you can read the data
# dt is the name of the data set I assigned, you may choose whatever you like
dt = read.csv("tute1_tutors.csv") 

# We will be working on ".csv" data files. ".csv" files are "format independent"
# data files. Long words short, ".csv" is more stable than ".xlsx".

# to look at the data (or just click on the "Environment")
View(dt) 

# what are the variable names of the data?
names(dt)[2]

## Dimensions of your dataset data
dim(dt)

# calculate the average favorite number
dt[2,]
dt[,2]
dt[2,2]

mean(dt[,3])
summary(dt[,3])

mean(dt[,1])

############### LOADING AND INSTALLING PACKAGES ############### 

# R automatically comes with a large set of commands and tools 
# However, through the semester we will sometimes need to install and/or load additional
# packages which often are very helpful and convenient

# install method 1: 
# We can install packages through "point and click"
# Find Packages in R studio (top right corner), then click install

# install method 2:
# code, for example
# Install "stargazer" package
install.packages("stargazer")

# Install "AER" package, where "AER" stands for "Applied Econometrics" 
# Note how it automatically installs other packages that "AER" depends on
install.packages("AER")

# --- note: you only need to install once for your device.

# Once the packages are installed, we can load them for usage in our code. Often our code
# in tutorials from week 2 onward will load packages at the top of the tutorial R file
# right after where we set the working directory

## Load Stargazer package for summary statistics and regression tables
library(stargazer)

# Load Applied Econometrics package
library(AER)


############### Notes ############### 
# this R.script will be uploaded to
# https://github.com/zhengf1/2024ECOM1

# Make sure your R is functional. 
# try the *Pre-tutorial Work* for our next tutorial

 