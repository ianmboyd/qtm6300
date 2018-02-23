# Force the RAND seed (for consistency)
set.seed(1234)

#HEADER
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")

library(gmodels)
library(class)
library(readr)
library(ggplot2)

# Load Data
df = read_csv('c:/Users/iboyd/Documents/GRAD/s-2018/BostonHousing.csv')

# Trying to predict ISHIGHVAL


# Data Management
df$MEDV = NULL
df$ISHIGHVAL = as.factor(df$ISHIGHVAL)

qplot(NOX, RM, data=df, color=ISHIGHVAL)

#zoom in
qplot(NOX, RM, data=df, color=ISHIGHVAL, xlim=c(.45, .55), ylim=c(5, 6))

# KNN - look a the "k" closest neighbors and let them vote on membership
#

#  Partition the data
# 
n = nrow(df)
trainingCases = sample(n, round(n*.75))

train = df[trainingCases, ]
test = df[-trainingCases, ]

#
# Build the model
# 

predictions = kNN(ISHIGHVAL ~ . , train, test, k=9) # different than knn 
 

#
# Compare obs vs predict
# 

observations = test$ISHIGHVAL

# askign ourselves where our predictions were correct and where they were wrong
# for categorial - we are either right or wrong


# test for equivalence
observations == predictions
#or
observations != predictions

error_rate = sum(observations != predictions) / nrow(test)
# is about 13.5%

# look at the errors
# 
# mis-classification table
table(predictions, observations)
# this builds a confusion matrix
# 
#  shows us HOW we are making the errors
#  
#  > table(predictions, observations)
#               observations
# predictions   0  1
#           0  51 12
#           1  5 58
# 
# What this means is that we predicted "0" correctly 51 times, and  predicted 1 correctly 58 times
# there were 5 times we predicted 1 but observed 0 and 12 times we predicted 0 but observed 1
# 
# Important because maybe error in one direction is more "costly" than the others
# 
# 
# Can use a CrossTable (which produces somethign called Chi squeared) 
# 
# 
# the "-F" means false ("we don't want expected, prop.r  (proportion per row, colomn, chi sq, etc)
CrossTable(predictions, observations, expected=F, prop.r = F, prop.c = F, prop.chisq = F, prop.t = F)
#
#
#   Cell Contents
# |-------------------------|
#   |                       N |
#   |-------------------------|
#   
#   
#   Total Observations in Table:  126 
# 
# 
#                 | observations 
# predictions     |         0 |         1 | Row Total | 
#    -------------|-----------|-----------|-----------|
#               0 |        51 |        12 |        63 | 
#    -------------|-----------|-----------|-----------|
#               1 |         5 |        58 |        63 | 
#    -------------|-----------|-----------|-----------|
#    Column Total |        56 |        70 |       126 | 
#    -------------|-----------|-----------|-----------|
# 
#  See that 126 total guesses
#  so error rate is the errors over the total
#  so 17/126 (our 13% from earlier)
#  

# Benchmark strategy - if there are more high value, then guess high value (and the counter)
# 
#  The totals are the "benchmark" errors 
#   
#  53 / 126 = .42
# 
# Can use this "benchmark" to determine if our model is better than a guess based on the most common - 
# here it's determinded by the higher count (70) for "high value : 1" 
# if we just chose high, then we'd be "right" 70/126 % of the time, and WRONG 56/126 % of the time
# 
#  can tell our model is good, becuse we brought down our benchmark error rate to 5 + 12 = 17 / 126 % of the time
#  
#  


benchmarkErrorRate(train$ISHIGHVAL, test$ISHIGHVAL)
# this is the best we could do by using benchmark
# 


# sensitivity  = the number of "true" predictions / # of actual true observations
#  58/70
#  
#  Specificity = the number of "false" predictions / # of actual false observations
#  51/56


# Looking at "Distance" now
# 
qplot(NOX, RM, data=df, color=ISHIGHVAL, asp=1) # force the aspect ration to 1:1

#
# Look at the plot : on the X Axis (NOX) .4 -> .5 is the same "Distance" as 4->5 on the y-axis (RM)
# The "RM" axis is 10 times the size as the NOX axis
# 
# Distance in NOX barely matters
# Not great from an analytics perspective when each var does not have the same impact
# we resolve this by nomilization of the  the data
# 
# Take every var and put it on the same interval
# Allows them to each have the same "voice"
# 
# x_norm = (x - min(x))/(max(x)-min(x)) 
# (x - the min of x divded by the range of X )
# 
# this puts them on the "unit" interval 
#
# kNN already normalized by default
# You can turn NORM off  to see how bad it is without
# 

predictions_no_norm = kNN(ISHIGHVAL ~ ., train, test, k=9, norm = F)
error_rate_no_norm = sum(observations != predictions_no_norm)/nrow(test)


# 
# HOW TO PICK K
#
# YOu could TRY to iterate over vals for K (1,3,5,7,....)
# Seems OK, but it's BAD, but you can't use TEST data to help build the model - it's CHEATING
# 
# This will over-fit to test, and will perform worse in the real world
# 
# 
# Will discuss later, but there is a tool in our babson analytics set
# 

k_best = kNNCrossVal(ISHIGHVAL ~ ., train) # ISHIGHVAL vs everything (.) - use training data 
# shows the error rate we choose "K"
# 
# 


