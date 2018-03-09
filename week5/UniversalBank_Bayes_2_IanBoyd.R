
#
# HEADER
#

# Force the RAND seed (for consistency)
set.seed(1234)

# Utilities
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")

# Load Libraries
library(gmodels)
library(class)
library(readr)
library(ggplot2)
library(e1071)

# /HEADER


#
# Load the data
# 
#
# (1).a Import the data
#

remoteData = TRUE; # Force 

if( remoteData ){
  #Attempt to load the data from GITHUB repository
  library(RCurl)
  urlData <- getURL("https://raw.githubusercontent.com/ianmboyd/qtm6300/master/week4/UniversalBank.csv")
  df= read.csv(text=urlData, check.names=FALSE) # Odd nunace of this util - it replaces spaces in column names with '.'
}else{
  #Load the data from local storage
  df = read_csv('c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week4/UniversalBank.csv')
}



str(df)
#
# The target is "Personal Loan"
# There are other variables which are categorical - all the boolean / tier based variables
#
# Classes 'tbl_df', 'tbl' and 'data.frame':	5000 obs. of  14 variables:
#   $ ID                : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Age               : int  25 45 39 35 35 37 53 50 35 34 ...
# $ Experience        : int  1 19 15 9 8 13 27 24 10 9 ...
# $ Income            : int  49 34 11 100 45 29 72 22 81 180 ...
# $ ZIP Code          : int  91107 90089 94720 94112 91330 92121 91711 93943 90089 93023 ...
# $ Family            : int  4 3 1 1 4 4 2 1 3 1 ...
# $ CCAvg             : num  1.6 1.5 1 2.7 1 0.4 1.5 0.3 0.6 8.9 ...
# $ Education         : int  1 1 1 2 2 2 2 3 2 3 ...
# $ Mortgage          : int  0 0 0 0 0 155 0 0 104 0 ...
# $ Personal Loan     : int  0 0 0 0 0 0 0 0 0 1 ...
# $ Securities Account: int  1 1 0 0 0 0 0 0 0 0 ...
# $ CD Account        : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Online            : int  0 0 0 0 0 1 1 0 1 0 ...
# $ CreditCard        : int  0 0 0 0 1 0 0 1 0 0 ...

# 
# (1) 
# Prepare the data
# 


# (1)a
# Convert the target to a factor
# 
df$borrower= as.factor(df$`Personal Loan`)
df$`Personal Loan`= NULL #having spaces in variable names is annoying


# (1).b
# Remove the following numeric/non-categorical data  or otherwise useless variables
# 
df$ID = NULL # Random assigned number without any bearing on actual customer

df$Age = NULL # numeric
df$Experience = NULL # numeric
df$ Income = NULL # numeric
df$`ZIP Code` = NULL # Torn on this one - it is categorical, but there are 467 ZIPs represente
df$CCAvg = NULL # numeric

#
# (1).b*
# We can categorize some data here, as in 
# "are the a mortage customer or not" this can be true/false
# "are they single?" is another
#
df$Mortgage = ifelse(df$Mortgage > 0, 1, 0)
df$Single = ifelse(df$Family <= 1, 1, 0)
df$Family = NULL# converted to "Single"

# Make all the data categorical 
everyColumn = colnames(df)
df[everyColumn] = lapply(df[everyColumn], factor)


#
# (2).a
# Partition the data 
# 

n = nrow(df)
trainingCases = sample(n, round(n*.60))

train = df[trainingCases, ]
test = df[-trainingCases, ]

#
# (2).b
# Build the model and construct the precition set
# 
model = naiveBayes(borrower ~ ., data = train)
predictions = predict(model, test, type="class")

#
# (3)
# Confusion matrix
# 
observations = test$borrower
table(predictions, observations)
#             observations
# predictions    0    1
#             0 1777  156
#           1   27   40

ct = CrossTable(predictions, observations, expected = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
#                        | observations 
# predictions            |         0 |         1 | Row Total | 
#           -------------|-----------|-----------|-----------|
#                     0  |      1777 |       156 |      1933 | 
#           -------------|-----------|-----------|-----------|
#                      1 |        27 |        40 |        67 | 
#           -------------|-----------|-----------|-----------|
#           Column Total |      1804 |       196 |      2000 | 
#           -------------|-----------|-----------|-----------|

# (3).a
# Error Rate
error_rate = sum(observations != predictions) / nrow(test) # (27+156)/2000
# 9% 
fn = ct$t[1,2] # false negatives (predicted calse, but was true)
fp = ct$t[2,1] # false positives (predicted true, but was false)
op = sum(ct$t[,2]) #  Observed Positives (actual true)
on = sum(ct$t[,1]) #  Observed negatives (actual false)
tp = ct$t[2,2] # true positives ( we were right when we predicted true)
tn = ct$t[1,1] # true negatives ( we were right when we predicted false)


# (3).b
# False Negative Rate : FN/(TP+FN) : 1-Sensitivity 
fn / (tp + fn) # 80%

# (3).c
# False Postive Rate :  FP/(FP+TN) : 1-Specificity 
fp / (fp+tn) # 1%

# 
#  The model is MUCH more likely to produce a false negative than a false positive (same as KNN)
# 

# (3).d
# Sensitivity: the number of  correct "true" predictions / # of actual true observations
sensitivity = tp/op # 20%

# (3).e
# Specificity: the number of correct "false" predictions / # of actual false observations
specificity  = tn/on # 98%

#
# (4).a
# Benchmark Error Rate
benchmarkErrorRate(test$borrower, train$borrower) # 9% (VERY LOW!!!)

# (4).b
# The benchmark error rate is the rate at which we woudld predict erroneously should we base our choice
# over the most likely outcome (non-borrower) In this case it's 1804/2000 so we'd only be wrong about 9% 
# of the time.

# (4).c
# Naive Bayes isn't all that much better than the benchmark at predicting if a customer is likely to take out a personal loan
#  9% Bayes vs 9% Benchmark

#
# (5)
# 
# The error rate for Bayes is slightly worse here than it is for KNN, but not that much. In both models, the fact that there is such 
# a strong propensity to *not* take out a personal loan, and choosing "false" consistently means you'll be right most of the time.
# 
# Bayes is better if you have more categorical data to work with, whereas KNN is stronger when you have numerically continuous data.
# In all actuality, you can take continuous data and "bin" it into categorical data (reduce fidelity). 
# 
