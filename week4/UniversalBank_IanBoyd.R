
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
# Remove the following categorical or otherwise useless variables
# 
df$ID = NULL # Random assigned number without any bearing on actual customer
df$`Securities Account` = NULL # boolean
df$`CD Account` = NULL # boolean
df$Online = NULL # boolean
df$`ZIP Code` = NULL # is either In or Out of ZIP, numerical similarty does not mean acutal distance
df$Education = NULL # categorical value
df$CreditCard = NULL # boolean

# Look at the data
qplot(Income, CCAvg, data=df, color=borrower)
qplot(Income, Mortgage, data=df, color=borrower)

# The more you're already leveraged the more likely you are to take out a personal loan

#
# (2)
# Partition the data 
# 

n = nrow(df)
trainingCases = sample(n, round(n*.60))

train = df[trainingCases, ]
test = df[-trainingCases, ]

# Use the utility to help choose the best "K"
k_best = kNNCrossVal(borrower ~ ., train)
# 1, 3, 5 are all showing up as "good" - I'll use 1

# 
# (2).a
# Build the kNN model
# 
predictions = kNN(borrower ~ . , train, test, k=k_best)

#
# (3)
# Confusion matrix
# 
observations = test$borrower
table(predictions, observations)
#               observations
# predictions    0    1
#           0 1765   81
#           1   34  120

CrossTable(predictions, observations, expected = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
#                 | observations 
# predictions     |         0 |         1 | Row Total | 
#    -------------|-----------|-----------|-----------|
#               0 |      1765 |        81 |      1846 | 
#    -------------|-----------|-----------|-----------|
#               1 |        34 |       120 |       154 | 
#    -------------|-----------|-----------|-----------|
#    Column Total |      1799 |       201 |      2000 | 
#    -------------|-----------|-----------|-----------|

# (3).a
# Error Rate
error_rate = sum(observations != predictions) / nrow(test) # (34+81)/2000
# 6% 

# (3).b
# False Negative Rate : FN/(TP+FN) : 1-Sensitivity 
fn = 81/(77+120) # 41%

# (3).c
# False Postive Rate :  FP/(FP+TN) : 1-Specificity 
fp = 34/(1779+34) # 2%

# 
#  The model is MUCH more likely to produce a false negative than a false positive
# 

# (3).d
# Sensitivity: the number of "true" predictions / # of actual true observations
sensitivity = 120/201 # 38%

# (3).e
# Specificity: the number of "false" predictions / # of actual false observations
specificity  = 1765/1799 # 98%

#
# (4) 
# Benchmark Error Rate
benchmarkErrorRate(test$borrower, train$borrower) # 9% (VERY LOW!!!)

# (4).a
# The benchmark error rate is the rate at which we woudld predict erroneously should we base our choice
# over the most likely outcome (non-borrower) In this case it's 1799/2000 so we'd only be wrong about 9% 
# of the time.
# 
# The model is only slightly better at 7% error rate

#
# (5) Re-Run without normalization
#
predictions_no_norm = kNN(borrower ~ ., train, test, k=k_best, norm = F)
error_rate_no_norm = sum(observations != predictions_no_norm)/nrow(test)

#
# Error rate is about 10%, which is as good as the benchmark rate and is only slightly worse than the 
# error rate when the data is normalized.
# 
# Given the strong benchmark performance, one might conclude that this model is a waste of time.
# 

