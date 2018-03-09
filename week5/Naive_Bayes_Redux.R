set.seed(12234)
library(zipcode)
data(zipcode)

# Get the data
df = read_csv('c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week4/UniversalBank.csv')


# clean the ZIP codes and then merge the zipdata into the banking data
df$zip = clean.zipcodes(df$`ZIP Code`)

df = merge(zipcode, df, by.x='zip', by.y='zip' )

# There is one datapoint which is listed as military - no lat/long
df = na.omit(df)

# factorize some of the date 
df$state_f = as.factor(df$state)
# all data is from CA, let's kill this useless data
df$state = NULL;
df$state_f = NULL;

# turn the cities into Factors
df$city = as.factor(df$city)

# Some of the data looks bad - there are customers who have negative years of experience.
# None of these customers have taken personal loans, but otherwise the data looks valid.
# In order to place some sanity around it, I will zero all negative experience values

sum(df$Experience < 0)
df$Experience[df$Experience < 0] = 0
sum(df$Experience < 0)

# Load the HMISC library for cut2
library(Hmisc)

# Reduce the numericals into quantiles
df$Age = cut2(df$Age, g=10)
df$Experience = cut2(df$Experience, g=10)
df$Income = cut2(df$Income, g=10)
df$Mortgage = cut2(df$Mortgage, g=10)
df$CCAvg = cut2(df$CCAvg, g=10)

# Reduce the ZIP code data set to be only CA Zipcodes
zipcode = zipcode[zipcode$state == 'CA', ]

# grab some graphics packages
library(ggplot2)
library(mapproj)
library(raster)

# Load the geom data for California
us = getData("GADM", country="USA", level=1)
states = c('California')
us.states = us[us$NAME_1 %in% states, ]



# Plot the data - Green is where we have personal loans
# This is mostly for fun, and because I wanted to learn how
g = ggplot() +
  geom_point(data=zipcode, aes(x=longitude, y=latitude), color='sky blue') +
  geom_point(data=df[df$`Personal Loan`==0, ], aes(x=longitude, y=latitude), color='red') + 
  geom_point(data=df[df$`Personal Loan`==1, ], aes(x=longitude, y=latitude), color='green') +
  geom_path(data=us.states, aes(x=long, y=lat, group=group)) + coord_map() +
  labs(x=NULL, y=NULL)

g
# Most customer data surrounds larger metropolitan areas

# Sanitize some more and then build Naive Bayes
# 
df$zip = NULL
df$city = NULL
df$latitude = NULL
df$longitude = NULL
df$`ZIP Code` = NULL
df$ID = NULL

# Make all the data categorical 
everyColumn = colnames(df)
df[everyColumn] = lapply(df[everyColumn], factor)

# need to fix column name
df$borrower = df$`Personal Loan`
df$`Personal Loan` = NULL

#
# Partition the data 
# 

n = nrow(df)
trainingCases = sample(n, round(n*.60))

train = df[trainingCases, ]
test = df[-trainingCases, ]


# Utilities
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")

# Load Libraries
library(gmodels)
library(class)
library(e1071)


#
# Build the model and construct the precition set
# 

model = naiveBayes(borrower ~ ., data = train)
predictions = predict(model, test, type="class")

#
# Confusion matrix
# 
observations = test$borrower
table(predictions, observations)
# Interestingly we've increased our true-positive predictions from before, and the False-Pos and False-Neg prdictions
# are balanced
# 
#               observations
# predictions    0    1
#             0 1761   62
#             1   41  122
#           

ct = CrossTable(predictions, observations, expected = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)


# 
# Error Rate
error_rate = sum(observations != predictions) / nrow(test)
# 5% 

fn = ct$t[1,2] # false negatives (predicted calse, but was true)
fp = ct$t[2,1] # false positives (predicted true, but was false)
op = sum(ct$t[,2]) #  Observed Positives (actual true)
on = sum(ct$t[,1]) #  Observed negatives (actual false)
tp = ct$t[2,2] # true positives ( we were right when we predicted true)
tn = ct$t[1,1] # true negatives ( we were right when we predicted false)


# 
# False Negative Rate : FN/(TP+FN) : 1-Sensitivity 
fn / (tp + fn) 

# 
# False Postive Rate :  FP/(FP+TN) : 1-Specificity 
fp / (fp+tn) 

# 
#  The model is MUCH more likely to produce a false negative than a false positive (same as KNN) 
#  BUT - it's also much less likely to produce a false-negative
# 

# 
# Sensitivity: the number of  correct "true" predictions / # of actual true observations
sensitivity = tp/op 

# 
# Specificity: the number of correct "false" predictions / # of actual false observations
specificity  = tn/on

#
# Benchmark Error Rate
benchmarkErrorRate(test$borrower, train$borrower) # 9%  - this doesn't change
