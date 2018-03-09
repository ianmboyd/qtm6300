library(e1071)
library(gmodels)
library(readr)

source("/Users/nathankarst/Dropbox/Teaching/Data Analytics in R/BabsonAnalytics.R")

df = read_csv("/Users/nathankarst/Dropbox/Teaching/Data Analytics in R/data/MovieReviews.csv")

everyColumn = colnames(df)
df[everyColumn] = lapply(df[everyColumn], factor)

N = nrow(df)
trainingSize = round(N*0.6)
trainingCases = sample(N, trainingSize)
training = df[trainingCases,]
test = df[-trainingCases,]

model = naiveBayes(PositiveTweet ~ ., data = training)
pred = predict(model, test, type="class")

error_rate = sum(pred != test$PositiveTweet)nrow(test)
error_bench = bencmarkErrorRate(training$PositiveTweet, test$PositiveTweet)

CrossTable(pred, test$PositiveTweet, prop.r = F, prop.c = F, prop.t = F, expected = F, prop.chisq = F)

# compute by taking the most frequent occurrance - here its positive...if you chose this all the time then the you'd be wrong  the number of count (column summ) of the negative tweets
# divide negative by the total to get the benchmark error rate

limited = df[df$awesome == "1",]
sum(limited$PositiveTweet == "1")/sum(limited$PositiveTweet == "0")

limited = df[df$absolutely == "1",]
sum(limited$PositiveTweet == "1")/sum(limited$PositiveTweet == "0")

model$table$awesome # rows sum to 1 : Target vs the Column 
# (0,0) : % of time that the word didn't happen and the tweet was positive 
# (1,1) : % of time the word happened and the tweet was positive
# 
# NLTK - in python, can get rid of the stop words
# 



