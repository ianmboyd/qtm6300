#HEADER
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")

# import the data
library(readr)
df = read_csv("C:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week2\\DairyPurchase.csv")

# FORCE the seed of the random number generator
set.seed(5)

# delete rows with missing data
df = na.omit(df)
df = easyStandardize(df, c(1,2,3,4))

# choose K.... (like the dendrogram analysis - but that is hierarchical)
elbowChart(df)

# Build the model
model = kmeans(df, 4)
summary(model)

plot(df, col=model$centers)

model$centers # the center point of each cluster
model$size # size of cluster
model$cluster # array listing the cluster of each row (from the input DF)

# Slice out the people in the survey set which are members of cluster 2
#df[model$cluster == 2, ]

# Slice out everyone not in cluster 2
#df[model$cluster != 2, ]

