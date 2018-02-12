#HEADER
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/myUtilities.R")

set.seed(5)
# import the data
library(readr)
df = read_csv("C:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week2\\yelp.csv")


# remove any empty rows
df = na.omit(df)

#calculate average reviews per month
df$RPM = df$Reviews / df$`Months Active`
df$ID = NULL
data = df
df = easyStandardize(df, c(1,2,3,4,5,6))

elbowChart(df)

# Build the model
model = kmeans(df, 4)
summary(model)

# inspect the centers of this model
model$centers

# create a new frame removing everyone the power-user cluster
df2 = df[model$cluster != 4, ]

# Review elbow to determine reasoable number of clusters
elbowChart(df2) # appears to begin leveling at 4 clusters (as before)

model2 = kmeans(df2, 4)

