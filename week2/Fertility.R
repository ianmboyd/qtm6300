#HEADER
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")

# import the data
library(readr)
df = read_csv("C:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week2\\ExpectancyFertility.csv")

# delete rows with missing data
df = na.omit(df)

# Build the model
model = kmeans(df, 2)
summary(model)

plot(df, col=model$cluster)

df = easyStandardize(df, c(1,2)) # standardize columns(1,2)
model = kmeans(df, 2)
plot(df, col=model$cluster)
