#HEADER
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")

# import the data
library(readr)
df = read_csv("C:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week2\\Utilities.csv")

#Slice out data 
df = df[ , c("Company", "Sales", "Fuel_Cost")]

df = easyStandardize(df, c(2,3))

d = dist(df)
model = hclust(d, method='single')
plot(model, labels= df$Company)
