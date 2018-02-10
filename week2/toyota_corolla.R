# Load the data
library(readr)
df = read_csv("C:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week2/ToyotaCorolla.csv")

#Manage the data

df$Fuel_Type = as.factor(df$Fuel_Type)
df$Met_Color= as.logical(df$Met_Color)
df$Automatic = as.logical(df$Automatic)
df$Model = NULL

#partition the data
N = nrow(df)
trainingSize = round(N*.8)
trainingCases = sample(N, trainingSize)
training = df[trainingCases, ]
test = df[-trainingCases, ] # WOW : the negation is the "everything but these rows)

# Build the model

# model = lm(Price ~ Age + KM, data = training) # price as a function of Age and KM

model = lm(Price ~ ., data = training) # price as a function of EVERYTHING
model = step(model) # iteratively removes the bad variables (based on some internal criteria)
summary(model)

# make some predictions

predictions = predict(model, test)

# evalutate the performance

# - calculate the errors
observations = test$Price
errors = observations-predictions

# - MAPE
mape = mean(abs(errors/observations))

# - RMSE
rmse = sqrt(mean(errors^2))

# Benchmarking
# The average is the best we could guess without building a model

errors_bench = observations - mean(training$Price)

mape_bench = mean(abs(errors_bench/observations))
rmse_bench = sqrt(mean(errors_bench^2))

