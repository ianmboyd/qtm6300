
## VIDEO 1
# import the data
library(readr)
df = read_csv("C:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week2/BostonHousing.csv")


# Clean the data

df$ISHIGHVAL = NULL # this value is directly related to the target value, so must remove it

df$CHAS = as.factor(df$CHAS)
df$RAD = as.factor(df$RAD)

#partition the data
N = nrow(df)
trainingSize = round(N*.6)
trainingCases = sample(N, trainingSize)
training = df[trainingCases, ]
test = df[-trainingCases, ] # WOW : the negation is the "everything but these rows)

# Build the model

# model = lm(Price ~ Age + KM, data = training) # price as a function of Age and KM

model = lm(MEDV ~ ., data = training) # price as a function of EVERYTHING
summary(model)


# make some predictions

predictions = predict(model, test)

# - calculate the errors
observations = test$MEDV
errors = observations-predictions

mape = mean(abs(errors/observations))
rmse = sqrt(mean(errors^2))

#
# OPTIMIZED MODEL (with step())
#

model_stepped = step(model) # iteratively removes the bad variables (based on some internal criteria)
summary(model_stepped)

predictions_stepped = predict(model_stepped, test)

# evalutate the performance

# - calculate the errors
observations_stepped = test$MEDV
errors_stepped = observations_stepped - predictions_stepped

mape_stepped = mean(abs(errors_stepped/observations_stepped))
rmse_stepped = sqrt(mean(errors_stepped^2))

# Benchmarking
# The average is the best we could guess without building a model

errors_bench = observations - mean(training$MEDV)

mape_bench = mean(abs(errors_bench/observations))
rmse_bench = sqrt(mean(errors_bench^2))

