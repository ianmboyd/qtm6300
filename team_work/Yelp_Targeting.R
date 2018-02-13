#HEADER
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/myUtilities.R")

# Force the RAND seed (for consistency)
set.seed(5)

# import the data
library(readr)
df = read_csv("C:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week2\\yelp.csv")

# remove any empty rows
df = na.omit(df)

# calculate average Reviews Per Month (How active a person is)
df$Activeness = df$Reviews / df$`Months Active`

# calculate the Votes Per Month (Influence)
df$Influence = df$Votes / df$`Months Active`

# Remove the ID (UUID of User) - not relevant
df$ID = NULL

# Preserve a non-standardized copy of the data
data = df

# Standardize the data (decompose to T-Score)
df = easyStandardize(df, c(1,2,3,4,5,6))

# Look for the "bend" of diminishing returns WRT group numbers
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

#
# Explore some data
# 
colMax(data)
#
# Months Active        Reviews Average Review          Votes        Friends            RPM 
# 118.00000     3166.00000        5.00000    67094.00000     1066.00000       29.04587 
#

colMin(data)
#
# Months Active        Reviews Average Review          Votes        Friends            RPM 
# 7.00000000     1.00000000     1.00000000     0.00000000     0.00000000     0.01075269 
#

colSd(data)
#
# Months Active        Reviews Average Review          Votes        Friends            RPM 
# 20.371425     112.147684       1.028393    1463.448201      22.729701       1.687960 
#

colMeans(data)
#
# Months Active        Reviews Average Review          Votes        Friends            RPM 
# 40.5815418     33.7587241      3.7430907    152.1117888      4.2656734      0.6453183 
# 
