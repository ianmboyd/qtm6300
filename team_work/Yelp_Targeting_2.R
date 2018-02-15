# Force the RAND seed (for consistency)
set.seed(123)

#HEADER
######################################################################################
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")
#source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/myUtilities.R")

######################################################################################
#inline Helper Functions
######################################################################################
colMax <- function(data) sapply( data, max, na.rm=TRUE)
colMin <- function(data) sapply( data, min, na.rm=TRUE)
colSd  <- function(data) sapply( data, sd, na.rm=TRUE)
colMedian <- function(data) sapply( data, median, na.rm=TRUE)
dfStats <- function(data){
  return( data.frame( min = colMin(data), max=colMax(data), mean=colMeans(data), median=colMedian(data), stdDev = colSd(data)) )
}
#####################################################################################


# import the data
library(readr)
df = read_csv("C:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week2\\yelp.csv")

# remove any empty rows
df = na.omit(df)

# calculate average Reviews Per Month (How active a person is)
df$Activeness = df$Reviews / df$`Months Active`

# calculate the Influence as a function of friends and review votes, over time
df$Influence = (df$Votes + df$Friends) / df$`Months Active`

# Remove the ID (UUID of User) - not relevant
df$ID = NULL

# Preserve a non-standardized copy of the data
data = df

#  Establish the base statistics of the data set
stats = dfStats(data)
print(stats)
# 
# There is a wide range of variation within some of the variables
# Further investigation should be taken
# 
#                       min         max        mean     median      stdDev
# Months Active  7.00000000   118.00000  40.5815418 39.0000000   20.371425
# Reviews        1.00000000  3166.00000  33.7587241  6.0000000  112.147684
# Average Review 1.00000000     5.00000   3.7430907  3.9000000    1.028393
# Votes          0.00000000 67094.00000 152.1117888  7.0000000 1463.448201
# Friends        0.00000000  1066.00000   4.2656734  0.0000000   22.729701
# Activeness     0.01075269    29.04587   0.6453183  0.1702128    1.687960
# Influence      0.00000000   890.09524   2.4906250  0.2641509   20.039285


# 
# De-Deminsionalize the data
# preserve Average Review, Activeness, Influence
# 
df = df[, c(3, 6, 7)]
# Standardize the data (decompose to T-Score)
df = easyStandardize(df, c(1,2,3))

# Look for the "bend" of diminishing returns WRT group numbers
elbowChart(df)

# Build the model
model = kmeans(df, 4)

# inspect the centers of this model
model$centers
#   Average Review Activeness    Influence
# 1    -0.01481297  7.5804081  6.588901612
# 2    -0.13951810  0.1442633 -0.005882454
# 3    -1.98604815 -0.3075898 -0.111243324
# 4     0.94827171 -0.2756896 -0.107529599



# inspect the cluster sizes
model$size
# [1]   84 5094 1316 3507

# Plot influnce vs activeness 
plot(data$Influence ~ data$Activeness , col=model$cluster)

# Looking at the results of this chart, it's clear that there are statistical outlyers which should be removed

#Look at the user makeup of cluster 1 (original data set - non-standardized)
cluster_1 = data.frame(data[model$cluster == 1 ,])
mean(cluster_1$Activeness) # [1] 13.44075
mean( cluster_1$Votes / cluster_1$Reviews) # [1] 10.45428

#
# It appears for some of these folks, they average about 13 reviews per month
# and receive about 10 votes per review
#
#

print(dfStats(cluster_1))
# 
#                       min         max        mean     median       stdDev
# Months.Active   16.000000   115.00000   64.035714   65.00000 2.223069e+01
# Reviews         93.000000  3166.00000  858.714286  739.50000 5.111488e+02
# Average.Review   2.530000     4.47000    3.727857    3.74000 2.741562e-01
# Votes          166.000000 67094.00000 9091.416667 5180.50000 1.271994e+04
# Friends          2.000000  1066.00000   91.047619   39.00000 1.607048e+02
# Activeness       5.384615    29.04587   13.440747   12.22870 5.569527e+00
# Influence        6.444444   890.09524  134.527504   79.38423 1.664241e+02

#
# Analysis
# There are statistical outliers which are affecting the segmentation balance in this model.
# Let us reserve these observations witout judement. They could be considered power users or could very well be automatons.
# 
power_users = cluster_1

# Remove the power_users cluster from the data set for future analysis
df_2 = df[model$cluster != 1, ]

# Review elbow to determine reasoable number of clusters
elbowChart(df_2) # appears to begin leveling at 4 clusters (as before)


# generate a new model
model2 = kmeans(df_2, 4)

# Inspect elements of the new model

model2$centers
# These groups are more similar, though cluster 3 appears again to have a greater activness and influence impact
#
#   Average Review  Activeness   Influence
# 1    -0.16202162 -0.09519675 -0.07396731
# 2     0.96145133 -0.26950675 -0.10708463
# 3     0.01220007  2.43753874  0.66161728
# 4    -2.03945047 -0.30862507 -0.11138579

model2$size
# Again, this out-lier group is the smallest
# 
# [1] 4769 3440  463 1245

# Plot influnce vs activeness of the sub-group
plot(df_2$Influence ~ df_2$Activeness , col=model2$cluster)

# Plot everyone excpet group 3
# Notice - the observations in this group vary more widely within their activity vs their influence.
plot(df[model2$cluster !=3, 'Influence'] ~ df[model2$cluster != 3, 'Activeness' ])

# reserve Cluster 3 users into a segement called active_infuencers
active_influencers = df[model2$cluster == 3, ]

# Remove the Cluster 3 users from the data set
df_3 = df_2[model2$cluster !=3, ]

# review the elbowchart of this reduced set to determine optimal group size
# - 3 groups seems to be a good choice
elbowChart(df_3)

# Build a new kmeans model with three segements
model3 = kmeans(df_3, 3)
model3$centers
# 
# This model indicates similar influence amongst all users, with a slightly higher activity in cluster 1
# Cluster three provides more positive reviews than the other two groupings
# 
# Average Review  Activeness   Influence
# 1     -0.1620216 -0.09519675 -0.07396731
# 2     -2.0394505 -0.30862507 -0.11138579
# 3      0.9614513 -0.26950675 -0.10708463

model3$size
# [1] 4769 1245 3440


# Notice - Influence and Activeness have less of an impact on clustering in this set
# The number of reviews, months active and averate review appear to vary much more, and impact assigned cluster
# 
# Confirmed with these plots
plot(df_3$Influence ~ df_3$Activeness, col=model3$cluster) # Not much relation to cluster

# There is a stronger relationship between Reviews value and Activeness 
plot( df_3$Activeness ~ df_3$`Average Review`, col=model3$cluster)

#
# Analysis
# Cluster 3 represents a target group which is likely to provide positive reviews
#

