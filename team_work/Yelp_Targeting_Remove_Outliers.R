# Force the RAND seed (for consistency)
set.seed(1234)

#HEADER
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")
#source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/myUtilities.R")

#inline Helper Functions
colMax <- function(data) sapply( data, max, na.rm=TRUE)
colMin <- function(data) sapply( data, min, na.rm=TRUE)
colSd  <- function(data) sapply( data, sd, na.rm=TRUE)
colMedian <- function(data) sapply( data, median, na.rm=TRUE)
dfStats <- function(data){
  return( data.frame( min = colMin(data), max=colMax(data), mean=colMeans(data), median=colMedian(data), stdDev = colSd(data)) )
}



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



# Standardize the data (decompose to T-Score)
df = easyStandardize(df, c(1,2,3,4,5,6,7))

# Look for the "bend" of diminishing returns WRT group numbers
elbowChart(df)

# Print stats of df
# 
dfStats(df)
#
# There are some pretty crazy outliers
#
#                      min       max          mean      median    stdDev
# Months Active  -1.6484630  3.800346  1.386894e-16 -0.07763531      1
# Reviews        -0.2921034 27.929612 -1.715886e-17 -0.24751937      1
# Average Review -2.6673575  1.222208  1.922567e-16  0.15257725      1
# Votes          -0.1039407 45.742574 -5.255296e-18 -0.09915745      1
# Friends        -0.1876696 46.711319  1.520992e-17 -0.18766958      1
# Activeness     -0.3759363 16.825367 -2.371650e-17 -0.28146720      1
# Influence      -0.1242871 44.293227  7.325438e-18 -0.11110546      1

std = 3
#Remove Activeness Outliers
df_clean<- df[ (df$Activeness < std) & (df$Activeness > -std) , ]
#Print number of rows minus Activeness Outliers 
# = 9805
nrow(df_clean)

df_clean = df_clean[(df_clean$Influence < std) & (df_clean$Influence > -std) , ]
nrow(df_clean)
#Print number of rows minus Activeness Outliers 
# = 9794

dfStats(df_clean)
# min       max          mean      median    stdDev
# Months Active  -1.6484630  3.800346 -0.0216503095 -0.12672367 0.9870657
# Reviews        -0.2921034  4.772647 -0.1059874866 -0.25643618 0.4423503
# Average Review -2.6673575  1.222208  0.0001816679  0.16230116 1.0097295
# Votes          -0.1039407  3.073486 -0.0651201492 -0.09915745 0.1521107
# Friends        -0.1876696 31.356960 -0.0479663112 -0.18766958 0.5938604
# Activeness     -0.3759363  2.994550 -0.1130665877 -0.28356802 0.4779023
# Influence      -0.1242871  2.789345 -0.0704609598 -0.11181162 0.1663597

df = df_clean
# Look for the "bend" of diminishing returns WRT group numbers
elbowChart(df)
# Build the model
model = kmeans(df, 4)
# inspect the centers of this model
model$centers
# 
# Months Active    Reviews Average Review       Votes     Friends Activeness   Influence
# 1    -0.3809391 -0.1810244    -0.19230738 -0.08937341 -0.10024252 -0.1225920 -0.08840621
# 2    -0.5329562 -0.2718052    -2.09131791 -0.09939558 -0.15492801 -0.3100276 -0.11230839
# 3     1.2889266  0.2485241     0.09693815  0.02342081  0.13264049  0.1767773  0.01854227
# 4    -0.5293566 -0.2583852     1.01280949 -0.09955493 -0.09949103 -0.2741378 -0.10969931


# inspect the cluster sizes
model$size
# [1] 3357 1165 2463 2809


# Plot influnce vs activeness 
plot(data$Influence ~ data$Activeness , col=model$cluster)

# Looking at the results of this chart, it's clear that there are statistical outlyers which should be removed

#Look at the user makeup of cluster 4
cluster_4 = data.frame(data[model$cluster == 4 ,])
mean(cluster_4$Activeness) # 18.2
mean( cluster_4$Votes / cluster_4$Reviews) # 23.63

#
# It appears for some of these folks, they average about 18 reviews per month
# and receive about 24 votes per review
#
#
print(cluster_4)

#     Months.Active Reviews Average.Review Votes Friends Activeness Influence
# 1             32     789           3.86 10669      41  24.656250  334.6875
# 2             80    1317           3.83 22106     281  16.462500  279.8375
# 3             76    1009           3.57 44439      40  13.276316  585.2500
# 4             79    1135           4.01  7064    1066  14.367089  102.9114
# 5             89    1401           3.95 17225     167  15.741573  195.4157
# 6             63     928           4.19 55571     505  14.730159  890.0952
# 7             85    1920           3.42 13647      98  22.588235  161.7059
# 8             89    1760           4.05 42721     456  19.775281  485.1348
# 9             80    1102           3.56 19550      52  13.775000  245.0250
# 10            82     688           3.42 14713     344   8.390244  183.6220
# 11            43    1081           3.85 19552     346  25.139535  462.7442
# 12            53    1168           3.96 12584      84  22.037736  239.0189
# 13            86    1759           3.48 43628     330  20.453488  511.1395
# 14            93    2548           3.31 21811      98  27.397849  235.5806
# 15            95    2240           3.97 67094     368  23.578947  710.1263
# 16            74     718           3.87 45008      47   9.702703  608.8514


#
# Analysis
# There are statistical outliers which are affecting the segmentation balance in this model.
# Let us reserve these observations witout judement. They could be considered power users or could very well be automatons.
# 
power_users = cluster_4

# Remove the power_users cluster from the data set for future analysis
df = df[model$cluster != 4, ]

# Review elbow to determine reasoable number of clusters
elbowChart(df) # appears to begin leveling at 4 clusters (as before)

# generate a new model
model2 = kmeans(df, 4)

# Inspect elements of the new model

model2$centers
# The groups is more heterogeneous, though group4 appears again to have an extreme activness and influence score
#
#   Months Active     Reviews Average Review       Votes     Friends  Activeness   Influence
# 1     1.0311727  0.06117523     0.09370120 -0.03532801  0.02238428  0.01240333 -0.04182136
# 2     1.1919835  3.81973049    -0.01415148  1.44467330  1.61808306  4.00270836  1.64446229
# 3    -0.5323773 -0.25519062    -1.54639581 -0.09780149 -0.15237128 -0.27181959 -0.10833452
# 4    -0.5996039 -0.22504132     0.62039823 -0.09574960 -0.09724138 -0.18954800 -0.09907650

model2$size
# Again, this out-lier group is the smallest
# 
# [1] 3250  294 1983 4458

# Plot influnce vs activeness of the sub-group
plot(df$Influence ~ df$Activeness , col=model2$cluster)

# Plot everyone excpet group 4
# Notice - the observations in this group vary more widely within their activity vs their influence.
plot(df[model2$cluster !=2, 'Influence'] ~ df[model2$cluster != 2, 'Activeness' ])

# reserve group_2 users into a segement called active_infuencers
active_influencers = df[model2$cluster == 2, ]

# Remove the group_2 users from the data set
df = df[model2$cluster !=2, ]

# review the elbowchart of this reduced set to determine optimal group size
# - 3 groups seems to be a good choice
elbowChart(df)

# Build a new kmeans model with three segements
model3 = kmeans(df, 3)
model3$centers
#   Months Active     Reviews Average Review       Votes     Friends  Activeness   Influence
# 1    -0.5996039 -0.22504132      0.6203982 -0.09574960 -0.09724138 -0.18954800 -0.09907650
# 2     1.0311727  0.06117523      0.0937012 -0.03532801  0.02238428  0.01240333 -0.04182136
# 3    -0.5323773 -0.25519062     -1.5463958 -0.09780149 -0.15237128 -0.27181959 -0.10833452

model3$size
# [1] 4458 3250 1983


# Notice - Influence and Activeness have less of an impact on clustering in this set
# The number of reviews, months active and averate review appear to vary much more, and impact assigned cluster
# 
# Confirmed with these plots
plot(df$Influence ~ df$Activeness, col=model3$cluster) # Not much relation to cluster

# There is a stronger relationship between Reviews/Months Active
plot(df$Reviews ~ df$`Months Active`, col=model3$cluster)

# There is an even stronger relationship between Reviews/Average Review Active and group 
plot(df$Reviews ~ df$`Average Review`, col=model3$cluster)

# The Strongest relationship between Months Active/Average Review Active and group 
plot(df$`Months Active` ~ df$`Average Review`, col=model3$cluster)


