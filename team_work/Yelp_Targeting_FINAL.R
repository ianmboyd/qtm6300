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

# Build the model
model = kmeans(df, 4)

# inspect the centers of this model
model$centers
#   Months Active    Reviews Average Review       Votes     Friends Activeness   Influence
# 1    1.21455210  3.5394978    -0.01537103  1.29165665  1.44384396  3.6941013  1.46562839
# 2    1.68647787 11.7160580     0.02495089 19.42963419 11.69931033 10.4322577 19.30987008
# 3    0.09052619 -0.1155997     0.43344466 -0.07310388 -0.04796749 -0.1173875 -0.07802259
# 4   -0.50829441 -0.2507438    -1.45995547 -0.09721415 -0.14978252 -0.2596637 -0.10706324



# inspect the cluster sizes
model$size
# [1]  346   16 7435 2204

# Plot influnce vs activeness 
plot(data$Influence ~ data$Activeness , col=model$cluster)

# Looking at the results of this chart, it's clear that there are statistical outlyers which should be removed

#Look at the user makeup of cluster 2
cluster_2 = data.frame(data[model$cluster == 2 ,])
mean(cluster_2$Activeness) # 18.2
mean( cluster_2$Votes / cluster_2$Reviews) # 23.63

#
# It appears for some of these folks, they average about 18 reviews per month
# and receive about 24 votes per review
#
#
print(cluster_2)

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
out_lyers = cluster_2
out_lyer_data = data[model$cluster == 2, ]
dfStats(out_lyer_data)
#                       min         max        mean      median       stdDev
# Months Active    32.000000    95.00000    74.93750    80.00000 1.817496e+01
# Reviews         688.000000  2548.00000  1347.68750  1151.50000 5.508271e+02
# Average Review    3.310000     4.19000     3.76875     3.85500 2.675537e-01
# Votes          7064.000000 67094.00000 28586.37500 20681.50000 1.824693e+04
# Friends          40.000000  1066.00000   270.18750   224.00000 2.652019e+02
# Activeness        8.390244    27.39785    18.25456    18.11889 5.732499e+00
# Influence       102.911392   890.09524   389.44662   307.26250 2.258987e+02


# Remove the power_users cluster from the data set and continue our analysis
df = df[model$cluster != 2, ]
data = data[model$cluster != 2, ]

# Review elbow to determine reasoable number of clusters
elbowChart(df) # appears to begin leveling at 4 clusters (as before)

# generate a new model
model2 = kmeans(df, 4)

# Inspect elements of the new model

model2$centers
# The groups is more heterogeneous, though group4 appears again to have an extreme activness and influence score
#
#   Months Active     Reviews Average Review       Votes     Friends  Activeness   Influence
# 1     1.1919835  3.81973049    -0.01415148  1.44467330  1.61808306  4.00270836  1.64446229
# 2     1.0311727  0.06117523     0.09370120 -0.03532801  0.02238428  0.01240333 -0.04182136
# 3    -0.5996039 -0.22504132     0.62039823 -0.09574960 -0.09724138 -0.18954800 -0.09907650
# 4    -0.5323773 -0.25519062    -1.54639581 -0.09780149 -0.15237128 -0.27181959 -0.10833452

model2$size
# Again, this out-lier group (1) is the smallest
# 
# [1]  294 3250 4458 1983

# Plot influnce vs activeness of the sub-group
plot(df$Influence ~ df$Activeness , col=model2$cluster)

# Plot everyone excpet cluster 1
# Notice - the observations in this group vary more widely within their activity vs their influence.
plot(df[model2$cluster !=1, 'Influence'] ~ df[model2$cluster != 1, 'Activeness' ])

# reserve Cluster 1  users into a segement called power_users
power_users = df[model2$cluster == 1, ]
nrow(power_users) # =294
power_user_data = data[model2$cluster == 1, ]
dfStats(power_user_data)
#                     min         max        mean      median       stdDev
# Months Active  16.000000   118.00000   64.863946   65.000000   20.8277969
# Reviews        90.000000  3166.00000  462.132653  398.000000  286.3221045
# Average Review  2.530000     4.47000    3.728537    3.720000    0.2771877
# Votes          85.000000 16993.00000 2266.316327 1327.000000 2541.9133811
# Friends         0.000000   717.00000   41.044218   16.000000   78.1688124
# Activeness      1.216216    29.04587    7.401732    6.229708    3.9226644
# Influence       3.000000   290.36364   35.444474   20.727456   39.8858814

# Remove the Cluster 1 users from the data set
df = df[model2$cluster !=1, ]
data = data[model2$cluster !=1, ]

# review the elbowchart of this reduced set to determine optimal group size
# - 3 groups seems to be a good choice
elbowChart(df)

# Build a new kmeans model with three segements
model3 = kmeans(df, 3)
model3$centers
#   Months Active     Reviews Average Review       Votes     Friends  Activeness   Influence
# 1    -0.5323773 -0.25519062     -1.5463958 -0.09780149 -0.15237128 -0.27181959 -0.10833452
# 2     1.0311727  0.06117523      0.0937012 -0.03532801  0.02238428  0.01240333 -0.04182136
# 3    -0.5996039 -0.22504132      0.6203982 -0.09574960 -0.09724138 -0.18954800 -0.09907650

model3$size
# [1] 1983 3250 4458



# Notice - Influence and Activeness have less of an impact on clustering in this set
# The number of reviews, months active and averate review appear to vary much more, and impact assigned cluster

# Build a new kmeans model with four segements
model4 = kmeans(df, 4)
model4$centers
#   Months Active    Reviews Average Review       Votes     Friends Activeness   Influence
# 1    -0.6499734 -0.2344003     0.64635107 -0.09701264 -0.10250437 -0.2079279 -0.10224291
# 2     1.0206440  0.9469552     0.01274441  0.14278689  0.42482864  1.0791868  0.18337320
# 3    -0.5540376 -0.2589368    -1.57274442 -0.09824766 -0.15312800 -0.2791944 -0.10924970
# 4     0.9145702 -0.1315509     0.11253572 -0.07539540 -0.06393823 -0.1914103 -0.08787643

model4$size
# [1] 4142  589 1922 3038

# Plot everyone excpet cluster 2
# Notice - the observations in this group vary more widely within their activity vs their influence.
plot(df[model4$cluster !=2, 'Influence'] ~ df[model4$cluster != 2, 'Activeness' ])

# reserve Cluster 2  users into a segement called active_influencers
active_influencers = df[model4$cluster == 2, ]
active_influencer_data = data[model4$cluster == 2, ]
nrow(active_influencers) # =589


dfStats(active_influencer_data)
#                     min         max       mean     median      stdDev
# Months Active  12.00000000  116.000000  61.373514  61.000000  20.0521356
# Reviews         2.00000000  356.000000 139.957555 126.000000  60.0992934
# Average Review  1.50000000    5.000000   3.756197   3.750000   0.3396193
# Votes           1.00000000 3836.000000 361.073005 253.000000 361.7205292
# Friends         0.00000000  187.000000  13.921902   4.000000  26.5490948
# Activeness      0.02985075    6.894737   2.466943   2.254902   1.1491779
# Influence       0.53846154   55.285714   6.165293   4.594595   5.4270692

