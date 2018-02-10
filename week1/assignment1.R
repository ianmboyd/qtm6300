#
# assignment1.R
# Author:   Ian Boyd
# License:  Free
# 
# Purpose:  Establish basic undestanding of data loading, manipulation, slicing, a
#           for Yelp sourced data
#

# Control variable, if TRUE, load data from GitHUB, otherwise use local file

remoteData = TRUE


#
# (1).a Import the data
#

if( remoteData ){
  #Attempt to load the data from GITHUB repository
  library(RCurl)
  urlData <- getURL("https://raw.githubusercontent.com/ianmboyd/qtm6300/master/week1/Yelp.csv")
  df= read.csv(text=urlData)
}else{
  #Load the data from local storage
  library(readr)
  df = read_csv("C:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week1/Yelp.csv")
}

#
# (1).b 
# The datatypes for each column are as follows 
# ID            : Factor
# Months.Active : int
# Reviews       : int
# Average.Review: num
# Votes         : int
# Friends       : int

str(df) # print dataframe statistics

#
# (2).a
#
library(ggplot2) #load ggplot2
qplot(df$Average.Review, geom="histogram", xlab="Average Review")

#
# (2).b
#
# Looking at the histogram, it is apparent that 5 star reviews outnumber each of the other
# possible review levels. Additionally there is a "fuzzy" normal distribution around 4 stars,
# leading one to surmise a large proportion of reviews around this level.


#
# (3).a
#
ggplot(df) + geom_point(aes(y=Reviews, x=Months.Active), colour="blue", alpha=0.25) 

#
# (3).b
# A pattern apears to be emerging indicating the increased longevity leads to higher
# review counts. Unfortunately the data is too clustered near the X-axis, begging for 
# a re-scale.
# 

#
# (3).a -> At Log scale
#
ggplot(df) + geom_point(aes(y=Reviews, x=Months.Active), colour="blue", alpha=0.25) +
  scale_x_log10(breaks = 10^(-2:2), 
                labels=format(10^(-2:2), scientific=FALSE, drop0trailing=TRUE)) +
  scale_y_log10(breaks=c(5,10,20,40,80,500, 1000, 2000))

#
# (3).b -> Re-Visit
#
# It is clearer here that as there is a loose correlation between a user's length of activity 
# and the count of reviews they have submitted. However it appears as though there are bands
# of users whos activity does not increase over time. 
#

#
# (4) -> Histogram of Reviews / Month (by user)
#
rpm = df$Reviews / df$Months.Active
qplot(rpm, geom="histogram", xlab="Reviews Per Month", ylab="Users")

#
# (5).a -> Omit users with average reviews per month > 2
# 
rpm_lt_2 = rpm[rpm <= 2]
qplot(rpm_lt_2, geom="histogram", xlab="Reviews Per Month", ylab="Users")


#
# (5).b
#
# By removing the "power reviewers", who amount to statistical anomalies, a clearer picture 
# of the "average" yelper is revealed. The vast majority of users appear to only post once 
# every 2-4 months