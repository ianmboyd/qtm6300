#
# myUtilities.R
# Helper functions for data manipulation
#

#
# column-wise MAX
#

colMax <- function(data) sapply( data, max, na.rm=TRUE)

#
# Column-wise MIN
# 
colMin <- function(data) sapply( data, min, na.rm=TRUE)

#
# Column-wise standard deviation
#
colSd  <- function(data) sapply( data, sd, na.rm=TRUE)

#
# Column-wise median
#
colMedian <- function(data) sapply( data, median, na.rm=TRUE)


#
# Build a statistical summary of a dataframe
# 
dfStats <- function(data){
   return( data.frame( min = colMin(data), max=colMax(data), mean=colMeans(data), median=colMedian(data), stdDev = colSd(data)) )
}