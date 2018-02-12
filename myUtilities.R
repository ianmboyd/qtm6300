#
# myUtilities.R
# Helper functions for data manipulation
#

#
# column-wise MAX
#

colMax <- function(data) sapply( data, max, na.rm=TRUE)
colMin <- function(data) sapply( data, min, na.rm=TRUE)
colSd  <- function(data) sapply( data, sd, na.rm=TRUE)