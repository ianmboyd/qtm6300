set.seed(123)
library(zipcode)
data(zipcode)

# Get the data
df = read_csv('c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/week4/UniversalBank.csv')


# clean the ZIP codes and then merge the zipdata into the banking data
df$zip = clean.zipcodes(df$`ZIP Code`)

df = merge(zipcode, df, by.x='zip', by.y='zip' )

# we lose some observations because of this (can't map the ZIP code)
str(df)

# There is one datapoint which is listed as military - no lat/long
df = na.omit(df)

# factorize some of the date 
df$state_f = as.factor(df$state)
str(df$state_f)
# all data is from CA, let's kill this useless data
df$state = NULL;
df$state_f = NULL;
str(df)

# turn the cities into Factors
df$city_f = as.factor(df$city)
str(df$city_f)



# Reduce the ZIP code data set to be only CA Zipcodes
zipcode = zipcode[zipcode$state == 'CA', ]

library(ggplot2)
library(mapproj)
library(raster)

# Load the geom data for California
us = getData("GADM", country="USA", level=1)
states = c('California')
us.states = us[us$NAME_1 %in% states, ]

# Plot the data - Green is where we have personal loans
g = ggplot() +
  geom_point(data=zipcode, aes(x=longitude, y=latitude), color='sky blue') +
  geom_point(data=df[df$`Personal Loan`==0, ], aes(x=longitude, y=latitude), color='red') + 
  geom_point(data=df[df$`Personal Loan`==1, ], aes(x=longitude, y=latitude), color='green') +
  geom_path(data=us.states, aes(x=long, y=lat, group=group)) + coord_map() +
  labs(x=NULL, y=NULL)

# Most customer data surrounds larger metropolitan areas


