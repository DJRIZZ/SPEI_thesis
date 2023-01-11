####Add count days values for number of extreme temperatures and winds to the INP file

# Packages
library(data.table)
library(tidyverse)
library(dplyr) #useful to create column based on a values of another column. Probably not using it here...


###Load the spei and winter data
spei<-read.csv("nest_survival/data/INP_03.csv", header = TRUE)
###Rename first column
names(spei)[names(spei) == "ï..Nest"] <- "Nest"

wx<-read.csv("nest_survival/data/ice_wx.csv", header = TRUE)

# add variable for breeding year, where ice and weather are for the winter_year preceding the breeding year
# e.g., winter_year=2015 is winter Dec 2015-April 2016 and should get linked to breeding year 2016
wx$year_br<-as.numeric(wx$year_winter)+1

# merge
spei_xvars<-merge(spei, wx, all.x=T, by.x="Year", by.y="year_br" )
str(spei_xvars)

# merge explained
#spei_xvars<-merge(spei, # the first dataframe listed is called the "x" data set
#                  wx, # the second dataframe listed is called the "y" data set
#                  all.x=T, # this says to keep all the rows in the x data set and only the rows in y that match x according to the "by" variable
#                  by.x="Year", # the variable used to merge x and y together is called "Year" in the x data set
#                  by.y="year_br" ) # and is called "year_br" in the y data set
