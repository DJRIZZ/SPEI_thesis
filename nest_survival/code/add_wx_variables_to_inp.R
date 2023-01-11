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

##########Create weather variable cloumns in spei data
##winter extreme temp
spei$wxt<-fifelse(spei$Year == 1994,14,
                  fifelse(spei$Year == 1995,13,
                          fifelse(spei$Year == 1996,6,
                                  fifelse(spei$Year == 1997,5,
                                          fifelse(spei$Year == 1998,9,
                                                  fifelse(spei$Year == 1999,6,
                                                          fifelse(spei$Year == 2000,15,
                                                                  fifelse(spei$Year == 2001,0,
                                                                          fifelse(spei$Year == 2002,19,
                                                                                  fifelse(spei$Year == 2003,0,
                                                                                          fifelse(spei$Year == 2004,0,
                                                                                                  fifelse(spei$Year == 2005,0,
                                                                                                          fifelse(spei$Year == 2006,10,
                                                                                                                  fifelse(spei$Year == 2007,14,
                                                                                                                          fifelse(spei$Year == 2008,19,
                                                                                                                                  fifelse(spei$Year == 2009,21,
                                                                                                                                          fifelse(spei$Year == 2010,23,
                                                                                                                                                  fifelse(spei$Year == 2011,1,
                                                                                                                                                          fifelse(spei$Year == 2012,36,
                                                                                                                                                                  fifelse(spei$Year == 2013,14,
                                                                                                                                                                          fifelse(spei$Year == 2014,5,
                                                                                                                                                                                  fifelse(spei$Year == 2015,0,
                                                                                                                                                                                          fifelse(spei$Year == 2016,0,
                                                                                                                                                                                                  fifelse(spei$Year == 2017,3,
                                                                                                                                                                                                          fifelse(spei$Year == 2018,0,
                                                                                                                                                                                                                  fifelse(spei$Year == 2019,0,0))))))))))))))))))))))))))
###spring mean min temp
spei$sxt<-fifelse(spei$Year == 1994,1,
                  fifelse(spei$Year == 1995,5,
                          fifelse(spei$Year == 1996,2,
                                  fifelse(spei$Year == 1997,1,
                                          fifelse(spei$Year == 1998,0,
                                                  fifelse(spei$Year == 1999,2,
                                                          fifelse(spei$Year == 2000,0,
                                                                  fifelse(spei$Year == 2001,0,
                                                                          fifelse(spei$Year == 2002,0,
                                                                                  fifelse(spei$Year == 2003,0,
                                                                                          fifelse(spei$Year == 2004,4,
                                                                                                  fifelse(spei$Year == 2005,1,
                                                                                                          fifelse(spei$Year == 2006,1,
                                                                                                                  fifelse(spei$Year == 2007,0,
                                                                                                                          fifelse(spei$Year == 2008,4,
                                                                                                                                  fifelse(spei$Year == 2009,5,
                                                                                                                                          fifelse(spei$Year == 2010,5,
                                                                                                                                                  fifelse(spei$Year == 2011,3,
                                                                                                                                                          fifelse(spei$Year == 2012,5,
                                                                                                                                                                  fifelse(spei$Year == 2013,7,
                                                                                                                                                                          fifelse(spei$Year == 2014,0,
                                                                                                                                                                                  fifelse(spei$Year == 2015,0,
                                                                                                                                                                                          fifelse(spei$Year == 2016,0,
                                                                                                                                                                                                  fifelse(spei$Year == 2017,0,
                                                                                                                                                                                                          fifelse(spei$Year == 2018,0,
                                                                                                                                                                                                                  fifelse(spei$Year == 2019,0,0))))))))))))))))))))))))))

####winter extreme wind
spei$wxw<-fifelse(spei$Year == 1994,7,
                  fifelse(spei$Year == 1995,9,
                          fifelse(spei$Year == 1996,8,
                                  fifelse(spei$Year == 1997,6,
                                          fifelse(spei$Year == 1998,4,
                                                  fifelse(spei$Year == 1999,7,
                                                          fifelse(spei$Year == 2000,6,
                                                                  fifelse(spei$Year == 2001,11,
                                                                          fifelse(spei$Year == 2002,14,
                                                                                  fifelse(spei$Year == 2003,3,
                                                                                          fifelse(spei$Year == 2004,7,
                                                                                                  fifelse(spei$Year == 2005,5,
                                                                                                          fifelse(spei$Year == 2006,8,
                                                                                                                  fifelse(spei$Year == 2007,6,
                                                                                                                          fifelse(spei$Year == 2008,6,
                                                                                                                                  fifelse(spei$Year == 2009,8,
                                                                                                                                          fifelse(spei$Year == 2010,5,
                                                                                                                                                  fifelse(spei$Year == 2011,7,
                                                                                                                                                          fifelse(spei$Year == 2012,11,
                                                                                                                                                                  fifelse(spei$Year == 2013,8,
                                                                                                                                                                          fifelse(spei$Year == 2014,9,
                                                                                                                                                                                  fifelse(spei$Year == 2015,11,
                                                                                                                                                                                          fifelse(spei$Year == 2016,8,
                                                                                                                                                                                                  fifelse(spei$Year == 2017,7,
                                                                                                                                                                                                          fifelse(spei$Year == 2018,9,
                                                                                                                                                                                                                  fifelse(spei$Year == 2019,11,0))))))))))))))))))))))))))

###spring extreme wind
spei$sxw<-fifelse(spei$Year == 1994,1,
                  fifelse(spei$Year == 1995,1,
                          fifelse(spei$Year == 1996,3,
                                  fifelse(spei$Year == 1997,0,
                                          fifelse(spei$Year == 1998,2,
                                                  fifelse(spei$Year == 1999,1,
                                                          fifelse(spei$Year == 2000,0,
                                                                  fifelse(spei$Year == 2001,3,
                                                                          fifelse(spei$Year == 2002,2,
                                                                                  fifelse(spei$Year == 2003,3,
                                                                                          fifelse(spei$Year == 2004,2,
                                                                                                  fifelse(spei$Year == 2005,3,
                                                                                                          fifelse(spei$Year == 2006,3,
                                                                                                                  fifelse(spei$Year == 2007,1,
                                                                                                                          fifelse(spei$Year == 2008,0,
                                                                                                                                  fifelse(spei$Year == 2009,2,
                                                                                                                                          fifelse(spei$Year == 2010,1,
                                                                                                                                                  fifelse(spei$Year == 2011,5,
                                                                                                                                                          fifelse(spei$Year == 2012,0,
                                                                                                                                                                  fifelse(spei$Year == 2013,1,
                                                                                                                                                                          fifelse(spei$Year == 2014,1,
                                                                                                                                                                                  fifelse(spei$Year == 2015,2,
                                                                                                                                                                                          fifelse(spei$Year == 2016,1,
                                                                                                                                                                                                  fifelse(spei$Year == 2017,1,
                                                                                                                                                                                                          fifelse(spei$Year == 2018,1,
                                                                                                                                                                                                                  fifelse(spei$Year == 2019,2,0))))))))))))))))))))))))))
INP_20220329<-spei
write.csv(INP_20220329, "nest_survival/data/INP_20220329.csv")
