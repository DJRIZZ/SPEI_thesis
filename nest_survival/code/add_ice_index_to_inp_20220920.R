####Add ice severity index to the current data

# Packages
library(data.table)
library(tidyverse)
library(dplyr) #useful to create column based on a values of another column. 


###Load the latest spei data
spei<-read.csv("nest_survival/data/INP_20220408.csv", header = TRUE)

##########Create weather variable cloumns in spei data
##winter spring index (wsi)
spei$wsi<-fifelse(spei$Year == 1994,127.995498805247,
                  fifelse(spei$Year == 1995,260.711091695671,
                          fifelse(spei$Year == 1996,186.633288816554,
                                  fifelse(spei$Year == 1997,109.697954015797,
                                          fifelse(spei$Year == 1998,221.200051668264,
                                                  fifelse(spei$Year == 1999,211.555377706007,
                                                          fifelse(spei$Year == 2000,202.981929520701,
                                                                  fifelse(spei$Year == 2001,12.476649250079,
                                                                          fifelse(spei$Year == 2002,183.057713957143,
                                                                                  fifelse(spei$Year == 2003,114.357038687245,
                                                                                          fifelse(spei$Year == 2004,117.589147365108,
                                                                                                  fifelse(spei$Year == 2005,72.0112792152632,
                                                                                                          fifelse(spei$Year == 2006,269.705356935888,
                                                                                                                  fifelse(spei$Year == 2007,173.895749819952,
                                                                                                                          fifelse(spei$Year == 2008,203.235519409773,
                                                                                                                                  fifelse(spei$Year == 2009,196.515161101914,
                                                                                                                                          fifelse(spei$Year == 2010,319.158856312909,
                                                                                                                                                  fifelse(spei$Year == 2011,197.118058727158,
                                                                                                                                                          fifelse(spei$Year == 2012,212.82512731416,
                                                                                                                                                                  fifelse(spei$Year == 2013,226.877715842988,
                                                                                                                                                                          fifelse(spei$Year == 2014,93.5283959413526,
                                                                                                                                                                                  fifelse(spei$Year == 2015,38.7983370460238,
                                                                                                                                                                                          fifelse(spei$Year == 2016,16.8189824036124,
                                                                                                                                                                                                  fifelse(spei$Year == 2017,85.1540268488292,
                                                                                                                                                                                                          fifelse(spei$Year == 2018,0,
                                                                                                                                                                                                                  fifelse(spei$Year == 2019,1.38629436111989,
                                                                                                                                                                                                                          fifelse(spei$Year == 2021,151.115282363492,0)))))))))))))))))))))))))))

##winter index (wi)
spei$wi<-fifelse(spei$Year == 1994,98.1253026040822,
                  fifelse(spei$Year == 1995,251.870077385187,
                          fifelse(spei$Year == 1996,178.586099254383,
                                  fifelse(spei$Year == 1997,74.9673180076768,
                                          fifelse(spei$Year == 1998,182.428026348894,
                                                  fifelse(spei$Year == 1999,176.616343102057,
                                                          fifelse(spei$Year == 2000,165.657013370172,
                                                                  fifelse(spei$Year == 2001,0,
                                                                          fifelse(spei$Year == 2002,141.161051379493,
                                                                                  fifelse(spei$Year == 2003,101.62771789795,
                                                                                          fifelse(spei$Year == 2004,114.293310499104,
                                                                                                  fifelse(spei$Year == 2005,42.1923994178072,
                                                                                                          fifelse(spei$Year == 2006,243.85175707887,
                                                                                                                  fifelse(spei$Year == 2007,150.869898890011,
                                                                                                                          fifelse(spei$Year == 2008,109.933793124867,
                                                                                                                                  fifelse(spei$Year == 2009,116.043265480209,
                                                                                                                                          fifelse(spei$Year == 2010,177.692529428457,
                                                                                                                                                  fifelse(spei$Year == 2011,114.85715847332,
                                                                                                                                                          fifelse(spei$Year == 2012,136.55183538581,
                                                                                                                                                                  fifelse(spei$Year == 2013,168.894925243765,
                                                                                                                                                                          fifelse(spei$Year == 2014,77.8159342928876,
                                                                                                                                                                                  fifelse(spei$Year == 2015,29.9573227355399,
                                                                                                                                                                                          fifelse(spei$Year == 2016,13.5231455376081,
                                                                                                                                                                                                  fifelse(spei$Year == 2017,76.7984981934947,
                                                                                                                                                                                                          fifelse(spei$Year == 2018,0,
                                                                                                                                                                                                                  fifelse(spei$Year == 2019,11.38629436111989,
                                                                                                                                                                                                                          fifelse(spei$Year == 2021,101.208685363176,0)))))))))))))))))))))))))))
##Write the csv file for new data
INP_20220921<-spei
write.csv(INP_20220921, "nest_survival/data/INP_20220921.csv")
