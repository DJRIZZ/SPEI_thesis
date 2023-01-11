###Subset nestign data, kig and utq
###add fox index variable to kig data


###load packages
library(data.table)
library(tidyverse)


###read in spei data
spei_data<- read.csv("nest_survival/data/INP_20220408.csv", header = TRUE)

###subset kig and utq data
kig_data<-spei_data[spei_data$Site=="kig",]
utq_data<-spei_data[spei_data$Site=="utq",]

###add fox data to kig_data. fox index are from fischer et al 2017. 2019 and 2021 do not have fox index
kig_data$fox<-fifelse(kig_data$Year==1994,0.16,
                      fifelse(kig_data$Year==1995,0.44,
                              fifelse(kig_data$Year==1996,0.45,
                                      fifelse(kig_data$Year==1997,0.35,
                                              fifelse(kig_data$Year==1998,0.34,
                                                      fifelse(kig_data$Year==1999,0.45,
                                                              fifelse(kig_data$Year==2000,0.30,
                                                                      fifelse(kig_data$Year==2001,0.89,
                                                                              fifelse(kig_data$Year==2002,0.60,
                                                                                      fifelse(kig_data$Year==2003,0.83,
                                                                                              fifelse(kig_data$Year==2004,0.65,
                                                                                                      fifelse(kig_data$Year==2005,0.55,
                                                                                                              fifelse(kig_data$Year==2006,0.59,
                                                                                                                      fifelse(kig_data$Year==2007,0.33,
                                                                                                                              fifelse(kig_data$Year==2008,0.71,
                                                                                                                                      fifelse(kig_data$Year==2009,0.41,
                                                                                                                                              fifelse(kig_data$Year==2010,0.42,
                                                                                                                                                      fifelse(kig_data$Year==2011,0.55,
                                                                                                                                                              fifelse(kig_data$Year==2012,0.43,
                                                                                                                                                                      fifelse(kig_data$Year==2013,0.42,
                                                                                                                                                                              fifelse(kig_data$Year==2014,0.61,
                                                                                                                                                                                      fifelse(kig_data$Year==2015,0.60,0))))))))))))))))))))))

###write csv files to data folder 
write.csv(kig_data,"nest_survival/data/kig_inp_20220524.csv")
write.csv(utq_data,"nest_survival/data/utq_inp_20220524.csv")
