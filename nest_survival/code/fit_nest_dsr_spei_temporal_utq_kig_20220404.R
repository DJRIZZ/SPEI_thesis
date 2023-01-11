# Code to fit daily nest survival models in program Mark with package RMark
# Data are from spectacled eiders breeding at Kigigak Island and Utqiagvik

# Nest data includes:
# Kigigak: 1994 - 2015
# Utqiagvik: 2010 - 2018

# Load packages
library(RMark)
library(tidyverse)

# Load the input file
all_data<-read.csv("nest_survival/data/input_spei_nestsurv_kig_utq_all_xvars_20220401.csv")

# Select variables needed for the analysis
keep.these<-c("FirstFound", "LastPresent", "LastChecked", "Fate", "Freq", "Init", "NestAge", "Year", "Site", "Nest", "w_days_tmin_below_15", 
              "sp_days_tmin_below_11", "w_days_awnd_above_13_8", "sp_days_awnd_above_12", "winter_count_sic_ge95", "spring_count_sic_ge95")     

sub_data<-as.data.frame(all_data[, names(all_data) %in% keep.these])

# shorten column names
long.names<-colnames(sub_data)
long.names

short.names<-c("FirstFound", "LastPresent", "LastChecked", "Fate", "Freq", "i", "AgeDay1", "s", "Nest", "wxt", "sxt", "wxw", "sxw","w95", "s95", "y")
test.names<-cbind.data.frame(long.names, short.names)
test.names

# change column names
colnames(sub_data)<-short.names
str(sub_data)

# reorder columns a bit
ev<-c("FirstFound", "LastPresent", "LastChecked", "Fate", "Freq", "Nest")
eh_vars<-as.data.frame(sub_data[, names(sub_data) %in% ev])
xv<-c("s", "y", "i", "AgeDay1", "wxt", "sxt", "wxw", "sxw","w95", "s95")
x_vars<-as.data.frame(sub_data[, names(sub_data) %in% xv])
kig_utq<-cbind(eh_vars, x_vars)
str(kig_utq)
data<-kig_utq

# Create an indicator variable column for each parameter the model will estimate, rather than using the "group" option

# Create indicator variable columns for year, set 2010 as the reference level of year (i.e., intercept)
data$y1994<-ifelse(data$y==1994,1,0) # data only from Kig
data$y1995<-ifelse(data$y==1995,1,0) # data only from Kig
data$y1996<-ifelse(data$y==1996,1,0) # data only from Kig
data$y1997<-ifelse(data$y==1997,1,0) # data only from Kig
data$y1998<-ifelse(data$y==1998,1,0) # data only from Kig
data$y1999<-ifelse(data$y==1999,1,0) # data only from Kig
data$y2000<-ifelse(data$y==2000,1,0) # data only from Kig
data$y2001<-ifelse(data$y==2001,1,0) # data only from Kig
data$y2002<-ifelse(data$y==2002,1,0) # data only from Kig
data$y2003<-ifelse(data$y==2003,1,0) # data only from Kig
data$y2004<-ifelse(data$y==2004,1,0) # data only from Kig
data$y2005<-ifelse(data$y==2005,1,0) # data only from Kig
data$y2006<-ifelse(data$y==2006,1,0) # data only from Kig
data$y2007<-ifelse(data$y==2007,1,0) # data only from Kig
data$y2008<-ifelse(data$y==2008,1,0) # data only from Kig
data$y2009<-ifelse(data$y==2009,1,0) # data only from Kig
#data$y2010<-ifelse(data$y==2010,1,0) # Not included, used as the reference level (estimated by intercept parameter)
data$y2011<-ifelse(data$y==2011,1,0) # data from both sites
data$y2012<-ifelse(data$y==2012,1,0) # data from both sites
data$y2013<-ifelse(data$y==2013,1,0) # data from both sites
data$y2014<-ifelse(data$y==2014,1,0) # data from both sites
data$y2015<-ifelse(data$y==2015,1,0) # data from both sites
data$y2016<-ifelse(data$y==2016,1,0) # data only from Utq
data$y2017<-ifelse(data$y==2017,1,0) # data only from Utq
data$y2018<-ifelse(data$y==2018,1,0) # data from both Utq

# Create indicator variable columns for site 
data$Utq<-ifelse(data$s=="Utq", 1,0)
#data$Kig<-ifelse(data$s=="Kig", 1,0) # Kigigak is the reference level, estimated by intercept

# Create Year*Site interaction indicator variables for years with data from both sites
#data$Utq2010<-data$Utq*yr2010 # Utq in 2010 is captured by B(Utq)+B(yr2010)
data$Utq2011<-data$Utq*data$y2011
data$Utq2012<-data$Utq*data$y2012
data$Utq2013<-data$Utq*data$y2013
data$Utq2014<-data$Utq*data$y2014
data$Utq2015<-data$Utq*data$y2015

str(data)
summary(data)

# apparent nest failure (since Fate = 1 = failed nests) by year*site
apparent_success<-aggregate(Fate~y + s , data=data, mean)

# Function to fit nest DSR models in Mark using RMark
run.models=function()
{
# null model, useful for assessing model fit
S.null = mark(data, nocc=50, model="Nest", model.name="S.null", model.parameters=list(S=list(formula=~1)))
                                              
# year + site model with indicator variables
S.ys=mark(data, nocc=50, model="Nest",  model.name="S.ys", model.parameters=list(S=list(formula=~y1994 + y1995 + y1996 + y1997 + y1998 + y1999 + y2000 + 
                                                                          y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 +
                                                                          y2008 + y2009 + y2011 + y2012 + y2013 + y2014 + y2015 +
                                                                          y2016 + y2017 + y2018 + Utq))) # y2010 and Kig are reference levels captured by the int est

# age + year + site model with indicator variables
S.ays=mark(data, nocc=50, model="Nest", model.name="S.ays", model.parameters=list(S=list(formula=~NestAge + y1994 + y1995 + y1996 + y1997 + y1998 + y1999 + y2000 + 
                                                                              y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 +
                                                                              y2008 + y2009 + y2011 + y2012 + y2013 + y2014 + y2015 +
                                                                              y2016 + y2017 + y2018 + Utq )))

# init + year + site model with indicator variables
S.iys=mark(data, nocc=50, model="Nest", model.name="S.iys", model.parameters=list(S=list(formula=~i + y1994 + y1995 + y1996 + y1997 + y1998 + y1999 + y2000 + 
                                                                             y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 +
                                                                             y2008 + y2009 + y2011 + y2012 + y2013 + y2014 + y2015 +
                                                                             y2016 + y2017 + y2018 + Utq )))

# age + init + year + site model with indicator variables
S.iays=mark(data, nocc=50, model="Nest", model.name="S.iays", model.parameters=list(S=list(formula=~NestAge + i + y1994 + y1995 + y1996 + y1997 + y1998 + y1999 + y2000 + 
                                                                              y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 +
                                                                              y2008 + y2009 + y2011 + y2012 + y2013 + y2014 + y2015 +
                                                                              y2016 + y2017 + y2018 + Utq )))

# year*site model with indicator variables and initial values from additive year + site model (model would not converge without initial values)
S.yxs=mark(data, nocc=50, model="Nest", model.name="S.yxs", model.parameters=list(S=list(formula=~y1994 + y1995 + y1996 + y1997 + y1998 + y1999 + y2000 + 
                                                                           y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 +
                                                                           y2008 + y2009 + y2011 + y2012 + y2013 + y2014 + y2015 +
                                                                           y2016 + y2017 + y2018 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 + Utq2015)), initial=S.ys)

# age + year*site model with indicator variables and initial values from year x site model
S.ayxs=mark(data, nocc=50, model="Nest", model.name="S.ayxs", model.parameters=list(S=list(formula=~NestAge + y1994 + y1995 + y1996 + y1997 + y1998 + y1999 + y2000 + 
                                                                              y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 +
                                                                              y2008 + y2009 + y2011 + y2012 + y2013 + y2014 + y2015 +
                                                                              y2016 + y2017 + y2018 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 + Utq2015)), initial=S.yxs)

# init + year*site model with indicator variables and initial values from year x site model
S.iyxs=mark(data, nocc=50, model="Nest", model.name="S.iyxs", model.parameters=list(S=list(formula=~i + y1994 + y1995 + y1996 + y1997 + y1998 + y1999 + y2000 + 
                                                                        y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 +
                                                                        y2008 + y2009 + y2011 + y2012 + y2013 + y2014 + y2015 +
                                                                        y2016 + y2017 + y2018 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 + Utq2015)), initial=S.yxs)

# age + init + year*site model with indicator variables and initial values from year x site model
S.aiyxs=mark(data, nocc=50, model="Nest", model.name="S.aiyxs", model.parameters=list(S=list(formula=~NestAge + i + y1994 + y1995 + y1996 + y1997 + y1998 + y1999 + y2000 + 
                                                                        y2001 + y2002 + y2003 + y2004 + y2005 + y2006 + y2007 +
                                                                        y2008 + y2009 + y2011 + y2012 + y2013 + y2014 + y2015 +
                                                                        y2016 + y2017 + y2018 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 + Utq2015)), initial=S.iyxs)


# Return model table and list of models

return(collect.models())
}

# Fit models
model.results=run.models()

# Model selection
model.results

# look at estimates from top model
model.results$S.aiyxs$results$beta

model.results                     # print model-selection table to screen
options(width = 150)              # set page width to 100 characters
sink("temp2.results.table.txt")         # capture screen output to file
print(model.results)              # send output file
sink()
model.results$S.aiyxs
