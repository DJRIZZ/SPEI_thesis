###Indicator variable approach using initial values
###Nest Survival Models from Spectacled Eider Data
###Kigigak: 1994 - 2015, 2019, 2021
###Utqiagvik: 2010 - 2019
###Covariates: Init, NestAge, Year, Site (Kig, Utq), Win_Hi, Win_Lo, Spr_Hi, Spr_Lo, wxt, sxt, wxw, sxw 

###Load packages to run NS models
library(RMark)
library(tidyverse)
library(readr)

###Load the SPei data INP file
spei_data<- read.csv("nest_survival/data/INP_20220408.csv", header = TRUE)

###Check the headers. Sometimes column name changes when reading csv files. 
head(spei_data)
summary(spei_data)

###Make year a factor variable
is.factor(spei_data$Year)
spei_data$Year<-as.factor(spei_data$Year)
is.factor(spei_data$Year) #it is now a factor variable

###Make Site a factor variable
is.factor(spei_data$Site)
spei_data$Site<-as.factor(spei_data$Site)
is.factor(spei_data$Site) #now a factor variable

###Square the sea ice variables. (Quadratic term and effect). This is adding another column.
spei_data$Win_Hi2<-spei_data$Win_Hi^2
spei_data$Win_Lo2<-spei_data$Win_Lo^2
spei_data$Spr_Hi2<-spei_data$Spr_Hi^2
spei_data$Spr_Lo2<-spei_data$Spr_Lo^2
head(spei_data) #these two columns are now in the dataframe


###Create an indicator variable column for each parameter the model will estimate,
###instead of using the "group" method. 2010 will be reference level of year (intercept)
spei_data$yr1994<-ifelse(spei_data$Year==1994,1,0) # spei_data only from Kig
spei_data$yr1995<-ifelse(spei_data$Year==1995,1,0) # spei_data only from Kig
spei_data$yr1996<-ifelse(spei_data$Year==1996,1,0) # spei_data only from Kig
spei_data$yr1997<-ifelse(spei_data$Year==1997,1,0) # spei_data only from Kig
spei_data$yr1998<-ifelse(spei_data$Year==1998,1,0) # spei_data only from Kig
spei_data$yr1999<-ifelse(spei_data$Year==1999,1,0) # spei_data only from Kig
spei_data$yr2000<-ifelse(spei_data$Year==2000,1,0) # spei_data only from Kig
spei_data$yr2001<-ifelse(spei_data$Year==2001,1,0) # spei_data only from Kig
spei_data$yr2002<-ifelse(spei_data$Year==2002,1,0) # spei_data only from Kig
spei_data$yr2003<-ifelse(spei_data$Year==2003,1,0) # spei_data only from Kig
spei_data$yr2004<-ifelse(spei_data$Year==2004,1,0) # spei_data only from Kig
spei_data$yr2005<-ifelse(spei_data$Year==2005,1,0) # spei_data only from Kig
spei_data$yr2006<-ifelse(spei_data$Year==2006,1,0) # spei_data only from Kig
spei_data$yr2007<-ifelse(spei_data$Year==2007,1,0) # spei_data only from Kig
spei_data$yr2008<-ifelse(spei_data$Year==2008,1,0) # spei_data only from Kig
spei_data$yr2009<-ifelse(spei_data$Year==2009,1,0) # spei_data only from Kig
#spei_data$yr2010<-ifelse(spei_data$Year==2010,1,0) # Not included, used as the reference level (estimated by intercept parameter)
spei_data$yr2011<-ifelse(spei_data$Year==2011,1,0) # spei_data from both sites
spei_data$yr2012<-ifelse(spei_data$Year==2012,1,0) # spei_data from both sites
spei_data$yr2013<-ifelse(spei_data$Year==2013,1,0) # spei_data from both sites
spei_data$yr2014<-ifelse(spei_data$Year==2014,1,0) # spei_data from both sites
spei_data$yr2015<-ifelse(spei_data$Year==2015,1,0) # spei_data from both sites
spei_data$yr2016<-ifelse(spei_data$Year==2016,1,0) # spei_data only from Utq
spei_data$yr2017<-ifelse(spei_data$Year==2017,1,0) # spei_data only from Utq
spei_data$yr2018<-ifelse(spei_data$Year==2018,1,0) # spei_data only from Utq
spei_data$yr2019<-ifelse(spei_data$Year==2019,1,0) # spei_data from both sites
spei_data$yr2021<-ifelse(spei_data$Year==2021,1,0) # spei_data only from Kig

# Create indicator variable columns for site 
spei_data$Utq<-ifelse(spei_data$Site=="utq",1,0)
#data$Kig<-ifelse(data$Site=="Kig", 1,0) # Kigigak is the reference level, estimated by intercept

# Create Year*Site interaction indicator variables for years with data from both sites
#spei_data$Utq2010<-ifelse(spei_data$Utq==1 & spei_data$yr2010==1,1,0) # Utq in 2010 is captured by Bo(int)+B(Utq)+B(yr2010)
spei_data$Utq2011<-ifelse(spei_data$Utq==1 & spei_data$yr2011==1,1,0)
spei_data$Utq2012<-ifelse(spei_data$Utq==1 & spei_data$yr2012==1,1,0)
spei_data$Utq2013<-ifelse(spei_data$Utq==1 & spei_data$yr2013==1,1,0)
spei_data$Utq2014<-ifelse(spei_data$Utq==1 & spei_data$yr2014==1,1,0)
spei_data$Utq2015<-ifelse(spei_data$Utq==1 & spei_data$yr2015==1,1,0)
spei_data$Utq2019<-ifelse(spei_data$Utq==1 & spei_data$yr2019==1,1,0)

###Make year a factor variable
is.factor(spei_data$Year)
spei_data$Year<-as.factor(spei_data$Year)
is.factor(spei_data$Year) #it is now a factor variable

###Make Site a factor variable
is.factor(spei_data$Site)
spei_data$Site<-as.factor(spei_data$Site)
is.factor(spei_data$Site) #now a factor variable


##### NEST SURVIVAL #####
### From 1994-2021 the earliest julian start day is 134 and the latest is 209. 
### I am taking the difference between these two to get the nesting season for all years (nocc). equals 75
###TEMPORAL
run.models = function()
{
  # 1. constant daily survival rate model (null)
  S.dot = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.dot",
               model.parameters = list(S=list(formula = ~ 1)))
  
  # 2. site + year
  S.sy = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sy",
              model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                               yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                               yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                               yr2018 + yr2019 + yr2021 + Utq))) #yr2010 and Kig are reference levels captured by the int est))
  
  # 3. site + init
  S.si = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.si",
              model.parameters = list(S=list(formula = ~ Utq + Init)))
  
  # 4. site + nestage
  S.sna = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sna",
               model.parameters = list(S=list(formula = ~ Utq + NestAge)))
  
  # 5. site + init + nestage
  S.sina = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sina",
                model.parameters = list(S=list(formula = ~ Utq + Init + NestAge)))
  
  # 6. site * year
  S.sxy = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxy",
               model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                Utq2015 + Utq2019)),
               initial = S.sy)
  
  # 7. site + init + site * init
  S.sxi = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxi",
               model.parameters = list(S=list(formula = ~ Utq + Init + Utq*Init)))
  
  # 8. site + nestage + site * nestage
  S.sxna = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxna",
               model.parameters = list(S=list(formula = ~ Utq + NestAge + Utq*NestAge)))
  
  # 9. nestage + site * year
  S.nasxy = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.nasxy",
                 model.parameters = list(S=list(formula = ~ NestAge + yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                  yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                  yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                  yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                  Utq2015 + Utq2019)),
                 initial = S.sxy)
  
  # 10. init + site * year
  S.isxy = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.isxy",
                model.parameters = list(S=list(formula = ~ Init + yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                 yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                 yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                 yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                 Utq2015 + Utq2019)),
                initial = S.sxy)
  
  # 11. init + nestage + site * year
  S.inasxy = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.inasxy",
                  model.parameters = list(S=list(formula = ~ Init + NestAge + yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                   yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                   yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                   yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                   Utq2015 + Utq2019)),
                  initial = S.isxy)
  
  # Return model table and list of models
  
  return(collect.models())
}

# Fit models
model.results=run.models()
model.results


run.models=function()
{
  # 2. site + year
  S.sy = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sy",
              model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                               yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                               yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                               yr2018 + yr2019 + yr2021 + Utq)))
}

model.results=run.models()
model.results

best<-model.results$S.inasxy



ffcc <- find.covariates(best, spei_data)
ffcc
