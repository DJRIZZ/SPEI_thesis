###Indicator variable approach using initial values
###Nest Survival Models from Spectacled Eider Data
###Kigigak: 1994 - 2015, 2019, 2021
###Utqiagvik: 2010 - 2019
###Covariates: Init, NestAge, Year, Site (Kig, Utq), Win_Hi, Win_Lo, Spr_Hi, Spr_Lo, wxt, sxt, wxw, sxw 

###Load packages to run NS models
library(RMark)
library(tidyverse)
library(readr)
library(msm)

###Load the SPEI data INP file
spei_data<- read.csv("nest_survival/data/INP_20220921.csv", header = TRUE)

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
  
  # 3. site + year + init
  S.syi = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.syi",
              model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                               yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                               yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                               yr2018 + yr2019 + yr2021 + Utq + Init)),
              initial = S.sy)
  
  # 4. site + year + nestage
  S.syna = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.syna",
               model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                yr2018 + yr2019 + yr2021 + Utq + NestAge)),
               initial = S.sy)
  
  # 5. site + year + init + nestage
  S.syina = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.syina",
                model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                 yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                 yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                 yr2018 + yr2019 + yr2021 + Utq + Init + NestAge)),
                initial = S.syi)
  
  # 6. site + year + init + site * init
  S.syi.sxi = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.syi.sxi",
               model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                yr2018 + yr2019 + yr2021 + Utq + Init + Utq*Init)),
               initial = S.syi)
  
  # 7. site + year + nestage + site * nestage
  S.syna.sxna = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.syna.sxna",
                model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                 yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                 yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                 yr2018 + yr2019 + yr2021 + Utq + NestAge + Utq*NestAge)),
                initial = S.syna)
  
  # 8. site + year + init + nestage + site * init
  S.syina.sxi = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.syina.sxi",
                    model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                     yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                     yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                     yr2018 + yr2019 + yr2021 + Utq + Init + NestAge + Utq*Init)),
                    initial = S.syina)
  
  # 9. site + year + init + nestage + site * nestage
  S.syina.sxna = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.syina.sxna",
                      model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                       yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                       yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                       yr2018 + yr2019 + yr2021 + Utq + Init + NestAge + Utq*NestAge)),
                      initial = S.syina)
  
  # 10. site + year + init + nestage + site * init + site * nestage
  S.syina.sxi.sxna = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.syina.sxi.sxna",
                          model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                           yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                           yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                           yr2018 + yr2019 + yr2021 + Utq + Init + NestAge + Utq*Init + Utq*NestAge)),
                          initial = S.syina.sxi)
  
  # 11. site * year
  S.sxy = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxy",
               model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                Utq2015 + Utq2019)),
               initial = S.sy)
  
  # 12. site * year + init
  S.sxy.i = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxy.i",
                 model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                  yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                  yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                  yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                  Utq2015 + Utq2019 + Init)),
                 initial = S.sxy)
  
  # 13. site * year + nestage
  S.sxy.na = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxy.na",
                 model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                  yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                  yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                  yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                  Utq2015 + Utq2019 + NestAge)),
                 initial = S.sxy)
  
  # 14. site * year + site * init
  S.sxy.sxi = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxy.sxi",
                   model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                    yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                    yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                    yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                    Utq2015 + Utq2019 + Init + Utq*Init)),
                   initial = S.sxy.i)
  
  # 15. site * year + site * nestage
  S.sxy.sxna = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxy.sxna",
                   model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                    yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                    yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                    yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                    Utq2015 + Utq2019 + NestAge + Utq*NestAge)),
                   initial = S.sxy.na)
  
  # 16. site * year + init + nestge
  S.sxy.ina = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxy.ina",
                   model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                    yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                    yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                    yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                    Utq2015 + Utq2019 + Init + NestAge)),
                   initial = S.sxy.i)
  
  # 17. site * year + init + nestage + site * init
  S.sxy.ina.sxi = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxy.ina.sxi",
                   model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                    yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                    yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                    yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                    Utq2015 + Utq2019 + Init + NestAge + Utq*Init)),
                   initial = S.sxy.ina)
  
  # 18. site * year + init + nestage + site * nestage
  S.sxy.ina.sxna = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxy.ina.sxna",
                       model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                        yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                        yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                        yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                        Utq2015 + Utq2019 + Init + NestAge + Utq*NestAge)),
                       initial = S.sxy.ina)
  
  # 19. site * year + init + nestage + site*init + site*nestage
  S.sxy.ina.sxi.sxna = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.sxy.ina.sxi.sxna",
                            model.parameters = list(S=list(formula = ~ yr1994 + yr1995 + yr1996 + yr1997 + yr1998 + yr1999 + yr2000 +
                                                             yr2001 + yr2002 + yr2003 + yr2004 + yr2005 + yr2006 + yr2007 + yr2008 +
                                                             yr2009 + yr2011 + yr2012 + yr2013 + yr2014 + yr2015 + yr2016 + yr2017 +
                                                             yr2018 + yr2019 + yr2021 + Utq + Utq2011 + Utq2012 + Utq2013 + Utq2014 +
                                                             Utq2015 + Utq2019 + Init + NestAge + Utq*Init + Utq*NestAge)),
                            initial = S.sxy.ina.sxi)
  
  
  
  # Return model table and list of models
  
  return(collect.models())
}

# Fit models
model.results=run.models()

# Look at model results
model.results



##to look at specific model output
#write the model in the console
model.results$S.dot #1
model.results$S.sy #2
model.results$S.syi #3
model.results$S.syna #4
model.results$S.syina #5
model.results$S.syi.sxi #6
model.results$S.syna.sxna #7
model.results$S.syina.sxi #8
model.results$S.syina.sxna #9
model.results$S.syina.sxi.sxna #10
model.results$S.sxy #11
model.results$S.sxy.i #12
model.results$S.sxy.na #13
model.results$S.sxy.sxi #14
model.results$S.sxy.sxna #15
model.results$S.sxy.ina #16 top model w=0.42
model.results$S.sxy.ina.sxi #17
model.results$S.sxy.ina.sxna #18 2nd top model w=.20
model.results$S.sxy.ina.sxi.sxna #19

###All the above models ran correctly.



###Write the results output into excel (csv)
spei_temp_modeloutput1 <- model.results$model.table
write.csv(spei_temp_modeloutput1, "nest_survival/output/spei_temp_modeloutput1.csv")


################################################################################
###2nd stage of model fitting. Look 
###Winter Conditions

run.models2 = function()
{
  # 1. constant 
  S.dot = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.dot",
                     model.parameters = list(S=list(formula = ~ 1)))
  
  # 2. sxt + sxw + winhi + init + nestage + site
  S.stswwhins = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.stswwhins", groups = "Site",
                   model.parameters = list(S=list(formula = ~ sxt + sxw + Win_Hi + Init + NestAge + Site)))
  
  # 3. sxt + sxw + winhi + winhi2 + init + nestage + site
  S.stswwhwh2ins = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.stswwhwh2ins", groups = "Site",
                       model.parameters = list(S=list(formula = ~ sxt + sxw + Win_Hi + Win_Hi2 + Init + NestAge + Site)))
  
  # 4. ice.severity.index + init + nestage +site
  S.wsiins = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.wsiins", groups = "Site",
                 model.parameters = list(S=list(formula = ~ wsi + Init + NestAge + Site)))
  
  # 5. winhi + winhi2 + init + nestage + site
  S.whwh2ins = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.whwh2ins", groups = "Site",
                   model.parameters = list(S=list(formula = ~ Win_Hi + Win_Hi2 + Init + NestAge + Site)))
  
  # 6. winhi + init + nestage + site
  S.whins = mark(spei_data, nocc = 75, model = "Nest", model.name = "S.whins", groups = "Site",
                model.parameters = list(S=list(formula = ~ Win_Hi + Init + NestAge + Site)))
  
  
  
  
  # Return model table and list of models
  
  return(collect.models())
  
}

# Fit models
model.results2=run.models2()

# Look at model results
model.results2



##to look at specific model output
#write the model in the console
model.results2$S.dot
model.results2$S.stswwhins # 2nd top model w=.1
model.results2$S.stswwhwh2ins # top model w=0.899 (nestage beta CIs overlap)
model.results2$S.wsiins
model.results2$S.whwh2ins
model.results2$S.whins


###Write the results output into excel (csv)
spei_temp_modeloutput2 <- model.results2$model.table
write.csv(spei_temp_modeloutput2, "nest_survival/output/spei_temp_modeloutput2.csv")

###Add top model to the environment with simple name
topmodel <- model.results2$S.stswwhwh2ins

###Look at data statistics summary to see what the minimum ice values are.
summary(spei_data)
##Win_Hi: min=0   max=79
##Win_Hi2: min=0   max=6241
##sxt: min=0   max=7
##sxw: min=0   max=5



##Create a new dataframe of the actual winter values prior to making a graph
sfc <- find.covariates(topmodel, spei_data)

###assign 1:24 for nestages on both sites
sfc$value[c(741:764, 815:838)] <- 1:24

###assign high ice days range (0:79) to winter days
sfc$value[c(297:370, 371:444)] <- seq(0, 79, length = 74)

###create a design matrix with the values you just changed above
sdesign <- fill.covariates(topmodel, sfc)

###Obtain real estimates
winter.survival <- compute.real(topmodel, design = sdesign)

###Insert number of high ice days
winter.survival$high_ice_days[c(1:74, 75:148)] <- seq(0, 79, length = 74)

###Add site for group variable
winter.survival$site[1:74] <- ("kig")
winter.survival$site[75:148] <- ("utq")

###Change column names
colnames(winter.survival) <- c("dsr", "se", "lci", "uci","fixed", "high_ice_days", "site")

###Make site a factor variable
is.factor(winter.survival$site)
winter.survival$site<-as.factor(winter.survival$site)
is.factor(winter.survival$site) #now a factor variable


###Graph the best supported model for high ice days
library(ggplot2)

ggplot(winter.survival, aes(x = high_ice_days, y = dsr, color = site)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.13) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  xlab("High Ice Days") + ylab("Estimate DSR") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("high_ice.jpg",width = 8, height = 6, dpi = 220)


###Look at data statistics summary to see what the ice values are.
summary(spei_data)
##Win_Hi: min=0   max=79
##Win_Hi2: min=0   max=6241
##sxt: min=0   max=7
##sxw: min=0   max=5


w2fc <- find.covariates(topmodel, spei_data)
w2fc$value[c(741:764, 815:838)] <- 1:24

###assign high ice days range (0:79) to winter days
w2fc$value[c(445:518, 519:592)] <- seq(0, 6241, length = 74)

###create a design matrix with the values you just changed above
w2design <- fill.covariates(topmodel, w2fc)

###Obtain real estimates
w2.survival <- compute.real(topmodel, design = w2design)

###Insert number of high ice days
w2.survival$high_ice_days[c(1:74, 75:148)] <- seq(0, 79, length = 74)

###Add site for group variable
w2.survival$site[1:74] <- ("kig")
w2.survival$site[75:148] <- ("utq")

###Change column names
colnames(w2.survival) <- c("dsr", "se", "lci", "uci","fixed", "Win_Hi2", "site")

###Make site a factor variable
is.factor(w2.survival$site)
w2.survival$site<-as.factor(w2.survival$site)
is.factor(w2.survival$site) #now a factor variable

###Graph the best supported model for high ice days
library(ggplot2)

ggplot(w2.survival, aes(x = Win_Hi2, y = dsr, color = site)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.13) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  xlab("High Ice Days2") + ylab("Estimate DSR") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("high_ice2.jpg",width = 8, height = 6, dpi = 220)


w2fc <- find.covariates(topmodel, spei_data)
w2fc$value[c(741:764, 815:838)] <- 1:24



####Plot spring extreme temp and spring extreme wind together
#Summary
#sxt: min=0 max=7
#sxw: min=0 max=5

spfc <- find.covariates(topmodel, spei_data)
spfc$value[c(741:764, 815:838)] <- 1:24

###assign sxt values 0:7
spfc$value[c(1:74, 75:148)] <- seq(0, 7, length = 74)

###assign sxw values 0:5
spfc$value[c(149:222, 223:296)] <- seq(0, 5, length = 74)

###create a design matrix with the values you just changed above
spdesign <- fill.covariates(topmodel, spfc)

###Obtain real estimates
sp.survival <- compute.real(topmodel, design = spdesign)

###Insert number spring extremes
sp.survival$spring_extreme[c(1:74, 75:148)] <- seq(0, 7, length = 74)

###Add site for group variable
sp.survival$site[1:74] <- ("kig")
sp.survival$site[75:148] <- ("utq")

###Change column names
colnames(sp.survival) <- c("dsr", "se", "lci", "uci","fixed", "spring_extreme", "site")

###Make site a factor variable
is.factor(sp.survival$site)
sp.survival$site<-as.factor(sp.survival$site)
is.factor(sp.survival$site) #now a factor variable

ggplot(sp.survival, aes(x = spring_extreme, y = dsr, color = site)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.13) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  xlab("spring_extreme") + ylab("Estimate DSR") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("spring_extreme.jpg",width = 8, height = 6, dpi = 220)
