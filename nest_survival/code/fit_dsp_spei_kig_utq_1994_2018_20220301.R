# fit_dsp_spei_kig_utq_1994_2018_20220228.R

# Code fits daily nest Survival models for data from Spectacled Eiders at
# Kigigak Island: 1994 - 2015, and
# Utqiagvik: 2010 - 2018
# Covariates: Init, NestAge, Year, Site, Win_Hi, Win_Lo, Spr_Hi, Spr_Lo

# RStudio base dir: spei_cross_seasonal_kig_utq

# Load packages
library(RMark)
library(tidyverse)

# Load INP file
data<-read.csv("nest_survival/data/nest_ec_kig_utq_1994_2019.csv", header=T)
head(data)
table(data[ ,8:9])

# Create an indicator variable column for each parameter the model will estimate, rather than using the "group" option

# Create indicator variable columns for year, set 2010 as the reference level of year (i.e., intercept)
data$yr1994<-ifelse(data$Year==1994,1,0) # data only from Kig
data$yr1995<-ifelse(data$Year==1995,1,0) # data only from Kig
data$yr1996<-ifelse(data$Year==1996,1,0) # data only from Kig
data$yr1997<-ifelse(data$Year==1997,1,0) # data only from Kig
data$yr1998<-ifelse(data$Year==1998,1,0) # data only from Kig
data$yr1999<-ifelse(data$Year==1999,1,0) # data only from Kig
data$yr2000<-ifelse(data$Year==2000,1,0) # data only from Kig
data$yr2001<-ifelse(data$Year==2001,1,0) # data only from Kig
data$yr2002<-ifelse(data$Year==2002,1,0) # data only from Kig
data$yr2003<-ifelse(data$Year==2003,1,0) # data only from Kig
data$yr2004<-ifelse(data$Year==2004,1,0) # data only from Kig
data$yr2005<-ifelse(data$Year==2005,1,0) # data only from Kig
data$yr2006<-ifelse(data$Year==2006,1,0) # data only from Kig
data$yr2007<-ifelse(data$Year==2007,1,0) # data only from Kig
data$yr2008<-ifelse(data$Year==2008,1,0) # data only from Kig
data$yr2009<-ifelse(data$Year==2009,1,0) # data only from Kig
#data$yr2010<-ifelse(data$Year==2010,1,0) # Not included, used as the reference level (estimated by intercept parameter)
data$yr2011<-ifelse(data$Year==2011,1,0) # data from both sites
data$yr2012<-ifelse(data$Year==2012,1,0) # data from both sites
data$yr2013<-ifelse(data$Year==2013,1,0) # data from both sites
data$yr2014<-ifelse(data$Year==2014,1,0) # data from both sites
data$yr2015<-ifelse(data$Year==2015,1,0) # data from both sites
data$yr2016<-ifelse(data$Year==2016,1,0) # data only from Utq
data$yr2017<-ifelse(data$Year==2017,1,0) # data only from Utq
data$yr2018<-ifelse(data$Year==2018,1,0) # data from both sites

# Create indicator variable columns for site 
data$Utq<-ifelse(data$Site=="Utq", 1,0)
#data$Kig<-ifelse(data$Site=="Kig", 1,0) # Kigigak is the reference level, estimated by intercept

# Create Year*Site interaction indicator variables for years with data from both sites
#data$Utq2010<-ifelse(data$Utq==1 & data$yr2010==1,1,0) # Utq in 2010 is captured by Bo(int)+B(Utq)+B(yr2010)
data$Utq2011<-ifelse(data$Utq==1 & data$yr2011==1,1,0)
data$Utq2012<-ifelse(data$Utq==1 & data$yr2012==1,1,0)
data$Utq2013<-ifelse(data$Utq==1 & data$yr2013==1,1,0)
data$Utq2014<-ifelse(data$Utq==1 & data$yr2014==1,1,0)
data$Utq2015<-ifelse(data$Utq==1 & data$yr2015==1,1,0)

str(data)
summary(data)

# Fit nest daily survival models with Program MARK
# From 1994-2018 the earliest julian start day is 134 and the latest is 209. 
# I am taking the difference between these two to get the nesting season for all years (nocc). equals 75

# set dir for output
setwd("nest_survival/output/")

# 1. site
S.s = mark(data, model="Nest", nocc=75,
              model.parameters=list(S=list(formula=~
                                             Utq
              )))
summary(S.s)
S.s$results$AICc
S.s$results$npar

# 2. year 
S.y = mark(data, model="Nest", nocc=75,
              model.parameters=list(S=list(formula=~
                                             yr1994+
                                             yr1995+
                                             yr1996+
                                             yr1997+
                                             yr1998+
                                             yr1999+
                                             yr2000+
                                             yr2001+
                                             yr2002+
                                             yr2003+
                                             yr2004+
                                             yr2005+
                                             yr2006+
                                             yr2007+
                                             yr2008+
                                             yr2009+
                                             yr2011+
                                             yr2012+
                                             yr2013+
                                             yr2014+
                                             yr2015+
                                             yr2016+
                                             yr2017+
                                             yr2018
              )))
S.y$results$AICc
S.y$results$npar

# 3. site and year additive model
S.sy = mark(data, model="Nest", nocc=75, 
                   model.parameters=list(S=list(formula=~
                                                  yr1994+
                                                  yr1995+
                                                  yr1996+
                                                  yr1997+
                                                  yr1998+
                                                  yr1999+
                                                  yr2000+
                                                  yr2001+
                                                  yr2002+
                                                  yr2003+
                                                  yr2004+
                                                  yr2005+
                                                  yr2006+
                                                  yr2007+
                                                  yr2008+
                                                  yr2009+
                                                  yr2011+
                                                  yr2012+
                                                  yr2013+
                                                  yr2014+
                                                  yr2015+
                                                  yr2016+
                                                  yr2017+
                                                  yr2018+
                                                  Utq
                   )))
S.sy$results$AICc
S.sy$results$npar

# 4. site and year interaction model
S.sXy = mark(data, model="Nest", nocc=75,initial=spei.results.SxY$results$beta$estimate, # use initial values from group model fit below
                   model.parameters=list(S=list(formula=~
                                                  yr1994+
                                                  yr1995+
                                                  yr1996+
                                                  yr1997+
                                                  yr1998+
                                                  yr1999+
                                                  yr2000+
                                                  yr2001+
                                                  yr2002+
                                                  yr2003+
                                                  yr2004+
                                                  yr2005+
                                                  yr2006+
                                                  yr2007+
                                                  yr2008+
                                                  yr2009+
                                                  yr2011+
                                                  yr2012+
                                                  yr2013+
                                                  yr2014+
                                                  yr2015+
                                                  yr2016+
                                                  yr2017+
                                                  yr2018+
                                                  Utq+
                                                  Utq2011+
                                                  Utq2012+
                                                  Utq2013+
                                                  Utq2014+
                                                  Utq2015
                   )))
S.sXy$results$AICc # fails to converge 
S.sXy$results$npar

# Try the using group for site and year for comparison with above indicator variable approach
spei.pr <- process.data(data,
                        nocc = 75,
                        model = "Nest",
                        groups = c("Year", "Site")) # I think specifying bith Year and Site as groups solved the problem

# 1. site model
run.spei <- function()
{
  S.S = list(formula = ~Site)
  
  spei.model.list = create.model.list("Nest")
  
  spei.results.S = mark.wrapper(spei.model.list,
                                data = spei.pr,
                                adjust = FALSE)
}

# Run
spei.results.S <- run.spei()

# Look at model results
model.table.S<-spei.results.S
model.table.S

# 2. year model
run.spei <- function()
{
  S.Y = list(formula = ~Year)
  
  spei.model.list = create.model.list("Nest")
  
  spei.results.Y = mark.wrapper(spei.model.list,
                                   data = spei.pr,
                                   adjust = FALSE)
}

# Run
spei.results.Y <- run.spei()

# Look at model results
model.table.Y<-spei.results.Y
model.table.Y


# 3. site and year model additive model
run.spei <- function()
{
S.SY = list(formula = ~Site + Year)

spei.model.list = create.model.list("Nest")

spei.results.SY = mark.wrapper(spei.model.list,
                            data = spei.pr,
                            adjust = FALSE)
}

# Run the models
spei.results.SY <- run.spei()

# Look at model results
model.table.SY<-spei.results.SY
model.table.SY


# 4. site and year model interaction model
run.spei <- function()
{
S.SxY = list(formula = ~Site:Year)

spei.model.list = create.model.list("Nest")

spei.results.SxY = mark.wrapper(spei.model.list,
                            data = spei.pr,
                            adjust = FALSE)
}

# Run the models
spei.results.SxY <- run.spei()

# Look at model results
model.table.SxY<-spei.results.SxY
model.table.SxY

# 5. site and year model interaction model no intercept
run.spei <- function()
{
S.SxYnoInt = list(formula = ~-1+Site:Year)
spei.model.list = create.model.list("Nest")

spei.results.SxYnoInt = mark.wrapper(spei.model.list,
                            data = spei.pr,
                            adjust = FALSE)
}
# Run the models
spei.results.SxYnoInt <- run.spei()

# Look at model results
model.table.SxYnoInt<-spei.results.SxYnoInt
model.table.SxYnoInt

