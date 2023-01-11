###Nest Survival
###Run the supported models from previous surival/nestsuccess analysis from
###previous work by Christe et al 2018, Flint et al 2016, Dunham et al 2021,
###and Peterson and Douglas 2004
###Kigigak: 1994-2015, 2019, and 2021

###Load Packages
library(RMark)
library(tidyverse)
library(data.table)
library(readr)

###Load the Kig kig data
kig_data<-read.csv("nest_survival/data/kig_inp_20220524.csv", header = TRUE)

###Check the headers. Sometimes column name changes when reading csv files. 
head(kig_data)
summary(kig_data)

###Make year a factor variable
is.factor(kig_data$Year)
kig_data$Year<-as.factor(kig_data$Year)
is.factor(kig_data$Year) #it is now a factor variable

###Make Site a factor variable
is.factor(kig_data$Site)
kig_data$Site<-as.factor(kig_data$Site)
is.factor(kig_data$Site) #now a factor variable

###Square the sea ice variables. (Quadratic term and effect). This is adding another column.
kig_data$Win_Hi2<-kig_data$Win_Hi^2
kig_data$Win_Lo2<-kig_data$Win_Lo^2
kig_data$Spr_Hi2<-kig_data$Spr_Hi^2
kig_data$Spr_Lo2<-kig_data$Spr_Lo^2
head(kig_data) #these two columns are now in the dataframe

kig_data<-kig_data[,-1]### remove the first "X" column
head(kig_data)

##### NEST SURVIVAL #####
### From 1994-2019 the earliest julian start day is 134 and the latest is 209. 
### I am taking the difference between these two to get the nesting season for all years (nocc). equals 75
kig.pr <- process.data(kig_data,
                        nocc = 75,
                        model = "Nest",
                        groups = c("Year","Site"))

###Write a function for your models to run
run.kig <- function()
{
  # 1. Flint model
  S.icedays = list(formula = ~ Win_Hi)
  
  # 2. Christie and Dunham model
  S.icedays2 = list(formula = ~ Win_Hi + Win_Hi2)
  
  # 3. Peterson and Douglas model 1
  S.stemp.swind = list(formula = ~ sxt + sxw)
  
  # 4. Peterson and Duglas model 2
  S.wice.swind.stemp = list(formula = ~ Win_Hi + sxt + sxw)
  
  # Return model table and list of models
  
  kig.model.list = create.model.list("Nest")
  
  kig.results = mark.wrapper(kig.model.list,
                              data = kig.pr,
                              adjust = FALSE)
}

###Run the models above
kig.results <- run.kig()

###Look at model results
kig.results

###model output results from winhi2 term
kig.results$S.icedays
kig.results$S.icedays2  #top supported model
kig.results$S.stemp.swind
kig.results$S.wice.swind.stemp #2nd top supported model


###Graph
icedays2<-kig.results$S.icedays2
ifc<-find.covariates(icedays2,kig_data)
ifc$value <- 0:79 #min win_hi value is 0, max win_hi value is 79
hi <- seq(0,79,1)
hi2 <- hi^2
beta <- rep(1, 80)
new <- cbind.data.frame(beta,hi,hi2)
new <- as.matrix(new)

hi.survival <- compute.real(icedays2, design = new)
hiplotdata <- cbind.data.frame(new, hi.survival)

ggplot(hiplotdata, aes(x = hi, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.3) +
  ggtitle("DSR with High Sea Ice") +
  xlab("Number of days with High Ice") +
  ylab("Estimated DSR") +
  theme_bw()

ggsave("high_ice_kig_20220531.jpg", width = 6, height = 4, dpi = 220)
