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
#View(spei_data)

###Check the headers. Sometimes column name changes when reading csv files. 
head(spei_data)
summary(spei_data)

###Rename first column. sometimes when you load data column name changes
#names(spei_data)[names(spei_data) == "ï..Nest"] <- "Nest"

###Remove first column. might have extra column 
#spei_data<-spei_data[,-1]

###Check colomn names again ###
head(spei_data) #Everything looks good

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

###Center the Initiation date
spei_data$cinit<-spei_data$Init-mean(spei_data$Init)

###How many nests are there in Kigigak and Utqiagvik
library(dplyr)
spei_data %>% count(Site)
#Kigigak has 2705
#Utqiagvik has 165
#2705 + 165 = 2870, good

##### NEST SURVIVAL #####
### From 1994-2021 the earliest julian start day is 134 and the latest is 209. 
### I am taking the difference between these two to get the nesting season for all years (nocc). equals 75
###TEMPORAL
spei.pr <- process.data(spei_data,
                        nocc = 75,
                        model = "Nest",
                        groups = c("Year","Site"))


###Write a function for your models to run
run.spei <- function()
{
  # 1. Intercept only model (null)
  S.dot = list(formula = ~ 1)
  
  # 2. site and year model
  S.site.year = list(formula = ~ Site + Year)
  
  # 3. site and init model
  S.site.init = list(formula = ~ Site + cinit)
  
  # 4. site and nestage model
  S.site.nestage = list(formula = ~ Site + NestAge)
  
  # 5. site and init and nestage model
  S.site.init.nestage = list(formula = ~ Site + cinit + NestAge)
  
  # 6. site and year interaction, site-specific slopes
  S.siteXyear = list(formula = ~ -1 + Site:Year)
  
  # 7. site and init interaction
  S.siteXinit = list(formula = ~ Site + Init + Site*cinit)
  
  # 8. site and nestage interaction
  S.siteXnestage = list(formula = ~ Site + NestAge + Site*NestAge)
  
  # 9. nestage and site*year interaction, site-specific slopes
  S.age.siteXyear = list(formula = ~ -1 + NestAge + Site:Year)
  
  # 10. Init and site year interaction, site-specific slopes
  S.init.siteXyear = list(formula = ~ -1 + cinit + Site:Year)
  
  # 11. site and init and site:year, site-specific slopes
  S.ageinit.siteXyear = list(formula = ~ -1 + cinit + NestAge + Site:Year)
  
  
  # Return model table and list of models
  
  spei.model.list = create.model.list("Nest")
  
  spei.results = mark.wrapper(spei.model.list,
                              data = spei.pr,
                              adjust = FALSE)
  
}


###Run the models above
spei.results <- run.spei()

###Look at model results
spei.results 


##to look at specific model output
#write the model in the console
spei.results$S.Dot # 1
spei.results$S.site.year #2
spei.results$S.site.init #3
spei.results$S.site.nestage #4
spei.results$S.site.init.nestage #5
spei.results$S.siteXyear #6 
spei.results$S.siteXinit #7
spei.results$S.siteXnestage #8
spei.results$S.age.siteXyear #9
spei.results$S.init.siteXyear #10
spei.results$S.ageinit.siteXyear #11 top model



###Top supported model
top_temp_model <- spei.results$S.ageinit.siteXyear


###Write the results output into excel (csv)
spei_temp_modeloutput1 <- spei.results$model.table
write.csv(spei_temp_modeloutput1, "figures_tables/spei_temp_modeloutput1.csv")

################################################################################

###Winter Conditions
spei.pr2 <- process.data(spei_data,
                         nocc = 75,
                         model = "Nest",
                         groups = c("Year","Site"))

###Write a function for your models to run
run.spei2 <- function()
{
  # 1. site and winterHi model
  S.site.winhi = list(formula = ~Site + Win_Hi)
  
  # 2. site and winterLo model
  S.site.winlo = list(formula = ~Site + Win_Lo)
  
  # 3. site and springHi model
  S.site.sprhi = list(formula = ~Site + Spr_Hi)
  
  # 4. site and springLo model
  S.site.sprlo = list(formula = ~Site + Spr_Lo)
  
  # 5. site and winterHi2 model
  S.site.winhi2 = list(formula = ~Site + Win_Hi + Win_Hi2)
  
  # 6. site and winterLo2 model
  S.site.winlo2 = list(formula = ~Site + Win_Lo + Win_Lo2)
  
  # 7. site and springHi2 model
  S.site.sprhi2 = list(formula = ~Site + Spr_Hi + Spr_Hi2)
  
  # 8. site and springLo2 model
  S.site.sprlo2 = list(formula = ~Site + Spr_Lo + Spr_Lo2)
  
  # 9. site and winterHi interaction model
  S.siteXwinhi = list(formula = ~Site + Win_Hi + Site*Win_Hi)
  
  # 10. site and winterHi2 interaction model
  S.siteXwinhi2 = list(formula = ~Site + Win_Hi + Win_Hi2 + Site*Win_Hi + Site*Win_Hi2)
  
  # 11. site and winterLo interaction model
  S.siteXwinlo = list(formula = ~Site + Win_Lo + Site*Win_Lo)
  
  # 12. site and winterLo2 interaction model
  S.siteXwinlo2 = list(formula = ~Site + Win_Lo + Win_Lo2 + Site*Win_Lo + Site*Win_Lo2)
  
  # 13. site and springHi interaction model 
  S.siteXsprhi = list(formula = ~Site + Spr_Hi + Site*Spr_Hi)
  
  # 14. site and springHi2 interaction model 
  S.siteXsprhi2 = list(formula = ~Site + Spr_Hi + Spr_Hi2 + Site*Spr_Hi + Site*Spr_Hi2)
  
  # 15. site and springLo interaction model
  S.siteXsprlo = list(formula = ~Site + Spr_Lo + Site*Spr_Lo)
  
  # 16. site and springLo2 interaction model
  S.siteXsprlo2 = list(formula = ~Site + Spr_Lo + Spr_Lo2 + Site*Spr_Lo + Site*Spr_Lo2)
  
  # 17. site and wxt model
  S.site.wxt = list(formula = ~Site + wxt)
  
  # 18. site and wxw model
  S.site.wxw = list(formula = ~Site + wxw)
  
  # 19. site and wxt and wxw model
  S.site.wxt.wxw = list(formula = ~Site + wxt + wxw)
  
  # 20. site and wxt interaction model
  S.siteXwxt = list(formula = ~Site + wxt + Site*wxt)
  
  # 21. site and wxw interaction model
  S.siteXwxw = list(formula = ~Site + wxw + Site*wxw)
  
  # 22. site and wxt and wxw interaction model
  S.siteXwxtXwxw = list(formula = ~Site + wxt + wxw + Site*wxt + Site*wxw)
  
  # 23. site and sxt model
  S.site.sxt = list(formula = ~Site + sxt)
  
  # 24. site and wxw model
  S.site.sxw = list(formula = ~Site + sxw)
  
  # 25. site and wxt and wxw model
  S.site.sxt.sxw = list(formula = ~Site + sxt + sxw)
  
  # 26. site and wxt interaction model
  S.siteXsxt = list(formula = ~Site + sxt + Site*sxt)
  
  # 27. site and wxw interaction model
  S.siteXsxw = list(formula = ~Site + sxw + Site*sxw)
  
  # 28. site and wxt and wxw interaction model
  S.siteXsxtXsxw = list(formula = ~Site + sxt + sxw + Site*sxt + Site*sxw)
  
  # 29. site + winhi + winhi2 + wxt + wxw
  S.site.whi.w = list(formula = ~Site + Win_Hi + Win_Hi2 + wxt + wxw)
  
  # 30. site + winhi + winhi2 + sxt + sxw
  S.site.whi.s = list(formula = ~Site + Win_Hi + Win_Hi2 + sxt + sxw)
  
  # 31. site + winlo + winlo2 + wxt + wxw
  S.site.shi.w = list(formula = ~Site + Win_Lo + Win_Lo2 + wxt + wxw)
  
  # 32. site + winlo + winlo2 + sxt + sxw
  S.site.slo.s = list(formula = ~Site + Win_Lo + Win_Lo2 + sxt + sxw)
  
  # 33. 
  S.init.na.lo = list(formula = ~ -1 + cinit + NestAge + Site:Year + Site:Win_Lo + Site:Win_Lo2)
  
  
  # Return model table and list of models
  
  spei.model.list2 = create.model.list("Nest")
  
  spei.results2 = mark.wrapper(spei.model.list2,
                               data = spei.pr2,
                               adjust = FALSE)
  
}

###Run the models above
spei.results2 <- run.spei2()

###Look at model results2
spei.results2

###Look at model results
spei.results

###Write the results output into excel (csv)
spei_winter_modeloutput1 <- spei.results2$model.table
write.csv(spei_winter_modeloutput1, "figures_tables/spei_winter_modeloutput1.csv")

##look at model results
spei.results2$S.init.na.lo



###Top temporal model
top_temp_model

###Top winter model
top_winter_model <- spei.results2$S.siteXwinlo2
top_winter_model

#########################################################################################

###Compete top model from temporal and ice condition variables
spei.pr3 <- process.data(spei_data,
                         nocc = 75,
                         model = "Nest",
                         groups = c("Year","Site"))

###Write a function for your models to run
run.spei3 <- function()
{
  # 1. age + init + site*year interaction
  S.ageinit.siteXyear.2 = list(formula = ~ -1 + cinit + NestAge + Site:Year)
  
  # 2. site and winterLo2 interaction model
  S.siteXwinlo2.2 = list(formula = ~Site + Win_Lo + Win_Lo2 + Site*Win_Lo + Site*Win_Lo2)
  
  # Return model table and list of models
  
  spei.model.list3 = create.model.list("Nest")
  
  spei.results3 = mark.wrapper(spei.model.list3,
                               data = spei.pr3,
                               adjust = FALSE)
  
}

###Run the models above
spei.results3 <- run.spei3()

###Look at model results3
spei.results3

###Write the results output into excel (csv)
spei_top2_modeloutput <- spei.results3$model.table
write.csv(spei_top2_modeloutput, "figures_tables/spei_top2_modeloutput.csv")


###best supported model from competing top 2
###S.ageinit.siteXyear.2 = list(formula = ~ -1 + NestAge + Init + Site:Year)
final_top_model <- spei.results3$S.ageinit.siteXyear.2
final_top_model

####Make a graph for the best supported model############################# Not yet..
###real.estimates <- spei.results3$S.ageinit.siteXyear.2$results$real
#write.csv(real.estimates, "nest_survival/output/real.estimates.csv") 


###Build a design matrix for nest ages 1:24, which is the incubation period until hatch for Specs
###1:24 are ages for specs. the following code assign's ages 1:24 for nest age for each year/site
fc <- find.covariates(final_top_model, spei_data)
fc$value[2517:2540] <- 1:24 #1994, assign nest ages 1:24 for mean covariate values of nest ages 1:24
fc$value[2591:2614] <- 1:24 #1995, we start at a huge number but cinit was first computed
fc$value[2665:2688] <- 1:24 #1996
fc$value[2739:2762] <- 1:24 #1997
fc$value[2813:2836] <- 1:24 #1998
fc$value[2887:2910] <- 1:24 #1999
fc$value[2961:2984] <- 1:24 #2000
fc$value[3035:3058] <- 1:24 #2001
fc$value[3109:3132] <- 1:24 #2002
fc$value[3183:3206] <- 1:24 #2003
fc$value[3257:3280] <- 1:24 #2004
fc$value[3331:3354] <- 1:24 #2005
fc$value[3405:3428] <- 1:24 #2006
fc$value[3479:3502] <- 1:24 #2007
fc$value[3553:3576] <- 1:24 #2008
fc$value[3627:3650] <- 1:24 #2009
fc$value[3701:3724] <- 1:24 #2010
fc$value[3775:3798] <- 1:24 #2011
fc$value[3849:3872] <- 1:24 #2012
fc$value[3923:3946] <- 1:24 #2013
fc$value[3997:4020] <- 1:24 #2014
fc$value[4071:4094] <- 1:24 #2015
fc$value[4145:4168] <- 1:24 #2019
fc$value[4219:4242] <- 1:24 #2021
fc$value[4293:4316] <- 1:24 #u2010 the 'u-' stands for site "utq"
fc$value[4367:4390] <- 1:24 #u2011
fc$value[4441:4464] <- 1:24 #u2012
fc$value[4515:4538] <- 1:24 #u2013
fc$value[4589:4612] <- 1:24 #u2014
fc$value[4663:4686] <- 1:24 #u2015
fc$value[4737:4760] <- 1:24 #u2016
fc$value[4811:4834] <- 1:24 #u2017
fc$value[4885:4908] <- 1:24 #u2018
fc$value[4959:4982] <- 1:24 #u2019

fc$value[fc$var == "cinit"] <- 0 #centered this mean covariate value, I guess here we can make 
#it zero since its centered 0 at its mean.

###fill design matrix with values  of interest that we just changed above.
design <- fill.covariates(final_top_model, fc)

###extract the 1st 24 nest ages for each year from design matrix
full.survival <- compute.real(final_top_model, design = design, vcv = TRUE)
real<-full.survival$real
vcv<-full.survival$vcv.real



###the function deltamethod.special computes delta-method std errors.
###After you run the function you will get sqrt of the variance.
###to get variance you just square that value
library(msm)
s <- deltamethod.special("prod",full.survival$real[1:3],full.survival$vcv.real[1:3,1:3])
##the above code gave s. to get variance you s^2. this was used to match if this results is the same as doing it by hand
###these codes below will now get the s.
s1994 <- deltamethod.special("prod",full.survival$real[1:24],full.survival$vcv.real[1:24,1:24])
s1995 <- deltamethod.special("prod",full.survival$real[75:98],full.survival$vcv.real[75:98,75:98])                                                                                    
s1996 <- deltamethod.special("prod",full.survival$real[149:172],full.survival$vcv.real[149:172,149:172])
s1997 <- deltamethod.special("prod",full.survival$real[223:246],full.survival$vcv.real[223:246,223:246])
s1998 <- deltamethod.special("prod",full.survival$real[297:320],full.survival$vcv.real[297:320,297:320])
s1999 <- deltamethod.special("prod",full.survival$real[371:394],full.survival$vcv.real[371:394,371:394])
s2000 <- deltamethod.special("prod",full.survival$real[445:468],full.survival$vcv.real[445:468,445:468])
s2001 <- deltamethod.special("prod",full.survival$real[519:542],full.survival$vcv.real[519:542,519:542])
s2002 <- deltamethod.special("prod",full.survival$real[593:616],full.survival$vcv.real[593:616,593:616])
s2003 <- deltamethod.special("prod",full.survival$real[667:690],full.survival$vcv.real[667:690,667:690])
s2004 <- deltamethod.special("prod",full.survival$real[741:764],full.survival$vcv.real[741:764,741:764])
s2005 <- deltamethod.special("prod",full.survival$real[815:838],full.survival$vcv.real[815:838,815:838])
s2006 <- deltamethod.special("prod",full.survival$real[889:912],full.survival$vcv.real[889:912,889:912])
s2007 <- deltamethod.special("prod",full.survival$real[963:986],full.survival$vcv.real[963:986,963:986])
s2008 <- deltamethod.special("prod",full.survival$real[1037:1060],full.survival$vcv.real[1037:1060,1037:1060])
s2009 <- deltamethod.special("prod",full.survival$real[1111:1134],full.survival$vcv.real[1111:1134,1111:1134])
s2010 <- deltamethod.special("prod",full.survival$real[1185:1208],full.survival$vcv.real[1185:1208,1185:1208])
s2011 <- deltamethod.special("prod",full.survival$real[1259:1282],full.survival$vcv.real[1259:1282,1259:1282])
s2012 <- deltamethod.special("prod",full.survival$real[1333:1356],full.survival$vcv.real[1333:1356,1333:1356])
s2013 <- deltamethod.special("prod",full.survival$real[1407:1430],full.survival$vcv.real[1407:1430,1407:1430])
s2014 <- deltamethod.special("prod",full.survival$real[1481:1504],full.survival$vcv.real[1481:1504,1481:1504])
s2015 <- deltamethod.special("prod",full.survival$real[1555:1578],full.survival$vcv.real[1555:1578,1555:1578])
s2019 <- deltamethod.special("prod",full.survival$real[1629:1652],full.survival$vcv.real[1629:1652,1629:1652])
s2021 <- deltamethod.special("prod",full.survival$real[1703:1726],full.survival$vcv.real[1703:1726,1703:1726])
s2010u <- deltamethod.special("prod",full.survival$real[1777:1800],full.survival$vcv.real[1777:1800,1777:1800])
s2011u <- deltamethod.special("prod",full.survival$real[1851:1874],full.survival$vcv.real[1851:1874,1851:1874])
s2012u <- deltamethod.special("prod",full.survival$real[1925:1948],full.survival$vcv.real[1925:1948,1925:1948])
s2013u <- deltamethod.special("prod",full.survival$real[1999:2022],full.survival$vcv.real[1999:2022,1999:2022])
s2014u <- deltamethod.special("prod",full.survival$real[2073:2096],full.survival$vcv.real[2073:2096,2073:2096])
s2015u <- deltamethod.special("prod",full.survival$real[2147:2170],full.survival$vcv.real[2147:2170,2147:2170])
s2016u <- deltamethod.special("prod",full.survival$real[2221:2244],full.survival$vcv.real[2221:2244,2221:2244])
s2017u <- deltamethod.special("prod",full.survival$real[2295:2318],full.survival$vcv.real[2295:2318,2295:2318])
s2018u <- deltamethod.special("prod",full.survival$real[2369:2392],full.survival$vcv.real[2369:2392,2369:2392])
s2019u <- deltamethod.special("prod",full.survival$real[2443:2466],full.survival$vcv.real[2443:2466,2443:2466])

###Obtain the the dsr estimates for nest ages 1:24
survival.24 <- compute.real(final_top_model, design = design)[c(1:24,
                                                                       75:98,
                                                                       149:172,
                                                                       223:246,
                                                                       297:320,
                                                                       371:394,
                                                                       445:468,
                                                                       519:542,
                                                                       593:616,
                                                                       667:690,
                                                                       741:764,
                                                                       815:838,
                                                                       889:912,
                                                                       963:986,
                                                                       1037:1060,
                                                                       1111:1134,
                                                                       1185:1208,
                                                                       1259:1282,
                                                                       1333:1356,
                                                                       1407:1430,
                                                                       1481:1504,
                                                                       1555:1578,
                                                                       1629:1652,
                                                                       1703:1726,
                                                                       1777:1800,
                                                                       1851:1874,
                                                                       1925:1948,
                                                                       1999:2022,
                                                                       2073:2096,
                                                                       2147:2170,
                                                                       2221:2244,
                                                                       2295:2318,
                                                                       2369:2392,
                                                                       2443:2466), ]

###Get the product of the 24 nest ages to get cumulative nest success probability
###There should be 34 of this. 24 for kigigak island, 10 for Utqiagvik.
product1994 <- prod(survival.24$estimate[1:24]) #1
product1995 <- prod(survival.24$estimate[25:48]) #2
product1996 <- prod(survival.24$estimate[49:72]) #3
product1997 <- prod(survival.24$estimate[73:96]) #4
product1998 <- prod(survival.24$estimate[97:120]) #5
product1999 <- prod(survival.24$estimate[121:144]) #6
product2000 <- prod(survival.24$estimate[145:168]) #7
product2001 <- prod(survival.24$estimate[169:192]) #8
product2002 <- prod(survival.24$estimate[193:216]) #9
product2003 <- prod(survival.24$estimate[217:240]) #10
product2004 <- prod(survival.24$estimate[241:264]) #11
product2005 <- prod(survival.24$estimate[265:288]) #12
product2006 <- prod(survival.24$estimate[289:312]) #13
product2007 <- prod(survival.24$estimate[313:336]) #14
product2008 <- prod(survival.24$estimate[337:360]) #15
product2009 <- prod(survival.24$estimate[361:384]) #16
product2010 <- prod(survival.24$estimate[385:408]) #17
product2011 <- prod(survival.24$estimate[409:432]) #18
product2012 <- prod(survival.24$estimate[433:456]) #19
product2013 <- prod(survival.24$estimate[457:480]) #20
product2014 <- prod(survival.24$estimate[481:504]) #21
product2015 <- prod(survival.24$estimate[505:528]) #22
product2019 <- prod(survival.24$estimate[529:552]) #23
product2021 <- prod(survival.24$estimate[553:576]) #24
product2010u <- prod(survival.24$estimate[577:600]) #25
product2011u <- prod(survival.24$estimate[601:624]) #26
product2012u <- prod(survival.24$estimate[625:648]) #27
product2013u <- prod(survival.24$estimate[649:672]) #28
product2014u <- prod(survival.24$estimate[673:696]) #29
product2015u <- prod(survival.24$estimate[697:720]) #30
product2016u <- prod(survival.24$estimate[721:744]) #31
product2017u <- prod(survival.24$estimate[745:768]) #32
product2018u <- prod(survival.24$estimate[769:792]) #33
product2019u <- prod(survival.24$estimate[793:816]) #34

###create a dataframe of the products of the 24 nest ages above
library(data.table)
probsuccess <- c(product1994,product1995,product1996,product1997,product1998,product1999,product2000,
             product2001,product2002,product2003,product2004,product2005,product2006,product2007,
             product2008,product2009,product2010,product2011,product2012,product2013,product2014,
             product2015,product2019,product2021,product2010u,product2011u,product2012u,product2013u,
             product2014u,product2015u,product2016u,product2017u,product2018u,product2019u)
probsuccess <- as.data.frame(probsuccess)


###create data frame for the square-root of the variance for each year and site
sqrt_s <- c(s1994,s1995,s1996,s1997,s1998,s1999,s2000,s2001,
            s2002,s2003,s2004,s2005,s2006,s2007,s2008,s2009,s2010,
            s2011,s2012,s2013,s2014,s2015,s2019,s2021,s2010u,s2011u,
            s2012u,s2013u,s2014u,s2015u,s2016u,s2017u,s2018u,s2019u)
sqrt_s <- as.data.frame(sqrt_s)

###combine probsuccess and sqrt_s to get a dataframe where you can work with to get lower and upper CI
data <- cbind(probsuccess,sqrt_s)
data$lci <- data$probsuccess - 1.96*(data$sqrt_s/sqrt(24))
data$uci <- data$probsuccess + 1.96*(data$sqrt_s/sqrt(24))


####YOU CAN FINALLY PLOT THIS DAMN FIGURE####

###Add site and year to data frame
data$site <- c("kig","kig","kig","kig","kig","kig",
          "kig","kig","kig","kig","kig","kig",
          "kig","kig","kig","kig","kig","kig",
          "kig","kig","kig","kig","kig","kig",
          "utq","utq","utq","utq","utq",
          "utq","utq","utq","utq","utq")

data$year <- c("1994","1995","1996","1997","1998","1999",
          "2000","2001","2002","2003","2004","2005",
          "2006","2007","2008","2009","2010","2011",
          "2012","2013","2014","2015","2019","2021",
          "2010","2011","2012","2013","2014",
          "2015","2016","2017","2018","2019")


data$probsuccess<-as.numeric(data$probsuccess)
###plot
ggplot(data, aes(x = year, y = probsuccess, color = site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci, width = 0.4)) +
  geom_point() +
  geom_hline(yintercept = mean(data$probsuccess[1:24]), color = "red", lty = "dashed") +
  geom_hline(yintercept = mean(data$probsuccess[25:34]), color = "blue", lty = "dashed") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  xlab("Year") + ylab("cumuluative nest succes (24-day)") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("nest_success_prob.jpg",width = 8, height = 4, dpi = 220)


###Average nest success probability between sites
kignestsuccess <- mean(data$probsuccess[1:24]) ##0.674130129
utqnestsuccess <- mean(data$probsuccess[25:34]) ##0.7461755994











#write.csv(survival.24, "nest_survival/output/survival.24.csv")
###Add year and site variables to top model survival estimates
#topmodel.survival$number <- 1:816  #create a column for row numbers for reference
#topmodel.survival$site <- ifelse(topmodel.survival$number < 577, "kig",
                                 ifelse(topmodel.survival$number > 576, "utq",0))

#topmodel.survival$year <- ifelse(topmodel.survival$number == 1:24,1994,
                                 ifelse(topmodel.survival$number == 25:48,1995,
                                        ifelse(topmodel.survival$number == 49:72,1996,
                                               ifelse(topmodel.survival$number == 73:96,1997,
                                                      ifelse(topmodel.survival$number == 97:120,1998,
                                                             ifelse(topmodel.survival$number == 121:144,1999,
                                                                    ifelse(topmodel.survival$number == 145:168,2000,
                                                                           ifelse(topmodel.survival$number == 169:192,2001,
                                                                                  ifelse(topmodel.survival$number == 193:216,2002,
                                                                                         ifelse(topmodel.survival$number == 217:240,2003,
                                                                                                ifelse(topmodel.survival$number == 241:264,2004,
                                                                                                       ifelse(topmodel.survival$number == 265:288,2005,
                                                                                                              ifelse(topmodel.survival$number == 289:312,2006,
                                                                                                                     ifelse(topmodel.survival$number == 313:336,2007,
                                                                                                                            ifelse(topmodel.survival$number == 337:360,2008,
                                                                                                                                   ifelse(topmodel.survival$number == 361:384,2009,
                                                                                                                                          ifelse(topmodel.survival$number == 385:408,2010,
                                                                                                                                                 ifelse(topmodel.survival$number == 409:432,2011,
                                                                                                                                                        ifelse(topmodel.survival$number == 433:456,2012,
                                                                                                                                                               ifelse(topmodel.survival$number == 457:480,2013,
                                                                                                                                                                      ifelse(topmodel.survival$number == 481:504,2014,
                                                                                                                                                                             ifelse(topmodel.survival$number == 505:528,2015,
                                                                                                                                                                                    ifelse(topmodel.survival$number == 529:552,2019,
                                                                                                                                                                                           ifelse(topmodel.survival$number == 553:576,2021,
                                                                                                                                                                                                  ifelse(topmodel.survival$number == 577:600,2010,
                                                                                                                                                                                                         ifelse(topmodel.survival$number == 601:624,2011,
                                                                                                                                                                                                                ifelse(topmodel.survival$number == 625:648,2012,
                                                                                                                                                                                                                       ifelse(topmodel.survival$number == 649:672,2013,
                                                                                                                                                                                                                              ifelse(topmodel.survival$number == 673:696,2014,
                                                                                                                                                                                                                                     ifelse(topmodel.survival$number == 697:720,2015,
                                                                                                                                                                                                                                            ifelse(topmodel.survival$number == 721:744,2016,
                                                                                                                                                                                                                                                   ifelse(topmodel.survival$number == 745:768,2017,
                                                                                                                                                                                                                                                          ifelse(topmodel.survival$number == 769:792,2018,
                                                                                                                                                                                                                                                                 ifelse(topmodel.survival$number == 793:816,2019,0))))))))))))))))))))))))))))))))))
#####End of the ifelse codes above
betavcv <- final_top_model$results$beta.vcv





####Try the delta method
###Get estimates of survival and its v-c matrix
library(msm)
deltamethod()









###Add site and year
site <- c("kig","kig","kig","kig","kig","kig",
          "kig","kig","kig","kig","kig","kig",
          "kig","kig","kig","kig","kig","kig",
          "kig","kig","kig","kig","kig","kig",
          "utq","utq","utq","utq","utq",
          "utq","utq","utq","utq","utq")

year <- c("1994","1995","1996","1997","1998","1999",
          "2000","2001","2002","2003","2004","2005",
          "2006","2007","2008","2009","2010","2011",
          "2012","2013","2014","2015","2019","2021",
          "2010","2011","2012","2013","2014",
          "2015","2016","2017","2018","2019")

df.estimates <- cbind(estimates,site,year)


###Graph the best supported model
ggplot(data, aes(x = year, y = probsuccess, color = site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lci, ymax = uci, width = 0.4)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  xlab("Year") + ylab("Estimate DSR") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
  






###Look at model matrix
#design_matrix <- model.matrix(~ -1 + Site:Year, spei_data)
#write.csv(design_matrix, "C:/Users/rjfriendly2/Desktop/RMARK/nest_survival/design_matrix_sitexyear.csv")


#real.e <- (spei.results$S.siteXyear$results$real)
#write.csv(real.e, "C:/Users/rjfriendly2/Desktop/RMARK/nest_survival/real.e.csv")
#head(real.e)
#summary(real.e)
#estimates<-read.csv("nest_survival/real.e.csv", header = TRUE)
#plot(estimates$year, estimates$estimate)
#lines(estimates$year, estimates$estimate)

###export model output file
export.MARK(spei.results3$S.siteXyear.2$data,
            "SPEI_DSR",
            spei.results3,
            replace = TRUE,
            ind.covariates = "all")

spei.results3                     # print model-selection table to screen
options(width = 150)              # set page width to 100 characters
sink("results.table.txt")         # capture screen output to file
print(spei.results3)              # send output file
sink()                            # return output to screen


###Clear the what Rmark spit out to working directory
cleanup(ask = FALSE)


