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
  
  # 6. site and year interaction
  S.siteXyear = list(formula = ~ -1 + Site:Year)
  
  # 7. site and init interaction
  S.siteXinit = list(formula = ~ Site + Init + Site*cinit)
  
  # 8. site and nestage interaction
  S.siteXnestage = list(formula = ~ Site + NestAge + Site*NestAge)
  
  # 9. nestage and site*year interaction
  S.age.siteXyear = list(formula = ~ -1 + NestAge + Site:Year)
  
  # 10. Init and site year interaction
  S.init.siteXyear = list(formula = ~ -1 + cinit + Site:Year)
  
  # 11. site and init and site:year
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

####Make a graph for the best supported model#############################
real.estimates <- spei.results3$S.ageinit.siteXyear.2$results$real
#write.csv(real.estimates, "nest_survival/output/real.estimates.csv") 

fc <- find.covariates(final_top_model, spei_data)











###Read the estimates data file
#estimates<-read.csv("nest_survival/output/real.estimates.csv", header = TRUE)

real.estimates$rnumber <- rep(1:74) #creating numbers to get reference points

###subset first 24 rows of each year
est1 <- subset(real.estimates, rnumber<25)

###add number to represent each row
est1$number <- 1:816

###add year variable
est1$year <- ifelse(est1$number == 1:24,1994,
                    ifelse(est1$number == 25:48,1995,
                           ifelse(est1$number == 49:72,1996,
                                  ifelse(est1$number == 73:96,1997,
                                         ifelse(est1$number == 97:120,1998,
                                                ifelse(est1$number == 121:144,1999,
                                                       ifelse(est1$number == 145:168,2000,
                                                              ifelse(est1$number == 169:192,2001,
                                                                     ifelse(est1$number == 193:216,2002,
                                                                            ifelse(est1$number == 217:240,2003,
                                                                                   ifelse(est1$number == 241:264,2004,
                                                                                          ifelse(est1$number == 265:288,2005,
                                                                                                 ifelse(est1$number == 289:312,2006,
                                                                                                        ifelse(est1$number == 313:336,2007,
                                                                                                               ifelse(est1$number == 337:360,2008,
                                                                                                                      ifelse(est1$number == 361:384,2009,
                                                                                                                             ifelse(est1$number == 385:408,2010,
                                                                                                                                    ifelse(est1$number == 409:432,2011,
                                                                                                                                           ifelse(est1$number == 433:456,2012,
                                                                                                                                                  ifelse(est1$number == 457:480,2013,
                                                                                                                                                         ifelse(est1$number == 481:504,2014,
                                                                                                                                                                ifelse(est1$number == 505:528,2015,
                                                                                                                                                                       ifelse(est1$number == 529:552,2019,
                                                                                                                                                                              ifelse(est1$number == 553:576,2021,
                                                                                                                                                                                     ifelse(est1$number == 577:600,2010,
                                                                                                                                                                                            ifelse(est1$number == 601:624,2011,
                                                                                                                                                                                                   ifelse(est1$number == 625:648,2012,
                                                                                                                                                                                                          ifelse(est1$number == 649:672,2013,
                                                                                                                                                                                                                 ifelse(est1$number == 673:696,2014,
                                                                                                                                                                                                                        ifelse(est1$number == 697:720,2015,
                                                                                                                                                                                                                               ifelse(est1$number == 721:744,2016,
                                                                                                                                                                                                                                      ifelse(est1$number == 745:768,2017,
                                                                                                                                                                                                                                             ifelse(est1$number == 769:792,2018,
                                                                                                                                                                                                                                                    ifelse(est1$number == 793:816,2019,0))))))))))))))))))))))))))))))))))

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
ggplot(df.estimates, aes(x = year, y = estimate, color = site)) +
  #geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl, width = 0.4)) +
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


