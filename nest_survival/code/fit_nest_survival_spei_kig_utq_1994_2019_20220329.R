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

##### NEST SURVIVAL #####
### From 1994-2019 the earliest julian start day is 134 and the latest is 209. 
### I am taking the difference between these two to get the nesting season for all years (nocc). equals 75
###TEMPORAL
spei.pr <- process.data(spei_data,
                        nocc = 75,
                        model = "Nest",
                        groups = c("Year","Site"))

###Write a function for your models to run
run.spei <- function()
{
  # 1. site and year model
  S.site.year = list(formula = ~Site + Year)
  
  # 2. site and init model
  S.site.init = list(formula = ~Site + Init)
  
  # 3. site and nestage model
  S.site.nestage = list(formula = ~Site + NestAge)
  
  # 4. site and init and nestage model
  S.site.init.nestage = list(formula = ~Site + Init + NestAge)
  
  # 5. site and year interaction
  S.siteXyear = list(formula = ~ -1 + Site:Year)
  
  # 6. site and init interaction
  S.siteXinit = list(formula = ~Site + Init + Site*Init)
  
  # 7. site and nestage interaction
  S.siteXnestage = list(formula = ~Site + NestAge + Site*NestAge)
  
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
spei.results$S.siteXyear

###Write the results output into excel (csv)
modeloutput1 <- spei.results$model.table
write.csv(modeloutput1, "figures_tables/modeloutput1.csv")


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
  
  # 9. site and winterHi2 interaction model
  S.siteXwinhi2 = list(formula = ~Site + Win_Hi + Win_Hi2 + Site*Win_Hi + Site*Win_Hi2)
  
  # 10. site and winterLo2 interaction model
  S.siteXwinlo2 = list(formula = ~Site + Win_Lo + Win_Lo2 + Site*Win_Lo + Site*Win_Lo2)
  
  # 11. site and springHi2 interaction model 
  S.siteXsprhi2 = list(formula = ~Site + Spr_Hi + Spr_Hi2 + Site*Spr_Hi + Site*Spr_Hi2)
  
  # 12. site and springLo2 interaction model
  S.siteXsprlo2 = list(formula = ~Site + Spr_Lo + Spr_Lo2 + Site*Spr_Lo + Site*Spr_Lo2)
  
  # 13. site and wxt model
  S.site.wxt = list(formula = ~Site + wxt)
  
  # 14. site and wxw model
  S.site.wxw = list(formula = ~Site + wxw)
  
  # 15. site and wxt and wxw model
  S.site.wxt.wxw = list(formula = ~Site + wxt + wxw)
  
  # 16. site and wxt interaction model
  S.siteXwxt = list(formula = ~Site + wxt + Site*wxt)
  
  # 17. site and wxw interaction model
  S.siteXwxw = list(formula = ~Site + wxw + Site*wxw)
  
  # 18. site and wxt and wxw interaction model
  S.siteXwxtXwxw = list(formula = ~Site + wxt + wxw + Site*wxt + Site*wxw)
  
  # 21. site and sxt model
  S.site.sxt = list(formula = ~Site + sxt)
  
  # 22. site and wxw model
  S.site.sxw = list(formula = ~Site + sxw)
  
  # 23. site and wxt and wxw model
  S.site.sxt.sxw = list(formula = ~Site + sxt + sxw)
  
  # 24. site and wxt interaction model
  S.siteXsxt = list(formula = ~Site + sxt + Site*sxt)
  
  # 25. site and wxw interaction model
  S.siteXsxw = list(formula = ~Site + sxw + Site*sxw)
  
  # 26. site and wxt and wxw interaction model
  S.siteXsxtXsxw = list(formula = ~Site + sxt + sxw + Site*sxt + Site*sxw)

  
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

###Look at model results output
spei.results$S.siteXyear

###Look at model results output
spei.results2$S.siteXwinlo2

###Write the results output into excel (csv)
modeloutput2 <- spei.results2$model.table
write.csv(modeloutput2, "figures_tables/modeloutput2.csv")


#########################################################################################

###Compete top model from temporal and ice condition variables
spei.pr3 <- process.data(spei_data,
                         nocc = 75,
                         model = "Nest",
                         groups = c("Year","Site"))

###Write a function for your models to run
run.spei3 <- function()
{
  # 1. site and year interaction
  S.siteXyear.2 = list(formula = ~ -1 + Site:Year)
  
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

spei.results3$S.siteXwinlo2.2

###Look at model matrix
#design_matrix <- model.matrix(~ -1 + Site:Year, spei_data)
#write.csv(design_matrix, "C:/Users/rjfriendly2/Desktop/RMARK/nest_survival/design_matrix_sitexyear.csv")

###Lets create a plot!!!
real.e <- (spei.results$S.siteXyear$results$real)
write.csv(real.e, "nest_survival/output/real.e.csv")
#head(real.e)
#summary(real.e)
estimates<-read.csv("nest_survival/output/real.e.csv", header = TRUE)

#got packages?
library(ggplot2)
#mytheme<-
ggplot(estimates, aes(x = year, y = estimate, group = site)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.13) +
  geom_line(size = 0.09, aes(color = site)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = c(0.75, 0.3)) +
  xlab("Year") + ylab("Estimate DSR") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(labels = estimates$year, breaks = estimates$year) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("yr_estimates.jpg", width = 8, height = 4, dpi = 220)

ggplot(estimates, aes(x = year, y = estimate, color = site)) +
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
scale_x_continuous(labels = estimates$year, breaks = estimates$year) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("yr_estimates_scatter.jpg", width = 8, height = 4, dpi = 220)

plot

initna <- spei.results$S.site.init.nestage
### buil design matrix with ages and init values of interest
### the incubation period for speis are 24
sfc <- find.covariates(initna,spei_data)
sfc$value[149:172] <- 1:24      # assign 1:24 to 1st 24 nest ages
sdesign <- fill.covariates(initna, sfc)
ina.survival <- compute.real(initna, design = sdesign)[1:24, ]

ina.survival <- cbind(sdesign[1:24, c(3:4)], ina.survival)
colnames(ina.survival) <- c("Init","Age","DSR","seDSR","lclDSR","uclDSR")

ggplot(ina.survival, aes(x = Age, y = DSR)) +
  geom_line() +
  geom_ribbon(aes(ymin = lclDSR, ymax = uclDSR), alpha = 0.3) +
  xlab("Nest Age (days)") +
  ylab("Estimated DSR") +
  theme_bw()



###Resource
#https://r-graphics.org/recipe-axes-axis-date

####Create tables using R ################

###Load packages
library(knitr)
library(rmarkdown)


spei.results$S.site.init.nestage


