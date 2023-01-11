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
#spei_data$cinit<-spei_data$Init-mean(spei_data$Init)

##### NEST SURVIVAL #####
### From 1994-2021 the earliest julian start day is 134 and the latest is 209. 
### I am taking the difference between these two to get the nesting season for all years (nocc). equals 75
###TEMPORAL
spei.pr <- process.data(spei_data,
                        nocc = 75,
                        model = "Nest",
                        groups = c("Year","Site"))

###lets explore and learn
model.matrix(~Site,spei_data) #Kigigak is the intercept and the utqiagvik effect is the difference from the intercept
model.matrix(~Year,spei_data) #1994 is the intercept here.

model.matrix(~-1 + Site, spei_data) #the '-1' removes the intercept, which was kigigak for site. 
#It stil creates two columns in the DM but there is now a separate comlumn for each factor level, instead of using k-1 levels
model.matrix(~-1 + Year, spei_data) #same goes for here.

###Remove the intercept in a model with additive factor
model.matrix(~-1 + Site + Year, spei_data) ### the first factor variable has separate columns but they represent
# the first level of the first level of the second factor variable.

###the interaction model of 2 factor variables where they have k1 and k2 levels, you get k1*k2 columns in the DM.
# You have an intercept, a main effect for each factor (k1-1 + k2-1), and the interactions ((k1-1)*(k2-1)).
# If you add these 3 values you'll see it equals k1*k2.
model.matrix(~Site*Year,spei_data)

# If you look at the matrix from the code above, you can see it has columns for utqiagvik for years it doesn't
# have data for. This was my problem that I have faced. A different way to model the above model is by taking 
# away the intercept, ~-1+Site:Year. If you use factor1:factor2 in a formula rather than factor1*factor2, 
# model.matrix will create an intercept and k1*k2 separate dummy variables that represent the subset of the data
# defined by both factor variables. The '-1' is needed to remove the intercept which is necessary or you will
# have too many columns in the DM.

###Interacting a facotr and numeric variable in a formula you are specifying that the slope of the numeric
# variable can vary across levels of the factor variable as well
# EXAMPLE Site*Init, will have site specific slopes and site specific intercept. diff slope and diff int
# Example Site:Init, will havedifferent slopes but same intercept

###test some models
model.matrix(~ -1 + Site:Year, spei_data)


model.matrix(~0 + Site:Year, spei_data)


model.matrix(~-1 + Site:Year + Site*Init, spei_data)

model.matrix(~ -1 + Site:Year + Site:Init + Site:NestAge,spei_data)


model.ma


spei.ex <- process.data(spei_data,
                        nocc = 75,
                        model = "Nest",
                        groups = c("Year","Site"))

###Write a function for your models to run
run.spei.ex <- function()
{

  # 14. siteXyear site init siteXinit model
  S.one = list(formula = ~ -1 + Site:Year + 0 + Site*Init)
  
  # 15. siteXyear site nestage siteXnestage model
  S.two = list(formula = ~ -1 + Site:Year + Site:NestAge)
  
  # Return model table and list of models
  
  spei.model.list.ex = create.model.list("Nest")
  
  spei.results.ex = mark.wrapper(spei.model.list.ex,
                              data = spei.ex,
                              adjust = FALSE)
  
}

###Run the models above
spei.results.ex <- run.spei.ex()

###Look at model results
spei.results.ex 


spei.results.ex$S.one
spei.results.ex$S.two


