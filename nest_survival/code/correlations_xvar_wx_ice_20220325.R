# code to look at correlation among sea ice and weather variables
# and get summary statistics
# strategically select variables to use in analysis as explanatory variables

# got packages?
library(psych) #to create panel of histograms
library(corrplot) #to create correlation plot

# merge weather and sea ice variables

# load sea ice data
ice<-read.csv("sea_ice_bs_core/output/sea_ice_vars_1979_2020.csv", header=T) ###updated this with year data 2020
head(ice)
ice<-ice[, 2:11] # drop un-needed first column
summary(ice)

# load St. Paul weather data
wx<-read.csv("weather_st_paul/output/wx_st_paul_1993_2020.csv", header=T) ###updated this with year data 2020
head(wx)
summary(wx)
wx<-wx[, 2:12] # drop un-needed first column

# merge
ice_wx<-merge(wx, ice, by="year_winter", all.x=T)
str(ice_wx)

# shorten variable names
var_abbrev<-c("year_winter", "wsxt","wxt","sxt", "wmt", "smt", "wsxw", "wxw","sxw","wmw","smw","ws95", "w95","s95", "ws15","w15","s15", "wsi", "wi", "si")
var_long<-colnames(ice_wx)
temp_names<-rbind(var_long, var_abbrev)
# check abbreviations match the long variable names
temp_names

colnames(ice_wx)<-var_abbrev
str(ice_wx)

# look at data
# scattergrams: look at pari-wise correlations
wx.scats<-pairs(ice_wx[2:7], lower.panel=NULL)
ice.scats<-pairs(ice_wx[8:20], lower.panel=NULL)
# histograms to look at distribution of data:  (from psyche package)
wx.dists<-multi.hist(ice_wx[2:7])
ice.dists<-multi.hist(ice_wx[8:20])

# calculate pairwise correlations (Person correlations are the default)
r<-cor(ice_wx[2:20]) 
# plot summarizing correlations for easier interpretation (from corrplot package)
r_plot<-corrplot(r, method="number", type="upper") #plot correlations using function from r package corrplot
r_plot

# Looking at the correlation plot, the days sea ice concentration >= 95% is highly correlated with  winter sea ice index (r> 0.9)
# essentially these two variables would be accounting for the same variation in statistical models
# so only need to include one. I would use sea ice >= 95% as that is what Christie et al. found support for in their analysis.

# for daily min temp in St. Paul: mean temperature is (-) correlated with days sic > 95%, the count of extreme temp days is not
# wind has no strong correlations, but use count of days with extreme winds for consistency

# for ice, temp, and wind the variables are all counts of days with extreme values

# Uncorrelated vars to use in models
# 1. winter extreme min temp days
# 2. spring extreme min temp days
# 3. winter extreme average wind speed days
# 4. spring extreme average wind speed days
# 5. winter extreme high ice days as linear and quadratic
# 6. spring extreme high ice days as linear and quadratic


