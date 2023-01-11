### Fitting mixed effect models

###Load Packages
library(lme4) #used for modeling mixed effects models
library(data.table) #for easy data manipulation and visuals
library(corrplot) #to create correlation plot

init_data <- read.csv("nest_initiation/data/initiation_wx.csv", header = TRUE)
head(init_data)

###Make sure data was loaded properly
str(init_data)
summary(init_data)

###Square the sea ice variables. (Quadratic term and effect). This is adding another column.
#init_data$Win_Hi2<-init_data$Win_Hi^2
## I realized in glms or lms we can square the linear term in the model.

###make sure site and year are factor variables
is.factor(init_data$Site)
init_data$Site <- as.factor(init_data$Site)
is.factor(init_data$Site)

is.factor(init_data$Year)
init_data$Year <- as.factor(init_data$Year)
is.factor(init_data$Year)

###check data again
head(init_data)
str(init_data)

###Visualize data and distributions
hist(init_data$Win_Hi)
hist(init_data$Init)
hist(init_data$wxt)
hist(init_data$sxt)
hist(init_data$wxw)
hist(init_data$sxw)
barplot(init_data$Site)



###Check for collinearity
pairs(init_data[6:11])
r <- cor(init_data[6:11])
r_plot <- corrplot(r, method = "number", type = "upper")
r_plot



###Do some summary statistics, data statistics. What the ranges are. distributions, by histogram or boxplots
###pairwise scattergrams
###rigoursu way is by one model set ata  time.

# Make sure to check for non-linearity
# ouliers and levarage points
# independence of residuals
# constant variance
# normality


###Fit mixed effect models
fit_1 <- lm(Init ~ 1, data = init_data)
summary(fit_1)

fit_2 <- lmer(Init ~ Win_Hi + Site + (1|Year), data = init_data, REML = TRUE)
summary(fit_2)

fit_3 <- lmer(Init ~ Win_Hi + I(Win_Hi^2) + Site + (1|Year), data = init_data, REML = TRUE)
summary(fit_3)

fit_4 <- lmer(Init ~ sxw + Site + (1|Year), data = init_data, REML = TRUE)
summary(fit_4)

fit_5 <- lmer(Init ~ sxt + Site + (1|Year), data = init_data, REML = TRUE)
summary(fit_5)

fit_6 <- lmer(Init ~ Win_Hi + Site + Win_Hi*Site + (1|Year), data = init_data, REML = TRUE)
summary(fit_6)

fit_7 <- lmer(Init ~ Win_Hi + I(Win_Hi^2) + Site + I(Win_Hi^2)*Site + (1|Year), data = init_data, REML = TRUE)
summary(fit_7)

fit_8 <- lmer(Init ~ sxw + sxt + Site + (1|Year), data = init_data, REML = TRUE)
summary(fit_8)

fit_9 <- lmer(Init ~ Win_Hi + I(Win_Hi^2) + sxw + sxt + Site + (1|Year), data = init_data, REML = TRUE)
summary(fit_9)



######
# 1. Check if the residuals are independent. Test for non-linearity. check for non-constant variance
plot(fit_2)
residuals(fit_2)
plot(residuals(fit_2))

fit2_resid <- residuals(fit_2) #assign residuals
#plot vs predictor
plot(fit2_resid ~ Win_Hi, data = init_data)
#different one
plot(residuals(fit_2) ~ fitted(fit_2))  #this seems to match with plot(fit_2), so is this the right one?

#normality
qqnorm(rstudent(fit_2))
shapiro.test(residuals(fit_2))


###Comapre models using Likelihood Ratio Test and AIC
anova(fit_1,fit_2,fit_3,fit_4,fit_5,fit_6,fit_7,fit_8,fit_9)
AIC(fit_1,fit_2,fit_3,fit_4,fit_5,fit_6,fit_7,fit_8,fit_9)
#fit_6 had the lowest AIC

