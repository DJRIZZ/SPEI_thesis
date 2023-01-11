### Goal of this R script is to prepare the data for nest initiation date.
###This is will come from the nest survival INP data and we will remove columns

###Activate packages
library(data.table)
library(tidyverse)

###Read in INP file to work from 
spei_data <- read.csv("nest_survival/data/INP_20220921.csv", header = TRUE)

###Remove columns by name
initiation_date <- spei_data[,!names(spei_data) %in% c("FirstFound",
                                                       "LastPresent",
                                                       "LastChecked",
                                                       "Fate",
                                                       "Freq",
                                                       "AgeDay1",
                                                       "Win_Lo",
                                                       "Spr_Lo")]

###Remove first column
initiation_date <- initiation_date[,-1]

### 8. write the csv file in the data folder
write.csv(initiation_date, "nest_initiation/data/initiation_wx.csv")
