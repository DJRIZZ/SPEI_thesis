###Load packages to run NS models
library(RMark)
library(tidyverse)
library(readr)
library(msm)



#nest survival
sp.pr <- process.data(spei_data,
                      nocc = 75,
                      model = "Nest",
                      groups = c("Year","Site"))


###Write a function for your models to run
run.spei <- function()
{
  # 1. Intercept only model (null)
  S.dot = list(formula = ~ 1)
  
  # 2. site and year model
  S.sy = list(formula = ~ Site + Year)
  
  # 3. site + year + init
  S.syi = list(formula = ~ Site + Year + Init)
  
  # 4. site + year + nestage
  S.syn = list(formula = ~ Site + Year + NestAge)
  
  # 5. site + year + init + nestage
  S.syin = list(formula = ~ Site + Year + Init + NestAge)
  
  # 6. site + year + init + site*Init
  S.syi.sxi = list(formula = ~ Site + Year + Init + Site*Init)
  
  # 7. site + year + nestage + site*nestage
  S.syn.sxn = list(formula = ~ Site + Year + Init + Site*NestAge)
  
  # 8. site + year + init + nestage + site*init
  S.syin.sxi = list(formula = ~ Site + Year + NestAge + Site*Init)
  
  # 9. site + year + init + nestage + site*nestage
  S.syin.sxn = list(formula = ~ Site + Year + Init + NestAge + Site*NestAge)
  
  # 10. site + year + init + nestage + site*init + site*nestage
  S.syin.sxi.sxn = list(formula = ~ Site + Year + Init + NestAge + Site*Init + Site*NestAge)
  
  # 11. site*year
  S.sxy = list(formula = ~ -1 + Site:Year)
  
  # 12. site*year + init
  S.sxy.i = list(formula = ~ -1 + Site:Year + Init)
  
  # 13. site*year + nestage
  S.sxy.n = list(formula = ~ -1 + Site:Year + NestAge)
  
  # 14. site*year + site*init
  S.sxy.sxi = list(formula = ~ -1 + Site:Year + Site:Init)
  
  # 15. site*year + site*nestage
  S.sxy.sxn = list(formula = ~ -1 + Site:Year + Site:NestAge)
  
  # 16. site*year + init + nestage
  S.sxy.in = list(formula = ~ -1 + Site:Year + Init + NestAge)
  
  # 17. site*year + init + nestage + site*init
  S.sxy.in.sxi = list(formula = ~ -1 + Site:Year + Init + NestAge + Site:Init)
  
  # 18. site*year + init + nestage + site*nestage
  S.sxy.in.sxn = list(formula = ~ -1 + Site:Year + Init + NestAge + Site:NestAge)
  
  # 19. site*year + init + nestage + site*init + site*nestage
  S.sxy.in.sxi.sxn = list(formula = ~ -1 + Site:Year + Init + NestAge + Site:Init + Site:NestAge)
  
  
  # Return model table and list of models
  
  sp.model.list = create.model.list("Nest")
  
  sp.results = mark.wrapper(spei.model.list,
                            data = sp.pr,
                            adjust = FALSE)
  
}


###Run the models above
sp.results <- run.spei()

###Look at model results
spei.results 