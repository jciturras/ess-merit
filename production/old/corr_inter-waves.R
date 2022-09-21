
# Correlaciones entre olas ELSOC ------------------------------------------

library(here)
library(sjlabelled)
library(dplyr)
library(corrplot)

setwd(dir = here())

wide <- read_stata(path = "code/data/ELSOC_Wide_2016_2018_v1.00_Stata13.dta")

names(wide)


wd <- wide %>% dplyr::select(starts_with(match = "c18_10")) 
wd <- wd %>%select(c18_10_w03,c18_10_w02) 

for (i in 1:ncol(wd)) {
  wd[,i][wd[,i] == c(-888)]  <- NA #Missing 
  wd[,i][wd[,i] == c(-999)]  <- NA #Missing   
}
summary(wd)
wd <- na.omit(wd)
cor(wd)


load(file = "code/data/merit_long.RData")

names(merit_final)

reco <- merit_final[,c("idencuesta","ola","re_esfu","re_inha")]
reco <- na.omit(reco)

psych::describeBy(reco$re_esfu,group = reco$ola,mat = TRUE)



ess <- wide %>% dplyr::select(starts_with(match = "d01_01"),idencuesta) 
# wd <- wd %>%select(c18_10_w03,c18_10_w02) 

for (i in 1:ncol(ess)) {
  ess[,i][ess[,i] == c(-888)]  <- NA #Missing 
  ess[,i][ess[,i] == c(-999)]  <- NA #Missing   
}
summary(ess)
ess <- na.omit(ess)
cor(ess)






