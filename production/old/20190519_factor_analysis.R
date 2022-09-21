library(psych)
library(lavaan)
library(sjPlot)
library(corrplot)
library(here)

here()

rm(list=ls())
load(here("code","data","ELSOC_ess_merit2016.RData")) #2016
load(here("code","data","ELSOC_ess_merit2017.RData")) #2016
load(here("code","data","ELSOC_ess_merit2018.RData")) #2018

elsoc <- elsoc_16
elsoc <- elsoc_17
elsoc <- elsoc_18

# Correlación -------------------------------------------------------------

merit <- elsoc %>% dplyr::select(d05_01,d05_02,d05_03,d05_04,c18_09,c18_10)
sjp.corr(merit)


corrplot.mixed(cor(merit,use = "complete.obs")) 


# Exploratory factor analysis ---------------------------------------------

library(GPArotation)
sjt.fa(merit, rotation = "promax", method = c("ml", "minres",
                                              "wls", "gls", "pa", "minchi", "minrank"), nmbr.fctr = NULL,
       fctr.load.tlrn = 0.1, title = "Factor Analysis")

sjt.fa(merit, rotation = "varimax", method = "ml", nmbr.fctr = NULL,
       fctr.load.tlrn = 0.1, title = "Factor Analysis")


# Confirmatory factor analysis --------------------------------------------

cfa<-'
salirad =~d05_02+d05_03+d05_04
recompe =~c18_09+c18_10
'

#Fit Continuas
fit1 <-cfa(cfa,data=merit)
fitmes<-fitMeasures(fit1,c("chisq","df","pvalue","cfi","tli","rmsea"))
print(fitmes)
summary(fit1, standardized=TRUE)


#Fit Ordinal
fit2 <-cfa(cfa,data=merit,ordered=c("d05_02","d05_03","d05_04","c18_09","c18_10"))
fitmes2<-fitMeasures(fit2,c("chisq","df","pvalue","cfi","tli","rmsea"))
print(fitmes2)

summary(fit2, standardized=TRUE)






