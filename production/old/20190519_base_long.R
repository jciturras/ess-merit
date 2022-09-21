
library(tidyverse)
library(here)
library(haven)
library(lavaan)

elsoc_long <- read_dta(file = here("data","ELSOC_Wide_2016_2018_v1.00_Stata13.dta"))

elsoc_longa <- elsoc_long %>% select(idencuesta,d05_02_w01:d05_04_w03,c18_09_w01:c18_10_w03)

for (i in 1:ncol(elsoc_longa)) {
  elsoc_longa[,i][elsoc_longa[,i] == c(-888)]  <- NA #Missing 
  elsoc_longa[,i][elsoc_longa[,i] == c(-999)] <- NA #Missing  
}


cfa1<-'
salirad1 =~d05_02_w01+d05_03_w01+d05_04_w01
recompe1 =~c18_09_w01+c18_10_w01
salirad2 =~d05_02_w02+d05_03_w02+d05_04_w02
recompe2 =~c18_09_w02+c18_10_w02
salirad3 =~d05_02_w03+d05_03_w03+d05_04_w03
recompe3 =~c18_09_w03+c18_10_w03
'

fit1 <-cfa(cfa1,data=elsoc_longa)
fitmes<-fitMeasures(fit1,c("chisq","df","pvalue","cfi","tli","rmsea"))
print(fitmes)
summary(fit1, standardized=TRUE)


semPlot::semPaths(fit1,
                  what = "std",
                  style = "lisrel",
                  layout = "tree2",
                  curve = 2.5)



# Seleccionar ID de casos en Ola 1 ----------------------------------------

load(here("data","ELSOC_W01_v3.10.RData")) #2016

ID2016 <- elsoc_2016$idencuesta

# wide a long -------------------------------------------------------------

elsoc_longa <- elsoc_longa %>% filter(idencuesta %in% ID2016)

elsoc_l <- elsoc_longa %>% select(idencuesta,d05_02_w01:d05_04_w03,c18_09_w01:c18_10_w03)
  
elsoc_l <- elsoc_longa %>% select(idencuesta,d05_02_w01:d05_02_w03)

long1 <- gather(data = elsoc_l,key =ola ,value = "gah_educ",d05_02_w01:d05_02_w03, -idencuesta)
table(long1$ola)
long1$ola[long1$ola=="d05_02_w01"] <- 2016
long1$ola[long1$ola=="d05_02_w02"] <- 2017
long1$ola[long1$ola=="d05_02_w03"] <- 2018

elsoc_l2 <- elsoc_longa %>% select(idencuesta,d05_03_w01:d05_03_w03)
long2 <- gather(data = elsoc_l2,
                key =ola ,
                value = "gah_ambi",
                d05_03_w01:d05_03_w03, 
                -idencuesta)
table(long3$ola)
long2$ola[long2$ola=="d05_03_w01"] <- 2016
long2$ola[long2$ola=="d05_03_w02"] <- 2017
long2$ola[long2$ola=="d05_03_w03"] <- 2018


elsoc_l3 <- elsoc_longa %>% select(idencuesta,d05_04_w01:d05_04_w03)
long3 <- gather(data = elsoc_l3,
                key =ola ,
                value = "gah_tdur",
                d05_04_w01:d05_04_w03, 
                -idencuesta)
table(long3$ola)
long3$ola[long3$ola=="d05_04_w01"] <- 2016
long3$ola[long3$ola=="d05_04_w02"] <- 2017
long3$ola[long3$ola=="d05_04_w03"] <- 2018


data_gah <- full_join(x = long1,y = long2,  )
data_gah <- full_join(x = data_gah,y = long3)


# -------------------------------------------------------------------------

elsoc_l <- elsoc_longa %>% select(idencuesta,c18_09_w01:c18_09_w03)
long4 <- gather(data = elsoc_l,key =ola ,value = "re_esfu",c18_09_w01:c18_09_w03, -idencuesta)
table(long4$ola)
long4$ola[long4$ola=="c18_09_w01"] <- 2016
long4$ola[long4$ola=="c18_09_w02"] <- 2017
long4$ola[long4$ola=="c18_09_w03"] <- 2018

elsoc_l <- elsoc_longa %>% select(idencuesta,c18_10_w01:c18_10_w03)
long5 <- gather(data = elsoc_l,key =ola ,value = "re_inha",c18_10_w01:c18_10_w03, -idencuesta)
table(long5$ola)
long5$ola[long5$ola=="c18_10_w01"] <- 2016
long5$ola[long5$ola=="c18_10_w02"] <- 2017
long5$ola[long5$ola=="c18_10_w03"] <- 2018


data_rec <- full_join(x = long4,y = long5)


merit_final <- inner_join(x = data_gah,y = data_rec)

save(merit_final,file = here("data","merit_long.RData"))


# invarianza long ---------------------------------------------------------

model1 <- '

get_ah=~ gah_educ+gah_ambi+gah_tdur
rec_pe=~ re_esfu+re_inha

'


data1 <- merit_final 

data1 <- data1 %>% na.omit()


# Invarianza Configural ---------------------------------------------------

fit.conf=cfa(model1,data=data1, group="ola", ordered = c("gah_educ","gah_ambi","gah_tdur", "re_esfu","re_inha"))

fit.conf=cfa(model1,data=data1, group="ola")


summary(fit.conf,fit.measures=TRUE, standardized=TRUE)


# Invarianza débil --------------------------------------------------------

fit.deb=cfa(model1,data=data1, group="ola", ordered = c("gah_educ","gah_ambi","gah_tdur", "re_esfu","re_inha"),group.equal=c("loadings") )
summary(fit.deb,fit.measures=TRUE,standardized=TRUE)

# parTable(fit.deb) # inspeccionar
anova(fit.conf,fit.deb)


