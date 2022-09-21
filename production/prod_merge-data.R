library(tidyverse)
library(sjmisc)


load(file = "input/data/original/ELSOC_W03_v1.00_R.RData")
load(url("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/0KIRBJ/DWXZL1"))# elsoc16
load(url("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/SRPWFW/FZFOJN"))# enacoes14



data16<- elsoc_2016 %>% select(idencuesta,ola,cod_m03,m06,m07) %>% mutate(year=2016)
data18<- elsoc_2018 %>% select(muestra,idencuesta,ola,cod_m03,m06,m07) %>% mutate(year=2018)

data16$cod_m03 <- as.numeric(data16$cod_m03)
data18$cod_m03 <- as.numeric(data18$cod_m03)


wide_merge<- bind_rows(data16,data18)

skimr::skim(wide_merge)


merge_wide <- full_join(data18,data16,by="idencuesta",suffix=c(".18",".16"))

# quiero identificar casos que son unique en 2016 y que no estan en 2018 pero que tienen informacion de ocupacion

merge_wide<-merge_wide %>%  mutate(duplicated(idencuesta))

merge_wide<- merge_wide %>% group_by(idencuesta) %>% mutate(dup=n()) %>% ungroup()

merge_wide$isco<- if_else(condition = (is.na(merge_wide$cod_m03.18)),true =merge_wide$cod_m03.16,false =merge_wide$cod_m03.18)
merge_wide$remp<- if_else(condition = (is.na(merge_wide$m07.18)),true =merge_wide$m07.16,false =merge_wide$m07.18)
merge_wide$nempleados<- if_else(condition = (is.na(merge_wide$m06.18)),true =merge_wide$m06.16,false =merge_wide$m06.18)

sjmisc::frq(merge_wide$remp)
sjmisc::frq(merge_wide$nempleados)

na_df <- merge_wide %>% filter(is.na(muestra))

View:(merge_wide[,c("idencuesta","cod_m03.18","cod_m03.16","m06.16","m06.18")])

data01<- merge_wide %>% select(idencuesta,
                               muestra,
                               isco,
                               # remp,
                               # nempleados
                               ) %>% 
  na.omit()
sjmisc::frq(data01$muestra)
table(is.na(merge_wide$isco))


labels<- data.frame(sjlabelled::get_label(enacoes))


# * Tratar cada base de manera independiente para todas las variables del script de tratamiento para 2018
# * Luego hacer el merge y recuperar casos para tener dos muestra con un dummy para cada año, no es estructura panel. 



