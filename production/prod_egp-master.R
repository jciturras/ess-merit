load(file = here::here("input/data/original/ELSOC_W03_v1.00_R.RData"))
elsoc <- elsoc_2018
library(dplyr)

for (i in 1:ncol(elsoc)) {
  elsoc[,i][elsoc[,i] == c(-888)]  <- NA #Missing 
  elsoc[,i][elsoc[,i] == c(-999)]  <- NA #Missing   
}

source(file = "production/prod_egp-respondent.R") #class scheme - respondent
source(file = "production/prod_egp-household.R")#class scheme - head household 

#if NA in class of respondent, replace with household

sjmisc::frq(elsoc$clase11e)
sjmisc::frq(elsoc$clase11h)
elsoc$class11 <- ifelse(is.na(elsoc$clase11e),elsoc$clase11h,elsoc$clase11e)
sjmisc::frq(elsoc$class11)

# if NA in respondents it is assumed to be lower than head of household
# know, with the cases in we have information for respondent and head of household
# we replace the value by the highest one, just for those

# Replace the highest class by dominance criteria 
# Keep the highest of two values:
elsoc$class11d <- pmax(elsoc$class11, #social class respondent + household vs:
                       elsoc$clase11h,#social class household
                       na.rm = T) # if NA, do nothing

elsoc$class11d <- sjlabelled::set_labels(elsoc$class11d,labels = sjlabelled::get_labels(elsoc$clase11e)) 
sjmisc::frq(elsoc$class11d)

# elsoc %>% 
#   select(clase11e,clase11h,class11d) %>% 
#   arrange(clase11e,clase11h) %>% 
#   View()


