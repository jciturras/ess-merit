library(dplyr)
library(sjPlot)
library(sjmisc)
library(texreg)
library(mice)
rm(list=ls())

load(file = "input/data/proc/ELSOC_ess_merit2018.RData")
elsoc<- elsoc_18;remove(elsoc_18)


# 01 Imputation using OLS -------------------------------------------------

lm02<- lm(inghogar~edcine2+isco_cat+sexo+edad+edad2+ess+factor(region)+estlab+factor(estrato),data=elsoc,na.action = na.exclude)
screenreg(lm02)

cooksd <- cooks.distance(lm02)

# plot_frq(elsoc$inghogar,type = "boxplot")

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

quantile(elsoc$inghogar, probs = c(0.90,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1.00),na.rm = T)
summary(elsoc$inghogar)

# outliers (2) | <17000000 -----------------------------------------------------
elsoc.2<- elsoc %>% filter(inghogar<17000000)
summary(elsoc.2$inghogar)
# plot_frq(elsoc.2$inghogar,type = "boxplot")

lm03<- lm(inghogar~edcine2+isco_cat+sexo+edad+edad2+ess+factor(region)+estlab+factor(estrato),data=elsoc.2,na.action = na.exclude)
screenreg(lm03)

quantile(elsoc.2$inghogar, probs = c(0.90,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1.00),na.rm = T)

# outliers (2) | <10050000 -----------------------------------------------------

elsoc.3<- elsoc.2 %>% filter(inghogar<10050000)

elsoc.3<- elsoc
summary(elsoc.3$inghogar)
# plot_frq(elsoc.3$inghogar,type = "boxplot")


# Imputation usgin OLS (2) ------------------------------------------------

# A. Using social class as predictor
lm04<- lm(inghogar~edcine2+sexo+edad+edad2+ess+factor(region)+estlab+factor(estrato)+egp05,data=elsoc.3,na.action = na.exclude)
screenreg(lm04)

elsoc.3$fitm04 <- predict.lm(object = lm04)

# B. Using log- income DV + ocupation as IV
elsoc.3$lninghogar <- log(elsoc.3$inghogar)
lm05<- lm(lninghogar~edcine2+isco_cat+sexo+edad+edad2+ess+factor(region)+estlab+factor(estrato),data=elsoc.3,na.action = na.exclude)
screenreg(lm05)

# Predicted values using ln(household income)
elsoc.3$fit05 <- predict.lm(object = lm05,newdata = elsoc[,c("edcine2","isco_cat","sexo","edad","edad2","ess","region","estlab","estrato")])
elsoc.3$fitm05_exp<- exp(predict.lm(object = lm05)) 


# log transform to original values (no funciona bien)
sqrt(mean(lm05$residuals^2)) #RMSE
elsoc.3$fitm05<- predict.lm(object = lm05) 
elsoc.3$fitm05_scaled<- exp(elsoc.3$fitm05)* exp(sqrt(mean(lm05$residuals^2))/2)

lm06<- glm(lninghogar~edcine2+isco_cat+sexo+edad+edad2+ess+factor(region)+estlab+factor(estrato),data=elsoc.3,na.action = na.exclude,family = "poisson")
elsoc.3$fitm06_po <- predict.glm(lm06,type = "response")
elsoc.3$fitm06_po.exp <- exp(elsoc.3$fitm06_po)
data01 <- elsoc.3 %>% dplyr::select(idencuesta,"edcine2","isco_cat","sexo","edad","edad2","ess","region","estlab","estrato",inghogar,lninghogar,fitm04,fitm05,fitm05_exp,fitm05_scaled,fitm06_po,fitm06_po.exp)

# Multiple imputation -----------------------------------------------------

data02 <- elsoc.3 %>% dplyr::select(inghogar,"edcine2","egp05","sexo","edad","edad2","ess","region","estlab","estrato")

imp1 <- mice(data02, m = 5)
dat.imp1<- complete(imp1)
summary(dat.imp1$inghogar)


# imp2 <- mice(data02, method = "mean", m = 1, maxit = 1)
# dat.imp2<- complete(imp2)
# summary(dat.imp2$inghogar)
# 
# imp3 <- mice(data02, method = "norm.predict", m = 1, maxit = 1)
# dat.imp3<- complete(imp3)
# summary(dat.imp3$inghogar)
# 
# 
# imp4 <- mice(data02, method = "norm.nob", m = 1, maxit = 1)
# dat.imp4<- complete(imp4)
# summary(dat.imp3$inghogar)mice.impute.norm.predict(y = inghogar)

summary(complete(imp1,1)$inghogar)
summary(complete(imp1,2)$inghogar)
summary(complete(imp1,3)$inghogar)
summary(complete(imp1,4)$inghogar)
summary(complete(imp1,5)$inghogar)

# data01$fit.mimp <- complete(imp1,5)$inghogar
data01$fit.mimp <-(complete(imp1,1)$inghogar+complete(imp1,2)$inghogar+complete(imp1,3)$inghogar+complete(imp1,4)$inghogar+complete(imp1,5)$inghogar)/5
summary(data01$fit.mimp)

# - guardamos el promedio del ingreso imputado de las 5 bases en data01 

cor(data01$fit.mimp,data01$fitm04,use = "complete.obs")

data01$inghogar.imp1 <- ifelse(test = is.na(data01$inghogar),yes = data01$fitm04,  no = data01$inghogar) 
data01$inghogar.imp <- ifelse(test = is.na(data01$inghogar),yes = data01$fit.mimp,no = data01$inghogar) 

plot_frq(data01$inghogar.imp1,type = "histogram")
summary(data01$inghogar.imp1)
plot_frq(data01$inghogar.imp,type = "histogram")
summary(data01$inghogar.imp)


elsoc.imputed<- data01 %>% dplyr::select(idencuesta,inghogar.imp)

# Save data ---------------------------------------------------------------
save(elsoc.imputed,file = "input/data/proc/elsoc.imputed.RData")

