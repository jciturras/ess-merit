# Analisis de regresion ----------------------------------
library(estimatr)
library(texreg)
library(sjlabelled)
library(dplyr)
library(ggplot2)
library(sjPlot)
library(interplot)
library(gridExtra)

# 00: Prep variables ------------------------------------------------------

options(scipen=999)
rm(list=ls())

load("input/data/proc/ELSOC_ess_merit2018.RData")
load("input/data/proc/elsoc_w03.RData")


view_df(x = elsoc_18)

# IPW para mercado laboral-----------------------------------------------
# Dummy mercado trabajo | dentro = 0, fuera =1
elsoc_18$work <- ifelse(test = (elsoc_18$estlab %in% c("jornada_completa","jornada_parcial","estudia_trabaja")),yes = 0,no = 1)
table(elsoc_18$work)
prop.table(table(elsoc_18$work))

prop01 <- glm(work~sexo+edad+edad2+edcine2+region+ess,data = elsoc_18,family = "binomial",na.action = na.exclude)
summary(prop01)
summary(prop01$fitted.values)

elsoc_18$prob <- predict.glm(object = prop01,newdata = elsoc_18[,c("sexo","edad","edad2","edcine2","region","ess","nhijos")],type = "response")

# En base a esto : http://sites.utexas.edu/prc/files/IPWRA.pdf
# * Revisar:
  # https://stats.stackexchange.com/questions/411711/why-do-stabilized-ipw-weights-give-the-same-estimates-and-ses-as-unstabilized-we
  # http://www.rebeccabarter.com/blog/2017-07-05-ip-weighting/

summary(elsoc_18$work/elsoc_18$prob + (1-elsoc_18$work)/(1-elsoc_18$prob)) # weights segun clases de LM (inferencia causal)

elsoc_18$ipw01 <- ifelse(test = (elsoc_18$work ==1),yes = (1/elsoc_18$prob),no = 1/ (1-elsoc_18$prob))
summary(elsoc_18$ipw01)

(elsoc_18$work) / (elsoc_18$prob)         # Treated = 1 = Fuera de mercado laboral
(1 - elsoc_18$work) / (1 - elsoc_18$prob) # Control = 0 = Dentro de mercado laboral

elsoc_18$ipw_s <- ifelse(test = (elsoc_18$work ==1),yes = (elsoc_18$work) / (elsoc_18$prob),no = (1 - elsoc_18$work) / (1 - elsoc_18$prob))
summary(elsoc_18$ipw_s) # stabilized ipw


table(elsoc_18$ipw_s>7.463204)
table(elsoc_18$ipw_s<1.038734)

elsoc_18$ipw_s[elsoc_18$ipw_s>7.463204] <- 7.463204
elsoc_18$ipw_s[elsoc_18$ipw_s<1.038734] <- 1.038734
summary(elsoc_18$ipw_s)

#- Ver P99 y remplazara todos los que sean > valor
#- ver P01 y remplazar todos los que sean <  valor






# Seleccionar variables de base analítica ---------------------------------

elsoc <- elsoc_18 %>% dplyr::select(ponderador02,region,meffort3,mtalent3,inc10h.imp,ess,edcine2,meffort,mtalent,recompe.sum,recompe,
                                    egp05,sexo,edad,edad2,ppolcat,estlab,ipw01,ipw_s,Q05h,inc10h.imp,inc05h.imp) 
# elsoc$meffort <- as.numeric(elsoc$meffort)
# elsoc$mtalent <- as.numeric(elsoc$mtalent)





# 00 Analisis con OLS -------------------------------------------------------------------------


# elsoc$meffort <- as.numeric(elsoc$meffort)
# elsoc$mtalent <- as.numeric(elsoc$mtalent)
# fa.fit1<- psych::fa(r = elsoc[,c("meffort","mtalent")],nfactors = 1)
# scores<- psych::factor.scores(x =elsoc[,c("meffort","mtalent")],f = fa.fit1)

# elsoc$merit.scores<- scores$scores 
# hist.default(elsoc$merit.scores)
hist.default(elsoc$recompe.sum)
hist.default(elsoc$recompe)



# 01 Análisis con Ordinal Logit ------------------------------------------
library(MASS)
library(lmtest)
library("sandwich")

# Modelos: Esfuerzo -------------------------------------------------------
or01  <- polr(meffort3 ~inc10h.imp,data= elsoc, Hess = TRUE, weights = ipw01)
or02  <- polr(meffort3 ~edcine2,data= elsoc, Hess = TRUE, weights = ipw01)
or03  <- polr(meffort3 ~egp05,data= elsoc, Hess = TRUE, weights = ipw01)
or04  <- polr(meffort3 ~inc10h.imp+edcine2+egp05,data= elsoc, Hess = TRUE, weights = ipw01)
or05  <- polr(meffort3 ~ess,data= elsoc, Hess = TRUE, weights = ipw01)
or08  <- polr(meffort3 ~inc10h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+factor(estlab),data= elsoc, Hess = TRUE, weights = ipw01)
or08.5cat  <- polr(meffort ~inc10h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+factor(estlab),data= elsoc, Hess = TRUE, weights = ipw01)

screenreg(l = list(or08.5cat,or08),stars = c(0.001,0.01,0.05,0.1))

or09ef <- polr(meffort3 ~inc10h.imp+edcine2+egp05+ess+inc10h.imp*ess+edad+sexo+ppolcat+factor(estlab), data=elsoc,Hess = TRUE,weights = ipw01)
or10ef <- polr(meffort3 ~inc10h.imp+edcine2+egp05+ess+edcine2*ess+edad+sexo+ppolcat+factor(estlab),data=elsoc,Hess = TRUE,weights = ipw01)
or11ef <- polr(meffort3 ~inc10h.imp+edcine2+egp05+ess+egp05*ess+edad+sexo+ppolcat+factor(estlab),  data=elsoc,Hess = TRUE,weights = ipw01)

or09ef.5cat <- polr(meffort ~inc10h.imp+edcine2+egp05+ess+inc10h.imp*ess+edad+sexo+ppolcat+factor(estlab), data=elsoc,Hess = TRUE,weights = ipw01)
or10ef.5cat <- polr(meffort ~inc10h.imp+edcine2+egp05+ess+edcine2*ess+edad+sexo+ppolcat+factor(estlab),data=elsoc,Hess = TRUE,weights = ipw01)
or11ef.5cat <- polr(meffort ~inc10h.imp+edcine2+egp05+ess+egp05*ess+edad+sexo+ppolcat+factor(estlab),  data=elsoc,Hess = TRUE,weights = ipw01)


coeftest(or09ef)
coeftest(or10ef)
coeftest(or11ef)

car::S(or11ef,vcov. = hccm(object, type = "hc2"))$coefficients[,c("Std. Error")]
screenreg(l = list(or09ef,or10ef,or11ef),omit.coef = "(D10No responde)|(egp06missing)|(edad)|(sexoMujer)|(ppolcatCentro)|(ppolcatDerecha/Centro Derecha)|(ppolcatIndependiente)|(ppolcatNinguno)")

screenreg(l = list(or09ef,or09ef.5cat,ols02),
          include.ci = FALSE,
          omit.coef = "(D10No responde)|(egp06missing)|(edad)|(sexoMujer)|(ppolcatCentro)|(ppolcatDerecha/Centro Derecha)|(ppolcatIndependiente)|(ppolcatNinguno)")


DescTools::PseudoR2(or08.5cat,which = "McFadden")
DescTools::PseudoR2(or08,which = "McFadden")

coef.names01 <-c("Decil Ingreso",
                 "Primaria y secundaria baja",
                 "Secundaria alta",
                 "Terciaria ciclo corto",
                 "Terciaria y Postgrado",
                 "Obrero calificado (V+VI)",
                 "Autoempleo (IVab+IVc)",
                 "Rutinas no manuales(III)",
                 "Servicio (I+II)", 
                 "Estatus Subjetivo",
                 "Jornada parcial",
                 "Estudia y trabaja",
                 "Decil Ingreso x Est. Subj",
                 "Primaria y secundaria baja x Est. Subj",
                 "Secundaria alta x Est. Subj",
                 "Terciaria ciclo corto x Est. Subj",
                 "Terciaria y Postgrado x Est. Subj",
                 "Obrero calificado (V+VI) x Est. Subj",
                 "Autoempleo(IVab+IVc) x Est. Subj",
                 "Rutinas no manuales(III) x Est. Subj",
                 "Servicio (I+II) x Est. Subj") 

tab.caption01 <- "Modelo regresión logística ordinal para Percepción de Meritocracia - Esfuerzo"
omit.coef01 <- c("(edad)|(sexoMujer)|(ppolcatCentro)|(ppolcatDerecha/Centro Derecha)|(ppolcatIndependiente)|(ppolcatNinguno)")

htmlreg(l = list(or01,or02,or03,or04,or05,or08,or09ef,or10ef,or11ef),
        custom.coef.names = coef.names01,
        caption.above = TRUE, 
        caption = tab.caption01,
        custom.model.names = paste0("Modelo ",c(1:9)),
        omit.coef = omit.coef01,
        file = "output/tables/tab.models01b.html")

texreg(l = list(or01,or02,or03,or04,or05,or08,or09ef,or10ef,or11ef),
        custom.coef.names = coef.names01,
        caption.above = TRUE, 
        caption = tab.caption01,
        custom.model.names = paste0("Modelo ",c(1:9)),
        omit.coef = omit.coef01,
        file = "output/tables/tab.models01.tex")

# Modelos: Inteligencia y habilidades -------------------------------------

or01tal  <- polr(mtalent3 ~inc10h.imp,data= elsoc)
or02tal  <- polr(mtalent3 ~edcine2,data= elsoc)
or03tal  <- polr(mtalent3 ~egp05,data= elsoc)
or04tal  <- polr(mtalent3 ~inc10h.imp+edcine2+egp05,data= elsoc,Hess = TRUE)
or05tal  <- polr(mtalent3 ~ess,data= elsoc,Hess = TRUE)
or08tal  <- polr(mtalent3 ~inc10h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+factor(estlab),data= elsoc,Hess = TRUE,weights = ipw01)

coeftest(or08tal)
coeftest(or08talb)

screenreg(l = list(or08tal,or08talb),stars = c(0.001,0.01,0.05,0.1))

or09tal <- polr(mtalent3 ~inc10h.imp+edcine2+egp05+ess+D10imp*ess+edad+sexo+ppolcat+factor(estlab), data= elsoc,Hess = TRUE,weights = ipw01)
or10tal <- polr(mtalent3 ~inc10h.imp+edcine2+egp05+ess+edcine2*ess+edad+sexo+ppolcat+factor(estlab),data= elsoc,Hess = TRUE,weights = ipw01)
or11tal <- polr(mtalent3 ~inc10h.imp+edcine2+egp05+ess+egp05*ess+edad+sexo+ppolcat+factor(estlab),  data= elsoc,Hess = TRUE,weights = ipw01)

coeftest(or09tal)
coeftest(or10tal)
coeftest(or11tal)

screenreg(l = list(or09tal,or10tal,or11tal),omit.coef = "(D10No responde)|(egp06missing)|(edad)|(sexoMujer)|(ppolcatCentro)|(ppolcatDerecha/Centro Derecha)|(ppolcatIndependiente)|(ppolcatNinguno)")

coef.names02 <-c("Decil Ingreso",
                 "Primaria y secundaria baja",
                 "Secundaria alta",
                 "Terciaria ciclo corto",
                 "Terciaria y Postgrado",
                 "Obrero calificado (V+VI)",
                 "Autoempleo (IVab+IVc)",
                 "Rutinas no manuales(III)",
                 "Servicio (I+II)", 
                 "Estatus Subjetivo",
                 "Jornada parcial",
                 "Estudia y trabaja",
                 "Decil Ingreso x Est. Subj",
                 "Primaria y secundaria baja x Est. Subj",
                 "Secundaria alta x Est. Subj",
                 "Terciaria ciclo corto x Est. Subj",
                 "Terciaria y Postgrado x Est. Subj",
                 "Obrero calificado (V+VI) x Est. Subj",
                 "Autoempleo(IVab+IVc) x Est. Subj",
                 "Rutinas no manuales(III) x Est. Subj",
                 "Servicio (I+II) x Est. Subj") 

tab.caption02 <- "Modelo regresión logística ordinal para Percepción de Meritocracia - Talento"
omit.coef02 <- c("(edad)|(sexoMujer)|(ppolcatCentro)|(ppolcatDerecha/Centro Derecha)|(ppolcatIndependiente)|(ppolcatNinguno)")

htmlreg(l = list(or01tal,or02tal,or03tal,or04tal,or05tal,or08tal,or09tal,or10tal,or11tal),
        custom.coef.names = coef.names02,
        caption.above = TRUE, 
        caption = tab.caption02,
        custom.model.names = paste0("Modelo ",c(1:9)),
        omit.coef = omit.coef02,
        file = "output/tables/tab.models02.html")

texreg(l = list(or01tal,or02tal,or03tal,or04tal,or05tal,or08tal,or09tal,or10tal,or11tal),
        custom.coef.names = coef.names02,
        caption.above = TRUE, 
        caption = tab.caption02,
        custom.model.names = paste0("Modelo ",c(1:9)),
        omit.coef = omit.coef02,
        file = "output/tables/tab.models02.tex")


# OLS con variable sumatoria de meritocracia ------------------------------

ols0a  <- lm_robust(recompe ~inc05h.imp++edad+sexo+ppolcat+factor(estlab),data= elsoc)
ols0b  <- lm_robust(recompe ~edcine2+edad+sexo+ppolcat+factor(estlab),data= elsoc)
ols0c  <- lm_robust(recompe ~egp05+edad+sexo+ppolcat+factor(estlab),data= elsoc)
ols0d  <- lm_robust(recompe ~ess+edad+sexo+ppolcat+factor(estlab),data= elsoc)


screenreg(l = list(ols0a,ols0b,ols0c,ols0d), include.ci = FALSE,
          omit.coef = "(D10No responde)|(egp06missing)|(edad)|(sexoMujer)|(ppolcatCentro)|(ppolcatDerecha/Centro Derecha)|(ppolcatIndependiente)|(ppolcatNinguno)")



ols01  <- lm_robust(recompe ~inc05h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+factor(estlab),data= elsoc)
ols02  <- lm_robust(recompe ~inc05h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+factor(estlab)+inc05h.imp*ess,data= elsoc)
ols03  <- lm_robust(recompe ~inc05h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+factor(estlab)+edcine2*ess,data= elsoc)
ols04  <- lm_robust(as.numeric(meffort) ~inc05h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+factor(estlab)+egp05*ess,data= elsoc)

screenreg(l = list(ols01,ols02,ols03,ols04), include.ci = FALSE,
          omit.coef = "(D10No responde)|(egp06missing)|(edad)|(sexoMujer)|(ppolcatCentro)|(ppolcatDerecha/Centro Derecha)|(ppolcatIndependiente)|(ppolcatNinguno)")


# 03 Coefplots ------------------------------------------------------------

coef   <- c("Decil Ingreso",
            "Primaria y secundaria baja",
            "Secundaria alta",
            "Terciaria ciclo corto",
            "Terciaria y Postgrado",
            "Obrero calificado (V+VI)",
            "Auto Empleo(IVab+IVc)",
            "Rutinas no manuales(III)",
            "Servicio I+II",
            "Estatus Subjetivo")

# 03.1 Coef plot: Esfuerzo  ----------------------------------------------------#


coef01 <- plot_model(model = or08,rm.terms = c("edad","sexoMujer",
                                               "ppolcatCentro","ppolcatDerecha/Centro Derecha",
                                               "ppolcatIndependiente","ppolcatNinguno",
                                               "Desacuerdo|Ni desacuerdo, Ni de acuerdo",
                                               "Ni desacuerdo, Ni de acuerdo|De acuerdo",
                                               "factor(estlab)jornada_parcial",
                                               "factor(estlab)estudia_trabaja"),
                     order.terms = c(1:10),
                     show.values = TRUE,show.p = TRUE,
                     vline.color = "black",
                     title = "En Chile se recompensa: Esfuerzo") +
  theme_bw() +
  scale_x_discrete(labels=rev(coef)) +
  scale_y_continuous(limits=c(0.1,2.0))+
  theme(strip.text.x     = element_text(size   = 18, face = "bold"),
        axis.title       = element_text(size  = 12,face   = "plain"),
        axis.text.x      = element_text(size  = 10,face   = "plain"),
        axis.text.y      = element_text(size  = 12,face   = "bold",hjust = 0),
        plot.title       = element_text(size  = 15,face   = "bold",hjust = 0.5))
coef01
ggsave(coef01,filename = "output/images/coef01.png",device = "png",width = 20,height = 20,dpi = "retina",units = "cm")
# 03.1 Coef plot: Talento  ----------------------------------------------------#

coef02 <- plot_model(model = or08tal,
                     rm.terms = c("edad","sexoMujer",
                                  "ppolcatCentro","ppolcatDerecha/Centro Derecha",
                                  "ppolcatIndependiente","ppolcatNinguno",
                                  "Desacuerdo|Ni desacuerdo, Ni de acuerdo",
                                  "Ni desacuerdo, Ni de acuerdo|De acuerdo"),
                     order.terms = c(1:10),
                     show.values = TRUE,show.p = TRUE,
                     vline.color = "black",
                     title = "En Chile se recompensa: Inteligencia y Habilidades") +
  theme_bw() +
  scale_x_discrete(labels=rev(coef)) +
  scale_y_continuous(limits=c(0.1,2.0))+
  theme(strip.text.x     = element_text(size   = 18, face = "bold"),
        axis.title       = element_text(size  = 12,face   = "plain"),
        axis.text.x      = element_text(size  = 10,face   = "plain"),
        axis.text.y      = element_text(size  = 12,face   = "bold",hjust = 0),
        plot.title       = element_text(size  = 15,face   = "bold",hjust = 0.5))
coef02
ggsave(coef02,filename = "output/images/coef02.png",device = "png",width = 20,height = 20,dpi = "retina",units = "cm")



# 04: Plots Interacciones---------------------------------------------------------------

# -1.6437172	2
#
# -0.9783501	3
#
# 1.017751	6

# A: Ingreso x Estatus Subjetivo ----------------------------------------

limits <- "ess [2,8]"

plot01<- plot_model(or09ef, type = "pred", terms = c("D10imp", limits),
                    legend.title = "Estatus social subjetivo",
                    title = "En Chile se recompensa: Esfuerzo") +
  geom_line() +
  theme_classic() +
  ylab(label = NULL)+
  xlab(label = "Decil de Ingreso")+
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(legend.position=c(0.06, 0.05),
        strip.text.x      = element_text(size   = 15, face = "bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title            = element_text(size  = 18,face   = "bold",hjust = 0.5))
plot01
ggsave(plot01,filename = "presentaciones/images/int01.png",device = "png",width = 40,height = 20,dpi = "retina",units = "cm")

plot02 <- plot_model(or09tal, type = "pred", terms = c("D10imp", limits),legend.title = "Estatus social subjetivo",
           title = "En Chile se recompensa: Inteligencia y Habilidades") +
  geom_line() +
  theme_classic() +
  ylab(label = NULL)+
  xlab(label = "Decil de Ingreso")+
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(legend.position=c(0.06, 0.05),
        strip.text.x      = element_text(size   = 15, face = "bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title            = element_text(size  = 18,face   = "bold",hjust = 0.5))
plot02
ggsave(plot02,filename = "output/images//int02.png",device = "png",width = 40,height = 20,dpi = "retina",units = "cm")


# B: Educación x Estatus Subjetivo ----------------------------------------

lbeduc <- c("Primaria incompleta \n  o  menos","Primaria y \n secundaria baja", "Secundaria \n alta","Terciaria \n ciclo corto","Terciaria \n y Postgrado")

plot03 <- plot_model(or10ef, type = "pred", terms = c("edcine2", limits),legend.title = "Estatus social subjetivo",
           title = "En Chile se recompensa: Esfuerzo") +
  geom_line() +
  theme_classic() +
  ylab(label = NULL)+
  xlab(label = "Nivel Educacional")+
  scale_x_discrete(name=NULL,limits=1:5, labels = lbeduc) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(legend.position=c(0.06, 0.05),
        strip.text.x      = element_text(size   = 15, face = "bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title            = element_text(size  = 18,face   = "bold",hjust = 0.5))

plot03
ggsave(plot03,filename = "output/images/int03.png",device = "png",width = 40,height = 20,dpi = "retina",units = "cm")


plot04 <- plot_model(or10tal, type = "pred", terms = c("edcine2", limits),legend.title = "Estatus social subjetivo",
           title = "En Chile se recompensa: Inteligencia y Habilidades") +
  geom_line() +
  theme_classic()+
  ylab(label = NULL)+
  xlab(label = "Nivel Educacional")+
  scale_x_discrete(name=NULL,limits=1:5, labels = lbeduc) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                              direction = "horizontal",
                                              title.hjust = 0.5)) +
    theme(legend.position=c(0.06, 0.05),
          strip.text.x      = element_text(size   = 15, face = "bold"),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          plot.title            = element_text(size  = 18,face   = "bold",hjust = 0.5))
plot04
ggsave(plot04,filename = "output/images/int04.png",device = "png",width = 40,height = 20,dpi = "retina",units = "cm")


# C: Clase x Estatus Subjetivo ----------------------------------------

labegp<- c("Obrero \n no calificado \n (VIIa+VIIb)","Obrero \n calificado \n (V+VI)","Auto Empleo \n (IVab+IVc)","Rutinas \n no manuales \n (III)","Servicio \n (I+II)")

plot05 <- plot_model(or11ef, type = "pred", terms = c("egp05", limits),legend.title = "Estatus social subjetivo",
           title = "En Chile se recompensa: Esfuerzo") +
  geom_line() +
  theme_classic() +
  ylab(label = NULL)+
  xlab(label = "Clase Social")+
  scale_x_discrete(name=NULL,limits=1:5, labels=labegp)+
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(legend.position=c(0.06, 0.05),
        strip.text.x      = element_text(size   = 15, face = "bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title            = element_text(size  = 18,face   = "bold",hjust = 0.5))
plot05
ggsave(plot05,filename = "presentaciones/images/int05.png",device = "png",width = 40,height = 20,dpi = "retina",units = "cm")


plot06 <- plot_model(or11tal, type = "pred", terms = c("egp05", limits),legend.title = "Estatus social subjetivo",
           title = "En Chile se recompensa: Inteligencia y Habilidades") +
  geom_line() +
  theme_classic() +
  scale_x_discrete(name=NULL,limits=1:5, labels=labegp)+
  ylab(label = NULL)+
  xlab(label = "Clase Social")+
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(legend.position=c(0.06, 0.05),
        strip.text.x      = element_text(size   = 15, face = "bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title            = element_text(size  = 18,face   = "bold",hjust = 0.5))
plot06
ggsave(plot06,filename = "output/images/int06.png",device = "png",width = 40,height = 20,dpi = "retina",units = "cm")


# Interacciones: Plots juntos ---------------------------------------------

plot01<- plot_model(or09ef, type = "pred", terms = c("D10imp", limits),
                    legend.title = "Estatus social subjetivo",
                    title = "En Chile se recompensa: Esfuerzo") +
  geom_line() +
  theme_classic() +
  ylab(label = NULL)+
  xlab(label = NULL)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10),
                     labels = c("D01","D02","D03","D04","D05",
                                "D06","D07","D08","D09","D10"))+
  scale_color_discrete(guide = guide_legend(title.position = "right",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(legend.position=c(0.9, 1.2),
        strip.text.x      = element_text(size   = 12, face = "bold"),
        legend.background = element_blank(),
        plot.title            = element_text(size  = 15,face   = "bold",hjust = 0.5))
plot01
plot03 <- plot_model(or10ef, type = "pred", terms = c("edcine2", limits),legend.title = "Estatus social subjetivo",
                     title = "") +
  geom_line() +
  theme_classic() +
  ylab(label = NULL)+
  xlab(label = NULL)+
  guides(color=FALSE)+
  scale_x_discrete(name=NULL,limits=1:5, labels = lbeduc) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(legend.position=c(0.06, 0.05),
        strip.text.x      = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title            = element_text(size  = 18,face   = "bold",hjust = 0.5))
plot03
plot05 <- plot_model(or11ef, type = "pred", terms = c("egp05", limits),
                     legend.title = "Estatus social subjetivo",
                     title = "") +
  geom_line() +
  theme_classic() +
  ylab(label = NULL)+
  xlab(label = NULL)+
  guides(color=FALSE)+
  scale_x_discrete(name=NULL, limits=1:5, labels=labegp) +
  scale_color_discrete() +
  theme(strip.text.x      = element_blank(),
        legend.position   = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title            = element_text(size  = 18,face   = "bold",hjust = 0.5))
plot05
grid.arrange(plot01,plot03,plot05,ncol=1)
ggsave(grid.arrange(plot01,plot03,plot05,ncol=1),
       filename = "output/images/intef01.png",
       device = "png",
       width = 40,height = 23,dpi = "retina",units = "cm")

# -------------------------------------------------------------------------

plot02<- plot_model(or09tal, type = "pred", terms = c("D10imp", limits),
                    legend.title = "Estatus social subjetivo",
                    title = "En Chile se recompensa: Inteligencia y habilidades") +
  geom_line() +
  theme_classic() +
  ylab(label = NULL)+
  xlab(label = NULL)+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10),labels = c("D01","D02","D03","D04","D05",
                                                                 "D06","D07","D08","D09","D10"))+
  scale_color_discrete(guide = guide_legend(title.position = "right",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(legend.position=c(0.9, 1.2),
        strip.text.x      = element_text(size   = 12, face = "bold"),
        legend.background = element_blank(),
        plot.title            = element_text(size  = 15,face   = "bold",hjust = 0.5))

plot04 <- plot_model(or10tal, type = "pred", terms = c("edcine2", limits),
                     legend.title = "Estatus social subjetivo",
                     title = "") +
  geom_line() +
  theme_classic() +
  ylab(label = NULL)+
  xlab(label = NULL)+
  guides(color=FALSE)+
  scale_x_discrete(name=NULL,limits=1:5, labels = lbeduc) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(legend.position=c(0.06, 0.05),
        strip.text.x      = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title            = element_text(size  = 18,face   = "bold",hjust = 0.5))

plot06 <- plot_model(or11tal, type = "pred", terms = c("egp05", limits),
                     legend.title = "Estatus social subjetivo",
                     title = "") +
  geom_line() +
  theme_classic() +
  ylab(label = NULL)+
  xlab(label = NULL)+
  guides(color=FALSE)+
  scale_x_discrete(name=NULL,limits=1:5, labels=labegp) +
  scale_color_discrete() +
  theme(strip.text.x      = element_blank(),
        legend.position   = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title            = element_text(size  = 18,face   = "bold",hjust = 0.5))


grid.arrange(plot02,plot04,plot06,ncol=1)
ggsave(grid.arrange(plot02,plot04,plot06,ncol=1),filename = "output/images/inttal01.png",
       device = "png",
       width  = 40,
       height = 23,dpi = "retina",units = "cm")


# Efectos marginales  :----------------------------------

# https://www.sociologicalscience.com/download/vol-6/february/SocSci_v6_81to117.pdf

# interplot(m = or09ef,var1 = "D10",var2 = "ess")     + geom_hline(yintercept=0,color="red")
# interplot(m = or10ef,var1 = "edcine2",var2 = "ess") + geom_hline(yintercept=0,color="red")
# interplot(m = or11ef,var1 = "egp05",var2 = "ess")   + geom_hline(yintercept=0,color="red")
#
# interplot(m = or09tal,var1 = "D10",var2 = "ess")     + geom_hline(yintercept=0,color="red")
# interplot(m = or10tal,var1 = "edcine2",var2 = "ess") + geom_hline(yintercept=0,color="red")
# interplot(m = or11tal,var1 = "egp05",var2 = "ess")   + geom_hline(yintercept=0,color="red")
#
