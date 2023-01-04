
```{r ols-effort, eval=FALSE, include=FALSE, results='asis'}
load(here::here("input/data/proc/elsoc_w03.RData"))
elsoc_w03 <- na.omit(elsoc_w03)
pacman::p_load(ordinal,texreg,car)
elsoc_w03$ess <- car::recode(elsoc_w03$ess+1,recodes = "10:11=10")
elsoc_w03$meffort <- as.numeric(elsoc_w03$meffort)
or01   <- lm(meffort ~inc10h.imp,data= elsoc_w03)
or02   <- lm(meffort ~edcine2,data= elsoc_w03)
or03   <- lm(meffort ~egp05,data= elsoc_w03)
or04   <- lm(meffort ~inc10h.imp+edcine2+egp05,data= elsoc_w03)
or05   <- lm(meffort ~ess,data= elsoc_w03)
or08   <- lm(meffort ~inc10h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+lngap_perc,data= elsoc_w03)
or09ef <- lm(meffort ~inc10h.imp+edcine2+egp05+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data=elsoc_w03)
or10ef <- lm(meffort ~inc10h.imp+edcine2+egp05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data=elsoc_w03)
or11ef <- lm(meffort ~inc10h.imp+edcine2+egp05+ess+egp05*ess+edad+sexo+ppolcat+lngap_perc,  data=elsoc_w03)
coef.names01 <-c("Household Income Decile",
                 "Primary and Lower secondary",
                 "Upper secondary",
                 "Short-cycle tertiary",
                 "Tertiary or higher",
                 "Skilled Worker (V+VI)",
                 "Self-Employed (IVab+IVc)",
                 "Routine Non-manual (III)",
                 "Service (I+II)",
                 "Subjective Social Status (SSS) ",
                 "Int. (Income Decile x Subj. Status.)",
                 "Primary and Lower secondary x SSS",
                 "Upper secondary x SSS",
                 "Short-cycle tertiary x SSS",
                 "Tertiary or higher x SSS",
                 "Skilled Worker (V+VI) x SSS",
                 "Self-Employed (IVab+IVc) x SSS",
                 "Routine Non-manual (III) x SSS",
                 "Service (I+II) x SSS")
tab.caption01 <- "Linear Regression Model for Perception of Meritocracy (Effort)"
omit.coef01 <- c("((Intercept))|(edad)|(sexo)|(ppolcat)|(ppolcat)|(lngap_perc)|(estlab)")

knitreg(l = list(or01,or02,or03,or05,or08,or09ef,or10ef,or11ef),
        # custom.coef.names = coef.names01,
        caption.above = TRUE,
        caption = tab.caption01,
        custom.model.names = paste0("Model ",c(1:8)),
        omit.coef = omit.coef01,
        scalebox = 1.0,    
        groups = list("Education (ref: Incomplete Primary or lower)" = 2:5,
                      "Social Class (ref: Unskilled worker)" =6:9,
                      "Int. (Education x Subj. Status.)" =12:15,
                      "Int. (Class x Subj. Status.)" =16:19)
)
```

```{r marginal-effort, eval=FALSE, fig.cap='Marginal effects and predictions for the interaction effect between objective an subjective status on perceived meritocracy (Effort)', fig.height=11, fig.width=20, include=FALSE}
load(here::here("input/data/proc/elsoc_w03.RData"))
elsoc_w03 <- na.omit(elsoc_w03)
pacman::p_load(interplot,sjPlot)
elsoc_w03$meffort <- as.numeric(elsoc_w03$meffort)
elsoc_w03$ess <- car::recode(elsoc_w03$ess+1,recodes = "10:11=10")
or09ef <- lm(meffort ~inc10h.imp+edcine2+egp05+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data=elsoc_w03)
or10ef <- lm(meffort ~inc10h.imp+edcine2+egp05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data=elsoc_w03)
or11ef <- lm(meffort ~inc10h.imp+edcine2+egp05+ess+egp05*ess+edad+sexo+ppolcat+lngap_perc,  data=elsoc_w03)

# Income......................................................................
marg09ef<- interplot(m = or09ef, var1 = "inc10h.imp", var2 = "ess",point = T) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name = NULL,
                     breaks = 1:10,labels = c("Bottom", 2:9,"Top")) +
  theme(axis.text=element_text(size=18))

pred09ef<-
  plot_model(model = or09ef,
             type = "int",
             legend.title = "Subj. Status") +
  labs(title =NULL) + ylab(label = NULL) +
  scale_x_continuous(name = "Income Decile",
                     breaks = 1:10,labels = paste0("D",1:10)) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(legend.position=c(0.1, 0.15),
        axis.text=element_text(size=15),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
# Education...................................................................
lbeduc <- c("Incomplete Primary \n  or  less","Primary & \n Lower secondary",
            "Upper \n secondary ","Short-cycle \n tertiary","Tertiary \n or higher")

marg10ef<- interplot(m = or10ef, var1 = "edcine2", var2 = "ess", point = T) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name = NULL,
                     breaks = 1:10,labels = c("B", 2:9,"T")) +
  theme(strip.text.x = element_text(size = 15),
        axis.text=element_text(size=17))

pred10ef<- plot_model(model = or10ef,
                      type = "int",
                      legend.title = "Subj. Status") +
  labs(title =NULL) +  geom_line() + ylab(label = NULL)+
  scale_x_discrete(name=NULL,limits=1:5, labels = lbeduc) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(
    legend.position=c(0.1, 0.15),
    axis.text=element_text(size=15),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"))
#Social Class..................................................................
labsegp <- c("Unskilled worker \n(VIIa+VIIb)",
             "Skilled Worker \n (V+VI)",
             "Self-Employment\n (IVab+IVc)",
             "Routine \n  Non-manual (III)",
             "Service \n (I+II)")
marg11ef<- interplot(m = or11ef, var1 = "egp05", var2 = "ess", point = T,
                     facet_labs = c("Skilled Worker \n (V+VI)",
                                    "Self-Employment\n (IVab+IVc)",
                                    "Routine \n  Non-manual (III)",
                                    "Service \n (I+II)") ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name = NULL,
                     breaks = 1:10,labels = c("B", 2:9,"T")) +
  theme(strip.text.x = element_text(size = 15),
        axis.text=element_text(size=17))

pred11ef <-
  plot_model(model = or11ef,
             type = "int",
             legend.title = "Subj. Status") +
  labs(title =NULL) +
  geom_line() +
  ylab(label = NULL) +
  scale_x_discrete(name=NULL,limits=1:5, labels = labsegp) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(
    legend.position=c(0.1, 0.15),
    axis.text=element_text(size=15),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"))

gridExtra::grid.arrange(marg09ef, pred09ef,
                        marg10ef, pred10ef,
                        marg11ef, pred11ef,
                        nrow = 3)
```

```{r ols-talent, eval=FALSE, include=FALSE, results='asis'}
load(here::here("input/data/proc/elsoc_w03.RData"))
elsoc_w03 <- na.omit(elsoc_w03)
pacman::p_load(ordinal,texreg)
elsoc_w03$mtalent <- as.numeric(elsoc_w03$mtalent)
or01tal  <- lm(mtalent ~inc10h.imp,data= elsoc_w03)
or02tal  <- lm(mtalent ~edcine2,data= elsoc_w03)
or03tal  <- lm(mtalent ~egp05,data= elsoc_w03)
or04tal  <- lm(mtalent ~inc10h.imp+edcine2+egp05,data= elsoc_w03)
or05tal  <- lm(mtalent ~ess,data= elsoc_w03)
or08tal  <- lm(mtalent ~inc10h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+lngap_perc,data= elsoc_w03)
or09tal  <- lm(mtalent ~inc10h.imp+edcine2+egp05+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= elsoc_w03)
or10tal  <- lm(mtalent ~inc10h.imp+edcine2+egp05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= elsoc_w03)
or11tal  <- lm(mtalent ~inc10h.imp+edcine2+egp05+ess+egp05*ess+edad+sexo+ppolcat+lngap_perc,  data= elsoc_w03)

coef.names02 <-c("Household Income Decile",
                 "Primary and Lower secondary",
                 "Upper secondary",
                 "Short-cycle tertiary",
                 "Tertiary or higher",
                 "Skilled Worker (V+VI)",
                 "Self-Employed (IVab+IVc)",
                 "Routine Non-manual (III)",
                 "Service (I+II)",
                 "Subjective Social Status (SSS) ",
                 "Int. (Income Decile x Subj. Status.)",
                 "Primary and Lower secondary x SSS",
                 "Upper secondary x SSS",
                 "Short-cycle tertiary x SSS",
                 "Tertiary or higher x SSS",
                 "Skilled Worker (V+VI) x SSS",
                 "Self-Employed (IVab+IVc) x SSS",
                 "Routine Non-manual (III) x SSS",
                 "Service (I+II) x SSS")

tab.caption02 <- "Linear Regression Model for Perception of Meritocracy (Talent)"
omit.coef02 <- c("((Intercept))|(edad)|(sexo)|(ppolcat)|(ppolcat)|(lngap_perc)|(estlab)")

knitreg(l = list(or01tal,or02tal,or03tal,or05tal,or08tal,or09tal,or10tal,or11tal),
        include.thresholds = FALSE,
        custom.coef.names = coef.names02,
        caption.above = TRUE,
        caption = tab.caption02,
        custom.model.names = paste0("Model ",c(1:8)),
        omit.coef = omit.coef02,
        scalebox = 0.75,
        groups = list("Education (ref: Incomplete Primary or lower)" = 2:5,
                      "Social Class (ref: Unskilled worker)" =6:9,
                      "Int. (Education x Subj. Status.)" =12:15,
                      "Int. (Class x Subj. Status.)" =16:19)
)
```

```{r marginal-talent, eval=FALSE, fig.cap='Marginal effects and predictions for the interaction effect between objective an subjective status on perceived meritocracy (Talent)', fig.height=11, fig.width=20, include=FALSE}
load(here::here("input/data/proc/elsoc_w03.RData"))
elsoc_w03 <- na.omit(elsoc_w03)
pacman::p_load(ordinal,texreg)
elsoc_w03$mtalent <- as.numeric(elsoc_w03$mtalent)
or09tal  <- lm(mtalent ~inc10h.imp+edcine2+egp05+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= elsoc_w03)
or10tal  <- lm(mtalent ~inc10h.imp+edcine2+egp05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= elsoc_w03)
or11tal  <- lm(mtalent ~inc10h.imp+edcine2+egp05+ess+egp05*ess+edad+sexo+ppolcat+lngap_perc,  data= elsoc_w03)

# Income......................................................................
marg09tal<- interplot(m = or09tal, var1 = "inc10h.imp", var2 = "ess",point = T) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name = NULL,
                     breaks = 0:10,labels = c("Bottom", 1:9,"Top")) +
  theme(axis.text=element_text(size=18))

pred09tal<-
  plot_model(model = or09tal,
             type = "int",
             legend.title = "Subj. Status") +
  labs(title =NULL) + ylab(label = NULL) +
  scale_x_continuous(name = "Income Decile",
                     breaks = 1:10,labels = paste0("D",1:10)) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(legend.position=c(0.1, 0.15),
        axis.text=element_text(size=15),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
# Education...................................................................
lbeduc <- c("Incomplete Primary \n  or  less","Primary & \n Lower secondary",
            "Upper \n secondary ","Short-cycle \n tertiary","Tertiary \n or higher")

marg10tal<- interplot(m = or10tal, var1 = "edcine2", var2 = "ess", point = T) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name = NULL,
                     breaks = 0:10,labels = c("B", 1:9,"T")) +
  theme(strip.text.x = element_text(size = 15),
        axis.text=element_text(size=17))

pred10tal<- plot_model(model = or10tal,
                       type = "int",
                       legend.title = "Subj. Status") +
  labs(title =NULL) +  geom_line() + ylab(label = NULL)+
  scale_x_discrete(name=NULL,limits=1:5, labels = lbeduc) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(
    legend.position=c(0.1, 0.15),
    axis.text=element_text(size=15),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"))
#Social Class..................................................................
labsegp <- c("Unskilled worker \n(VIIa+VIIb)",
             "Skilled Worker \n (V+VI)",
             "Self-Employment\n (IVab+IVc)",
             "Routine \n  Non-manual (III)",
             "Service \n (I+II)")
marg11tal<- interplot(m = or11tal, var1 = "egp05", var2 = "ess", point = T,
                      facet_labs = c("Skilled Worker \n (V+VI) ",
                                     "Self-employed \n (VIIa+VIIb)",
                                     "Routine Non-manual\n (III) ",
                                     "Service \n (I+II)")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name = NULL,
                     breaks = 0:10,labels = c("B", 1:9,"T")) +
  theme(strip.text.x = element_text(size = 15),
        axis.text=element_text(size=17))

pred11tal <-
  plot_model(model = or11tal,
             type = "int",
             legend.title = "Subj. Status") +
  labs(title =NULL) +
  geom_line() +
  ylab(label = NULL) +
  scale_x_discrete(name=NULL,limits=1:5, labels = labsegp) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(
    legend.position=c(0.1, 0.15),
    axis.text=element_text(size=15),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"))

gridExtra::grid.arrange(marg09tal, pred09tal,
                        marg10tal, pred10tal,
                        marg11tal, pred11tal,
                        nrow = 3)

```

```{r models-effort3, eval=FALSE, include=FALSE, results='asis'}
load(here::here("input/data/proc/elsoc_w03.RData"))
elsoc_w03 <- na.omit(elsoc_w03)
pacman::p_load(ordinal,texreg)
or01  <- ordinal::clm(meffort3 ~inc10h.imp,data= elsoc_w03, Hess = TRUE)
or02  <- ordinal::clm(meffort3 ~edcine2,data= elsoc_w03, Hess = TRUE)
or03  <- ordinal::clm(meffort3 ~egp05,data= elsoc_w03, Hess = TRUE)
or04  <- ordinal::clm(meffort3 ~inc10h.imp+edcine2+egp05,data= elsoc_w03, Hess = TRUE)
or05  <- ordinal::clm(meffort3 ~ess,data= elsoc_w03, Hess = TRUE)
or08  <- ordinal::clm(meffort3 ~inc10h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+lngap_perc,data= elsoc_w03, Hess = TRUE)
or09ef <- ordinal::clm(meffort3 ~inc10h.imp+edcine2+egp05+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data=elsoc_w03,Hess = TRUE)
or10ef <- ordinal::clm(meffort3 ~inc10h.imp+edcine2+egp05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data=elsoc_w03,Hess = TRUE)
or11ef <- ordinal::clm(meffort3 ~inc10h.imp+edcine2+egp05+ess+egp05*ess+edad+sexo+ppolcat+lngap_perc,  data=elsoc_w03,Hess = TRUE)

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
                 # "Jornada parcial",
                 # "Estudia y trabaja",
                 "Decil Ingreso x Est. Subj",
                 "Primaria y secundaria baja x Est. Subj",
                 "Secundaria alta x Est. Subj",
                 "Terciaria ciclo corto x Est. Subj",
                 "Terciaria y Postgrado x Est. Subj",
                 "Obrero calificado (V+VI) x Est. Subj",
                 "Autoempleo(IVab+IVc) x Est. Subj",
                 "Rutinas no manuales(III) x Est. Subj",
                 "Servicio (I+II) x Est. Subj")

tab.caption01 <- "Modelo regresión logística ordinal para Percepción de Meritocracia - Esfuerzo (3)"
omit.coef01 <- c("(edad)|(sexoMujer)|(ppolcatCentro)|(ppolcatDerecha/Centro Derecha)|(ppolcatIndependiente)|(ppolcatNinguno)|(lngap_perc)|(estlab)")

knitreg(l = list(or01,or02,or03,or05,or08,or09ef,or10ef,or11ef),
        include.thresholds = FALSE,
        custom.coef.names = coef.names01,
        caption.above = TRUE,
        caption = tab.caption01,
        custom.model.names = paste0("Modelo ",c(1:8)),
        omit.coef = omit.coef01,
        scalebox = 0.75)
```

```{r models-effort5, eval=FALSE, include=FALSE, results='asis'}
load(here::here("input/data/proc/elsoc_w03.RData"))
elsoc_w03 <- na.omit(elsoc_w03)
pacman::p_load(ordinal,texreg)
or01  <- ordinal::clm(meffort ~inc10h.imp,data= elsoc_w03, Hess = TRUE)
or02  <- ordinal::clm(meffort ~edcine2,data= elsoc_w03, Hess = TRUE)
or03  <- ordinal::clm(meffort ~egp05,data= elsoc_w03, Hess = TRUE)
or04  <- ordinal::clm(meffort ~inc10h.imp+edcine2+egp05,data= elsoc_w03, Hess = TRUE)
or05  <- ordinal::clm(meffort ~ess,data= elsoc_w03, Hess = TRUE)
or08  <- ordinal::clm(meffort ~inc10h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+lngap_perc,data= elsoc_w03, Hess = TRUE)
or09ef <- ordinal::clm(meffort ~inc10h.imp+edcine2+egp05+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data=elsoc_w03,Hess = TRUE)
or10ef <- ordinal::clm(meffort ~inc10h.imp+edcine2+egp05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data=elsoc_w03,Hess = TRUE)
or11ef <- ordinal::clm(meffort ~inc10h.imp+edcine2+egp05+ess+egp05*ess+edad+sexo+ppolcat+lngap_perc,  data=elsoc_w03,Hess = TRUE)

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
                 # "Jornada parcial",
                 # "Estudia y trabaja",
                 "Decil Ingreso x Est. Subj",
                 "Primaria y secundaria baja x Est. Subj",
                 "Secundaria alta x Est. Subj",
                 "Terciaria ciclo corto x Est. Subj",
                 "Terciaria y Postgrado x Est. Subj",
                 "Obrero calificado (V+VI) x Est. Subj",
                 "Autoempleo(IVab+IVc) x Est. Subj",
                 "Rutinas no manuales(III) x Est. Subj",
                 "Servicio (I+II) x Est. Subj")

tab.caption01 <- "Modelo regresión logística ordinal para Percepción de Meritocracia - Esfuerzo (5)"
omit.coef01 <- c("((Intercept))|(edad)|(sexo)|(ppolcat)|(ppolcat)|(lngap_perc)|(estlab)")

knitreg(l = list(or01,or02,or03,or05,or08,or09ef,or10ef,or11ef),
        include.thresholds = FALSE,
        custom.coef.names = coef.names01,
        caption.above = TRUE,
        caption = tab.caption01,
        custom.model.names = paste0("Modelo ",c(1:8)),
        omit.coef = omit.coef01,
        scalebox = 0.75)
```

```{r models-talent3, eval=FALSE, include=FALSE, results='asis'}
load(here::here("input/data/proc/elsoc_w03.RData"))
elsoc_w03 <- na.omit(elsoc_w03)
pacman::p_load(ordinal,texreg)
or01tal  <- ordinal::clm(mtalent3 ~inc10h.imp,data= elsoc_w03)
or02tal  <- ordinal::clm(mtalent3 ~edcine2,data= elsoc_w03)
or03tal  <- ordinal::clm(mtalent3 ~egp05,data= elsoc_w03)
or04tal  <- ordinal::clm(mtalent3 ~inc10h.imp+edcine2+egp05,data= elsoc_w03,Hess = TRUE)
or05tal  <- ordinal::clm(mtalent3 ~ess,data= elsoc_w03,Hess = TRUE)
or08tal  <- ordinal::clm(mtalent3 ~inc10h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+lngap_perc,data= elsoc_w03,Hess = TRUE)
or09tal  <- ordinal::clm(mtalent3 ~inc10h.imp+edcine2+egp05+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= elsoc_w03,Hess = TRUE)
or10tal  <- ordinal::clm(mtalent3 ~inc10h.imp+edcine2+egp05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= elsoc_w03,Hess = TRUE)
or11tal  <- ordinal::clm(mtalent3 ~inc10h.imp+edcine2+egp05+ess+egp05*ess+edad+sexo+ppolcat+lngap_perc,  data= elsoc_w03,Hess = TRUE)

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

tab.caption02 <- "Modelo regresión logística ordinal para Percepción de Meritocracia - Talento (3)"
omit.coef02 <- c("(edad)|(sexoMujer)|(ppolcatCentro)|(ppolcatDerecha/Centro Derecha)|(ppolcatIndependiente)|(ppolcatNinguno)|(lngap_perc)")

knitreg(l = list(or01tal,or02tal,or03tal,or05tal,or08tal,or09tal,or10tal,or11tal),
        include.thresholds = FALSE,
        custom.coef.names = coef.names02,
        caption.above = TRUE,
        caption = tab.caption02,
        custom.model.names = paste0("Modelo ",c(1:8)),
        omit.coef = omit.coef02,
        scalebox = 0.75)
```

```{r models-talent5, eval=FALSE, include=FALSE, results='asis'}
load(here::here("input/data/proc/elsoc_w03.RData"))
elsoc_w03 <- na.omit(elsoc_w03)
pacman::p_load(ordinal,texreg)
or01tal  <- ordinal::clm(mtalent ~inc10h.imp,data= elsoc_w03)
or02tal  <- ordinal::clm(mtalent ~edcine2,data= elsoc_w03)
or03tal  <- ordinal::clm(mtalent ~egp05,data= elsoc_w03)
or04tal  <- ordinal::clm(mtalent ~inc10h.imp+edcine2+egp05,data= elsoc_w03,Hess = TRUE)
or05tal  <- ordinal::clm(mtalent ~ess,data= elsoc_w03,Hess = TRUE)
or08tal  <- ordinal::clm(mtalent ~inc10h.imp+edcine2+egp05+ess+edad+sexo+ppolcat+lngap_perc,data= elsoc_w03,Hess = TRUE)
or09tal  <- ordinal::clm(mtalent ~inc10h.imp+edcine2+egp05+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= elsoc_w03,Hess = TRUE)
or10tal  <- ordinal::clm(mtalent ~inc10h.imp+edcine2+egp05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= elsoc_w03,Hess = TRUE)
or11tal  <- ordinal::clm(mtalent ~inc10h.imp+edcine2+egp05+ess+egp05*ess+edad+sexo+ppolcat+lngap_perc,  data= elsoc_w03,Hess = TRUE)

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
                 # "Jornada parcial",
                 # "Estudia y trabaja",
                 "Decil Ingreso x Est. Subj",
                 "Primaria y secundaria baja x Est. Subj",
                 "Secundaria alta x Est. Subj",
                 "Terciaria ciclo corto x Est. Subj",
                 "Terciaria y Postgrado x Est. Subj",
                 "Obrero calificado (V+VI) x Est. Subj",
                 "Autoempleo(IVab+IVc) x Est. Subj",
                 "Rutinas no manuales(III) x Est. Subj",
                 "Servicio (I+II) x Est. Subj")

tab.caption02 <- "Modelo regresión logística ordinal para Percepción de Meritocracia - Talento (5)"
omit.coef02 <- c("((Intercept))|(edad)|(sexo)|(ppolcat)|(ppolcat)|(lngap_perc)|(estlab)")

knitreg(l = list(or01tal,or02tal,or03tal,or05tal,or08tal,or09tal,or10tal,or11tal),
        include.thresholds = FALSE,
        custom.coef.names = coef.names02,
        caption.above = TRUE,
        caption = tab.caption02,
        custom.model.names = paste0("Modelo ",c(1:8)),
        omit.coef = omit.coef02,
        scalebox = 0.75)
```


```{r marginal-merit, fig.cap='Marginal effects for the interaction of subjective social status with income, education and social class on perceived meritocracy', fig.height=11, fig.width=11}
pacman::p_load(ordinal,texreg,interplot,margins,dplyr,sjPlot) 
load(here::here("input/data/proc/elsoc_w03.RData"))
elsoc_w03$merit <- (as.numeric(elsoc_w03$meffort)+as.numeric(elsoc_w03$mtalent))/2 
elsoc_w03<- elsoc_w03 %>%
  dplyr::select(merit,inc10h.imp,edcine2,egpisko05,ess,lngap_perc,
                sexo,edad,ppolcat) %>% 
  na.omit()

or09tal  <- lm(merit ~inc10h.imp+edcine2+egpisko05+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, 
               data= elsoc_w03)
or10tal  <- lm(merit ~inc10h.imp+edcine2+egpisko05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,
               data= elsoc_w03)
or11tal  <- lm(merit ~inc10h.imp+edcine2+egpisko05+ess+egpisko05*ess+edad+sexo+ppolcat+lngap_perc, 
               data= elsoc_w03)

# Income......................................................................
marg09tal<- interplot(m = or09tal, var1 = "inc10h.imp", var2 = "ess",point = T,esize = 1,hist = F) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(name = NULL,
                     breaks = 0:10,labels = c("Bottom", 1:9,"Top")) +
  # ylab("A")+
  theme(axis.text=element_text(size=18),
        axis.title.y=element_text(size = 20,face = "bold", angle=0,vjust = 1)) 

pred09tal<-
  plot_model(model = or09tal,
             type = "int",
             legend.title = "Subj. Status") +
  labs(title =NULL) + ylab(label = NULL) +
  scale_x_continuous(name = "Income Decile",
                     breaks = 1:10,labels = paste0("D",1:10)) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  
  theme(legend.position=c(0.1, 0.15),
        axis.text=element_text(size=15),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
# Education...................................................................
lbeduc <- c("Incomplete Primary \n  or  less","Primary & \n Lower secondary",
            "Upper \n secondary ","Short-cycle \n tertiary","Tertiary \n or higher")

marg10tal<- 
  interplot(m = or10tal, var1 = "edcine2", var2 = "ess", point = T,
            facet_labs = c("Primary & \n Lower secondary",
                           "Upper \n secondary ",
                           "Short-cycle \n tertiary","Tertiary \n or higher")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # ylab("B")+    
  scale_x_continuous(name = NULL,
                     breaks = 0:10,labels = c("B", 1:9,"T")) +
  theme(strip.text.x = element_text(size = 17),
        axis.text=element_text(size=17),
        axis.title.y=element_text(size = 20,face = "bold", angle=0,vjust = 1.2))

pred10tal<- plot_model(model = or10tal,
                       type = "int",
                       legend.title = "Subj. Status") +
  labs(title =NULL) +  geom_line() + ylab(label = NULL)+
  scale_x_discrete(name=NULL,limits=1:5, labels = lbeduc) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(
    legend.position=c(0.1, 0.15),
    axis.text=element_text(size=15),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"))
#Social Class..................................................................
labsegp <- c("Unskilled worker \n(VII.ab+IVc)",
             "Skilled Manual Worker  \n(V+VI)",
             "Self-Employment\n (IVab)",
             "Routine \n  Non-manual (III)",
             "Service \n (I+II)")

marg11tal<- interplot(m = or11tal, var1 = "egpisko05", var2 = "ess", point = T,
                      facet_labs = c("Skilled Manual Worker  \n (V+VI) ",
                                     "Self-Employment \n (IV.ab)",
                                     "Routine Non-manual\n (III) ",
                                     "Service \n (I+II)")
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # ylab("C")+        
  scale_x_continuous(name = NULL,
                     breaks = 0:10,labels = c("B", 1:9,"T")) +
  theme(strip.text.x = element_text(size = 17),
        axis.text=element_text(size=17),
        axis.title.y=element_text(size = 20,
                                  face = "bold",
                                  angle=0,
                                  vjust = 1.2));marg11tal$data$value <-  
  rev(marg11tal$data$value)
pred11tal <-
  plot_model(model = or11tal,
             type = "int",
             legend.title = "Subj. Status") +
  labs(title =NULL) +
  geom_line() +
  ylab(label = NULL) +
  scale_x_discrete(name=NULL,limits=1:5, labels = labsegp) +
  scale_color_discrete(guide = guide_legend(title.position = "top",
                                            direction = "horizontal",
                                            title.hjust = 0.5)) +
  theme(
    legend.position=c(0.1, 0.15),
    axis.text=element_text(size=15),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"))

cowplot::plot_grid(marg09tal,
                   pred09tal,
                   marg10tal, 
                   pred10tal,
                   marg11tal, 
                   pred11tal,
                   labels=c("A",
                            " ",
                            "B",
                            " ",
                            "C",
                            " "
                   ),
                   nrow = 3,label_size = 25)

cowplot::plot_grid(marg09tal,
                   pred09tal,
                   marg10tal, 
                   pred10tal,
                   marg11tal, 
                   pred11tal,
                   labels=c("A",
                            " ",
                            "B",
                            " ",
                            "C",
                            " "
                   ),
                   nrow = 3,label_size = 25)

cowplot::plot_grid(marg09tal,marg10tal,marg11tal,
                   pred09tal,pred10tal,pred11tal,
                   # labels=c("A"," ","B"," ","C"," "),
                   nrow = 2,label_size = 25)
```


