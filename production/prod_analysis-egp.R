load(here::here("input/data/proc/elsoc_w03.RData"))
# elsoc_w03 <- elsoc_w03 %>% dplyr::filter(!(egptorche == "Autoempleo agrícola (IVc)"),!(egptorche == "Labores agrícolas (VIIb)") ) 

elsoc_w03$merit <- (as.numeric(elsoc_w03$meffort)+as.numeric(elsoc_w03$mtalent))/2
pacman::p_load(ordinal,texreg,dplyr)
elsoc_w03$mtalent <- as.numeric(elsoc_w03$mtalent)

name_isko<- elsoc_w03 %>% select(starts_with("egp")) %>% names()
# for (i in name_isko) {
#   elsoc_w03[[i]] <- forcats::fct_rev(elsoc_w03[[i]])
#   
# }


df_egp7 <- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,edad,sexo,ppolcat,lngap_perc,edcine2,edfam,egpisko07,estlab,contrato,ponderador02) %>% na.omit()

df_egp7u <- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,edad,sexo,ppolcat,lngap_perc,edcine2,edfam,egpisko07ur,estlab,ponderador02) %>% na.omit()

df_egp6 <- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,edad,sexo,ppolcat,lngap_perc,edcine2,edfam,egpisko06,estlab,estlab,ponderador02) %>% na.omit()

df_egp6u <- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,edad,sexo,ppolcat,lngap_perc,edcine2,edfam,egpisko06ur,estlab,ponderador02) %>% na.omit()

df_egp5 <- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,edad,sexo,ppolcat,lngap_perc,edcine2,edfam,egpisko05,estlab,contrato,ponderador02) %>% na.omit()

df_egp5u <- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,edad,sexo,ppolcat,lngap_perc,edcine2,edfam,egpisko05ur,estlab,contrato,ponderador02) %>% na.omit()

df_isei <- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,edad,sexo,ppolcat,lngap_perc,edcine2,edfam,isei,estlab,contrato,ponderador02) %>% na.omit()

df_siops<- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,edad,sexo,ppolcat,lngap_perc,edcine2,edfam,siops,estlab,contrato,ponderador02) %>% na.omit()


df_egp_4 <- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,edad,sexo,ppolcat,lngap_perc,edcine2,edfam,egp_4,egp_4b,estlab,contrato,ponderador02) %>% na.omit()

df_egp_5 <- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,edad,sexo,ppolcat,lngap_perc,edcine2,edfam,egp_5,egp_5b,estlab,contrato,ponderador02) %>% na.omit()

df_egp_7<- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,edad,sexo,ppolcat,lngap_perc,edcine2,edfam,egp_7,estlab,contrato,ponderador02) %>% na.omit()


# Esquema EGP 07 ----------------------------------------------------------

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc,data= df_egp7)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc,data= df_egp7)
m3  <- lm(merit ~egpisko07+edad+sexo+ppolcat+lngap_perc,data= df_egp7)
m4  <- lm(merit ~inc10h.imp+edcine2+egpisko07+edad+sexo+ppolcat+lngap_perc,data= df_egp7)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc,data= df_egp7)
m6  <- lm(merit ~inc10h.imp+edcine2+egpisko07+ess+edad+sexo+ppolcat+lngap_perc,data= df_egp7)
m7  <- lm(merit ~inc10h.imp+edcine2+egpisko07+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= df_egp7)
m8  <- lm(merit ~inc10h.imp+edcine2+egpisko07+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= df_egp7)
m9  <- lm(merit ~inc10h.imp+edcine2+egpisko07+ess+egpisko07*ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp7)
# m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egpisko07*ess+ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp7)


l_egp7 <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp7)


m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc,data= df_egp7u)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc,data= df_egp7u)
m3  <- lm(merit ~egpisko07ur+edad+sexo+ppolcat+lngap_perc,data= df_egp7u)
m4  <- lm(merit ~inc10h.imp+edcine2+egpisko07ur+edad+sexo+ppolcat+lngap_perc,data= df_egp7u)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc,data= df_egp7u)
m6  <- lm(merit ~inc10h.imp+edcine2+egpisko07ur+ess+edad+sexo+ppolcat+lngap_perc,data= df_egp7u)
m7  <- lm(merit ~inc10h.imp+edcine2+egpisko07ur+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= df_egp7u)
m8  <- lm(merit ~inc10h.imp+edcine2+egpisko07ur+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= df_egp7u)
m9  <- lm(merit ~inc10h.imp+edcine2+egpisko07ur+ess+egpisko07ur*ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp7u)
# m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egpisko07ur*ess+ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp7u)

l_egp7ur <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp7ur)

# Esquema EGP 06 ----------------------------------------------------------

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc,data= df_egp6)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc,data= df_egp6)
m3  <- lm(merit ~egpisko06+edad+sexo+ppolcat+lngap_perc,data= df_egp6)
m4  <- lm(merit ~inc10h.imp+edcine2+egpisko06+edad+sexo+ppolcat+lngap_perc,data= df_egp6)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc,data= df_egp6)
m6  <- lm(merit ~inc10h.imp+edcine2+egpisko06+ess+edad+sexo+ppolcat+lngap_perc,data= df_egp6)
m7  <- lm(merit ~inc10h.imp+edcine2+egpisko06+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= df_egp6)
m8  <- lm(merit ~inc10h.imp+edcine2+egpisko06+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= df_egp6)
m9  <- lm(merit ~inc10h.imp+edcine2+egpisko06+ess+egpisko06*ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp6)
# m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egpisko06*ess+ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp6)


l_egp6 <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp6)


m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc,data= df_egp6u)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc,data= df_egp6u)
m3  <- lm(merit ~egpisko06ur+edad+sexo+ppolcat+lngap_perc,data= df_egp6u)
m4  <- lm(merit ~inc10h.imp+edcine2+egpisko06ur+edad+sexo+ppolcat+lngap_perc,data= df_egp6u)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc,data= df_egp6u)
m6  <- lm(merit ~inc10h.imp+edcine2+egpisko06ur+ess+edad+sexo+ppolcat+lngap_perc,data= df_egp6u)
m7  <- lm(merit ~inc10h.imp+edcine2+egpisko06ur+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= df_egp6u)
m8  <- lm(merit ~inc10h.imp+edcine2+egpisko06ur+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= df_egp6u)
m9  <- lm(merit ~inc10h.imp+edcine2+egpisko06ur+ess+egpisko06ur*ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp6u)
# m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egpisko06ur*ess+ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp6u)

l_egp6ur <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp6ur)


# Esquema EGP 05 ----------------------------------------------------------
df_egp5 <- elsoc_w03 %>% 
  select(merit,ess,inc10h.imp,inc10.imp,edad,sexo,ppolcat,lngap_perc,edcine2,egpisko05,ponderador02) %>% na.omit()
dim(df_egp5)
levels(df_egp5$egpisko05)

# df_egp5$egpisko05 <- relevel(df_egp5$egpisko05,ref = "4")

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m3  <- lm(merit ~egpisko05+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m4  <- lm(merit ~inc10h.imp+edcine2+egpisko05+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m6  <- lm(merit ~inc10h.imp+edcine2+egpisko05+ess+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m7  <- lm(merit ~inc10h.imp+edcine2+egpisko05+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= df_egp5)
m8  <- lm(merit ~inc10h.imp+edcine2+egpisko05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m9  <- lm(merit ~inc10h.imp+edcine2+egpisko05+ess+egpisko05*ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp5)
m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egpisko05*ess+ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp5)

l_egp5 <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp5)


m1  <- lm(merit ~inc10.imp+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m3  <- lm(merit ~egpisko05+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m4  <- lm(merit ~inc10.imp+edcine2+egpisko05+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m6  <- lm(merit ~inc10.imp+edcine2+egpisko05+ess+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m7  <- lm(merit ~inc10.imp+edcine2+egpisko05+ess+inc10.imp*ess+edad+sexo+ppolcat+lngap_perc, data= df_egp5)
m8  <- lm(merit ~inc10.imp+edcine2+egpisko05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= df_egp5)
m9  <- lm(merit ~inc10.imp+edcine2+egpisko05+ess+egpisko05*ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp5)
m10 <- lm(merit ~inc10.imp*ess+edcine2*ess+egpisko05*ess+ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp5)

l_egp5inc <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp5inc)

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc,data= df_egp5,weights = ponderador02)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc,data= df_egp5,weights = ponderador02)
m3  <- lm(merit ~egpisko05+edad+sexo+ppolcat+lngap_perc,data= df_egp5,weights = ponderador02)
m4  <- lm(merit ~inc10h.imp+edcine2+egpisko05+edad+sexo+ppolcat+lngap_perc,data= df_egp5,weights = ponderador02)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc,data= df_egp5,weights = ponderador02)
m6  <- lm(merit ~inc10h.imp+edcine2+egpisko05+ess+edad+sexo+ppolcat+lngap_perc,data= df_egp5,weights = ponderador02)
m7  <- lm(merit ~inc10h.imp+edcine2+egpisko05+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= df_egp5,weights = ponderador02)
m8  <- lm(merit ~inc10h.imp+edcine2+egpisko05+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= df_egp5,weights = ponderador02)
m9  <- lm(merit ~inc10h.imp+edcine2+egpisko05+ess+egpisko05*ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp5,weights = ponderador02)
m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egpisko05*ess+ess+edad+sexo+ppolcat+lngap_perc,  data= df_egp5,weights = ponderador02)

l_egp5wei <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp5wei)

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp5u)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp5u)
m3  <- lm(merit ~egpisko05ur+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp5u)
m4  <- lm(merit ~inc10h.imp+edcine2+egpisko05ur+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp5u)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp5u)
m6  <- lm(merit ~inc10h.imp+edcine2+egpisko05ur+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp5u)
m7  <- lm(merit ~inc10h.imp+edcine2+egpisko05ur+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato, data= df_egp5u)
m8  <- lm(merit ~inc10h.imp+edcine2+egpisko05ur+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp5u)
m9  <- lm(merit ~inc10h.imp+edcine2+egpisko05ur+ess+egpisko05ur*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp5u)
m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egpisko05ur*ess+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp5u)

l_egp5ur <- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp5ur)



# USENDO ISEI ----------------------------------------------------------

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc,data= df_isei)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc,data= df_isei)
m3  <- lm(merit ~isei+edad+sexo+ppolcat+lngap_perc,data= df_isei)
m4  <- lm(merit ~inc10h.imp+edcine2+isei+edad+sexo+ppolcat+lngap_perc,data= df_isei)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc,data= df_isei)
m6  <- lm(merit ~inc10h.imp+edcine2+isei+ess+edad+sexo+ppolcat+lngap_perc,data= df_isei)
m7  <- lm(merit ~inc10h.imp+edcine2+isei+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= df_isei)
m8  <- lm(merit ~inc10h.imp+edcine2+isei+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= df_isei)
m9  <- lm(merit ~inc10h.imp+edcine2+isei+ess+isei*ess+edad+sexo+ppolcat+lngap_perc,  data= df_isei)
m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+isei*ess+ess+edad+sexo+ppolcat+lngap_perc,  data= df_isei)


l_isei<- list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
knitreg(l_isei)

# USANDO SIOPS ----------------------------------------------------------

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc,data= df_siops)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc,data= df_siops)
m3  <- lm(merit ~siops+edad+sexo+ppolcat+lngap_perc,data= df_siops)
m4  <- lm(merit ~inc10h.imp+edcine2+siops+edad+sexo+ppolcat+lngap_perc,data= df_siops)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc,data= df_siops)
m6  <- lm(merit ~inc10h.imp+edcine2+siops+ess+edad+sexo+ppolcat+lngap_perc,data= df_siops)
m7  <- lm(merit ~inc10h.imp+edcine2+siops+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc, data= df_siops)
m8  <- lm(merit ~inc10h.imp+edcine2+siops+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc,data= df_siops)
m9  <- lm(merit ~inc10h.imp+edcine2+siops+ess+siops*ess+edad+sexo+ppolcat+lngap_perc,  data= df_siops)
m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+siops*ess+ess+edad+sexo+ppolcat+lngap_perc,  data= df_siops)


l_siops<- list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
knitreg(l_siops)


# USANDO EGP_4 ----------------------------------------------------------

levels(df_egp_4$egp_4)
# df_egp_4$egp_4<- relevel(df_egp_4$egp_4, ref = 3)

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m3  <- lm(merit ~egp_4+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m4  <- lm(merit ~inc10h.imp+edcine2+egp_4+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m6  <- lm(merit ~inc10h.imp+edcine2+egp_4+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m7  <- lm(merit ~inc10h.imp+edcine2+egp_4+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato, data= df_egp_4)
m8  <- lm(merit ~inc10h.imp+edcine2+egp_4+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m9  <- lm(merit ~inc10h.imp+edcine2+egp_4+ess+egp_4*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_4)
m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egp_4*ess+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_4)

l_egp4<- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp4)

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m3  <- lm(merit ~egp_4b+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m4  <- lm(merit ~inc10h.imp+edcine2+egp_4b+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m6  <- lm(merit ~inc10h.imp+edcine2+egp_4b+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m7  <- lm(merit ~inc10h.imp+edcine2+egp_4b+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato, data= df_egp_4)
m8  <- lm(merit ~inc10h.imp+edcine2+egp_4b+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_4)
m9  <- lm(merit ~inc10h.imp+edcine2+egp_4b+ess+egp_4b*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_4)
m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egp_4b*ess+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_4)

l_egp4b<- list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp4b)


# USANDO EGP_5 ----------------------------------------------------------

levels(df_egp_5$egp_5)
# df_egp_4$egp_4<- relevel(df_egp_4$egp_4, ref = 3)

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m3  <- lm(merit ~egp_5+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m4  <- lm(merit ~inc10h.imp+edcine2+egp_5+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m6  <- lm(merit ~inc10h.imp+edcine2+egp_5+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m7  <- lm(merit ~inc10h.imp+edcine2+egp_5+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato, data= df_egp_5)
m8  <- lm(merit ~inc10h.imp+edcine2+egp_5+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m9  <- lm(merit ~inc10h.imp+edcine2+egp_5+ess+egp_5*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_5)
m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egp_5*ess+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_5)

l_egp5 <-  list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp5)


levels(df_egp_5$egp_5b)

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m3  <- lm(merit ~egp_5b+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m4  <- lm(merit ~inc10h.imp+edcine2+egp_5b+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m6  <- lm(merit ~inc10h.imp+edcine2+egp_5b+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m7  <- lm(merit ~inc10h.imp+edcine2+egp_5b+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato, data= df_egp_5)
m8  <- lm(merit ~inc10h.imp+edcine2+egp_5b+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m9  <- lm(merit ~inc10h.imp+edcine2+egp_5b+ess+egp_5b*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_5)
m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egp_5b*ess+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_5)

l_egp5b <-  list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp5b)


# df_egp_4$egp_4<- relevel(df_egp_4$egp_4, ref = 3)

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m3  <- lm(merit ~egp_5+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m4  <- lm(merit ~inc10h.imp+edcine2+egp_5+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m6  <- lm(merit ~inc10h.imp+edcine2+egp_5+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m7  <- lm(merit ~inc10h.imp+edcine2+egp_5+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato, data= df_egp_5)
m8  <- lm(merit ~inc10h.imp+edcine2+egp_5+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_5)
m9  <- lm(merit ~inc10h.imp+edcine2+egp_5+ess+egp_5*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_5)
m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egp_5*ess+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_5)

l_egp5 <-  list(m1,m2,m3,m4,m5,m6,m7,m8,m9)
knitreg(l_egp5)



# USANDO EGP_7 ----------------------------------------------------------

# levels(df_egp_7$egp_7)
# df_egp_7$egp_7<- relevel(df_egp_7$egp_7, ref = 4)

m1  <- lm(merit ~inc10h.imp+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_7)
m2  <- lm(merit ~edcine2+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_7)
m3  <- lm(merit ~egp_7+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_7)
m4  <- lm(merit ~inc10h.imp+edcine2+egp_7+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_7)
m5  <- lm(merit ~ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_7)
m6  <- lm(merit ~inc10h.imp+edcine2+egp_7+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_7)
m7  <- lm(merit ~inc10h.imp+edcine2+egp_7+ess+inc10h.imp*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato, data= df_egp_7)
m8  <- lm(merit ~inc10h.imp+edcine2+egp_7+ess+edcine2*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,data= df_egp_7)
m9  <- lm(merit ~inc10h.imp+edcine2+egp_7+ess+egp_7*ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_7)
m10 <- lm(merit ~inc10h.imp*ess+edcine2*ess+egp_7*ess+ess+edad+sexo+ppolcat+lngap_perc+estlab+contrato,  data= df_egp_7)


l_egp7<- list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
knitreg(l_egp7)


