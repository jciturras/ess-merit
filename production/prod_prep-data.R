library(dplyr)
library(sjmisc)
rm(list=ls())

load(file = "input/data/original/ELSOC_W03_v1.00_R.RData")


load(url("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/0KIRBJ/DWXZL1"))# elsoc16
# load(url("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/SRPWFW/FZFOJN"))# enacoes14

elsoc <- elsoc_2018; remove(elsoc_2018)


# Missing values ----

for (i in 1:ncol(elsoc)) {
  elsoc[,i][elsoc[,i] == c(-888)]  <- NA #Missing 
  elsoc[,i][elsoc[,i] == c(-999)]  <- NA #Missing   
  }

# Variables Clase Social EGP -----

elsoc$m06 #En su trabajo, a cuantas personas supervisa usted? Convertir a numeric
elsoc$nempleados <- as.numeric(elsoc$m06)

elsoc$m10 #En su trabajo principal, tiene contrato de trabajo escrito?] [Estatus Contractual]
elsoc$contrato <- as.factor(elsoc$m10) #Colapsar 1 y 2 == Contrato,3 == Sin Contrato
elsoc$contrato[elsoc$contrato==2] <- 1 #1 == Tiene Contrato
elsoc$contrato[elsoc$contrato==3] <- 2 #0 == Sin Contrato
elsoc$contrato[elsoc$contrato==-888]<- NA
elsoc$contrato[elsoc$contrato==-999]<- NA
elsoc$contrato <-factor(elsoc$contrato,levels=c(1,2),labels=c("Tiene Contrato", "Sin Contrato"))

# Actividad principal

# 1.	Trabaja	de	manera	remunerada	con	jornada	completa
# 2.	Trabaja	de	manera	remunerada	a	tiempo	parcial	o  hace	trabajos	ocasionales
# 3.	Estudia	y	trabaja
# 4.	Sólo	estudia
# 5.	Jubilado	o	pensionado
# 6.	Desempleado,	buscando	trabajo
# 7.	Realiza	tareas	no	remuneradas
# 8.	Está	enfermo	o	tiene	una	discapacidad
# 9.	No	estudia,	no	trabaja	y	no	busca	trabajo
estlabels <- c("jornada_completa",
               "jornada_parcial",
               "estudia_trabaja",
               "s_estudia",
               "jubilado",
               "desempleado_buscatrabajo",
               "trabajo_noremunerado",
               "enfermo_discap",
               "no_estudia_trabaja_busca")

elsoc$estlab <-  factor(elsoc$m02,levels = c(1:9),labels = estlabels)
rel_emp_lab <- 
  c("Empleado u obrero en empresa privada ",
    "Empleado u obrero del sector publico (incluso empresa publica o municipalidad) ",
    "Miembro de las Fuerzas Armadas y de Orden ",
    "Patron/a o empleador/a (contrata o paga a honorarios a uno/o o mas trabajadores/a's)" ,
    "Trabaja solo, no tiene empleados/as ",
    "Familiar no remunerado ",
    "Servicio domestico ")

elsoc$m07 #Relación de Empleo
elsoc$remp<- factor(elsoc$m07,levels = c(1:7), labels = rel_emp_lab)
elsoc$semp <- as.factor(elsoc$m07)
elsoc$semp <- car::recode(elsoc$semp, "c(4,5)=1;c(1,2,3,6,7)=2")
elsoc$semp <- factor(elsoc$semp,levels=c(1,2), labels=c("Autoempleo","Empleado"))

# Ocupación u oficio actual ISCO 88
elsoc$cod_m03 
elsoc$isco88 <- as.numeric(elsoc$cod_m03)

# recodificar isco 08 a isco88 --------------------------------------------
recode <- ("0110=0110;0210=0110;0310=0110;0100=0110;0200=0110;
0300=0110;1000=1000;
1100=1100;110=1100;1111=1110;1112=1120;1113=1130;1114=1140;1120=1210;1200=1200;
1210=1230;1211=1231;1212=1232;1213=1239;1219=1229;1220=1230;1221=1233;1222=1234;1223=1237;1300=1220;
1310=1221;1311=1221;1312=1221;1320=1220;1321=1222;1322=1222;1323=1223;1324=1235;1330=1236;1340=1229;1341=1229;1342=1229;1343=1229;1344=1229;1345=1229;1346=1227;1349=1229;
1400=1310;1410=1315;1411=1315;1412=1315;1420=1314;1430=1319;1431=1319;1439=1319;2000=2000;
2100=2100;2110=2110;2111=2111;2112=2112;2113=2113;2114=2114;2120=2120;2130=2210;2131=2211;2132=2213;2133=2200;2140=2140;2141=2149;2142=2142;2143=2149;2144=2145;2145=2146;2146=2147;2149=2149;2150=2140;2151=2143;2152=2144;2153=2144;2160=2140;2161=2141;2162=2141;2163=3471;2164=2141;2165=2148;2166=3471;2200=2200;
2210=2220;2211=2221;2212=2221;2220=2230;2221=2230;2222=2230;2230=3229;2240=3221;2250=2223;2260=3210;2261=2222;2262=2224;2263=2229;2264=3226;2265=3223;2266=3229;2267=3224;2269=2229;
2310=2310;2320=2320;2330=2320;2300=2300;2340=2330;2341=2331;2342=2332;2350=2350;2351=2351;2351=2352;2352=2340;2353=2359;2354=2359;2355=2359;2356=2359;2359=2359;2400=2400;
2410=2410;2411=2411;2412=2419;2413=2419;2420=2419;2421=2419;2422=2419;2423=2412;2424=2412;2430=2410;2431=2419;2432=2419;2433=3415;2434=3415;2500=2100;
2510=2130;2511=2131;2512=2131;2513=2131;2513=2139;2514=2132;2519=2131;2520=2131;2521=2131;2522=2131;2523=2131;2529=2139;2600=2400;
2610=2420;2611=2421;2612=2422;2619=2429;2620=2430;2621=2431;2622=2432;2630=2440;2631=2441;2632=2442;2633=2443;2634=2445;2635=2446;2636=2460;2640=2450;2641=2451;2642=2451;2643=2444;2650=2450;2651=2452;2652=2453;2653=2454;2654=2455;2655=2455;2656=3472;2659=3474;3000=3000;
3100=3100;3110=3110;3111=3111;3112=3112;3113=3113;3114=3114;3115=3115;3116=3116;3117=3117;3118=3118;3119=3119;3120=1220;3121=1229;3122=1222;3123=1223;3130=8160;3131=8161;3132=8163;3133=8150;3134=8155;3135=8120;3139=8290;3140=3210;3141=3211;3142=3212;3143=3212;3150=3140;3151=3141;3152=3142;3153=3143;3154=3144;3155=3145;3200=3100;
3210=3130;3211=3133;3212=3211;3213=3228;3214=7311;3220=3230;3221=3231;3222=3232;3230=3241;3240=3227;3250=3220;3251=3225;3252=3229;3253=3229;3254=3224;3255=3226;3256=3221;3257=3152;3258=3152;3259=3229;3300=3300;
3310=3410;3311=3411;3312=3419;3313=3433;3314=3434;3315=3417;3320=3410;3321=3412;3322=3415;3323=3416;3324=3421;3330=3420;3331=3422;3332=3414;3332=3439;3333=3423;3334=3413;3339=3429;3340=3430;3341=3431;3342=3431;3343=3431;3344=3431;3350=3440;3351=3441;3352=3442;3353=3443;3354=3444;3355=3450;3359=3449;3400=3400;
3410=3430;3411=3432;3412=3460;3413=3480;3420=3470;3421=3475;3422=3475;3423=3340;3423=3475;3430=3470;3431=3131;3432=3471;3433=3470;3434=5122;3435=3470;3500=3100;
3510=3120;3511=3122;3512=3121;3513=2139;3514=3121;3520=3130;3521=3130;3522=3114;3522=3132;4000=4000;
4110=4100;4120=4115;4100=4100;4130=4110;4131=4112;4132=4113;4200=4200;
4210=4210;4211=4211;4212=4213;4213=4214;4214=4215;4220=4220;4221=3414;4222=4222;4223=4223;4224=4222;4225=4222;4226=4222;4227=4190;4229=4222;4300=4100;
4310=4120;4311=4121;4312=4122;4313=4121;4320=4130;4321=4131;4322=4132;4323=4133;4400=4100;
4410=4140;4411=4141;4412=4142;4413=4143;4414=4144;4415=4141;4416=4190;4419=4190;5000=5000;
5100=5100;5110=5110;5111=5111;5112=5112;5113=5113;5120=5122;5130=5120;5131=5123;5132=5123;5140=5140;5141=5141;5142=5141;5150=5120;5151=5121;5152=5121;5153=9141;5160=5140;5161=5152;5162=5142;5163=5143;5164=5149;5165=3340;5169=5149;5200=5200;
5210=5230;5211=5230;5212=9111;5220=1314;5221=1314;5222=1314;5223=5220;5230=4211;5240=5220;5241=5210;5242=5220;5243=9113;5244=9113;5245=5220;5246=5123;5249=5220;5300=5100;
5310=5130;5311=5131;5312=5131;5320=5130;5321=5132;5322=5133;5329=5139;5400=5100;
5410=5160;5411=5161;5412=5162;5413=5163;5414=5169;5419=5169;6000=6000;
6100=6100;6110=6110;6111=6111;6112=6112;6113=6113;6114=6114;6120=6120;6121=6124;6122=6122;6123=6123;6129=6129;6130=6130;6210=6141;
6200=6150;6220=6150;6221=6151;6222=6152;6223=6153;6224=6154;6300=6200;
6310=6210;6320=6210;6330=6210;6340=6210;7000=7000;
7100=7100;7110=7120;7111=7129;7112=7122;7113=7113;7114=7123;7115=7124;7119=7129;7120=7130;7121=7131;7122=7132;7123=7133;7124=7134;7125=7135;7126=7136;7127=7240;7130=7140;7131=7141;7132=7142;7133=7143;7200=7200;7210=7210;
7211=7211;7212=7212;7213=7213;7214=7214;7215=7215;7220=7220;7221=7221;7222=7222;7223=7223;7224=7224;7230=7230;7231=7231;7232=7232;7233=7233;7234=7231;7300=7300;
7310=7310;7311=7311;7312=7312;7313=7313;7314=7321;7315=7322;7316=3471;7317=7331;7318=7332;7319=7330;7320=7340;7321=7340;7322=7340;7323=7345;7400=7200;
7410=7240;7411=7137;7412=7241;7413=7245;7420=7240;7421=7242;7421=7243;7422=7243;7500=7400;
7510=7410;7511=7411;7512=7412;7513=7413;7514=7414;7515=7415;7516=7416;7520=7420;7521=7421;7522=7422;7523=7423;7530=7430;7531=7434;7532=7435;7533=7436;7534=7437;7535=7441;7536=7442;7540=7200;7541=7216;7542=7112;7543=3152;7544=7143;7549=7000;8000=8000;
8100=8100;8110=8110;8111=7111;8112=8112;8113=8113;8114=8212;8120=8120;8121=8120;8122=8223;8130=8220;8131=8220;8132=8224;8140=8230;8141=8231;8142=8232;8143=8253;8150=8260;8151=8261;8152=8262;8153=8263;8154=8264;8155=8265;8156=8266;8157=8264;8159=8269;8160=8270;8170=8140;8171=8140;8172=8141;8180=8290;8181=8131;8182=8162;8183=8290;8189=8290;8200=8200;
8210=8280;8211=8281;8212=8283;8219=8290;8300=8300;
8310=8310;8311=8311;8312=8312;8320=8320;8321=8321;8322=8322;8330=8320;8331=8323;8332=8324;8340=8330;8341=8331;8342=8332;8343=8333;8344=8334;8350=8340;9000=9000;
9100=9100;9110=9130;9111=9131;9112=9132;9120=9140;9121=9133;9122=9142;9123=9142;9129=9140;9200=9200;
9210=9210;9211=9211;9212=9211;9213=9211;9214=9211;9215=9212;9216=9213;9300=9300;
9310=9310;9311=9311;9312=9312;9313=9313;9320=9320;9321=9322;9329=9320;9330=9330;9331=9331;9332=9332;9333=9333;9334=9333;9400=9100;
9410=9130;9411=5122;9412=9132;9500=9100;
9510=9120;9520=9112;9600=9100;
9610=9160;9611=9161;9612=9161;9613=9162;9620=9140;9621=9151;9622=9160;9623=9153;9624=9160;9629=9100;9999=0000")
class(elsoc$isco88)
elsoc$isco88 <- car::recode(elsoc$isco88,recodes = recode)

# Recodificación Variables meritocracia -----------------------------------
elsoc$d05_01 <- as.numeric(elsoc$d05_01)# Salir Adelante:
elsoc$d05_02 <- as.numeric(elsoc$d05_02)# Salir Adelante:
elsoc$d05_03 <- as.numeric(elsoc$d05_03)# Salir Adelante:
elsoc$d05_04 <- as.numeric(elsoc$d05_04)# Salir Adelante:
elsoc$c18_09 <- as.numeric(elsoc$c18_09)
elsoc$c18_10 <- as.numeric(elsoc$c18_10) 

# Variables meritocracia promedio -----------------------------------------
elsoc <- elsoc %>% mutate(salirad=(d05_02+d05_03+d05_04)/3) # Salir adelante
elsoc <- elsoc %>% mutate(recompe=(c18_09+c18_10)/2)        # Recompensa percibida
elsoc <- elsoc %>% mutate(recompe.sum=(c18_09+c18_10))      # Recompensa percibida suma

elsoc <- rename(elsoc,meffort=c18_09,mtalent=c18_10) # cambio de nombre

elsoc$meffort1 <- (elsoc$meffort -min(elsoc$meffort ))/(max(elsoc$meffort )-min(elsoc$meffort ))*100 #re-scale 1 a 5 -> 1 a 100
elsoc$mtalent1 <- (elsoc$mtalent -min(elsoc$mtalent ))/(max(elsoc$mtalent )-min(elsoc$mtalent ))*100 #re-scale 1 a 5 -> 1 a 100

elsoc$meffort <- ordered(elsoc$meffort)
elsoc$meffort3 <- car::recode(elsoc$meffort,recodes = "c(1,2)=1;3=2;c(4,5)=3")
elsoc$meffort3 <- factor(elsoc$meffort3,levels = 1:3,labels = c("Desacuerdo","Ni desacuerdo, Ni de acuerdo", "De acuerdo"),ordered = T)

elsoc$mtalent <- ordered(elsoc$mtalent)
elsoc$mtalent3 <- car::recode(elsoc$mtalent,recodes = "c(1,2)=1;3=2;c(4,5)=3")
elsoc$mtalent3 <- factor(elsoc$mtalent3,levels = 1:3,labels = c("Desacuerdo","Ni desacuerdo, Ni de acuerdo", "De acuerdo"),ordered = T)


# Variable: Salario Percibido y Justo -------------------------------------
summary(elsoc$d03_01) # Salario percibido: Gerente gran empresa
summary(elsoc$d03_02) # Salario percibido: Obrero no calificado

summary(elsoc$d04_01) # Salario justo: Gerente gran empresa
summary(elsoc$d04_02) # Salario justo: Obrero no calificado
elsoc$gap_perc <- (elsoc$d03_01/elsoc$d03_02)
elsoc$gap_just <- (elsoc$d04_01/elsoc$d04_02)

# Recodificación Variables Estatus subjetivo -----------------------------------

elsoc$ess      <- as.numeric(elsoc$d01_01) #Estatus Social Subjetivo
elsoc$essfam   <- as.numeric(elsoc$d01_02) #Estatus Social Subjetivo familiar
elsoc$esshijos <- as.numeric(elsoc$d01_03) #Estatus Social Subjetivo Hijos

# Recodificación Variables Estatus objetivo -----------------------------------

# Ingresos del hogar-----------------------------------------------------------#

summary(elsoc$m29) # ingresos total ; NA == 668
summary(elsoc$m30) # ingresos tramos 
summary(elsoc$m54) # tamannio del hogar


# Pareto corrected formula  (Hout, 2004)--------------------------------------#
Ltop_1<- 1850001
Ltop  <- 2700000

ftop_1 <- 14
ftop   <- 10

V  = (log(ftop_1 + ftop) - log(ftop)) / (log(Ltop) -log(Ltop_1))
M_top = 0.5* Ltop *(1+(V/(V-1))) # = $ 3.726.106

elsoc$mean_tramos[elsoc$m30==1] <-110000
elsoc$mean_tramos[elsoc$m30==2] <-250000.5
elsoc$mean_tramos[elsoc$m30==3] <-305000.5
elsoc$mean_tramos[elsoc$m30==4] <-355000.5
elsoc$mean_tramos[elsoc$m30==5] <-400000.5
elsoc$mean_tramos[elsoc$m30==6] <-445000.5
elsoc$mean_tramos[elsoc$m30==7] <-490000.5
elsoc$mean_tramos[elsoc$m30==8] <-535000.5
elsoc$mean_tramos[elsoc$m30==9] <-585000.5
elsoc$mean_tramos[elsoc$m30==10]<-640000.5
elsoc$mean_tramos[elsoc$m30==11]<-700000.5
elsoc$mean_tramos[elsoc$m30==12]<-765000.5
elsoc$mean_tramos[elsoc$m30==13]<-845000.5
elsoc$mean_tramos[elsoc$m30==14]<-935000.5
elsoc$mean_tramos[elsoc$m30==15]<-1040000.5
elsoc$mean_tramos[elsoc$m30==16]<-1180000.5
elsoc$mean_tramos[elsoc$m30==17]<-1375000.5
elsoc$mean_tramos[elsoc$m30==18]<-1670000.5
elsoc$mean_tramos[elsoc$m30==19]<-2275000.5
elsoc$mean_tramos[elsoc$m30==20]<-M_top
table(elsoc$mean_tramos)

elsoc$m29 <- ifelse(test = (is.na(elsoc$m29)),yes = elsoc$mean_tramos,no = elsoc$m29)
summary(elsoc$m29)

elsoc <- rename(elsoc, inghogar=m29, nhogar=m54) 
elsoc$ingneto   <- as.numeric(elsoc$inghogar/elsoc$nhogar) # ingreso neto
elsoc$lningneto <- log(elsoc$ingneto)                      # logaritmo del ingreso neto


#---Ingresos individuales ----------------------------------------------------
#Remplazar NA por media de categorías Ingreso -------------------------------#

summary(elsoc$m13) # Ingreso individual; NA = 1716
summary(elsoc$m14) # Ingreso individual por tramos

Ltop_1<- 850001
Ltop  <- 1300001 

ftop_1 <- 16
ftop   <- 9

V  = (log(ftop_1 + ftop) - log(ftop)) / (log(Ltop) -log(Ltop_1))
M_top = 0.5* Ltop *(1+(V/(V-1))) # = $1.762.784

elsoc$mean_tramos.ind <- NA
elsoc$mean_tramos.ind[elsoc$m14 == 1]  <- 40000
elsoc$mean_tramos.ind[elsoc$m14 == 2]  <- 62501
elsoc$mean_tramos.ind[elsoc$m14 == 3]  <- 105001
elsoc$mean_tramos.ind[elsoc$m14 == 4]  <- 147501
elsoc$mean_tramos.ind[elsoc$m14 == 5]  <- 190001
elsoc$mean_tramos.ind[elsoc$m14 == 6]  <- 220001
elsoc$mean_tramos.ind[elsoc$m14 == 7]  <- 255001
elsoc$mean_tramos.ind[elsoc$m14 == 8]  <- 300001
elsoc$mean_tramos.ind[elsoc$m14 == 9]  <- 340001
elsoc$mean_tramos.ind[elsoc$m14 == 10] <- 380001
elsoc$mean_tramos.ind[elsoc$m14 == 11] <- 432501
elsoc$mean_tramos.ind[elsoc$m14 == 12] <- 502501
elsoc$mean_tramos.ind[elsoc$m14 == 13] <- 602501
elsoc$mean_tramos.ind[elsoc$m14 == 14] <- 757501
elsoc$mean_tramos.ind[elsoc$m14 == 15] <- 1075001
elsoc$mean_tramos.ind[elsoc$m14 == 16] <- M_top
table(elsoc$mean_tramos.ind)
elsoc$m13 <- ifelse(test = (is.na(elsoc$m13)),yes = elsoc$mean_tramos.ind,no = elsoc$m13)
summary(elsoc$m13) # NA = 1523

elsoc$ingreso <- as.numeric(elsoc$m13)

#---Deciles (ingreso individual)------------------------------------------------
elsoc <- elsoc %>% mutate(inc10 = ntile(ingreso, 10)) #Crear Deciles de ingreso
elsoc$inc10 <- car::recode(elsoc$inc10, "NA=99") #Deciles de ingreso + "no responde"
elsoc$D10 <- factor(elsoc$inc10, levels = c(1,2,3,4,5,6,7,8,9,10,99), 
                    labels = c("D01","D02","D03","D04","D05",
                               "D06","D07","D08","D09","D10","No responde"));table(elsoc$D10)



#---Deciles (ingreso per capita hogar)------------------------------------------------
elsoc <- elsoc %>% mutate(inc10h = ntile(ingneto, 10)) #Crear Deciles de ingreso neto
elsoc$inc10h <- car::recode(elsoc$inc10h, "NA=99") #Deciles de ingreso + "no responde"
elsoc$D10h <- factor(elsoc$inc10h, levels = c(1,2,3,4,5,6,7,8,9,10,99), 
                    labels = c("D01","D02","D03","D04","D05", 
                               "D06","D07","D08","D09","D10","No responde"));table(elsoc$D10h)



elsoc <- elsoc %>% mutate(inc05h = ntile(ingneto, 5)) #Crear Quintiles de ingreso neto
elsoc$inc05h <- car::recode(elsoc$inc05h, "NA=99") #Deciles de ingreso + "no responde"
elsoc$Q05h <- factor(elsoc$inc05h, levels = c(1,2,3,4,5,99), 
                     labels = c("Q01","Q02","Q03","Q04","Q05","No responde"));table(elsoc$Q05h)



#---Educación---------------------------------------------------

elsoc$educ <- as.numeric(elsoc$m01) #Nivel Educacional contínuo
elsoc$edcat <- car::recode(elsoc$m01, "c(1,2)=1; c(3,4)=2;c(5,6)=3; c(7,8)=4;c(9,10)=5") 
elsoc$edcat <- factor(elsoc$edcat,levels = c(1,2,3,4,5), 
                      labels=c("Básica Incompleta o Inferior",
                               "Básica completa o Media Incompleta",
                               "Media Completa o Técnica superior incompleta",
                               "Técnico superior completa o Universitaria incompleta",
                               "Universitaria completa o superior"));table(elsoc$edcat)

#Propuesta según CINE 2011 (UNESCO) ----
# 1.	Sin	estudios		                               0.012 [Cine 0  ]  1
# 2.	Educación	Básica	o	Preparatoria	incompleta 	 0.109 [Cine 0  ]  1
# 3.	Educación	Básica	o	Preparatoria	completa	   0.101 [Cine 1,2]  2
# 4.	Educación	Media	o	Humanidades	incompleta       0.134 [Cine 3  ]  3
# 5.	Educación	Media	o	Humanidades	completa	       0.297 [Cine 3  ]  3
# 6.	Técnico	Superior	incompleta	                 0.034 [Cine 5  ]  4
# 7.	Técnico	Superior	completa	                   0.130 [Cine 5  ]  4
# 8.	Universitaria	incompleta                       0.063 [Cine 6  ]  5
# 9.	Universitaria	completa                         0.104 [Cine 6  ]	 6
# 10.	Estudios	de	posgrado (magíster	o	doctorado) 0.015 [Cine 7 + Cine 8] 6

#operacionalización A - Educación----
elsoc$edcine <- car::recode(elsoc$m01, "c(1,2)=1; c(3)=2;c(3,4,5)=3; c(6,7)=4;c(8,9)=5;10=6")
elsoc$edcine <- factor(elsoc$edcine,
                       levels = c(1,2,3,4,5,6), 
                       labels=c("cine0",
                                "cine1.2",
                                "cine3.4",
                                "cine5",
                                "cine6",
                                "cine7")) 
prop.table(table(elsoc$edcine))
table(elsoc$edcine)

#operacionalización B - Educación----
elsoc$edcine2 <- car::recode(elsoc$m01, "c(1,2)=1; c(3)=2;c(4,5)=3;c(6,7)=4;c(8,9,10)=5")
round(prop.table(table(elsoc$edcine2)), 3)

elsoc$edcine2 <- factor(elsoc$edcine2,levels = c(1,2,3,4,5), 
                        labels=c("cine0",
                                 "cine12",
                                 "cine34",
                                 "cine5",
                                 "cine678"))
table(elsoc$edcine2)
#---OCUPACION (ISCO88) ---------------------------------------------------

elsoc$isco_cat  <- car::recode(elsoc$isco88,
                               "c(1229,1232,1239,1252,1312,1313,1314,1315,1316,1317,1318,1319)=1;
c(2114,2122,2131,2132,2141,2142,2143,2146,2147,2149,2211,2213,2221,2222,2223,2229,2230,2310,2320,2331,2332,2340,2359,2411,2412,2419,2421,2429,2432,2441,2442,2445,2446,2451,2452,2455,2460)=2;
c(3112,3114,3118,3121,3131,3132,3152,3212,3213,3221,3222,3223,3225,3226,3229,3231,3310,3320,3330,3340,3411,3412,3413,3415,3416,3417,3419,3422,3423,3432,3433,3441,3449,3452,3460,3471,3472,3473,3474,3475)=3;
c(4113,4115,4121,4122,4131,4132,4133,4142,4190,4211,4215,4222,4223)=4;
c(5112,5121,5122,5123,5131,5132,5133,5141,5142,5162,5163,5164,5169,5220,5230)=5;
c(6111,6112,6113,6124,6141,6152)=6;
c(7111,7112,7122,7123,7124,7129,7134,7135,7136,7137,7141,7142,7212,7213,7221,7224,7231,7233,7241,7242,7243,7244,7245,7311,7313,7322,7343,7344,7345,7411,7412,7413,7415,7421,7422,7432,7433,7436,7437,7442)=7;
c(8112,8121,8124,8141,8143,8159,8163,8211,8221,8232,8240,8251,8264,8272,8274,8275,8276,8311,8322,8323,8324,8331,8332,8333,8334,8340)=8;
c(9111,9112,9131,9132,9133,9141,9142,9151,9152,9153,9161,9162,9211,9312,9313,9322,9333,9955,9988,9999)=9;
NA=99")
elsoc$isco_cat  <- NA
elsoc$isco_cat[elsoc$isco88<=1999] <-1
elsoc$isco_cat[elsoc$isco88>=2000 & elsoc$isco88 <=2999] <-2
elsoc$isco_cat[elsoc$isco88>=3000 & elsoc$isco88 <=3999] <-3
elsoc$isco_cat[elsoc$isco88>=4000 & elsoc$isco88 <=4999] <-4
elsoc$isco_cat[elsoc$isco88>=5000 & elsoc$isco88 <=5999] <-5
elsoc$isco_cat[elsoc$isco88>=6000 & elsoc$isco88 <=6999] <-6
elsoc$isco_cat[elsoc$isco88>=7000 & elsoc$isco88 <=7999] <-7
elsoc$isco_cat[elsoc$isco88>=8000 & elsoc$isco88 <=8999] <-8
elsoc$isco_cat[elsoc$isco88>=9000 & elsoc$isco88 <=9999] <-9
elsoc$isco_cat[is.na(elsoc$isco88)] <-99
table(elsoc$isco_cat)

elsoc$isco_cat <- factor(elsoc$isco_cat, levels = c(1,2,3,4,5,6,7,8,9,99),
                         labels = c("Directores y gerentes",
                                    "Profesionales, científicos e intelectuales",
                                    "Técnicos y profesionales de nivel medio",
                                    "Personal de apoyo administrativo",
                                    "Trabajadores de los servicios y vendedores de comercios y mercados",
                                    "Agricultores y trabajadores calificados agropecuarios, forestales y pesquero",
                                    "Oficiales, operarios y artesanos de artes mecánicas y de otros oficios",
                                    "Operadores de instalaciones y máquinas y ensambladores",
                                    "Ocupaciones elementales",
                                    "No responde"))

#---VARIABLES CONTROL----

#---Sexo----
elsoc$sexo <- car::recode(elsoc$m0_sexo, "1=1;2=0")
elsoc$sexo <- factor(elsoc$sexo, levels = c(0,1), labels = c("Hombre","Mujer")) #Sexo
#Hombre=0
#Mujer=1

#---Edad----
elsoc$edad <- as.numeric(elsoc$m0_edad) #Edad

#---Posición Política----
elsoc$ppolcat  <- car::recode(elsoc$c15, "c(0,1,2,3,4)=1;5=2;c(6,7,8,9,10)=3;11=4;12=5") 
elsoc$ppolcat  <- factor(elsoc$ppolcat, levels = c(1,2,3,4,5), labels = c("Izquierda/Centro Izquierda",
                                                      "Centro",
                                                      "Derecha/Centro Derecha",
                                                      "Independiente",
                                                      "Ninguno"))
#--- Numero de hijos----

summary(elsoc$m37_01)
summary(elsoc$m37_02)
elsoc$nhijos <- elsoc$m37_01 + elsoc$m37_02
table(elsoc$nhijos)
elsoc$nhijos[is.na(elsoc$nhijos)] <- 0

# DUMMIES para cada categoría -----

library(fastDummies)
elsoc <-dummy_cols(elsoc, select_columns = c("edcine2","D10", "isco_cat","ppolcat"))
names(elsoc)

{
names(elsoc)[names(elsoc)=="edcine2_cine0"  ] <- 'ecin0'
names(elsoc)[names(elsoc)=="edcine2_cine12" ] <- 'ecin1.2'
names(elsoc)[names(elsoc)=="edcine2_cine34" ] <- 'ecin3.4'
names(elsoc)[names(elsoc)=="edcine2_cine5"  ] <- 'ecin5'
names(elsoc)[names(elsoc)=="edcine2_cine678"] <- 'ecin6.7.8'
names(elsoc)[names(elsoc)=="edcine2_NA"     ] <- 'ecinNA'

names(elsoc)[names(elsoc) =="D10_D01"        ] <- 'ID01'            
names(elsoc)[names(elsoc) =="D10_D02"        ] <- 'ID02'            
names(elsoc)[names(elsoc) =="D10_D03"        ] <- 'ID03'            
names(elsoc)[names(elsoc) =="D10_D04"        ] <- 'ID04'            
names(elsoc)[names(elsoc) =="D10_D05"        ] <- 'ID05'            
names(elsoc)[names(elsoc) =="D10_D06"        ] <- 'ID06'
names(elsoc)[names(elsoc) =="D10_D07"        ] <- 'ID07'            
names(elsoc)[names(elsoc) =="D10_D08"        ] <- 'ID08'            
names(elsoc)[names(elsoc) =="D10_D10"        ] <- 'ID09'            
names(elsoc)[names(elsoc) =="D10_D09"        ] <- 'ID10'            
names(elsoc)[names(elsoc) =="D10_No responde"] <- 'IDNR'  

names(elsoc)[names(elsoc)=="isco_cat_Directores y gerentes"                                                       ] <- 'isco01'
names(elsoc)[names(elsoc)=="isco_cat_Profesionales, científicos e intelectuales"                                  ] <- 'isco02'
names(elsoc)[names(elsoc)=="isco_cat_Técnicos y profesionales de nivel medio"                                     ] <- 'isco03'
names(elsoc)[names(elsoc)=="isco_cat_Personal de apoyo administrativo"                                            ] <- 'isco04'
names(elsoc)[names(elsoc)=="isco_cat_Trabajadores de los servicios y vendedores de comercios y mercados"          ] <- 'isco05'
names(elsoc)[names(elsoc)=="isco_cat_Agricultores y trabajadores calificados agropecuarios, forestales y pesquero"] <- 'isco06'
names(elsoc)[names(elsoc)=="isco_cat_Oficiales, operarios y artesanos de artes mecánicas y de otros oficios"      ] <- 'isco07'
names(elsoc)[names(elsoc)=="isco_cat_Operadores de instalaciones y máquinas y ensambladores"                      ] <- 'isco08'
names(elsoc)[names(elsoc)=="isco_cat_Ocupaciones elementales"                                                     ] <- 'isco09'
names(elsoc)[names(elsoc)=="isco_cat_No responde"                                                                 ] <- 'isco_nr'

names(elsoc)[names(elsoc) =="ppolcat_Independiente"             ] <- 'ppol_ind'                       
names(elsoc)[names(elsoc) =="ppolcat_Ninguno"                   ] <- 'ppol_ninguno'                       
names(elsoc)[names(elsoc) =="ppolcat_Centro"                    ] <- 'ppol_cen'                       
names(elsoc)[names(elsoc) =="ppolcat_Izquierda/Centro Izquierda"] <- 'ppol_izq'                       
names(elsoc)[names(elsoc) =="ppolcat_Derecha/Centro Derecha"    ] <- 'ppol_der'                        
names(elsoc)[names(elsoc) =="ppolcat_NA"                        ] <- 'ppol_na' 
}



# A: Imputacion de Ingresos -----------------------------------------------

# Data imputation para ingresos -------------------------------------------
# método con OLS ---------------------------------------------------------#
# year 2018--------------------------------------------------------------------------#

elsoc$D10h[elsoc$D10h == "No responde"] <- NA
elsoc$edad2 <- elsoc$edad^2


# recuperar variables bienes de 2016 ----------------------------------

for (i in 1:ncol(elsoc_2016)) {
  elsoc_2016[,i][elsoc_2016[,i] == c(-888)]  <- NA #Missing
  elsoc_2016[,i][elsoc_2016[,i] == c(-999)]  <- NA #Missing
}
data2016 <- elsoc_2016 %>% mutate(bienes16=m31_01+m31_02+m31_03+m31_04) %>% dplyr::select(idencuesta,bienes16)
elsoc<- elsoc %>% left_join(data2016,by = "idencuesta")
summary(elsoc$bienes16)

lm01<- lm(as.numeric(D10h)~edcine2+sexo+edad+edad2+ess+factor(region)+estlab,data=elsoc,na.action = na.exclude)
lm02<- lm(as.numeric(D10h)~edcine2+sexo+edad+edad2+ess+factor(region)+estlab+factor(estrato),data=elsoc,na.action = na.exclude)
lm03<- lm(as.numeric(D10h)~edcine2+sexo+edad+edad2+ess+factor(region)+estlab+factor(estrato)+factor(bienes16),data=elsoc,na.action = na.exclude)
summary(lm01)$adj.r.squared; summary(fitted(lm01))
summary(lm02)$adj.r.squared; summary(fitted(lm02))
summary(lm03)$adj.r.squared; summary(fitted(lm03))

elsoc$fit <- predict.lm(object = lm02,newdata = elsoc[,c("edcine2","sexo","edad","edad2","ess","region","estlab","estrato")])
elsoc$fit <- predict.lm(object = lm03,newdata = elsoc[,c("edcine2","sexo","edad","edad2","ess","region","estlab","estrato","bienes16")])

elsoc$D10i <- ifelse(test = (elsoc$fit<0 | elsoc$fit <=1),yes = 1, no = elsoc$fit)
elsoc$D10i <- ifelse(test = (elsoc$fit>1 & elsoc$fit <=2),yes = 2, no = elsoc$D10i)
elsoc$D10i <- ifelse(test = (elsoc$fit>2 & elsoc$fit <=3),yes = 3, no = elsoc$D10i)
elsoc$D10i <- ifelse(test = (elsoc$fit>3 & elsoc$fit <=4),yes = 4, no = elsoc$D10i)
elsoc$D10i <- ifelse(test = (elsoc$fit>4 & elsoc$fit <=5),yes = 5, no = elsoc$D10i)
elsoc$D10i <- ifelse(test = (elsoc$fit>5 & elsoc$fit <=6),yes = 6, no = elsoc$D10i)
elsoc$D10i <- ifelse(test = (elsoc$fit>6 & elsoc$fit <=7),yes = 7, no = elsoc$D10i)
elsoc$D10i <- ifelse(test = (elsoc$fit>7 & elsoc$fit <=8),yes = 8, no = elsoc$D10i)
elsoc$D10i <- ifelse(test = (elsoc$fit>8 & elsoc$fit <=9),yes = 9, no = elsoc$D10i)
elsoc$D10i <- ifelse(test = (elsoc$fit>9 | elsoc$fit >10),yes = 10,no = elsoc$D10i)
table(elsoc$D10i)

# Remplazamos los NA de D10 por los valores predichos por el modelo
elsoc$D10imp <- ifelse(test = is.na(elsoc$D10h),yes = elsoc$D10i,no = elsoc$D10h) 
summary(elsoc$D10imp)
hist.default(elsoc$D10imp)
summary(as.numeric(elsoc$D10h))
hist.default(as.numeric(elsoc$D10h))

# View(elsoc[,c("D10h","fit","D10i","D10imp")])


t.test(x = summary(elsoc$D10imp),y = summary(as.numeric(elsoc$D10h))) # ttest medias de D10 hogar

# Keep variables relevantes -----------------------------------------------

names(elsoc) 
elsoc_18 <- elsoc %>% dplyr::select(idencuesta,region,estrato,ponderador02,edad2,estlab,nhijos,meffort,meffort1,meffort3,mtalent,mtalent1,mtalent3,recompe.sum,isco88:ppol_na,region,comuna,semp,nempleados,inghogar,nhogar,D10h,D10imp,ingneto)
names(elsoc_18)

# Save Data ---------------------------------------------------------------
save(elsoc_18,file = "input/data/proc/ELSOC_ess_merit2018.RData")
rm(list=ls())
load("input/data/proc/ELSOC_ess_merit2018.RData") #2018

# Cargar base con Esquema EGP ---------------------------------------------
egp01 <- sjlabelled::read_stata(path = "input/data/proc/ELSOC_egp-siops-isei2018.dta")
egp01 <- egp01 %>% dplyr::select(idencuesta,egp,egptorche,isei,siops)

elsoc_18 <- left_join(elsoc_18,egp01,"idencuesta") #Pegar ELSOC con EGP 
names(elsoc_18)
# Crear dummies para EGP --------------------------------------------------

library(fastDummies)
attr(elsoc_18$egp,"labels")
attr(elsoc_18$egptorche,"labels")

egplab  <- sjlabelled::get_labels(elsoc_18$egp) # Etiquetas de clase EGP10
egptlab <- sjlabelled::get_labels(elsoc_18$egptorche) # Etiquetas de clase EGP7
egp10   <- sjlabelled::get_labels(elsoc_18$egp) # Etiquetas de clase EGP7


# EGP 05 categorias (Adaptado de EGP07) --------------------------#

# [1] "Servicio (I+II)"            
# [2] "Rutinas no manuales(III)"   
# [3] "Auto Empleo(IVab+IVc)"          
# [4] "Obrero calificado (V+VI)"   
# [5] "Obrero no calificado (VIIa+VIIb)"

# recode 6 a 4 = Autoempleo agro en Autoempleo 
# recode 7 a 5 = Labores agricolas a trabajo no calificado

elsoc_18$egp05 <- car::recode(elsoc_18$egptorche, "6=4;7=5",as.factor = TRUE)
elsoc_18$egp05 <- factor(elsoc_18$egp05,
                         levels =c(1:5), 
                         labels =c("Servicio (I+II)",
                                   "Rutinas no manuales(III)",
                                   "Auto Empleo(IVab+IVc)",
                                   "Obrero calificado (V+VI)",
                                   "Obrero no calificado (VIIa+VIIb)"))
elsoc_18$egp05 <- factor(x = elsoc_18$egp05,levels = c("Obrero no calificado (VIIa+VIIb)",
                                                       "Obrero calificado (V+VI)",
                                                       "Auto Empleo(IVab+IVc)",
                                                       "Rutinas no manuales(III)",
                                                       "Servicio (I+II)"))
knitr::kable(table(elsoc_18$egp05),format = "markdown")

# EGP 06 categorias (Adaptado de EGP10)

# [1] "I Alto control"  (1)                   
# [2] "II Bajo control" (2)                 
# [3] "III Rutinas no manuales" (3)         
# [4] "IVa Auto-empleo con empleados" (4)   
# [5] "IVb Auto-empleo sin empleados" (4)  
# [6] "V Supervisor de labores manuales" (5)
# [7] "VI Obrero calificado" (5)            
# [8] "VIIa Obrero semicalificado" (6)      
# [9] "VIIb Labores agrícolas" (6)          
# [10] "IVc Autoempleo agrícola" (4)

elsoc_18$egp <- as.numeric(elsoc_18$egp)
knitr::kable(table(elsoc_18$egp),format = "markdown")

# recode 4,5,10 = Autoempleo (IVa, IVb, IVc)  (4)
# recode 6,5    = Obrero calificado (V, VI) (5)
# recode 8,9    = Obrero no calificado (VIIab) (6)

elsoc_18$egp06 <- elsoc_18$egp
elsoc_18$egp06[elsoc_18$egp06 %in% c(4,5,10)] <- 4 
elsoc_18$egp06[elsoc_18$egp06 %in% c(6,7)]    <- 5 
elsoc_18$egp06[elsoc_18$egp06 %in% c(8,9)]    <- 6 

elsoc_18$egp06 <- factor(elsoc_18$egp06,
                         levels =c(1:6), 
                         labels =c("Servicio I",
                                   "Servicio II",
                                   "Rutinas no manuales(III)",
                                   "Autoempleo(IVabc)",
                                   "Obrero calificado (V+VI)",
                                   "Obrero no calificado (VIIabc)"))
knitr::kable(table(elsoc_18$egp06),format = "markdown")


# EGP 03 categorias (Adaptado de EGP05)

# [1] "Servicio (I+II)" 1           
# [2] "Rutinas no manuales(III)"  1 
# [3] "Auto Empleo(IVab+IVc)"     2     
# [4] "Obrero calificado (V+VI)"  3 
# [5] "Obrero no calificado (VIIa+VIIb)" 3

# recode 6 a 4 = Autoempleo agro en Autoempleo 
# recode 7 a 5 = Labores agricolas a trabajo no calificado

elsoc_18$egp03 <- car::recode(as.numeric(elsoc_18$egp05), "c(1,2)=1;3=2;c(4,5)=3",as.factor = TRUE)

elsoc_18$egp03 <- factor(elsoc_18$egp03,
                         levels =c(1:3), 
                         labels =c("No manual (I+II+III)",
                                   "Auto Empleo(IVab+IVc)",
                                   "Trabajo Manual (V+VI+VIIa+VIIb)"))
knitr::kable(table(elsoc_18$egp03))


# EGP 10 categorias
elsoc_18$egp <- factor(x = elsoc_18$egp,
                       levels = c(1,2,3,4,5,6,7,8,9,10),
                       labels = egplab)
# EGP 07 categorias (Torche, 2007)
elsoc_18$egptorche <- factor(x = elsoc_18$egptorche,
                       levels = c(1,2,3,4,5,6,7),
                       labels = egptlab)
elsoc_18 <- dummy_cols(elsoc_18, select_columns = c("egp","egptorche", "egp05"))



# Cambiar nombres a EGP 05 ------------------------------------------------

names(elsoc_18)[names(elsoc_18)=="egp05_Rutinas no manuales(III)"        ] <- "egp05_rutnomanualIII"
names(elsoc_18)[names(elsoc_18)=="egp05_NA"                              ] <- "egp05_na"
names(elsoc_18)[names(elsoc_18)=="egp05_Obrero calificado (V+VI)"        ] <- "egp05_obrcalV_VI"
names(elsoc_18)[names(elsoc_18)=="egp05_Auto Empleo(IVab+IVc)"           ] <- "egp05_autoemp_IVab_IV_c"
names(elsoc_18)[names(elsoc_18)=="egp05_Obrero no calificado (VIIa+VIIb)"] <- "egp05_obnocalVIIa_VIIb"
names(elsoc_18)[names(elsoc_18)=="egp05_Servicio (I+II)"                 ] <- "egp05_servicioI_II"

# Cambiar nombres a EGP 07 ------------------------------------------------

names(elsoc_18)[names(elsoc_18)=="egptorche_Rutinas no manuales(III)"     ] <- "egp7_rutnomanuales_III"             
names(elsoc_18)[names(elsoc_18)=="egptorche_NA"                           ] <- "egp7_na"                                   
names(elsoc_18)[names(elsoc_18)=="egptorche_Obrero calificado (V+VI)"     ] <- "egp7_obcalV_VI"             
names(elsoc_18)[names(elsoc_18)=="egptorche_Auto Empleo(IVab)"            ] <- "egp7_autoemp_IVab"                    
names(elsoc_18)[names(elsoc_18)=="egptorche_Obrero no calificado (VIIa)"  ] <- "egp7_obnocalVIIa"          
names(elsoc_18)[names(elsoc_18)=="egptorche_Servicio (I+II)"              ] <- "egp7_servicioI_II"                      
names(elsoc_18)[names(elsoc_18)=="egptorche_Labores agrícolas (VIIb)"     ] <- "egp7_labagroVIIb"             
names(elsoc_18)[names(elsoc_18)=="egptorche_Autoempleo agrícola (IVc)"    ] <- "egp7_autoemp_agroIVc" 

# Cambiar nombres a EGP 10 ------------------------------------------------
names(elsoc_18)[names(elsoc_18)=="egp_III Rutinas no manuales"           ] <- "egp10_rutnomanuales_III"             
names(elsoc_18)[names(elsoc_18)=="egp_NA"                                ] <- "egp10_na"                                   
names(elsoc_18)[names(elsoc_18)=="egp_VI Obrero calificado"              ] <- "egp10_obcalVI"             
names(elsoc_18)[names(elsoc_18)=="egp_IVb Auto-empleo sin empleados"     ] <- "egp10_autoemp_IVa"                    
names(elsoc_18)[names(elsoc_18)=="egp_VIIa Obrero semicalificado"        ] <- "egp10_obsemicalVIIa"          
names(elsoc_18)[names(elsoc_18)=="egp_V Supervisor de labores manuales"  ] <- "egp10_supvmanual_V"                      
names(elsoc_18)[names(elsoc_18)=="egp_II Bajo control"                   ] <- "egp10_bajocontrolII"             
names(elsoc_18)[names(elsoc_18)=="egp_IVa Auto-empleo con empleados"     ] <- "egp10_autoemp_IVb"            
names(elsoc_18)[names(elsoc_18)=="egp_I Alto control"                    ] <- "egp10_altocontro_I"            
names(elsoc_18)[names(elsoc_18)=="egp_VIIb Labores agrícolas"            ] <- "egp10_labagro_VIIb"            
names(elsoc_18)[names(elsoc_18)=="egp_IVc Autoempleo agrícola"           ] <- "egp10_autoemp_agroIVc"            

# Save Data ---------------------------------------------------------------
save(elsoc_18,file = "input/data/proc/ELSOC_ess_merit2018.RData")

# Merge main data with imputed dataset ------------------------------------
rm(list=ls())
# source(file = "production/prod_prep-imputation.R")

load(file = "input/data/proc/ELSOC_ess_merit2018.RData")
load(file = "input/data/proc/elsoc.imputed.RData")

elsoc_18<- elsoc_18 %>% left_join(y =elsoc.imputed, by = "idencuesta")

elsoc_18$ingneto.imp <- elsoc_18$inghogar.imp/elsoc_18$nhogar
summary(elsoc_18$ingneto.imp)

elsoc_18 <- elsoc_18 %>% mutate(inc10h.imp = ntile(ingneto.imp, 10)) 
elsoc_18 <- elsoc_18 %>% mutate(inc05h.imp = ntile(ingneto.imp, 5)) 
elsoc_18$inc05h.imp <- factor(elsoc_18$inc05h.imp,levels = 1:5,labels = c("Q01","Q02","Q03","Q04","Q05"))



View(elsoc_18[,c("ingneto","inc10h","ingneto.imp","inc10h.imp")])


# Save data final ---------------------------------------------------------

save(elsoc_18,file = "input/data/proc/ELSOC_ess_merit2018.RData")


