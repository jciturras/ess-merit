## Ocupación

# Ocupación u oficio actual ISCO 88
str(elsoc$cod_m03)
table(elsoc$cod_m03==9999)
elsoc$cod_m03 <- as.numeric(elsoc$cod_m03)
elsoc$isco88 <- as.numeric(elsoc$cod_m03)
elsoc$isco88[elsoc$isco88==9999] <- NA
table(elsoc$isco88==9999)

str(elsoc$cod_m22)
table(elsoc$cod_m22==9999)
elsoc$cod_m22[elsoc$cod_m22==9999] <- NA
table(elsoc$cod_m22==9999)

# replace missing in respondent ISCO with head household (if missing) 
sjmisc::find_var(elsoc_2018,pattern = "hogar")
elsoc$isco08 <- elsoc$isco88 
table(elsoc$isco88==9999)

summary(as.numeric(elsoc$cod_m03))
summary(as.numeric(elsoc$cod_m22))
summary(as.numeric(elsoc$isco88))
sjmisc::descr(elsoc$isco88)

# Recode CIUO_encuestado
elsoc$isco88 <- occupar::isco08to88(elsoc$isco88)
sjmisc::descr(elsoc$isco88)

# Make sempl
sjmisc::find_var(elsoc_2018,pattern = "empleo")
#Relación de Empleo encuestado
sjmisc::frq(elsoc$m07)

elsoc$semp <- as.factor(elsoc$m07)
elsoc$semp <- car::recode(elsoc$semp, "c(4,5)=1;c(1,2,3,7)=2;6=NA")
sjmisc::frq(elsoc$semp)

elsoc$semp <- sjlabelled::set_labels(x = elsoc$semp,labels=c('Employer and Self-employed','Employee'))
sjmisc::frq(elsoc$semp)


# superivison
sjmisc::find_var(elsoc_2018,pattern = "personas")
sjmisc::frq(elsoc$m06) #supervision respondent
elsoc$supvis <- elsoc$m06
sjmisc::frq(elsoc$supvis)

# head(elsoc[,c('cod_m03','cod_m22',"isco88",'m06','m25',"supvis")],50)
# View(elsoc[,c('cod_m03','cod_m22',"isco88",'m06','m25',"supvis")])


# Create a temporary variable `tmpisco88` and assign it the value of `isco88`
elsoc$tmpisco88 <- elsoc$isco88

# Replace values in `tmpelsoc$isco88` based on specified conditions
elsoc$tempisco <- ifelse((elsoc$isco88 >= 6100 & elsoc$isco88 <= 6133) & elsoc$supvis >= 1 & !is.na(elsoc$supvis), 1311, elsoc$isco88)
elsoc$tempisco <- ifelse((elsoc$isco88 >= 9200 & elsoc$isco88 <= 9213) & elsoc$supvis > 1 & !is.na(elsoc$supvis), 6132, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1311 & elsoc$supvis > 10 & !is.na(elsoc$supvis), 1221, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1312 & elsoc$supvis > 10 & !is.na(elsoc$supvis), 1222, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1313 & elsoc$supvis > 10 & !is.na(elsoc$supvis), 1223, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1314 & elsoc$supvis > 10 & !is.na(elsoc$supvis), 1224, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1315 & elsoc$supvis > 10 & !is.na(elsoc$supvis), 1225, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1316 & elsoc$supvis > 10 & !is.na(elsoc$supvis), 1226, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1317 & elsoc$supvis > 10 & !is.na(elsoc$supvis), 1227, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1318 & elsoc$supvis > 10 & !is.na(elsoc$supvis), 1228, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1319 & elsoc$supvis > 10 & !is.na(elsoc$supvis), 1229, elsoc$isco88)
elsoc$tempisco <- ifelse((elsoc$isco88 == 1300 | elsoc$isco88 == 1310) & elsoc$supvis > 10 & !is.na(elsoc$supvis), 1220, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1221 & (elsoc$supvis >= 1 & elsoc$supvis <= 10), 1311, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1222 & (elsoc$supvis >= 1 & elsoc$supvis <= 10), 1312, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1223 & (elsoc$supvis >= 1 & elsoc$supvis <= 10), 1313, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1224 & (elsoc$supvis >= 1 & elsoc$supvis <= 10), 1314, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1225 & (elsoc$supvis >= 1 & elsoc$supvis <= 10), 1315, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1226 & (elsoc$supvis >= 1 & elsoc$supvis <= 10), 1316, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1227 & (elsoc$supvis >= 1 & elsoc$supvis <= 10), 1317, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1228 & (elsoc$supvis >= 1 & elsoc$supvis <= 10), 1318, elsoc$isco88)
elsoc$tempisco <- ifelse(elsoc$isco88 == 1229 & (elsoc$supvis >= 1 & elsoc$supvis <= 10), 1319, elsoc$isco88)
elsoc$tempisco <- ifelse((elsoc$isco88 == 1200 | elsoc$isco88 == 1210 | elsoc$isco88 == 1220) & (elsoc$supvis >= 1 & elsoc$supvis <= 10), 1310, elsoc$isco88)
elsoc$tempisco <- ifelse((elsoc$isco88 == 1220 | (elsoc$isco88 >= 1222 & elsoc$isco88 <= 1229)) & elsoc$semp == 1 & elsoc$supvis >= 11 & !is.na(elsoc$supvis), 1210, elsoc$isco88)

sjmisc::frq(elsoc$tempisco) 

# haven::write_dta(data = elsoc[,c("tempisco","supvis","semp")],
#                  path = here::here("input/data/proc/elsoc18_egp.dta"))

# View(elsoc[,c("isco88",'tempisco')])

elsoc$egp10 <- NA 
# recode from ISCO88 to EGP11
{
elsoc$egp10[elsoc$tempisco == 1000] <- 1
elsoc$egp10[elsoc$tempisco == 1100] <- 1
elsoc$egp10[elsoc$tempisco == 1110] <- 1
elsoc$egp10[elsoc$tempisco == 1120] <- 1
elsoc$egp10[elsoc$tempisco == 1130] <- 2
elsoc$egp10[elsoc$tempisco == 1140] <- 2
elsoc$egp10[elsoc$tempisco == 1141] <- 2
elsoc$egp10[elsoc$tempisco == 1142] <- 2
elsoc$egp10[elsoc$tempisco == 1143] <- 2
elsoc$egp10[elsoc$tempisco == 1200] <- 1
elsoc$egp10[elsoc$tempisco == 1210] <- 1
elsoc$egp10[elsoc$tempisco == 1220] <- 1
elsoc$egp10[elsoc$tempisco == 1221] <- 11
elsoc$egp10[elsoc$tempisco == 1222] <- 1
elsoc$egp10[elsoc$tempisco == 1223] <- 1
elsoc$egp10[elsoc$tempisco == 1224] <- 1
elsoc$egp10[elsoc$tempisco == 1225] <- 1
elsoc$egp10[elsoc$tempisco == 1226] <- 1
elsoc$egp10[elsoc$tempisco == 1227] <- 1
elsoc$egp10[elsoc$tempisco == 1228] <- 1
elsoc$egp10[elsoc$tempisco == 1229] <- 1
elsoc$egp10[elsoc$tempisco == 1230] <- 1
elsoc$egp10[elsoc$tempisco == 1231] <- 1
elsoc$egp10[elsoc$tempisco == 1232] <- 1
elsoc$egp10[elsoc$tempisco == 1233] <- 1
elsoc$egp10[elsoc$tempisco == 1234] <- 1
elsoc$egp10[elsoc$tempisco == 1235] <- 1
elsoc$egp10[elsoc$tempisco == 1236] <- 1
elsoc$egp10[elsoc$tempisco == 1237] <- 1
elsoc$egp10[elsoc$tempisco == 1239] <- 1
elsoc$egp10[elsoc$tempisco == 1240] <- 2
elsoc$egp10[elsoc$tempisco == 1250] <- 1
elsoc$egp10[elsoc$tempisco == 1251] <- 1
elsoc$egp10[elsoc$tempisco == 1252] <- 2
elsoc$egp10[elsoc$tempisco == 1300] <- 2
elsoc$egp10[elsoc$tempisco == 1310] <- 2
elsoc$egp10[elsoc$tempisco == 1311] <- 11
elsoc$egp10[elsoc$tempisco == 1312] <- 2
elsoc$egp10[elsoc$tempisco == 1313] <- 2
elsoc$egp10[elsoc$tempisco == 1314] <- 2
elsoc$egp10[elsoc$tempisco == 1315] <- 2
elsoc$egp10[elsoc$tempisco == 1316] <- 2
elsoc$egp10[elsoc$tempisco == 1317] <- 2
elsoc$egp10[elsoc$tempisco == 1318] <- 2
elsoc$egp10[elsoc$tempisco == 1319] <- 2
elsoc$egp10[elsoc$tempisco == 2000] <- 1
elsoc$egp10[elsoc$tempisco == 2100] <- 1
elsoc$egp10[elsoc$tempisco == 2110] <- 1
elsoc$egp10[elsoc$tempisco == 2111] <- 1
elsoc$egp10[elsoc$tempisco == 2112] <- 1
elsoc$egp10[elsoc$tempisco == 2113] <- 1
elsoc$egp10[elsoc$tempisco == 2114] <- 1
elsoc$egp10[elsoc$tempisco == 2120] <- 1
elsoc$egp10[elsoc$tempisco == 2121] <- 1
elsoc$egp10[elsoc$tempisco == 2122] <- 1
elsoc$egp10[elsoc$tempisco == 2130] <- 1
elsoc$egp10[elsoc$tempisco == 2131] <- 1
elsoc$egp10[elsoc$tempisco == 2132] <- 2
elsoc$egp10[elsoc$tempisco == 2139] <- 2
elsoc$egp10[elsoc$tempisco == 2140] <- 1
elsoc$egp10[elsoc$tempisco == 2141] <- 1
elsoc$egp10[elsoc$tempisco == 2142] <- 1
elsoc$egp10[elsoc$tempisco == 2143] <- 1
elsoc$egp10[elsoc$tempisco == 2144] <- 1
elsoc$egp10[elsoc$tempisco == 2145] <- 1
elsoc$egp10[elsoc$tempisco == 2146] <- 1
elsoc$egp10[elsoc$tempisco == 2147] <- 1
elsoc$egp10[elsoc$tempisco == 2148] <- 2
elsoc$egp10[elsoc$tempisco == 2149] <- 1
elsoc$egp10[elsoc$tempisco == 2200] <- 1
elsoc$egp10[elsoc$tempisco == 2210] <- 1
elsoc$egp10[elsoc$tempisco == 2211] <- 1
elsoc$egp10[elsoc$tempisco == 2212] <- 1
elsoc$egp10[elsoc$tempisco == 2213] <- 1
elsoc$egp10[elsoc$tempisco == 2220] <- 1
elsoc$egp10[elsoc$tempisco == 2221] <- 1
elsoc$egp10[elsoc$tempisco == 2222] <- 1
elsoc$egp10[elsoc$tempisco == 2223] <- 1
elsoc$egp10[elsoc$tempisco == 2224] <- 1
elsoc$egp10[elsoc$tempisco == 2229] <- 1
elsoc$egp10[elsoc$tempisco == 2230] <- 2
elsoc$egp10[elsoc$tempisco == 2300] <- 2
elsoc$egp10[elsoc$tempisco == 2310] <- 1
elsoc$egp10[elsoc$tempisco == 2320] <- 2
elsoc$egp10[elsoc$tempisco == 2321] <- 2
elsoc$egp10[elsoc$tempisco == 2322] <- 2
elsoc$egp10[elsoc$tempisco == 2323] <- 2
elsoc$egp10[elsoc$tempisco == 2330] <- 2
elsoc$egp10[elsoc$tempisco == 2331] <- 2
elsoc$egp10[elsoc$tempisco == 2332] <- 2
elsoc$egp10[elsoc$tempisco == 2340] <- 2
elsoc$egp10[elsoc$tempisco == 2350] <- 1
elsoc$egp10[elsoc$tempisco == 2351] <- 1
elsoc$egp10[elsoc$tempisco == 2352] <- 1
elsoc$egp10[elsoc$tempisco == 2359] <- 2
elsoc$egp10[elsoc$tempisco == 2400] <- 1
elsoc$egp10[elsoc$tempisco == 2410] <- 2
elsoc$egp10[elsoc$tempisco == 2411] <- 1
elsoc$egp10[elsoc$tempisco == 2412] <- 2
elsoc$egp10[elsoc$tempisco == 2419] <- 2
elsoc$egp10[elsoc$tempisco == 2420] <- 1
elsoc$egp10[elsoc$tempisco == 2421] <- 1
elsoc$egp10[elsoc$tempisco == 2422] <- 1
elsoc$egp10[elsoc$tempisco == 2429] <- 1
elsoc$egp10[elsoc$tempisco == 2430] <- 2
elsoc$egp10[elsoc$tempisco == 2431] <- 2
elsoc$egp10[elsoc$tempisco == 2432] <- 2
elsoc$egp10[elsoc$tempisco == 2440] <- 1
elsoc$egp10[elsoc$tempisco == 2441] <- 1
elsoc$egp10[elsoc$tempisco == 2442] <- 1
elsoc$egp10[elsoc$tempisco == 2443] <- 1
elsoc$egp10[elsoc$tempisco == 2444] <- 2
elsoc$egp10[elsoc$tempisco == 2445] <- 1
elsoc$egp10[elsoc$tempisco == 2446] <- 2
elsoc$egp10[elsoc$tempisco == 2450] <- 2
elsoc$egp10[elsoc$tempisco == 2451] <- 2
elsoc$egp10[elsoc$tempisco == 2452] <- 2
elsoc$egp10[elsoc$tempisco == 2453] <- 2
elsoc$egp10[elsoc$tempisco == 2454] <- 2
elsoc$egp10[elsoc$tempisco == 2455] <- 2
elsoc$egp10[elsoc$tempisco == 2460] <- 2
elsoc$egp10[elsoc$tempisco == 3000] <- 2
elsoc$egp10[elsoc$tempisco == 3100] <- 2
elsoc$egp10[elsoc$tempisco == 3110] <- 2
elsoc$egp10[elsoc$tempisco == 3111] <- 2
elsoc$egp10[elsoc$tempisco == 3112] <- 2
elsoc$egp10[elsoc$tempisco == 3113] <- 2
elsoc$egp10[elsoc$tempisco == 3114] <- 2
elsoc$egp10[elsoc$tempisco == 3115] <- 2
elsoc$egp10[elsoc$tempisco == 3116] <- 2
elsoc$egp10[elsoc$tempisco == 3117] <- 2
elsoc$egp10[elsoc$tempisco == 3118] <- 2
elsoc$egp10[elsoc$tempisco == 3119] <- 2
elsoc$egp10[elsoc$tempisco == 3120] <- 2
elsoc$egp10[elsoc$tempisco == 3121] <- 2
elsoc$egp10[elsoc$tempisco == 3122] <- 2
elsoc$egp10[elsoc$tempisco == 3123] <- 2
elsoc$egp10[elsoc$tempisco == 3130] <- 2
elsoc$egp10[elsoc$tempisco == 3131] <- 2
elsoc$egp10[elsoc$tempisco == 3132] <- 2
elsoc$egp10[elsoc$tempisco == 3133] <- 2
elsoc$egp10[elsoc$tempisco == 3139] <- 2
elsoc$egp10[elsoc$tempisco == 3140] <- 2
elsoc$egp10[elsoc$tempisco == 3141] <- 2
elsoc$egp10[elsoc$tempisco == 3142] <- 2
elsoc$egp10[elsoc$tempisco == 3143] <- 1
elsoc$egp10[elsoc$tempisco == 3144] <- 1
elsoc$egp10[elsoc$tempisco == 3145] <- 2
elsoc$egp10[elsoc$tempisco == 3150] <- 2
elsoc$egp10[elsoc$tempisco == 3151] <- 2
elsoc$egp10[elsoc$tempisco == 3152] <- 2
elsoc$egp10[elsoc$tempisco == 3200] <- 2
elsoc$egp10[elsoc$tempisco == 3210] <- 2
elsoc$egp10[elsoc$tempisco == 3211] <- 2
elsoc$egp10[elsoc$tempisco == 3212] <- 2
elsoc$egp10[elsoc$tempisco == 3213] <- 2
elsoc$egp10[elsoc$tempisco == 3220] <- 2
elsoc$egp10[elsoc$tempisco == 3221] <- 2
elsoc$egp10[elsoc$tempisco == 3222] <- 2
elsoc$egp10[elsoc$tempisco == 3223] <- 2
elsoc$egp10[elsoc$tempisco == 3224] <- 2
elsoc$egp10[elsoc$tempisco == 3225] <- 2
elsoc$egp10[elsoc$tempisco == 3226] <- 2
elsoc$egp10[elsoc$tempisco == 3227] <- 2
elsoc$egp10[elsoc$tempisco == 3228] <- 2
elsoc$egp10[elsoc$tempisco == 3229] <- 2
elsoc$egp10[elsoc$tempisco == 3230] <- 3
elsoc$egp10[elsoc$tempisco == 3231] <- 3
elsoc$egp10[elsoc$tempisco == 3232] <- 3
elsoc$egp10[elsoc$tempisco == 3240] <- 2
elsoc$egp10[elsoc$tempisco == 3241] <- 2
elsoc$egp10[elsoc$tempisco == 3242] <- 2
elsoc$egp10[elsoc$tempisco == 3300] <- 3
elsoc$egp10[elsoc$tempisco == 3310] <- 3
elsoc$egp10[elsoc$tempisco == 3320] <- 3
elsoc$egp10[elsoc$tempisco == 3330] <- 3
elsoc$egp10[elsoc$tempisco == 3340] <- 3
elsoc$egp10[elsoc$tempisco == 3400] <- 2
elsoc$egp10[elsoc$tempisco == 3410] <- 2
elsoc$egp10[elsoc$tempisco == 3411] <- 2
elsoc$egp10[elsoc$tempisco == 3412] <- 2
elsoc$egp10[elsoc$tempisco == 3413] <- 2
elsoc$egp10[elsoc$tempisco == 3414] <- 2
elsoc$egp10[elsoc$tempisco == 3415] <- 2
elsoc$egp10[elsoc$tempisco == 3416] <- 2
elsoc$egp10[elsoc$tempisco == 3417] <- 2
elsoc$egp10[elsoc$tempisco == 3419] <- 2
elsoc$egp10[elsoc$tempisco == 3420] <- 2
elsoc$egp10[elsoc$tempisco == 3421] <- 2
elsoc$egp10[elsoc$tempisco == 3422] <- 2
elsoc$egp10[elsoc$tempisco == 3423] <- 2
elsoc$egp10[elsoc$tempisco == 3429] <- 2
elsoc$egp10[elsoc$tempisco == 3430] <- 3
elsoc$egp10[elsoc$tempisco == 3431] <- 2
elsoc$egp10[elsoc$tempisco == 3432] <- 2
elsoc$egp10[elsoc$tempisco == 3433] <- 3
elsoc$egp10[elsoc$tempisco == 3434] <- 2
elsoc$egp10[elsoc$tempisco == 3439] <- 3
elsoc$egp10[elsoc$tempisco == 3440] <- 2
elsoc$egp10[elsoc$tempisco == 3441] <- 2
elsoc$egp10[elsoc$tempisco == 3442] <- 2
elsoc$egp10[elsoc$tempisco == 3443] <- 2
elsoc$egp10[elsoc$tempisco == 3444] <- 2
elsoc$egp10[elsoc$tempisco == 3449] <- 2
elsoc$egp10[elsoc$tempisco == 3450] <- 2
elsoc$egp10[elsoc$tempisco == 3451] <- 2
elsoc$egp10[elsoc$tempisco == 3452] <- 7
elsoc$egp10[elsoc$tempisco == 3460] <- 3
elsoc$egp10[elsoc$tempisco == 3470] <- 2
elsoc$egp10[elsoc$tempisco == 3471] <- 2
elsoc$egp10[elsoc$tempisco == 3472] <- 2
elsoc$egp10[elsoc$tempisco == 3473] <- 2
elsoc$egp10[elsoc$tempisco == 3474] <- 2
elsoc$egp10[elsoc$tempisco == 3475] <- 2
elsoc$egp10[elsoc$tempisco == 3480] <- 3
elsoc$egp10[elsoc$tempisco == 4000] <- 3
elsoc$egp10[elsoc$tempisco == 4100] <- 3
elsoc$egp10[elsoc$tempisco == 4110] <- 3
elsoc$egp10[elsoc$tempisco == 4111] <- 3
elsoc$egp10[elsoc$tempisco == 4112] <- 3
elsoc$egp10[elsoc$tempisco == 4113] <- 3
elsoc$egp10[elsoc$tempisco == 4114] <- 3
elsoc$egp10[elsoc$tempisco == 4115] <- 3
elsoc$egp10[elsoc$tempisco == 4120] <- 3
elsoc$egp10[elsoc$tempisco == 4121] <- 3
elsoc$egp10[elsoc$tempisco == 4122] <- 3
elsoc$egp10[elsoc$tempisco == 4130] <- 3
elsoc$egp10[elsoc$tempisco == 4131] <- 3
elsoc$egp10[elsoc$tempisco == 4132] <- 3
elsoc$egp10[elsoc$tempisco == 4133] <- 3
elsoc$egp10[elsoc$tempisco == 4140] <- 3
elsoc$egp10[elsoc$tempisco == 4141] <- 3
elsoc$egp10[elsoc$tempisco == 4142] <- 9
elsoc$egp10[elsoc$tempisco == 4143] <- 3
elsoc$egp10[elsoc$tempisco == 4144] <- 3
elsoc$egp10[elsoc$tempisco == 4190] <- 3
elsoc$egp10[elsoc$tempisco == 4200] <- 3
elsoc$egp10[elsoc$tempisco == 4210] <- 3
elsoc$egp10[elsoc$tempisco == 4211] <- 3
elsoc$egp10[elsoc$tempisco == 4212] <- 3
elsoc$egp10[elsoc$tempisco == 4213] <- 3
elsoc$egp10[elsoc$tempisco == 4214] <- 3
elsoc$egp10[elsoc$tempisco == 4215] <- 3
elsoc$egp10[elsoc$tempisco == 4220] <- 3
elsoc$egp10[elsoc$tempisco == 4221] <- 3
elsoc$egp10[elsoc$tempisco == 4222] <- 3
elsoc$egp10[elsoc$tempisco == 4223] <- 3
elsoc$egp10[elsoc$tempisco == 5000] <- 3
elsoc$egp10[elsoc$tempisco == 5100] <- 3
elsoc$egp10[elsoc$tempisco == 5110] <- 3
elsoc$egp10[elsoc$tempisco == 5111] <- 3
elsoc$egp10[elsoc$tempisco == 5112] <- 3
elsoc$egp10[elsoc$tempisco == 5113] <- 3
elsoc$egp10[elsoc$tempisco == 5120] <- 3
elsoc$egp10[elsoc$tempisco == 5121] <- 2
elsoc$egp10[elsoc$tempisco == 5122] <- 8
elsoc$egp10[elsoc$tempisco == 5123] <- 9
elsoc$egp10[elsoc$tempisco == 5130] <- 9
elsoc$egp10[elsoc$tempisco == 5131] <- 3
elsoc$egp10[elsoc$tempisco == 5132] <- 9
elsoc$egp10[elsoc$tempisco == 5133] <- 3
elsoc$egp10[elsoc$tempisco == 5139] <- 9
elsoc$egp10[elsoc$tempisco == 5140] <- 8
elsoc$egp10[elsoc$tempisco == 5141] <- 8
elsoc$egp10[elsoc$tempisco == 5142] <- 9
elsoc$egp10[elsoc$tempisco == 5143] <- 8
elsoc$egp10[elsoc$tempisco == 5149] <- 9
elsoc$egp10[elsoc$tempisco == 5150] <- 2
elsoc$egp10[elsoc$tempisco == 5151] <- 2
elsoc$egp10[elsoc$tempisco == 5152] <- 2
elsoc$egp10[elsoc$tempisco == 5160] <- 9
elsoc$egp10[elsoc$tempisco == 5161] <- 8
elsoc$egp10[elsoc$tempisco == 5162] <- 8
elsoc$egp10[elsoc$tempisco == 5163] <- 9
elsoc$egp10[elsoc$tempisco == 5164] <- 8
elsoc$egp10[elsoc$tempisco == 5169] <- 9
elsoc$egp10[elsoc$tempisco == 5200] <- 3
elsoc$egp10[elsoc$tempisco == 5210] <- 3
elsoc$egp10[elsoc$tempisco == 5220] <- 3
elsoc$egp10[elsoc$tempisco == 5230] <- 3
elsoc$egp10[elsoc$tempisco == 6000] <- 10
elsoc$egp10[elsoc$tempisco == 6100] <- 10
elsoc$egp10[elsoc$tempisco == 6110] <- 10
elsoc$egp10[elsoc$tempisco == 6111] <- 10
elsoc$egp10[elsoc$tempisco == 6112] <- 10
elsoc$egp10[elsoc$tempisco == 6113] <- 10
elsoc$egp10[elsoc$tempisco == 6114] <- 10
elsoc$egp10[elsoc$tempisco == 6120] <- 10
elsoc$egp10[elsoc$tempisco == 6121] <- 10
elsoc$egp10[elsoc$tempisco == 6122] <- 10
elsoc$egp10[elsoc$tempisco == 6123] <- 10
elsoc$egp10[elsoc$tempisco == 6124] <- 10
elsoc$egp10[elsoc$tempisco == 6129] <- 10
elsoc$egp10[elsoc$tempisco == 6130] <- 10
elsoc$egp10[elsoc$tempisco == 6131] <- 11
elsoc$egp10[elsoc$tempisco == 6132] <- 11
elsoc$egp10[elsoc$tempisco == 6133] <- 11
elsoc$egp10[elsoc$tempisco == 6134] <- 10
elsoc$egp10[elsoc$tempisco == 6140] <- 10
elsoc$egp10[elsoc$tempisco == 6141] <- 10
elsoc$egp10[elsoc$tempisco == 6142] <- 10
elsoc$egp10[elsoc$tempisco == 6150] <- 10
elsoc$egp10[elsoc$tempisco == 6151] <- 10
elsoc$egp10[elsoc$tempisco == 6152] <- 10
elsoc$egp10[elsoc$tempisco == 6153] <- 10
elsoc$egp10[elsoc$tempisco == 6154] <- 10
elsoc$egp10[elsoc$tempisco == 6200] <- 11
elsoc$egp10[elsoc$tempisco == 6210] <- 11
elsoc$egp10[elsoc$tempisco == 7000] <- 8
elsoc$egp10[elsoc$tempisco == 7100] <- 8
elsoc$egp10[elsoc$tempisco == 7110] <- 8
elsoc$egp10[elsoc$tempisco == 7111] <- 8
elsoc$egp10[elsoc$tempisco == 7112] <- 8
elsoc$egp10[elsoc$tempisco == 7113] <- 8
elsoc$egp10[elsoc$tempisco == 7120] <- 8
elsoc$egp10[elsoc$tempisco == 7121] <- 9
elsoc$egp10[elsoc$tempisco == 7122] <- 9
elsoc$egp10[elsoc$tempisco == 7123] <- 9
elsoc$egp10[elsoc$tempisco == 7124] <- 8
elsoc$egp10[elsoc$tempisco == 7129] <- 8
elsoc$egp10[elsoc$tempisco == 7130] <- 8
elsoc$egp10[elsoc$tempisco == 7131] <- 9
elsoc$egp10[elsoc$tempisco == 7132] <- 8
elsoc$egp10[elsoc$tempisco == 7133] <- 8
elsoc$egp10[elsoc$tempisco == 7134] <- 8
elsoc$egp10[elsoc$tempisco == 7135] <- 9
elsoc$egp10[elsoc$tempisco == 7136] <- 8
elsoc$egp10[elsoc$tempisco == 7137] <- 8
elsoc$egp10[elsoc$tempisco == 7140] <- 8
elsoc$egp10[elsoc$tempisco == 7141] <- 8
elsoc$egp10[elsoc$tempisco == 7142] <- 9
elsoc$egp10[elsoc$tempisco == 7143] <- 9
elsoc$egp10[elsoc$tempisco == 7200] <- 8
elsoc$egp10[elsoc$tempisco == 7210] <- 8
elsoc$egp10[elsoc$tempisco == 7211] <- 8
elsoc$egp10[elsoc$tempisco == 7212] <- 8
elsoc$egp10[elsoc$tempisco == 7213] <- 8
elsoc$egp10[elsoc$tempisco == 7214] <- 8
elsoc$egp10[elsoc$tempisco == 7215] <- 8
elsoc$egp10[elsoc$tempisco == 7216] <- 8
elsoc$egp10[elsoc$tempisco == 7220] <- 8
elsoc$egp10[elsoc$tempisco == 7221] <- 8
elsoc$egp10[elsoc$tempisco == 7222] <- 8
elsoc$egp10[elsoc$tempisco == 7223] <- 8
elsoc$egp10[elsoc$tempisco == 7224] <- 8
elsoc$egp10[elsoc$tempisco == 7230] <- 8
elsoc$egp10[elsoc$tempisco == 7231] <- 8
elsoc$egp10[elsoc$tempisco == 7232] <- 8
elsoc$egp10[elsoc$tempisco == 7233] <- 8
elsoc$egp10[elsoc$tempisco == 7234] <- 9
elsoc$egp10[elsoc$tempisco == 7240] <- 8
elsoc$egp10[elsoc$tempisco == 7241] <- 8
elsoc$egp10[elsoc$tempisco == 7242] <- 8
elsoc$egp10[elsoc$tempisco == 7243] <- 8
elsoc$egp10[elsoc$tempisco == 7244] <- 8
elsoc$egp10[elsoc$tempisco == 7245] <- 8
elsoc$egp10[elsoc$tempisco == 7300] <- 8
elsoc$egp10[elsoc$tempisco == 7310] <- 8
elsoc$egp10[elsoc$tempisco == 7311] <- 8
elsoc$egp10[elsoc$tempisco == 7312] <- 8
elsoc$egp10[elsoc$tempisco == 7313] <- 8
elsoc$egp10[elsoc$tempisco == 7320] <- 9
elsoc$egp10[elsoc$tempisco == 7321] <- 9
elsoc$egp10[elsoc$tempisco == 7322] <- 9
elsoc$egp10[elsoc$tempisco == 7323] <- 8
elsoc$egp10[elsoc$tempisco == 7324] <- 8
elsoc$egp10[elsoc$tempisco == 7330] <- 9
elsoc$egp10[elsoc$tempisco == 7331] <- 9
elsoc$egp10[elsoc$tempisco == 7332] <- 9
elsoc$egp10[elsoc$tempisco == 7340] <- 8
elsoc$egp10[elsoc$tempisco == 7341] <- 8
elsoc$egp10[elsoc$tempisco == 7342] <- 8
elsoc$egp10[elsoc$tempisco == 7343] <- 8
elsoc$egp10[elsoc$tempisco == 7344] <- 8
elsoc$egp10[elsoc$tempisco == 7345] <- 8
elsoc$egp10[elsoc$tempisco == 7346] <- 8
elsoc$egp10[elsoc$tempisco == 7400] <- 8
elsoc$egp10[elsoc$tempisco == 7410] <- 8
elsoc$egp10[elsoc$tempisco == 7411] <- 8
elsoc$egp10[elsoc$tempisco == 7412] <- 8
elsoc$egp10[elsoc$tempisco == 7413] <- 8
elsoc$egp10[elsoc$tempisco == 7414] <- 8
elsoc$egp10[elsoc$tempisco == 7415] <- 8
elsoc$egp10[elsoc$tempisco == 7416] <- 8
elsoc$egp10[elsoc$tempisco == 7420] <- 8
elsoc$egp10[elsoc$tempisco == 7421] <- 9
elsoc$egp10[elsoc$tempisco == 7422] <- 8
elsoc$egp10[elsoc$tempisco == 7423] <- 8
elsoc$egp10[elsoc$tempisco == 7424] <- 9
elsoc$egp10[elsoc$tempisco == 7430] <- 8
elsoc$egp10[elsoc$tempisco == 7431] <- 9
elsoc$egp10[elsoc$tempisco == 7432] <- 9
elsoc$egp10[elsoc$tempisco == 7433] <- 8
elsoc$egp10[elsoc$tempisco == 7434] <- 8
elsoc$egp10[elsoc$tempisco == 7435] <- 8
elsoc$egp10[elsoc$tempisco == 7436] <- 8
elsoc$egp10[elsoc$tempisco == 7437] <- 8
elsoc$egp10[elsoc$tempisco == 7440] <- 8
elsoc$egp10[elsoc$tempisco == 7441] <- 8
elsoc$egp10[elsoc$tempisco == 7442] <- 8
elsoc$egp10[elsoc$tempisco == 7500] <- 8
elsoc$egp10[elsoc$tempisco == 7510] <- 7
elsoc$egp10[elsoc$tempisco == 7520] <- 8
elsoc$egp10[elsoc$tempisco == 7530] <- 9
elsoc$egp10[elsoc$tempisco == 8000] <- 9
elsoc$egp10[elsoc$tempisco == 8100] <- 9
elsoc$egp10[elsoc$tempisco == 8110] <- 8
elsoc$egp10[elsoc$tempisco == 8111] <- 8
elsoc$egp10[elsoc$tempisco == 8112] <- 8
elsoc$egp10[elsoc$tempisco == 8113] <- 8
elsoc$egp10[elsoc$tempisco == 8120] <- 8
elsoc$egp10[elsoc$tempisco == 8121] <- 8
elsoc$egp10[elsoc$tempisco == 8122] <- 8
elsoc$egp10[elsoc$tempisco == 8123] <- 8
elsoc$egp10[elsoc$tempisco == 8124] <- 8
elsoc$egp10[elsoc$tempisco == 8130] <- 9
elsoc$egp10[elsoc$tempisco == 8131] <- 9
elsoc$egp10[elsoc$tempisco == 8139] <- 9
elsoc$egp10[elsoc$tempisco == 8140] <- 9
elsoc$egp10[elsoc$tempisco == 8141] <- 9
elsoc$egp10[elsoc$tempisco == 8142] <- 9
elsoc$egp10[elsoc$tempisco == 8143] <- 9
elsoc$egp10[elsoc$tempisco == 8150] <- 8
elsoc$egp10[elsoc$tempisco == 8151] <- 8
elsoc$egp10[elsoc$tempisco == 8152] <- 8
elsoc$egp10[elsoc$tempisco == 8153] <- 8
elsoc$egp10[elsoc$tempisco == 8154] <- 8
elsoc$egp10[elsoc$tempisco == 8155] <- 8
elsoc$egp10[elsoc$tempisco == 8159] <- 8
elsoc$egp10[elsoc$tempisco == 8160] <- 8
elsoc$egp10[elsoc$tempisco == 8161] <- 8
elsoc$egp10[elsoc$tempisco == 8162] <- 8
elsoc$egp10[elsoc$tempisco == 8163] <- 8
elsoc$egp10[elsoc$tempisco == 8170] <- 8
elsoc$egp10[elsoc$tempisco == 8171] <- 8
elsoc$egp10[elsoc$tempisco == 8172] <- 8
elsoc$egp10[elsoc$tempisco == 8200] <- 9
elsoc$egp10[elsoc$tempisco == 8210] <- 8
elsoc$egp10[elsoc$tempisco == 8211] <- 8
elsoc$egp10[elsoc$tempisco == 8212] <- 9
elsoc$egp10[elsoc$tempisco == 8220] <- 9
elsoc$egp10[elsoc$tempisco == 8221] <- 9
elsoc$egp10[elsoc$tempisco == 8222] <- 9
elsoc$egp10[elsoc$tempisco == 8223] <- 9
elsoc$egp10[elsoc$tempisco == 8224] <- 9
elsoc$egp10[elsoc$tempisco == 8229] <- 9
elsoc$egp10[elsoc$tempisco == 8230] <- 9
elsoc$egp10[elsoc$tempisco == 8231] <- 9
elsoc$egp10[elsoc$tempisco == 8232] <- 9
elsoc$egp10[elsoc$tempisco == 8240] <- 9
elsoc$egp10[elsoc$tempisco == 8250] <- 9
elsoc$egp10[elsoc$tempisco == 8251] <- 9
elsoc$egp10[elsoc$tempisco == 8252] <- 9
elsoc$egp10[elsoc$tempisco == 8253] <- 9
elsoc$egp10[elsoc$tempisco == 8260] <- 9
elsoc$egp10[elsoc$tempisco == 8261] <- 9
elsoc$egp10[elsoc$tempisco == 8262] <- 9
elsoc$egp10[elsoc$tempisco == 8263] <- 9
elsoc$egp10[elsoc$tempisco == 8264] <- 9
elsoc$egp10[elsoc$tempisco == 8265] <- 9
elsoc$egp10[elsoc$tempisco == 8266] <- 9
elsoc$egp10[elsoc$tempisco == 8269] <- 9
elsoc$egp10[elsoc$tempisco == 8270] <- 9
elsoc$egp10[elsoc$tempisco == 8271] <- 9
elsoc$egp10[elsoc$tempisco == 8272] <- 9
elsoc$egp10[elsoc$tempisco == 8273] <- 9
elsoc$egp10[elsoc$tempisco == 8274] <- 9
elsoc$egp10[elsoc$tempisco == 8275] <- 9
elsoc$egp10[elsoc$tempisco == 8276] <- 9
elsoc$egp10[elsoc$tempisco == 8277] <- 9
elsoc$egp10[elsoc$tempisco == 8278] <- 9
elsoc$egp10[elsoc$tempisco == 8279] <- 9
elsoc$egp10[elsoc$tempisco == 8280] <- 9
elsoc$egp10[elsoc$tempisco == 8281] <- 9
elsoc$egp10[elsoc$tempisco == 8282] <- 9
elsoc$egp10[elsoc$tempisco == 8283] <- 9
elsoc$egp10[elsoc$tempisco == 8284] <- 9
elsoc$egp10[elsoc$tempisco == 8285] <- 9
elsoc$egp10[elsoc$tempisco == 8286] <- 9
elsoc$egp10[elsoc$tempisco == 8290] <- 9
elsoc$egp10[elsoc$tempisco == 8300] <- 9
elsoc$egp10[elsoc$tempisco == 8310] <- 9
elsoc$egp10[elsoc$tempisco == 8311] <- 8
elsoc$egp10[elsoc$tempisco == 8312] <- 9
elsoc$egp10[elsoc$tempisco == 8320] <- 9
elsoc$egp10[elsoc$tempisco == 8321] <- 9
elsoc$egp10[elsoc$tempisco == 8322] <- 9
elsoc$egp10[elsoc$tempisco == 8323] <- 9
elsoc$egp10[elsoc$tempisco == 8324] <- 9
elsoc$egp10[elsoc$tempisco == 8330] <- 9
elsoc$egp10[elsoc$tempisco == 8331] <- 10
elsoc$egp10[elsoc$tempisco == 8332] <- 8
elsoc$egp10[elsoc$tempisco == 8333] <- 8
elsoc$egp10[elsoc$tempisco == 8334] <- 9
elsoc$egp10[elsoc$tempisco == 8340] <- 9
elsoc$egp10[elsoc$tempisco == 8400] <- 9
elsoc$egp10[elsoc$tempisco == 9000] <- 9
elsoc$egp10[elsoc$tempisco == 9100] <- 3
elsoc$egp10[elsoc$tempisco == 9110] <- 3
elsoc$egp10[elsoc$tempisco == 9111] <- 3
elsoc$egp10[elsoc$tempisco == 9112] <- 3
elsoc$egp10[elsoc$tempisco == 9113] <- 3
elsoc$egp10[elsoc$tempisco == 9120] <- 9
elsoc$egp10[elsoc$tempisco == 9130] <- 9
elsoc$egp10[elsoc$tempisco == 9131] <- 9
elsoc$egp10[elsoc$tempisco == 9132] <- 9
elsoc$egp10[elsoc$tempisco == 9133] <- 9
elsoc$egp10[elsoc$tempisco == 9140] <- 9
elsoc$egp10[elsoc$tempisco == 9141] <- 9
elsoc$egp10[elsoc$tempisco == 9142] <- 9
elsoc$egp10[elsoc$tempisco == 9150] <- 9
elsoc$egp10[elsoc$tempisco == 9151] <- 9
elsoc$egp10[elsoc$tempisco == 9152] <- 9
elsoc$egp10[elsoc$tempisco == 9153] <- 9
elsoc$egp10[elsoc$tempisco == 9160] <- 9
elsoc$egp10[elsoc$tempisco == 9161] <- 9
elsoc$egp10[elsoc$tempisco == 9162] <- 9
elsoc$egp10[elsoc$tempisco == 9200] <- 9
elsoc$egp10[elsoc$tempisco == 9210] <- 10
elsoc$egp10[elsoc$tempisco == 9211] <- 10
elsoc$egp10[elsoc$tempisco == 9212] <- 10
elsoc$egp10[elsoc$tempisco == 9213] <- 10
elsoc$egp10[elsoc$tempisco == 9300] <- 9
elsoc$egp10[elsoc$tempisco == 9310] <- 9
elsoc$egp10[elsoc$tempisco == 9311] <- 9
elsoc$egp10[elsoc$tempisco == 9312] <- 9
elsoc$egp10[elsoc$tempisco == 9313] <- 9
elsoc$egp10[elsoc$tempisco == 9320] <- 9
elsoc$egp10[elsoc$tempisco == 9321] <- 9
elsoc$egp10[elsoc$tempisco == 9322] <- 9
elsoc$egp10[elsoc$tempisco == 9330] <- 9
elsoc$egp10[elsoc$tempisco == 9331] <- 9
elsoc$egp10[elsoc$tempisco == 9332] <- 9
elsoc$egp10[elsoc$tempisco == 9333] <- 9
}

sjmisc::frq(elsoc$egp10) 

# Start of ISCOEGP.INC section
# `p' codes promotability of certain occupations
elsoc$p <- rep(NA, length(elsoc$tempisco))
elsoc$p[elsoc$tempisco >= 1000 & elsoc$tempisco <= 9299] <- 1
sjmisc::frq(elsoc$p)


# `d' codes degradability of certain occupations
elsoc$d <- rep(NA, length(elsoc$tempisco))
elsoc$d[elsoc$tempisco >= 1300 & elsoc$tempisco <= 1319 |
          elsoc$tempisco >= 3400 & elsoc$tempisco <= 3439 |
          elsoc$tempisco >= 4000 & elsoc$tempisco <= 5230] <- 1
sjmisc::frq(elsoc$d)

elsoc$egp10_b <- elsoc$egp10
sjmisc::frq(elsoc$egp10_b)

# quietly replace `varlist'=2  if  (`varlist' == 3 & `tmpsv' >= 1 & `tmpsv' ~= .);
elsoc$egp10_b[elsoc$egp10_b == 3 & elsoc$supvis >= 1 & !is.na(elsoc$supvis)] <- 2

# quietly replace `varlist'=4  if ((`varlist' == 3 | `varlist' == 2) &
# 	                                  `sempl' == 1 &
# 	                                  `d' == 1);
	                                  
elsoc$egp10_b[elsoc$egp10_b %in% c(3, 2) & elsoc$semp == 1 & elsoc$d == 1] <- 4

# quietly replace `varlist'=5  if ((`varlist' >= 7 & `varlist' <= 9) &
# 	                                  `sempl' == 1 &
# 	                                  `p' == 1);
elsoc$egp10_b[elsoc$egp10_b %in% c(7, 8, 9) & elsoc$semp == 1 & elsoc$p == 1] <- 5

# quietly replace `varlist'=7  if (`varlist' == 8  & `tmpsv' >= 1 & `tmpsv' ~= .);
elsoc$egp10_b[elsoc$egp10_b == 8 & elsoc$supvis >= 1 & !is.na(elsoc$supvis)] <- 7

# quietly replace `varlist'=11 if (`varlist' == 10 & `sempl' == 1);
elsoc$egp10_b[elsoc$egp10_b == 10 & elsoc$semp == 1] <- 11

# quietly replace `varlist'=5  if (`varlist' == 4 & `tmpsv' < 1);
elsoc$egp10_b[elsoc$egp10_b == 4 & elsoc$supvis < 1] <- 5


# quietly replace `varlist'=4 if  (`varlist' == 5 & `tmpsv' >= 1 & `tmpsv' ~= .);
elsoc$egp10_b[elsoc$egp10_b == 5 & elsoc$supvis >= 1 & !is.na(elsoc$supvis)] <- 4

# quietly replace `varlist'=1 if  ((`varlist' == 2 | `varlist' == 3 | `varlist' == 4) &
#                                   `tmpsv' >= 10 & `tmpsv' ~= .);
elsoc$egp10_b[elsoc$egp10_b %in% c(2, 3, 4) & elsoc$supvis >= 10 & !is.na(elsoc$supvis)] <- 1

sjmisc::frq(elsoc$egp10_b)

elsoc$egp10_b <- sjlabelled::set_labels(x = elsoc$egp10_b,
                                        labels = c("higher controllers"=1 ,
                                                                     "lo controllers"=2,
                                                                     "routine nonmanual"=3 ,
                                                                     "sempl with emp"=4,
                                                                     "sempl without empl"=5,
                                                                     "manual supervisor"=7,
                                                                     "skilled manual"=8,
                                                                     "semi-unskilld manual"=9,
                                                                     "farm labor"=10,
                                                                     "selfempl farm"=11))

sjmisc::frq(elsoc$egp10_b)

elsoc$egp10 <- elsoc$egp10_b
elsoc$egp10_b <- NULL
sjmisc::frq(elsoc$egp10)

# Adaptaciones para AL --------------------------------------------------

# * Step 2.	Changing order of classes
elsoc$clase11 <- car::recode(elsoc$egp10, "7=8; 8=9; 9=10; 10=11; 11=7")
elsoc$clase11 <- sjlabelled::set_labels(elsoc$clase11, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                        labels = c("I", "II", "IIIa", "IIIb", "IVa", "IVb", "IVc", "V", "VI", "VIIa", "VIIb"))

sjmisc::frq(elsoc$clase11)
# * Step 3.	Genearating variable "situacion", which combines "sempl" and "supvis"
elsoc$situacion <- ifelse(elsoc$semp == 1 & elsoc$supvis == 0, 1,
                    ifelse(elsoc$semp == 1 & elsoc$supvis >= 1 & elsoc$supvis <= 9, 2,
                           ifelse(elsoc$semp == 1 & elsoc$supvis >= 10, 3,
                                  ifelse(elsoc$semp == 2 & elsoc$supvis == 0, 4,
                                         ifelse(elsoc$semp == 2 & elsoc$supvis >= 1 & elsoc$supvis <= 9, 5,
                                                ifelse(elsoc$semp == 2 & elsoc$supvis >= 10, 6, NA))))))
elsoc$situacion <- sjlabelled::set_labels(elsoc$situacion,
                    levels = c(1, 2, 3, 4, 5, 6),
                    labels = c("Dep, 0", "Dep, 1-9", "Dep, 10+", "SE, 0", "SE, 1-9", "SE, 10+"))
sjmisc::frq(elsoc$situacion)

# * Step 4. Modifications discussed and approved on March 1, 2012

# * Managers and administrators in specialized organizations (groups 1141-1319) 

                  # * 1. Self-employed with no dependents to IVb
elsoc$clase11 <- ifelse(elsoc$tempisco >= 1141 & elsoc$tempisco <= 1319 & elsoc$tempisco != 1221 & elsoc$tempisco != 1311 & elsoc$situacion == 4, 6,
                  # * 2. Employers with less than 10 dependents to IVa      
                  ifelse(elsoc$tempisco >= 1141 & elsoc$tempisco <= 1319 & elsoc$tempisco != 1221 & elsoc$tempisco != 1311 & elsoc$situacion == 5, 5,
                  # * 3. Supervisors with less than 10 persons at charge from 1 to 2        
                         ifelse(elsoc$tempisco >= 1141 & elsoc$tempisco <= 1319 & elsoc$tempisco != 1221 & elsoc$tempisco != 1311 & elsoc$situacion <= 2, 2, elsoc$clase11)))

# * Profesionals (great group 2, 2111 a 2460)

  # * 4. Inspectors in education and similar occupations are given equal treatment than teachers in basic education(<10 dep = clase II)
elsoc$clase11 <- ifelse((elsoc$tempisco >= 2351 & elsoc$tempisco <= 2352 & elsoc$situacion <= 2) |
                    (elsoc$tempisco >= 2351 & elsoc$tempisco <= 2352 & elsoc$situacion == 4) |
                    (elsoc$tempisco >= 2351 & elsoc$tempisco <= 2352 & elsoc$situacion == 5), 2, elsoc$clase11)


# * Tecnichians and middle-level professionals (great group 3)

# * 5. Healers and similar workers removed from higher classes
elsoc$clase11  <- ifelse((elsoc$tempisco >= 3241 & elsoc$tempisco <= 3242 & elsoc$situacion == 1), 10,
                  ifelse((elsoc$tempisco >= 3241 & elsoc$tempisco <= 3242 & elsoc$situacion >= 2 & elsoc$situacion <= 3), 8,
                         ifelse((elsoc$tempisco >= 3241 & elsoc$tempisco <= 3242 & elsoc$situacion == 4), 6,
                                ifelse((elsoc$tempisco >= 3241 & elsoc$tempisco <= 3242 & elsoc$situacion >= 5), 5,
                                       elsoc$clase11 ))))


# * 6. Teachers in elementary education treated similarly than other similar positions (2320 a 2359)

elsoc$clase11  <- ifelse((elsoc$tempisco >= 3310 & elsoc$tempisco <= 3340 & (elsoc$situacion == 1 | elsoc$situacion == 4)), 2, elsoc$clase11 )

# * 7. Middle-level non-manual workers (no technicians) to class IIIa+b if they do not have subordinate workers
elsoc$clase11  <- ifelse((elsoc$tempisco == 3419 & elsoc$situacion == 1) | (elsoc$tempisco >= 3429 & elsoc$tempisco <= 3480 & elsoc$situacion == 1), 3, elsoc$clase11 )


# * 8. Middle-level non-manual workers (no technicians) to class IVb if they are self-employed with no employees 
elsoc$clase11  <- ifelse(elsoc$tempisco == 3419 & elsoc$situacion == 4, 6,
                  ifelse(elsoc$tempisco >= 3429 & elsoc$tempisco <= 3479 & elsoc$situacion == 4, 6,
                         ifelse(elsoc$tempisco == 3480, 4, elsoc$clase11 )))

# * 9. Middle-level non-manual workers (no technicians) to class IVb if they are employers with <10 employees	
elsoc$clase11  <- ifelse(elsoc$tempisco >= 3411 & elsoc$tempisco <= 3480 & elsoc$situacion == 5, 5, elsoc$clase11 )

# * Workers in personal services and sales (group 5)

# * 10. Transport inspectors waiters, and other workers in personals services and semi-skilled occupations		
# *	are classified as unkilled or manual supervisors according to their labor situation

elsoc$clase11 <- 
  ifelse(elsoc$tempisco == 5111 & elsoc$situacion == 1, 10,
         ifelse(elsoc$tempisco == 5111 & (elsoc$situacion == 2 | elsoc$situacion == 3), 8,
                ifelse(elsoc$tempisco == 5111 & elsoc$situacion == 4, 10,
                       ifelse(elsoc$tempisco == 5111 & elsoc$situacion >= 5, 8,
                              ifelse((elsoc$tempisco == 5121 | (elsoc$tempisco > 5123 & elsoc$tempisco <= 5139)) & elsoc$situacion == 1, 10,
                                     ifelse((elsoc$tempisco == 5121 | (elsoc$tempisco > 5123 & elsoc$tempisco <= 5139)) & (elsoc$situacion == 2 | elsoc$situacion == 3), 8,
                                            ifelse((elsoc$tempisco == 5121 | (elsoc$tempisco > 5123 & elsoc$tempisco <= 5139)) & elsoc$situacion == 4, 10,
                                                   ifelse((elsoc$tempisco == 5121 | (elsoc$tempisco > 5123 & elsoc$tempisco <= 5139)) & elsoc$situacion >= 5, 8,
                                                          ifelse(elsoc$tempisco == 5132 & elsoc$situacion == 4, 9,
                                                                 ifelse(elsoc$tempisco >= 5142 & elsoc$tempisco <= 5152 & elsoc$situacion == 1, 10,
                                                                        ifelse(elsoc$tempisco >= 5142 & elsoc$tempisco <= 5152 & (elsoc$situacion == 2 | elsoc$situacion == 3), 8,
                                                                               ifelse(elsoc$tempisco >= 5142 & elsoc$tempisco <= 5152 & elsoc$situacion == 4, 10,
                                                                                      ifelse(elsoc$tempisco >= 5142 & elsoc$tempisco <= 5152 & elsoc$situacion >= 5, 8,
                                                                                             ifelse(elsoc$tempisco >= 5163 & elsoc$tempisco <= 5169 & elsoc$situacion == 1, 10,
                                                                                                    ifelse(elsoc$tempisco >= 5163 & elsoc$tempisco <= 5169 & (elsoc$situacion == 2 | elsoc$situacion == 3), 8,
                                                                                                           ifelse(elsoc$tempisco >= 5163 & elsoc$tempisco <= 5169 & elsoc$situacion == 4, 10,
                                                                                                                  ifelse(elsoc$tempisco >= 5163 & elsoc$tempisco <= 5169 & elsoc$situacion >= 5, 8, elsoc$clase11)))))))))))))))))
                
# * Farmers and farm laborers (group 6)
  # * 11. Farm laborers to VIIb
elsoc$clase11 <- ifelse(elsoc$tempisco >= 6111 & elsoc$tempisco <= 6210 & elsoc$situacion <= 3, 11, elsoc$clase11)

# * Manual Operatives (gran grupo 8)
# * 12. Operatives in chemical industry equalized to other skilled manual workers
elsoc$clase11 <- ifelse(elsoc$tempisco >= 8221 & elsoc$tempisco <= 8229 & elsoc$situacion == 1, 9,
                  ifelse(elsoc$tempisco >= 8221 & elsoc$tempisco <= 8229 & elsoc$situacion >= 2 & elsoc$situacion <= 3, 8, elsoc$clase11))

# * 13. Machine operatives in any sector to class V if they are supervisors
elsoc$clase11 <- ifelse((elsoc$tempisco >= 8131 & elsoc$tempisco <= 8143 | elsoc$tempisco == 8212 | elsoc$tempisco >= 8231 & elsoc$tempisco <= 8324 | elsoc$tempisco >= 8334 & elsoc$tempisco <= 8340) & elsoc$situacion >= 2 & elsoc$situacion <= 3, 8, elsoc$clase11)

# * 14. Operatives in groups 8231 to 8279 who are dependent workers and are not supervisors to class VI 
elsoc$clase11 <- ifelse(elsoc$tempisco >= 8231 & elsoc$tempisco <= 8279 & elsoc$situacion == 1, 8, elsoc$clase11)

# * Unskilled workers (group 9)
# * 15. Unskilled workers to class VIIa (except when they are employers or supervisors)
elsoc$clase11 <- ifelse(elsoc$tempisco >= 9111 & elsoc$tempisco <= 9113 & elsoc$situacion >= 2 & elsoc$situacion <= 3, 8, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$tempisco >= 9111 & elsoc$tempisco <= 9113 & elsoc$situacion >= 5 & elsoc$situacion <= 6, 5, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$tempisco >= 9111 & elsoc$tempisco <= 9113 & (elsoc$situacion == 1 | elsoc$situacion == 4), 10, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$tempisco >= 9114 & elsoc$tempisco <= 9162, 10, elsoc$clase11)

# ** 16. (final adjustments may 5 2012): salaried workers with no workers at charge in 7123, 7131, 7421 and  7432 
# * reasigned to  VI 
elsoc$clase11 <- ifelse((elsoc$tempisco == 7123 | elsoc$tempisco == 7131 | elsoc$tempisco == 7421 | elsoc$tempisco == 7432) & elsoc$situacion == 1, 9, elsoc$clase11)


# * Step 5. Adjustment of "petit bourgeoisie". The adjustment excludes from this group self-employed workers
# 					in unskilled jobs that typically are part of the "informal economy" in Latin America. 
# 					Many of these workers perform their jobs in "masked" or "hidden" precarious contractual relationships,
# 					i.e. as day laborers, although they may declare that are self-employed. 
# 					The list also excludes from IVb those workers in occupations that cannot be performed as self-employed,
# 					i.e. 8152 Chemical-heat-treating-plant operators . Many of these codes are "empty" in the data set, that
# 					is, with no observed cases, thus the impact of these modifications is not as high as could be expected 
# 					given the high number of codes included in the adjustment.
{
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 5112, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 5161, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 5162, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7111, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7112, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7113, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7124, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7134, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7135, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7136, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7137, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7141, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7142, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7415, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7416, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7421, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7423, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7436, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8111, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8112, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8113, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8121, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8122, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8123, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8124, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8131, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8139, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8141, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8142, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8143, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8151, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8152, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8153, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8154, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8155, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8159, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8161, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8162, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8163, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8171, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8172, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8211, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8212, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8221, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8222, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8223, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8224, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8229, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8231, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8232, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8240, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8251, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8252, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8253, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8261, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8262, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8264, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8271, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8272, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8273, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8274, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8275, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8276, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8277, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8278, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8279, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8290, 10, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8311, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8312, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8322, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8323, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8324, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8332, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8333, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8334, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8340, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 3241, 10, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 3242, 10, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7121, 10, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7122, 10, elsoc$clase11)
# ** Modificacion 5 de mayo: a clase VI
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7123, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7129, 10, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7131, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7132, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7133, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7143, 10, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7413, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7414, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7424, 10, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7431, 10, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 7432, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8281, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8282, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8283, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8284, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8285, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8286, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8321, 10, elsoc$clase11)
# // ** Modificacion 5 de mayo: a clase VI, mensaje Raul 12 de marzo
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8263, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8265, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8266, 9, elsoc$clase11)
elsoc$clase11 <- ifelse(elsoc$clase11 == 6 & elsoc$tempisco == 8269, 9, elsoc$clase11)
}

elsoc$clase11 <- sjlabelled::set_labels(elsoc$clase11, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                                        labels = c("I", "II", "IIIa", "IIIb", "IVa", "IVb", "IVc", "V", "VI", "VIIa", "VIIb"))

elsoc$clase11e <- elsoc$clase11
sjmisc::frq(elsoc$clase11)





