
use "C:\Users\JC\Dropbox\paper_sss-perc_meritocracy\code\data\ELSOC_ess_merit2018.dta" 
use "C:\Users\JC\Dropbox\papers\paper_ess-meritocracy\code\data\ELSOC_ess_merit2018.dta" 



tab isco88
*recode nempleados (.=0)
iskoegp egp , isko(isco88) sempl(semp) supvis(nempleados)



*isko specifies the variable to be recoded.  This must be a 4 digit integer containing ISCO-88 occupational codes.

*sempl specifies a variable indicating whether or not the respondent is self-employed. A 1 indicates self-employment, all other values are ignored.

*supvis specifies a variable indicating the number of employees the respondent supervises. The values 1 and 10 are significant for placement in certain EGP categories.
tab egp

recode egp (7=6) (8=7) (9=8) (10=9)(11=10) //para arreglar la escala 

label define ocup 1 "I Alto control" 2 "II Bajo control" 3 "III Rutinas no manuales" ///
4 "IVa Auto-empleo con empleados" 5 "IVb Auto-empleo sin empleados" ///
6 "V Supervisor de labores manuales" 7 "VI Obrero calificado" ///
8 "VIIa Obrero semicalificado" 9 "VIIb Labores agrícolas"  10 "IVc Autoempleo agrícola"
label value egp ocup
tab egp

*#########################
*ESQUEMA DE TORCHE (2005)#
*#########################

*drop egptorche
gen egptorche=egp
recode egptorche (1 2=1)(3=2)(4 5=3) (6 7 =4) (8=5) (10=6)(9=7)
*label drop egptorlab
label define egptorlab 1 "Servicio (I+II)" 2 "Rutinas no manuales(III)" ///
3 "Auto Empleo(IVab)" 4 "Obrero calificado (V+VI)" 5 "Obrero no calificado (VIIa)" ///
6 "Autoempleo agrícola (IVc)" 7 "Labores agrícolas (VIIb)"

label value egptorche egptorlab
tab egptorche

save "C:\Users\JC\Dropbox\paper_sss-perc_meritocracy\code\data\ELSOC_egp2018.dta"
