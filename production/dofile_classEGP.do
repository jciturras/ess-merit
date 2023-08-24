clear
use "P:\papers\ess-merit\input\data\proc\ELSOC_ocup.dta" 

*ssc install isko
*help isko

*ssc install iscogen
*help iscogen
*codebook isco88
*recode nempleados (.=0)
iskoegp egp , isko(isco88) sempl(selfemp) supvis(nempleados)
tab egp
iskoisei isei , isko(isco88)

iskotrei siops , isko(isco88)

*iskolab

*isko specifies the variable to be recoded.  This must be a 4 digit integer containing ISCO-88 occupational codes.

*sempl specifies a variable indicating whether or not the respondent is self-employed. A 1 indicates self-employment, all other values are ignored.

*supvis specifies a variable indicating the number of employees the respondent supervises. The values 1 and 10 are significant for placement in certain EGP categories.

save "P:\papers\ess-merit\input\data\proc\ELSOC_egp-isei-siops.dta", replace

iscogen job88 = isco88(isco08), from(isco08)
*sum job08
*drop job08 

iscogen EGP = egp11(isco08 selfemp nempleados), from(isco88)
tab EGP

bro cod_m03 cod_m22 isco88 isco08 job88 selfemp nempleados egp11 egp7 egp EGP
 
