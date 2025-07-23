****prueba por sexo*******************************************************************
**************************************************************************************


******2022*********************************************************************************
**Gasto en salud de los hogares 2022
use "D:\PP\BASES ENIGH\POBLACION 2022\poblacion.dta"
sort foliohog
merge m:m foliohog folioviv using "D:\PP\BASES ENIGH\POBLACION 2022\concentradohogar.dta" 
tab _merge
drop _merge
collapse (mean) salud, by (entidad)
list
clear all

***poblacion que no trabajó el mes pasado**********************************************
*tab trabajo_mp [w=factor], m
**************************************************************************************
use "D:\PP\BASES ENIGH\POBLACION 2022\poblacion.dta"

destring sexo, replace
destring entidad, replace
tab sexo [w=factor], m
tabstat sexo  [w=factor], by (ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sexo, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad sexo [w=factor], m //numerador y denominador muestral 




tab trabajo_mp [w=factor]
tab trabajo_mp [w=factor], m
desc trabajo_mp
tab trabajo_mp, m
describe trabajo_mp
destring trabajo_mp, replace
gen notrabajomespasado = "1" if trabajo_mp==2
replace notrabajomespasado = "." if trabajo_mp==.
replace notrabajomespasado = "0" if trabajo_mp==1
destring notrabajomespasado, replace

tab notrabajomespasado [w=factor]
tab notrabajomespasado [w=factor], m
destring notrabajomespasado, replace
tabstat notrabajomespasado  [w=factor], by (ent) f(%10.9gc)

desc notrabajomespasado

destring entidad, replace
tab entidad [w=factor], m
tab entidad sexo [w=factor], m
tab sexo notrabajomespasado [w=factor], m

*tab final 

*tabstat sin_pago_p  [w=fac_tri] if  clase2==1 & (eda>=15 & eda<=98), by (ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajomespasado, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad notrabajomespasado [w=factor], m //numerador y denominador muestral 

tab entidad notrabajomespasado if sexo ==1 [w=factor], m
tab entidad notrabajomespasado if sexo ==2 [w=factor], m




***poblacion inscrita a seguro de salud*****2022*************************************
*** afiliado o inscrito para recibir atención*****************************************
***médica de parte de alguna institución************************************************
****************************************************************************************

tab atemed [w=factor]
tab atemed [w=factor], m
gen inscrsalud = "1" if atemed=="1"
replace inscrsalud = "0" if atemed=="2"
replace inscrsalud = "." if atemed=="."

tab inscrsalud [w=factor]
tab inscrsalud [w=factor], m
*destring notrabajomespasado, replace
destring inscrsalud, replace
tabstat inscrsalud  [w=factor], by (ent) f(%10.9gc)
destring sexo, replace
*precisiones 
svyset upm [w=factor] ,  strata(est_dis) vce(linearized) singleunit(centered)  
destring entidad, replace
svy: mean inscrsalud, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad  inscrsalud [w=factor], m  //numerador y denominador muestral 
tab entidad  inscrsalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab entidad  inscrsalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 




***pob  trabajo mes pasado***2022*********************
***************************************************************************************
***************************************************************************************
tab trabajo_mp [w=factor]
tab trabajo_mp [w=factor], m
desc trabajo_mp
tab trabajo_mp, m
describe trabajo_mp
destring trabajo_mp, replace


tab trabajo_mp [w=factor]
tab trabajo_mp[w=factor], m
destring trabajo_mp, replace
tabstat trabajo_mp  [w=factor], by (ent) f(%10.9gc)

desc trabajo_mp

destring entidad, replace
tab entidad [w=factor], m
tab entidad sexo [w=factor], m
tab sexo trabajo_mp [w=factor], m
destring trabajo_mp, replace

*precisiones 
svyset upm [w=factor] ,  strata(est_dis) vce(linearized) singleunit(centered)  
destring entidad, replace
svy: mean   trabajo_mp, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad  trabajo_mp [w=factor], m  //numerador y denominador muestral 


tab entidad  trabajo_mp if trabajo_mp==1 & sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab entidad  trabajo_mp if trabajo_mp==1 & sexo ==2 [w=factor], m  //numerador y denominador muestral 

















***poblacion no inscrita a seguro de salud*****2022*************************************
***NO afiliado o inscrito para recibir atención*****************************************
***médica de parte de alguna institución************************************************
****************************************************************************************
tab atemed [w=factor]
tab atemed [w=factor], m
gen noinscrsalud = "1" if atemed=="2"
replace noinscrsalud = "0" if atemed=="1"
replace noinscrsalud = "." if atemed=="."

tab noinscrsalud [w=factor]
tab noinscrsalud [w=factor], m
*destring notrabajomespasado, replace
destring noinscrsalud, replace
tabstat noinscrsalud  [w=factor], by (ent) f(%10.9gc)
destring sexo, replace
*precisiones 
svyset upm [w=factor] ,  strata(est_dis) vce(linearized) singleunit(centered)  
destring entidad, replace
svy: mean noinscrsalud, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad  noinscrsalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab entidad  noinscrsalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 



***pob no trabajo y no está inscrita a un sistema de salud ***2022*********************
***************************************************************************************
***************************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen notrabajonosalud = "1" if notrabajomespasado==1 & noinscrsalud==1
replace notrabajonosalud = "0" if notrabajomespasado==0 | noinscrsalud==0
replace notrabajonosalud = "." if notrabajomespasado==. | noinscrsalud==.

tab notrabajonosalud [w=factor]
tab notrabajonosalud [w=factor], m
destring notrabajonosalud, replace
tabstat notrabajonosalud  [w=factor], by (ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajonosalud, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad  notrabajonosalud [w=factor], m //numerador y denominador muestral 

tab entidad  notrabajonosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab entidad  notrabajonosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 


***pob no trabajo y está inscrita a un sistema de salud *****2022**********************
***************************************************************************************
***************************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen notrabajosisalud = "1" if notrabajomespasado==1 & noinscrsalud==0
replace notrabajosisalud = "0" if notrabajomespasado==0 | noinscrsalud==1
replace notrabajosisalud = "." if notrabajomespasado==. | noinscrsalud==.

tab notrabajosisalud [w=factor]
tab notrabajosisalud [w=factor], m
destring notrabajosisalud, replace
tabstat notrabajosisalud  [w=factor], by (ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajosisalud, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad  notrabajosisalud [w=factor], m  //numerador y denominador muestral 

tab entidad  notrabajosisalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab entidad  notrabajosisalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 




********pob con trabajo y NO está inscrita a un sistema de salud**2022************* ***********************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen sitrabajonosalud = "1" if notrabajomespasado==0 & noinscrsalud==1
replace sitrabajonosalud = "0" if notrabajomespasado==1 | noinscrsalud==0
replace sitrabajonosalud = "." if notrabajomespasado==. | noinscrsalud==.

tab sitrabajonosalud [w=factor]
tab sitrabajonosalud [w=factor], m
destring sitrabajonosalud, replace
tabstat sitrabajonosalud  [w=factor], by (ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sitrabajonosalud, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad  sitrabajonosalud [w=factor], m //numerador y denominador muestral 

tab entidad  sitrabajonosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab entidad  sitrabajonosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 


********pob con trabajo y  está inscrita a un sistema de salud 2022*************** **********************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen sitrabajosisalud = "1" if notrabajomespasado==0 & noinscrsalud==0
replace sitrabajosisalud = "0" if notrabajomespasado==1 | noinscrsalud==1
replace sitrabajosisalud = "." if notrabajomespasado==. | noinscrsalud==.

tab sitrabajosisalud [w=factor]
tab sitrabajosisalud [w=factor], m
destring sitrabajosisalud, replace
tabstat sitrabajosisalud  [w=factor], by (ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sitrabajosisalud, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad  sitrabajosisalud [w=factor], m //numerador y denominador muestral 

tab sitrabajosisalud sexo [w=factor], m
tab sexo sitrabajosisalud [w=factor], m
tab sexo notrabajonosalud [w=factor], m

tab entidad  sitrabajosisalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab entidad  sitrabajosisalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 

**
**
**poblacion en condición de discapacidad 2022
destring disc_acti, replace
tab disc_acti [w=factor], m 
gen pobcondisc = "1" if disc_acti=="1"| disc_acti=="2"|disc_acti=="3"|disc_acti=="&"
replace pobcondisc = "0" if  disc_acti=="4"
replace pobcondisc = "." if  disc_acti=="."

tab pobcondisc [w=factor], m 
tab noinscrsalud [w=factor], m

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean pobcondisc, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad  pobcondisc [w=factor], m //numerador y denominador muestral 

tab pobcondisc sexo [w=factor], m
tab sexo pobcondisc [w=factor], m

tab entidad  pobcondisc if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab entidad  pobcondisc if sexo ==2 [w=factor], m  //numerador y denominador muestral 



destring pobcondisc, replace
tab pobcondisc noinscrsalud [w=factor], m
gen dicsnosalud = 1 if pobcondisc == 1 & noinscrsalud == 1
replace dicsnosalud = 0 if (pobcondisc ==0 & noinscrsalud ==0) | (pobcondisc ==0 & noinscrsalud ==1) | (pobcondisc ==1 & noinscrsalud ==0) 
replace dicsnosalud = . if pobcondisc ==. | noinscrsalud ==.
tab dicsnosalud [w=factor], m

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean dicsnosalud, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad  dicsnosalud [w=factor], m //numerador y denominador muestral 

tab dicsnosalud sexo [w=factor], m
tab sexo dicsnosalud [w=factor]

tab entidad dicsnosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab entidad dicsnosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 



**************************************************************************************
**********2020************************************************************************
use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2020\concentradohogar.dta"
use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2020\hogares.dta"

use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2020\poblacion.dta"

sort foliohog
merge m:m foliohog folioviv using "D:\PP\BASES ENIGH\POBLACION CONCHOG 2020\concentradohogar.dta" 
tab _merge
drop _merge
save "D:\PP\BASES ENIGH\POBLACION CONCHOG 2020\base1 2020.dta", replace

destring sexo, replace
tab sexo  [w=factor], m

use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2020\base1 2020.dta", clear 
sort foliohog
merge m:m foliohog using "D:\PP\BASES ENIGH\POBLACION CONCHOG 2020\hogares.dta" 
tab _merge
drop _merge
save "D:\PP\BASES ENIGH\POBLACION CONCHOG 2020\base2 2020.dta", replace

use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2020\base2 2020.dta"

destring sexo, replace
tab sexo  [w=factor], m


****Gastos del hogar en salud*****2016*************************************
collapse (mean) salud, by (clave_ent)
list


describe trabajo_mp
destring trabajo_mp, replace
tab clave_ent trabajo_mp [w=factor], m


gen notrabajomespasado = "."
replace notrabajomespasado =  "1" if trabajo_mp==2
replace notrabajomespasado = "." if trabajo_mp==.
replace notrabajomespasado = "0" if trabajo_mp==1
destring notrabajomespasado, replace

tab notrabajomespasado [w=factor]
*tab clave_ent notrabajomespasado [w=factor], m
destring notrabajomespasado, replace

desc ubica_geo
tab ubica_geo
*drop clave_ent
*drop clave_mun
gen clave_mun = substr(ubica_geo, 3, 5)
tab clave_mun
*drop clave_ent
gen clave_ent = substr(ubica_geo,1, 2)
tab clave_ent [w=factor]

**2020 no sale
destring sexo, replace
destring clave_ent, replace
tab clave_ent sexo [w=factor], m
tabstat  sexo  [w=factor], by (clave_ent) f(%10.9gc)

gen muj=.
replace muj=1 if sexo==2
replace muj=. if sexo !=2

tab muj[w=factor], m

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajomespasado sexo, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent notrabajomespasado [w=factor], m //numerador y denominador muestral 

tab clave_ent notrabajomespasado if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent notrabajomespasado if sexo ==2 [w=factor], m  //numerador y denominador muestral 



***poblacion inscrita a seguro de salud*****2020*************************************
*** afiliado o inscrito para recibir atención*****************************************
***médica de parte de alguna institución************************************************
****************************************************************************************

tab atemed [w=factor]
tab atemed [w=factor], m
gen inscrsalud = "1" if atemed=="1"
replace inscrsalud = "0" if atemed=="2"
replace inscrsalud = "." if atemed=="."

tab inscrsalud [w=factor]
tab inscrsalud [w=factor], m
*destring notrabajomespasado, replace
destring inscrsalud, replace
tabstat inscrsalud  [w=factor], by (clave_ent) f(%10.9gc)
destring sexo, replace
*precisiones 
svyset upm [w=factor] ,  strata(est_dis) vce(linearized) singleunit(centered)  
destring clave_ent, replace
svy: mean inscrsalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  inscrsalud [w=factor], m  //numerador y denominador muestral 
tab clave_ent  inscrsalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent inscrsalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 














************************************************************************************
************************************************************************************
***poblacion no inscrita a seguro de salud*****2020*************************************
***NO afiliado o inscrito para recibir atención*****************************************
***médica de parte de alguna institución************************************************
****************************************************************************************
tab atemed [w=factor]
tab atemed [w=factor], m
gen noinscrsalud = "1" if atemed=="2"
replace noinscrsalud = "0" if atemed=="1"
replace noinscrsalud = "." if atemed=="."

tab noinscrsalud [w=factor]
tab noinscrsalud [w=factor], m
*destring notrabajomespasado, replace
destring noinscrsalud, replace
tabstat noinscrsalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor] ,  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean noinscrsalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  noinscrsalud [w=factor], m  //numerador y denominador muestral 

tab clave_ent noinscrsalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent noinscrsalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 




***pob no trabajo y no está inscrita a un sistema de salud ***2020*********************
***************************************************************************************
***************************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen notrabajonosalud = "1" if notrabajomespasado==1 & noinscrsalud==1
replace notrabajonosalud = "0" if notrabajomespasado==0 | noinscrsalud==0
replace notrabajonosalud = "." if notrabajomespasado==. | noinscrsalud==.

tab notrabajonosalud [w=factor]
tab notrabajonosalud [w=factor], m
destring notrabajonosalud, replace
tabstat notrabajonosalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajonosalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  notrabajonosalud [w=factor], m //numerador y denominador muestral 

tab clave_ent notrabajonosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent notrabajonosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 




***pob no trabajo y está inscrita a un sistema de salud *****2020**********************
***************************************************************************************
***************************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen notrabajosisalud = "1" if notrabajomespasado==1 & noinscrsalud==0
replace notrabajosisalud = "0" if notrabajomespasado==0 | noinscrsalud==1
replace notrabajosisalud = "." if notrabajomespasado==. | noinscrsalud==.

tab notrabajosisalud [w=factor]
tab notrabajosisalud [w=factor], m
destring notrabajosisalud, replace
tabstat notrabajosisalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajosisalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  notrabajosisalud [w=factor], m  //numerador y denominador muestral 

tab clave_ent notrabajosisalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent notrabajosisalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 


********pob con trabajo y NO está inscrita a un sistema de salud**2020************* ***********************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen sitrabajonosalud = "1" if notrabajomespasado==0 & noinscrsalud==1
replace sitrabajonosalud = "0" if notrabajomespasado==1 | noinscrsalud==0
replace sitrabajonosalud = "." if notrabajomespasado==. | noinscrsalud==.

tab sitrabajonosalud [w=factor]
tab sitrabajonosalud [w=factor], m
destring sitrabajonosalud, replace
tabstat sitrabajonosalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sitrabajonosalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  sitrabajonosalud [w=factor], m //numerador y denominador muestral 

tab clave_ent sitrabajonosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent sitrabajonosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 



********pob con trabajo y  está inscrita a un sistema de salud 2020*************** ***********************************************************************************

tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen sitrabajosisalud = "1" if notrabajomespasado==0 & noinscrsalud==0
replace sitrabajosisalud = "0" if notrabajomespasado==1 | noinscrsalud==1
replace sitrabajosisalud = "." if notrabajomespasado==. | noinscrsalud==.

tab sitrabajosisalud [w=factor]
tab sitrabajosisalud [w=factor], m
destring sitrabajosisalud, replace
tabstat sitrabajosisalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sitrabajosisalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  sitrabajosisalud [w=factor], m //numerador y denominador muestral 

tab sitrabajosisalud sexo [w=factor], m
tab sexo sitrabajosisalud [w=factor], m
tab sexo notrabajonosalud [w=factor], m


tab clave_ent sitrabajosisalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent sitrabajosisalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 


**
**
**poblacion en condición de discapacidad 2020
destring disc_acti, replace
tab disc_acti [w=factor], m 
gen pobcondisc = "1" if disc_acti=="1"| disc_acti=="2"|disc_acti=="3"|disc_acti=="&"
replace pobcondisc = "0" if  disc_acti=="4"
replace pobcondisc = "." if  disc_acti=="."

tab pobcondisc [w=factor], m 
tab noinscrsalud [w=factor], m
destring pobcondisc, replace

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean pobcondisc, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  pobcondisc [w=factor], m //numerador y denominador muestral 

tab pobcondisc sexo [w=factor], m
tab sexo pobcondisc [w=factor], m

tab clave_ent pobcondisc if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent pobcondisc if sexo ==2 [w=factor], m  //numerador y denominador muestral 



tab pobcondisc noinscrsalud [w=factor], m
gen dicsnosalud = 1 if pobcondisc == 1 & noinscrsalud == 1
replace dicsnosalud = 0 if (pobcondisc ==0 & noinscrsalud ==0) | (pobcondisc ==0 & noinscrsalud ==1) | (pobcondisc ==1 & noinscrsalud ==0) 
replace dicsnosalud = . if pobcondisc ==. | noinscrsalud ==.
tab dicsnosalud [w=factor], m

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean dicsnosalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  dicsnosalud [w=factor], m //numerador y denominador muestral 

tab dicsnosalud sexo [w=factor], m
tab sexo dicsnosalud [w=factor]

tab clave_ent dicsnosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent dicsnosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 





**********************************************************************************
**********************************************************************************
**2018
**************************************************************************************
**********************************************************************************

**Gasto de los hogare en salud 2018
use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2018\poblacion.dta"
sort foliohog
merge m:m foliohog folioviv using "D:\PP\BASES ENIGH\POBLACION CONCHOG 2018\concentradohogar.dta" 
 gen clave_ent = substr(ubica_geo,1, 2)
tab clave_ent
tab _merge
drop _merge
collapse (mean) salud, by (clave_ent)
list
clear all


use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2018\concentradohogar.dta"
use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2018\hogares.dta"

use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2018\poblacion.dta"


sort foliohog
merge m:m foliohog folioviv using "D:\PP\BASES ENIGH\POBLACION CONCHOG 2018\concentradohogar.dta" 
tab _merge
drop _merge
save "D:\PP\BASES ENIGH\POBLACION CONCHOG 2018\base1 2018.dta", replace

destring sexo, replace
tab sexo  [w=factor], m

use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2018\base1 2018.dta", clear 
sort foliohog
merge m:m foliohog using "D:\PP\BASES ENIGH\POBLACION CONCHOG 2018\hogares.dta" 
tab _merge
drop _merge
save "D:\PP\BASES ENIGH\POBLACION CONCHOG 2018\base2 2018.dta", replace

use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2018\base2 2018.dta", clear

destring sexo, replace
tab sexo  [w=factor], m

describe trabajo_mp
destring trabajo_mp, replace
gen notrabajomespasado = "."
replace notrabajomespasado =  "1" if trabajo_mp==2
replace notrabajomespasado = "." if trabajo_mp==.
replace notrabajomespasado = "0" if trabajo_mp==1
destring notrabajomespasado, replace

tab notrabajomespasado [w=factor]
tab notrabajomespasado [w=factor], m
destring notrabajomespasado, replace

desc ubica_geo
tab ubica_geo
*drop clave_ent
*drop clave_mun
gen clave_mun = substr(ubica_geo, 3, 5)
tab clave_mun
*drop clave_ent
gen clave_ent = substr(ubica_geo,1, 2)
tab clave_ent

destring sexo, replace
destring clave_ent, replace
tab sexo [w=factor], m
tabstat sexo  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajomespasado, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent notrabajomespasado [w=factor], m //numerador y denominador muestral 

tab clave_ent notrabajomespasado if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent notrabajomespasado if sexo ==2 [w=factor], m  //numerador y denominador muestral 




************************************************************************************
************************************************************************************
***poblacion no inscrita a seguro de salud*****2018*************************************
***NO afiliado o inscrito para recibir atención*****************************************
***médica de parte de alguna institución************************************************
****************************************************************************************
tab atemed [w=factor]
tab atemed [w=factor], m
gen noinscrsalud = "1" if atemed=="2"
replace noinscrsalud = "0" if atemed=="1"
replace noinscrsalud = "." if atemed=="."

tab noinscrsalud [w=factor]
tab noinscrsalud [w=factor], m
*destring notrabajomespasado, replace
destring noinscrsalud, replace
tabstat noinscrsalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor] ,  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean noinscrsalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  noinscrsalud [w=factor], m  //numerador y denominador muestral 

tab clave_ent noinscrsalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent noinscrsalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 




***pob no trabajo y no está inscrita a un sistema de salud ***2018*********************
***************************************************************************************
***************************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen notrabajonosalud = "1" if notrabajomespasado==1 & noinscrsalud==1
replace notrabajonosalud = "0" if notrabajomespasado==0 | noinscrsalud==0
replace notrabajonosalud = "." if notrabajomespasado==. | noinscrsalud==.

tab notrabajonosalud [w=factor]
tab notrabajonosalud [w=factor], m
destring notrabajonosalud, replace
tabstat notrabajonosalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajonosalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  notrabajonosalud [w=factor], m //numerador y denominador muestral 

tab clave_ent notrabajonosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent notrabajonosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 




***pob no trabajo y está inscrita a un sistema de salud *****2018**********************
***************************************************************************************
***************************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen notrabajosisalud = "1" if notrabajomespasado==1 & noinscrsalud==0
replace notrabajosisalud = "0" if notrabajomespasado==0 | noinscrsalud==1
replace notrabajosisalud = "." if notrabajomespasado==. | noinscrsalud==.

tab notrabajosisalud [w=factor]
tab notrabajosisalud [w=factor], m
destring notrabajosisalud, replace
tabstat notrabajosisalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajosisalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  notrabajosisalud [w=factor], m  //numerador y denominador muestral 

tab clave_ent notrabajosisalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent notrabajosisalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 




********pob con trabajo y NO está inscrita a un sistema de salud**2018************* ***********************************************************************************
tab sitrabajomespasado noinscrsalud [w=factor]
tab sitrabajomespasado noinscrsalud [w=factor], m
gen sitrabajonosalud = "1" if notrabajomespasado==0 & noinscrsalud==1
replace sitrabajonosalud = "0" if notrabajomespasado==1 | noinscrsalud==0
replace sitrabajonosalud = "." if notrabajomespasado==. | noinscrsalud==.

tab sitrabajonosalud [w=factor]
tab sitrabajonosalud [w=factor], m
destring sitrabajonosalud, replace
tabstat sitrabajonosalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sitrabajonosalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  sitrabajonosalud [w=factor], m //numerador y denominador muestral 

tab clave_ent sitrabajonosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent sitrabajonosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 



********pob con trabajo y  está inscrita a un sistema de salud 2018*************** ***********************************************************************************

tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen sitrabajosisalud = "1" if notrabajomespasado==0 & noinscrsalud==0
replace sitrabajosisalud = "0" if notrabajomespasado==1 | noinscrsalud==1
replace sitrabajosisalud = "." if notrabajomespasado==. | noinscrsalud==.

tab sitrabajosisalud [w=factor]
tab sitrabajosisalud [w=factor], m
destring sitrabajosisalud, replace
tabstat sitrabajosisalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sitrabajosisalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  sitrabajosisalud [w=factor], m //numerador y denominador muestral 

tab sitrabajosisalud sexo [w=factor], m
tab sexo sitrabajosisalud [w=factor], m
tab sexo notrabajonosalud [w=factor], m


tab clave_ent sitrabajosisalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent sitrabajosisalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 




**
**
**poblacion en condición de discapacidad 2018
destring disc1, replace
tab disc1 [w=factor], m
gen pobcondisc = "1" if disc1=="1"| disc1=="2"|disc1=="3"|disc1=="4"|disc1=="5"| disc1=="6"|disc1=="7"
replace pobcondisc = "0" if  disc1=="8"
replace pobcondisc = "." if  disc1=="&" 

tab pobcondisc [w=factor], m 
tab noinscrsalud [w=factor], m
destring pobcondisc, replace

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean pobcondisc, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  pobcondisc [w=factor], m //numerador y denominador muestral 

tab pobcondisc sexo [w=factor], m
tab sexo pobcondisc [w=factor], m

tab clave_ent pobcondisc if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent pobcondisc if sexo ==2 [w=factor], m  //numerador y denominador muestral 



tab pobcondisc noinscrsalud [w=factor], m
gen dicsnosalud = 1 if pobcondisc == 1 & noinscrsalud == 1
replace dicsnosalud = 0 if (pobcondisc ==0 & noinscrsalud ==0) | (pobcondisc ==0 & noinscrsalud ==1) | (pobcondisc ==1 & noinscrsalud ==0) 
replace dicsnosalud = . if pobcondisc ==. | noinscrsalud ==.
tab dicsnosalud [w=factor], m

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean dicsnosalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  dicsnosalud [w=factor], m //numerador y denominador muestral 

tab dicsnosalud sexo [w=factor], m
tab sexo dicsnosalud [w=factor]

tab clave_ent dicsnosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent dicsnosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 



**********************************************************************************
**********************************************************************************
**2016
**************************************************************************************
**********************************************************************************
**Gasto de los hogare en salud 2016
use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2016\poblacion.dta"
sort foliohog
merge m:m foliohog folioviv using "D:\PP\BASES ENIGH\POBLACION CONCHOG 2016\concentradohogar.dta" 
 gen clave_ent = substr(ubica_geo,1, 2)
tab clave_ent
***Para calcular el gasto en salud de los hogares por entidad federativa
collapse (mean) salud, by (clave_ent)
list
clear all

gen tgsalud = .
replace tgsalud = salud


gen poptot = .
replace poptot = 1 if sexo == 1 | sexo == 2

ssc install inequal7
gen gsaludppersona = .
replace gsaludppersona = tgsalud/poptot


* Crear una nueva variable para almacenar el índice de Atkinson
gen atkinson_index = .
levelsof clave_ent, local(entities)

foreach ent of local entities {
    * Filtrar los datos para la entidad actual
    preserve
    keep if clave_ent == `ent'
    
    * Verificar si hay observaciones después de filtrar
    count
    if r(N) > 0 {
        * Calcular el índice de Atkinson
        ineqrbd gsaludppersona [aw=poptot], atkinson(1)
        
        * Guardar el resultado en la variable atkinson_index
        local atkinson_value = r(atkinson1)
        restore
        replace atkinson_index = `atkinson_value' if clave_ent == `ent'
    } else {
        restore
    }
}



Imprimir los resultados:
stata
Copiar código
foreach ent in `entities' {
    display "Entity: `ent'"
    list clave_ent atkinson_index if clave_ent == `ent'
}





* Imprimir los resultados
foreach ent in `entities' {
    list clave_ent atkinson_index if clave_ent == `ent'
}

tab atkinson_index


use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2016\concentradohogar.dta"
use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2016\hogares.dta"

use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2016\poblacion.dta"

sort foliohog
merge m:m foliohog folioviv using "D:\PP\BASES ENIGH\POBLACION CONCHOG 2016\concentradohogar.dta" 
tab _merge
drop _merge
save "D:\PP\BASES ENIGH\POBLACION CONCHOG 2016\base1 2016.dta", replace

destring sexo, replace
tab sexo  [w=factor], m

use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2016\base1 2016.dta", clear 
sort foliohog
merge m:m foliohog using "D:\PP\BASES ENIGH\POBLACION CONCHOG 2016\hogares.dta" 
tab _merge
drop _merge
save "D:\PP\BASES ENIGH\POBLACION CONCHOG 2016\base2 2016.dta", replace

use "D:\PP\BASES ENIGH\POBLACION CONCHOG 2016\base2 2016.dta", clear

destring sexo, replace
tab sexo  [w=factor], m

describe trabajo_mp
destring trabajo_mp, replace
gen notrabajomespasado = "."
replace notrabajomespasado =  "1" if trabajo_mp==2
replace notrabajomespasado = "." if trabajo_mp==.
replace notrabajomespasado = "0" if trabajo_mp==1
destring notrabajomespasado, replace

tab notrabajomespasado [w=factor]
tab notrabajomespasado [w=factor], m
destring notrabajomespasado, replace

desc ubica_geo
tab ubica_geo
*drop clave_ent
*drop clave_mun
*gen clave_mun = substr(ubica_geo, 3, 5)
*tab clave_mun
*drop clave_ent
gen clave_ent = substr(ubica_geo,1, 2)
tab clave_ent

destring sexo, replace
destring clave_ent, replace
tab sexo [w=factor], m
tabstat sexo  [w=factor], by (clave_ent) f(%10.9gc)
tab clave_ent sexo [w=factor]

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajomespasado, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent notrabajomespasado [w=factor], m //numerador y denominador muestral 


tab clave_ent notrabajomespasado if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent notrabajomespasado if sexo ==2 [w=factor], m  //numerador y denominador muestral 



************************************************************************************
************************************************************************************
***poblacion no inscrita a seguro de salud*****2016*************************************
***NO afiliado o inscrito para recibir atención*****************************************
***médica de parte de alguna institución************************************************
****************************************************************************************
tab atemed [w=factor]
tab atemed [w=factor], m
gen noinscrsalud = "1" if atemed=="2"
replace noinscrsalud = "0" if atemed=="1"
replace noinscrsalud = "." if atemed=="."

tab noinscrsalud [w=factor]
tab noinscrsalud [w=factor], m
*destring notrabajomespasado, replace
destring noinscrsalud, replace
tabstat noinscrsalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor] ,  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean noinscrsalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  noinscrsalud [w=factor], m  //numerador y denominador muestral 

tab clave_ent noinscrsalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent noinscrsalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 



***pob no trabajo y no está inscrita a un sistema de salud ***2016*********************
***************************************************************************************
***************************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen notrabajonosalud = "1" if notrabajomespasado==1 & noinscrsalud==1
replace notrabajonosalud = "0" if notrabajomespasado==0 | noinscrsalud==0
replace notrabajonosalud = "." if notrabajomespasado==. | noinscrsalud==.

tab notrabajonosalud [w=factor]
tab notrabajonosalud [w=factor], m
destring notrabajonosalud, replace
tabstat notrabajonosalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajonosalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  notrabajonosalud [w=factor], m //numerador y denominador muestral 

tab clave_ent notrabajonosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent notrabajonosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 


***pob no trabajo y está inscrita a un sistema de salud *****2016**********************
***************************************************************************************
***************************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen notrabajosisalud = "1" if notrabajomespasado==1 & noinscrsalud==0
replace notrabajosisalud = "0" if notrabajomespasado==0 | noinscrsalud==1
replace notrabajosisalud = "." if notrabajomespasado==. | noinscrsalud==.

tab notrabajosisalud [w=factor]
tab notrabajosisalud [w=factor], m
destring notrabajosisalud, replace
tabstat notrabajosisalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean notrabajosisalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  notrabajosisalud [w=factor], m  //numerador y denominador muestral 

tab clave_ent notrabajosisalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent notrabajosisalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 




********pob con trabajo y NO está inscrita a un sistema de salud**2016************* ***********************************************************************************
tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen sitrabajonosalud = "1" if notrabajomespasado==0 & noinscrsalud==1
replace sitrabajonosalud = "0" if notrabajomespasado==1 | noinscrsalud==0
replace sitrabajonosalud = "." if notrabajomespasado==. | noinscrsalud==.

tab sitrabajonosalud [w=factor]
tab sitrabajonosalud [w=factor], m
destring sitrabajonosalud, replace
tabstat sitrabajonosalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sitrabajonosalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  sitrabajonosalud [w=factor], m //numerador y denominador muestral 

tab clave_ent sitrabajonosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent sitrabajonosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 


********pob con trabajo y  está inscrita a un sistema de salud 2016*************** ***********************************************************************************

tab notrabajomespasado noinscrsalud [w=factor]
tab notrabajomespasado noinscrsalud [w=factor], m
gen sitrabajosisalud = "1" if notrabajomespasado==0 & noinscrsalud==0
replace sitrabajosisalud = "0" if notrabajomespasado==1 | noinscrsalud==1
replace sitrabajosisalud = "." if notrabajomespasado==. | noinscrsalud==.

tab sitrabajosisalud [w=factor]
tab sitrabajosisalud [w=factor], m
destring sitrabajosisalud, replace
tabstat sitrabajosisalud  [w=factor], by (clave_ent) f(%10.9gc)

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sitrabajosisalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  sitrabajosisalud [w=factor], m //numerador y denominador muestral 

tab sitrabajosisalud sexo [w=factor], m
tab sexo sitrabajosisalud [w=factor], m
tab sexo notrabajonosalud [w=factor], m


tab clave_ent sitrabajosisalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent sitrabajosisalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 


**
**
**poblacion en condición de discapacidad 2016
destring disc1, replace
tab disc1 [w=factor], m
gen pobcondisc = "1" if disc1=="1"| disc1=="2"|disc1=="3"|disc1=="4"|disc1=="5"| disc1=="6"|disc1=="7"
replace pobcondisc = "0" if  disc1=="8"
replace pobcondisc = "." if  disc1=="&" 

tab pobcondisc [w=factor], m 
tab noinscrsalud [w=factor], m
destring pobcondisc, replace

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean pobcondisc, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  pobcondisc [w=factor], m //numerador y denominador muestral 

tab pobcondisc sexo [w=factor], m
tab sexo pobcondisc [w=factor], m

tab clave_ent pobcondisc if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent pobcondisc if sexo ==2 [w=factor], m  //numerador y denominador muestral 



tab pobcondisc noinscrsalud [w=factor], m
gen dicsnosalud = 1 if pobcondisc == 1 & noinscrsalud == 1
replace dicsnosalud = 0 if (pobcondisc ==0 & noinscrsalud ==0) | (pobcondisc ==0 & noinscrsalud ==1) | (pobcondisc ==1 & noinscrsalud ==0) 
replace dicsnosalud = . if pobcondisc ==. | noinscrsalud ==.
tab dicsnosalud [w=factor], m

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean dicsnosalud, over(clave_ent) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab clave_ent  dicsnosalud [w=factor], m //numerador y denominador muestral 

tab dicsnosalud sexo [w=factor], m
tab sexo dicsnosalud [w=factor]

tab clave_ent dicsnosalud if sexo ==1 [w=factor], m  //numerador y denominador muestral 
tab clave_ent dicsnosalud if sexo ==2 [w=factor], m  //numerador y denominador muestral 



******2022*********************************************************************************
***poblacion que no trabajó el mes pasado**********************************************
*tab trabajo_mp [w=factor], m
**************************************************************************************
use "C:\Users\lftrujillo\Desktop\pp\poblacion 2022.dta" 

tab trabajo_mp [w=factor]
tab trabajo_mp [w=factor], m
desc trabajo_mp
tab trabajo_mp, m
tab sexo [w=factor], m 


describe trabajo_mp
destring trabajo_mp, replace
gen notrabajomespasado = "1" if trabajo_mp==2
replace notrabajomespasado = "." if trabajo_mp==.
replace notrabajomespasado = "0" if trabajo_mp==1
destring notrabajomespasado, replace

tab notrabajomespasado [w=factor]
tab notrabajomespasado [w=factor], m
destring notrabajomespasado, replace
tabstat notrabajomespasado  [w=factor], by (ent) f(%10.9gc)

desc notrabajomespasado


tab sexo notrabajomespasado [w=factor], m



*****************************
*****************************
******INDICADORES************
gen I_notrabajomespasadoh16= (notrabajomespasado16h/notrabajomespasado16)*100000
gen I_notrabajomespasadom16= (notrabajomespasado16m/notrabajomespasado16)*100000
gen I_notrabajomespasado16= (notrabajomespasado16/notrabajomespasado16)*100000


gen I_noinscrsaludh16= (noinscrsalud16h/ noinscrsalud16)*100000
gen I_noinscrsaludm16= (noinscrsalud16m/ noinscrsalud16)*100000
gen I_noinscrsalud16= (noinscrsalud16/ poblacion16 )*100000





gen I_notrabajonosaludh16= (notrabajonosalud16h/ notrabajonosalud16)*100000
gen I_notrabajonosaludm16= (notrabajonosalud16m/ notrabajonosalud16)*100000
gen I_notrabajonosalud16= (notrabajonosalud16/ notrabajonosalud16)*100000


gen I_notrabajonosaludh16= (notrabajonosalud16h/poblacion16)*100000
gen I_notrabajonosaludm16= (notrabajonosalud16m/poblacion16)*100000
gen I_notrabajonosalud16= (notrabajonosalud16/poblacion16)*100000


gen I_notrabajosisaludm16= (notrabajosisalud16m/poblacion16)*100000
gen I_notrabajosisaludh16=( notrabajosisalud16h/poblacion16)*100000
gen I_notrabajosisalud16=( notrabajosisalud16/poblacion16)*100000


gen I_sitrabajonosaludm16= (sitrabajonosalud16m/poblacion16)*100000
gen I_sitrabajonosaludh16=( sitrabajonosalud16h/poblacion16)*100000
gen I_sitrabajonosalud16=( sitrabajonosalud16/poblacion16)*100000



gen I_sitrabajosisaludh16=( sitrabajosisalud16h/ sitrabajosisalud16)*100000
gen I_sitrabajosisaludm16=( sitrabajosisalud16m/ sitrabajosisalud16)*100000
gen I_sitrabajosisalud16=( sitrabajosisalud16/ sitrabajosisalud16)*100000



gen I_extorsion16=( extorsion16/ poblacion16)*100000
gen I_homicidio16 =( homicidio16 / poblacion16)*100000
gen I_secuestro16=( secuestro16 / poblacion16)*100000
gen I_trata16=( trata16 / poblacion16)*100000



















