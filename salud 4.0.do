************* 2022*************************************************************
*******************************************************************************

use "D:\PP\INDICADOR SALUD\enigh2022_ns_hogares_dta\hogares.dta"
merge m:m folioviv foliohog using "D:\PP\INDICADOR SALUD\enigh2022_ns_concentradohogar_dta\concentradohogar.dta"
tab _merge
drop _merge
save "D:\PP\INDICADOR SALUD\hogares concentradohogar2022.dta", replace
use "D:\PP\INDICADOR SALUD\hogares concentradohogar2022.dta"

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sexo_jefe, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad sexo_jefe [w=factor], m //numerador y denominador muestral 
destring sexo_jefe, replace
destring entidad, replace


preserve
collapse (sum) salud [w=factor], by(entidad)

table entidad, statistic(sum salud)

restore

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2022 gsalud.dta"

use "D:\PP\INDICADOR SALUD\hogares concentradohogar2022 gsalud.dta"

egen hogar_unico = group(folioviv foliohog)

duplicates report hogar_unico

tab foliohog [w=factor]

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2022 gsalud.dta", replace

use "D:\PP\INDICADOR SALUD\enigh2022_ns_poblacion_dta\poblacion.dta"

destring entidad, replace



use "D:\PP\INDICADOR SALUD\enigh2022_ns_poblacion_dta\poblacion.dta" 
merge m:m folioviv foliohog using "D:\PP\INDICADOR SALUD\hogares concentradohogar2022 gsalud.dta", force

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2022 gsalud afiliacion.dta"

use "D:\PP\INDICADOR SALUD\hogares concentradohogar2022 gsalud afiliacion.dta"

describe foliohog salud
destring foliohog salud, replace
destring atemed, replace

* crear variable binaria de afiliación
*gen afiliado = 0
*replace afiliado = 1 if atemed == 1
*replace afiliado = 0 if atemed == 0 | atemed == .
*tab entidad afiliado [w=factor], m






* 1. Crear indicador de persona afiliada
gen afiliado = (atemed == 1)

* 2. Crear identificador único del hogar
egen hogar_id = group(folioviv foliohog)

* 3. Crear indicador binario a nivel hogar: al menos una persona afiliada
egen hay_afiliado = max(afiliado), by(hogar_id)


gen afiliacion_hogar = .
replace afiliacion_hogar = 0 if afiliado == 0
replace afiliacion_hogar = 1 if afiliado == 1
label define afil 0 "Sin afiliados" 1 "Al menos 1 afiliado"
label values afiliacion_hogar afil


* Conservar una sola observación por hogar (para evitar duplicar gasto)
bysort hogar_id (hay_afiliado): keep if _n == 1
mean hay_afiliado [pw=factor], over(entidad)

mean salud [w=factor] if hay_afiliado == 1, over(entidad)

bysort hogar_id (hay_afiliado): keep if _n == 1

* Promedio nacional de hogares con al menos una persona afiliada
mean hay_afiliado [pw=factor]

* Promedio nacional del gasto en salud entre hogares con afiliación
mean salud [pw=factor] if hay_afiliado == 1
sum salud [w=factor] if hay_afiliado == 1


************* 2020*************************************************************
*******************************************************************************

use "D:\PP\INDICADOR SALUD\enigh2020_ns_hogares_dta\hogares.dta"
merge m:m folioviv foliohog using "D:\PP\INDICADOR SALUD\enigh2020_ns_concentradohogar_dta\concentradohogar.dta"
tab _merge
drop _merge
save "D:\PP\INDICADOR SALUD\hogares concentradohogar2020.dta", replace
use "D:\PP\INDICADOR SALUD\hogares concentradohogar2020.dta"

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sexo_jefe, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad sexo_jefe [w=factor], m //numerador y denominador muestral 
destring sexo_jefe, replace
destring entidad, replace


preserve
collapse (sum) salud [w=factor], by(entidad)

table entidad, statistic(sum salud)

restore

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2020 gsalud.dta"

use "D:\PP\INDICADOR SALUD\hogares concentradohogar2020 gsalud.dta"

egen hogar_unico = group(folioviv foliohog)

duplicates report hogar_unico

tab foliohog [w=factor]

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2020 gsalud.dta", replace

use "D:\PP\INDICADOR SALUD\enigh2020_ns_poblacion_dta\poblacion.dta"




use "D:\PP\INDICADOR SALUD\enigh2020_ns_poblacion_dta\poblacion.dta" 
merge m:m folioviv foliohog using "D:\PP\INDICADOR SALUD\hogares concentradohogar2020 gsalud.dta", force

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2020 gsalud afiliacion.dta"

use "D:\PP\INDICADOR SALUD\hogares concentradohogar2020 gsalud afiliacion.dta"

desc ubica_geo
tab ubica_geo
*drop clave_ent
*drop clave_mun
gen clave_mun = substr(ubica_geo, 3, 5)
tab clave_mun
*drop clave_ent
gen entidad = substr(ubica_geo,1, 2)
tab entidad [w=factor]

describe foliohog salud
destring foliohog salud, replace
destring atemed, replace

* crear variable binaria de afiliación
*gen afiliado = 0
*replace afiliado = 1 if atemed == 1
*replace afiliado = 0 if atemed == 0 | atemed == .
*tab entidad afiliado [w=factor], m






* 1. Crear indicador de persona afiliada
gen afiliado = (atemed == 1)

* 2. Crear identificador único del hogar
egen hogar_id = group(folioviv foliohog)

* 3. Crear indicador binario a nivel hogar: al menos una persona afiliada
egen hay_afiliado = max(afiliado), by(hogar_id)


gen afiliacion_hogar = .
replace afiliacion_hogar = 0 if afiliado == 0
replace afiliacion_hogar = 1 if afiliado == 1
label define afil 0 "Sin afiliados" 1 "Al menos 1 afiliado"
label values afiliacion_hogar afil

destring entidad, replace

* Conservar una sola observación por hogar (para evitar duplicar gasto)
bysort hogar_id (hay_afiliado): keep if _n == 1
mean hay_afiliado [pw=factor], over(entidad)

mean salud [w=factor] if hay_afiliado == 1, over(entidad)



**Calculo del pais
* Conservar una sola observación por hogar
bysort hogar_id (hay_afiliado): keep if _n == 1

* Promedio nacional de hogares con al menos una persona afiliada
mean hay_afiliado [pw=factor]

* Promedio nacional del gasto en salud entre hogares con afiliación
bysort hogar_id (hay_afiliado): keep if _n == 1
mean salud [pw=factor] if hay_afiliado == 1

mean TTRATA22 [pw=factor]





************* 2018*************************************************************
*******************************************************************************

use "D:\PP\INDICADOR SALUD\enigh2018_ns_hogares_dta\hogares.dta"
merge m:m folioviv foliohog using "D:\PP\INDICADOR SALUD\enigh2018_ns_concentradohogar_dta\concentradohogar.dta"
tab _merge
drop _merge
save "D:\PP\INDICADOR SALUD\hogares concentradohogar2018.dta", replace
use "D:\PP\INDICADOR SALUD\hogares concentradohogar2018.dta"

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sexo_jefe, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad sexo_jefe [w=factor], m //numerador y denominador muestral 
destring sexo_jefe, replace
destring entidad, replace


preserve
collapse (sum) salud [w=factor], by(entidad)

table entidad, statistic(sum salud)

restore

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2018 gsalud.dta"

use "D:\PP\INDICADOR SALUD\hogares concentradohogar2018 gsalud.dta"

egen hogar_unico = group(folioviv foliohog)

duplicates report hogar_unico

tab foliohog [w=factor]

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2018 gsalud.dta", replace

use "D:\PP\INDICADOR SALUD\enigh2018_ns_poblacion_dta\poblacion.dta"




merge m:m folioviv foliohog using "D:\PP\INDICADOR SALUD\hogares concentradohogar2018 gsalud.dta", force

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2018 gsalud afiliacion.dta"

use "D:\PP\INDICADOR SALUD\hogares concentradohogar2018 gsalud afiliacion.dta"

desc ubica_geo
tab ubica_geo
*drop clave_ent
*drop clave_mun
gen clave_mun = substr(ubica_geo, 3, 5)
tab clave_mun
*drop clave_ent
gen entidad = substr(ubica_geo,1, 2)
tab entidad [w=factor]

describe foliohog salud
destring foliohog salud, replace
destring atemed, replace

* crear variable binaria de afiliación
*gen afiliado = 0
*replace afiliado = 1 if atemed == 1
*replace afiliado = 0 if atemed == 0 | atemed == .
*tab entidad afiliado [w=factor], m






* 1. Crear indicador de persona afiliada
gen afiliado = (atemed == 1)

* 2. Crear identificador único del hogar
egen hogar_id = group(folioviv foliohog)

* 3. Crear indicador binario a nivel hogar: al menos una persona afiliada
egen hay_afiliado = max(afiliado), by(hogar_id)


gen afiliacion_hogar = .
replace afiliacion_hogar = 0 if afiliado == 0
replace afiliacion_hogar = 1 if afiliado == 1
label define afil 0 "Sin afiliados" 1 "Al menos 1 afiliado"
label values afiliacion_hogar afil

destring entidad, replace

* Conservar una sola observación por hogar (para evitar duplicar gasto)
bysort hogar_id (hay_afiliado): keep if _n == 1
mean hay_afiliado [pw=factor], over(entidad)

mean salud [w=factor] if hay_afiliado == 1, over(entidad)


**Calculo del pais
* Conservar una sola observación por hogar
bysort hogar_id (hay_afiliado): keep if _n == 1

* Promedio nacional de hogares con al menos una persona afiliada
mean hay_afiliado [pw=factor]

* Promedio nacional del gasto en salud entre hogares con afiliación
mean salud [pw=factor] if hay_afiliado == 1
mean salud [pw=factor]


************* 2016*************************************************************
*******************************************************************************

use "D:\PP\INDICADOR SALUD\enigh2016_ns_hogares_dta\hogares.dta"
merge m:m folioviv foliohog using "D:\PP\INDICADOR SALUD\enigh2016_ns_concentradohogar_dta\concentradohogar.dta"
tab _merge
drop _merge
save "D:\PP\INDICADOR SALUD\hogares concentradohogar2016.dta", replace
use "D:\PP\INDICADOR SALUD\hogares concentradohogar2016.dta"

*precisiones 
svyset upm [w=factor],  strata(est_dis) vce(linearized) singleunit(centered)  

svy: mean sexo_jefe, over(entidad) cformat(%9.8f)
estat cv
estat size //denominador muestral 
tab entidad sexo_jefe [w=factor], m //numerador y denominador muestral 
destring sexo_jefe, replace
destring entidad, replace


preserve
collapse (sum) salud [w=factor], by(entidad)

table entidad, statistic(sum salud)

restore

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2016 gsalud.dta"

use "D:\PP\INDICADOR SALUD\hogares concentradohogar2016 gsalud.dta"

egen hogar_unico = group(folioviv foliohog)

duplicates report hogar_unico

tab foliohog [w=factor]

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2016 gsalud.dta", replace

use "D:\PP\INDICADOR SALUD\enigh2016_ns_poblacion_dta\poblacion.dta"
merge m:m folioviv foliohog using "D:\PP\INDICADOR SALUD\hogares concentradohogar2016 gsalud.dta", force

save "D:\PP\INDICADOR SALUD\hogares concentradohogar2016 gsalud afiliacion.dta"

use "D:\PP\INDICADOR SALUD\hogares concentradohogar2016 gsalud afiliacion.dta"

desc ubica_geo
tab ubica_geo
*drop clave_ent
*drop clave_mun
gen clave_mun = substr(ubica_geo, 3, 5)
tab clave_mun
*drop clave_ent
gen entidad = substr(ubica_geo,1, 2)
tab entidad [w=factor]

describe foliohog salud
destring foliohog salud, replace
destring atemed, replace

* crear variable binaria de afiliación
*gen afiliado = 0
*replace afiliado = 1 if atemed == 1
*replace afiliado = 0 if atemed == 0 | atemed == .
*tab entidad afiliado [w=factor], m






* 1. Crear indicador de persona afiliada
gen afiliado = (atemed == 1)

* 2. Crear identificador único del hogar
egen hogar_id = group(folioviv foliohog)

* 3. Crear indicador binario a nivel hogar: al menos una persona afiliada
egen hay_afiliado = max(afiliado), by(hogar_id)


gen afiliacion_hogar = .
replace afiliacion_hogar = 0 if afiliado == 0
replace afiliacion_hogar = 1 if afiliado == 1
label define afil 0 "Sin afiliados" 1 "Al menos 1 afiliado"
label values afiliacion_hogar afil

destring entidad, replace

* Conservar una sola observación por hogar (para evitar duplicar gasto)
bysort hogar_id (hay_afiliado): keep if _n == 1
mean hay_afiliado [pw=factor], over(entidad)

mean salud [w=factor] if hay_afiliado == 1, over(entidad)

* Promedio nacional de hogares con al menos una persona afiliada
mean hay_afiliado [pw=factor]


bysort hogar_id (hay_afiliado): keep if _n == 1
* Promedio nacional del gasto en salud entre hogares con afiliación
mean salud [pw=factor] if hay_afiliado == 1
