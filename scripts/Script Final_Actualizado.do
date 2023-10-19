*-------------------------------------------------------------------------------
// Script final Tesis MECA //
*-------------------------------------------------------------------------------
ssc install estout
ssc install outreg2
ssc install psmatch2


// Importar base de datos

** Directorios:
* Lucía
cd "/Users/luciafillippo/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Proyecto de grado - Tesis/Documentos compartidos"

* Irina
cd "/Users/irina/Coding/Repositorios/Impact_DTA_on_FDI_Colombia"

* Miguel
cd "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Bases_de_datos"


** Importar base
import excel "stores/Base_datos_trimestral_actualizada.xlsx", sheet("Base General") firstrow clear 


*-------------------------------------------------------------------------------
// Específicación de la bases de datos como panel
xtset ID Trimestre
format Trimestre %tq
 
*-------------------------------------------------------------------------------
// Creación y ajuste de variables

* IED en Logaritmo
gen IED_0 = max(0, IED)
gen LN_IED = log(IED_0 + 1)
label variable LN_IED "Ln_IED"

* Diferencia impuesto renta
gen dif_imp = Impuesto_renta_corp - Impuesto_renta_COL
label variable dif_imp "Diff ImpPais - ImpCOL"

* PIB
rename PIB_tasa_crecimiento PIB

* Tratamiento
rename ADT Tratamiento
label variable Tratamiento "1 si tiene acuerdo firmado, 0 en caso contrario"

* ADT
rename Inicio_vigencia_ADT ADT

* PIB_percapita
rename PIB_percapita PIB_per

* Interacción ADTs
gen ADT_Trat = ADT*Tratamiento
label variable ADT_Trat "Interacción entre Tratamiento y el inicio de vigencia ADT"


*-------------------------------------------------------------------------------
// Análisis de los datos

summarize IED_0 PIB PIB_per IPC BC dif_imp SP_500 Brent Distancia_km
hist IED_0, normal

* Tabla de Correlación entre las variables
corr IED PIB PIB_per IPC BC dif_imp SP_500 Brent Distancia_km


*-------------------------------------------------------------------------------
// Estadísticas descriptivas

* Estadísticas descriptivas generales
tabstat IED PIB PIB_per IPC BC dif_imp, by(País) stat(mean sd min max) save


* Estadísticas descriptivas tratamiento
keep if Tratamiento == 1
tabstat IED PIB PIBper IPC BC, by(ID) stat(mean sd n)

import excel "Base_datos_trimestral.xlsx", sheet("Base General") firstrow clear 
xtset ID Fecha
format Fecha %tq


* Estadísticas descriptivas control
keep if Tratamiento == 0
tabstat IED PIB PIBper IPC BC, by(ID) stat(mean sd n)

import excel "Base_datos_trimestral.xlsx", sheet("Base General") firstrow clear 
xtset ID Fecha
ssc install estout

// Pruebas de Normalidad de IED

* Prueba Shapiro-Wilk
swilk IED // Claramente los datos no siguen una distribución normal

* Prueba Shapiro-Wilk sobre los datos transformados
swilk LN_IED // Los datos tampoco siguen una distribución normal


*-------------------------------------------------------------------------------
// Gráficas
* Preliminar
graph matrix ADT PIB PIB_per IPC BC dif_imp Acuerdo_comercial Acuerdo_prot_inversión Distancia_km SP_500 Brent Oro , title("Gráfico de Dispersión") name(scatter_matrix, replace)

* Final
graph matrix ADT PIB PIB_per IPC BC dif_imp Distancia_km SP_500 Brent, title("Gráfico de Dispersión") name(scatter_matrix, replace)


graph export "C:\Users\migue\OneDrive - Universidad de los andes\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables finales\Scripts\Grafico_1.png", replace



*-------------------------------------------------------------------------------
// Regresiones POLS

* Modelo simple
xtreg LN_IED ADT
eststo reg_pols_1

* Con variable macro
xtreg LN_IED ADT dif_imp PIB PIB_per IPC BC 
eststo reg_pols_2

* Con impuesto y acuerdos
xtreg LN_IED ADT dif_imp PIB PIB_per IPC BC Acuerdo_comercial Acuerdo_prot_inversión 
eststo reg_pols_3

* Con variables de efecto fijo
xtreg LN_IED ADT dif_imp PIB PIB_per IPC BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km SP_500 Brent
eststo reg_pols_4

** Exportar
outreg2 [reg_pols_1 reg_pols_2 reg_pols_3 reg_pols_4] using "views/Regress_POLS.doc", replace   // Repositorio GitHub

outreg2 [reg_pols_1 reg_pols_2 reg_pols_3 reg_pols_4] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables finales\Tablas_Gráficas\Regress_POLS.doc", replace 

*-------------------------------------------------------------------------------
// Regresiones DID

* Modelo simple
xtreg LN_IED ADT_Trat, fe
eststo reg_did_1

* Con variable macro
xtreg LN_IED ADT_Trat dif_imp PIB PIB_per IPC BC, fe
eststo reg_did_2

estimates store Modelo_DID_2

* Con impuesto y acuerdos
xtreg LN_IED ADT_Trat dif_imp PIB PIB_per IPC BC  Acuerdo_comercial Acuerdo_prot_inversión, fe
eststo reg_did_3

* Con variables de efecto fijo
xtreg LN_IED ADT_Trat dif_imp PIB PIB_per IPC BC  Acuerdo_comercial Acuerdo_prot_inversión SP_500 Brent, fe
eststo reg_did_4

** Exportar
outreg2 [reg_pols_1 reg_pols_2 reg_pols_3 reg_pols_4] using "views/Regress_DID.doc", replace   // Repositorio GitHub

outreg2 [reg_did_1 reg_did_2 reg_did_3 reg_did_4] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables finales\Tablas_Gráficas\Regress_DID.doc", replace 

*-------------------------------------------------------------------------------
// Regresiones DID con PSM

* Generar el puntaje de propensión (Regresion logistica)
logit ADT PIB PIB_per IPC BC, robust
eststo reg_did_psm_et_1
outreg2 using reg_did_psm_et_1, replace word
predict ps, p

* Crear el PSM (emparejamiento)
psmatch2 ADT ps, caliper(0.1)

* Creación de variables
gen tratamiento = (Tratamiento == 1)
gen tratado_tratamiento = tratamiento * _treated
gen tratado_control = tratamiento * (1 - _treated)

** Regresiones

* Modelo simple
xtreg LN_IED tratado_tratamiento tratado_control _pscore, cluster(País) robust
eststo reg_did_psm_1

* Con variable macro
xtreg LN_IED tratado_tratamiento tratado_control _pscore dif_imp PIB PIB_per IPC BC, cluster(País) robust
eststo reg_did_psm_2

* Con impuesto y acuerdos
xtreg LN_IED tratado_tratamiento tratado_control _pscore dif_imp PIB PIB_per IPC BC  Acuerdo_comercial Acuerdo_prot_inversión, cluster(País) robust
eststo reg_did_psm_3

* Con variables de efecto fijo
xtreg LN_IED tratado_tratamiento tratado_control _pscore dif_imp PIB PIB_per IPC BC  Acuerdo_comercial Acuerdo_prot_inversión SP_500 Brent, cluster(País) robust
eststo reg_did_psm_4

** Exportar 

outreg2 [reg_did_psm_1 reg_did_psm_2 reg_did_psm_3 reg_did_psm_4] using "views/Regress_DID_PSM.doc", replace   // Repositorio GitHub

outreg2 [reg_did_psm_1 reg_did_psm_2 reg_did_psm_3 reg_did_psm_4] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables finales\Tablas_Gráficas\Regress_DID_PSM.doc", replace 

 *** Resultados finales
outreg2 [reg_pols_4 reg_did_4 reg_did_psm_4] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables finales\Tablas_Gráficas\Resultado_final.doc", replace 
 
*-------------------------------------------------------------------------------








