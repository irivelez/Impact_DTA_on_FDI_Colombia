*-------------------------------------------------------------------------------
// Script final Tesis MECA //
*-------------------------------------------------------------------------------

// Entorno de trabajo
ssc install estout
ssc install outreg2
ssc install psmatch2

** Directorios
* Lucía
cd "/Users/luciafillippo/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Proyecto de grado - Tesis/Documentos compartidos/Entregables"

* Irina
cd "/Users/irina/Coding/Repositorios/Impact_DTA_on_FDI_Colombia"

* Miguel
cd "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables"


*-------------------------------------------------------------------------------
// Importar base de datos
import excel "Datos/Base_datos_trimestral_actualizada.xlsx", sheet("Base General") firstrow clear

// Específicación de la bases de datos como panel
* Convertir la columna Trimestre a fecha trimestral de Stata
gen trimestre_stata = qofd(Trimestre)
format trimestre_stata %tq
order trimestre_stata, after(Trimestre)

* Establecer la base de datos como panel
xtset ID trimestre_stata

*-------------------------------------------------------------------------------
// Creación y ajuste de variables

* IED en Logaritmo
gen IED_0 = max(0, IED)
replace IED_0 = 0.01 if IED_0 == 0
gen LN_IED = log(IED_0)
label variable LN_IED "Ln_IED"

* Diferencia impuesto renta
gen dif_imp = Impuesto_renta_corp - Impuesto_renta_COL
label variable dif_imp "Diff ImpPaís - ImpCOL"
order dif_imp, after(Impuesto_renta_COL)

* Diferencia tasa mercado monetario
gen diftasa_mercmonetario = Tasa_mer_monetario - Tasa_mer_monetario_COL
label variable diftasa_mercmonetario "Diff Tasa_mer_monetario_País - Tasa_mer_monetario_COL"
order diftasa_mercmonetario, after(Tasa_mer_monetario_COL)

* Diferencia tasa de préstamo
gen diftasa_prestamo = Tasa_prestamo - Tasa_prestamo_COL
label variable diftasa_prestamo "Diff Tasa_préstamo_País - Tasa_préstamo_COL"
order diftasa_prestamo, after(Tasa_prestamo_COL)

* PIB
rename PIB_tasa_crecimiento PIB

* Tratamiento
rename ADT Tratamiento
label variable Tratamiento "1 si tiene acuerdo firmado, 0 en caso contrario"
label define Tratamiento 1 "Países con ADT Vigente" 0 "Países sin ADT"
label values Tratamiento Tratamiento

* ADT
rename Inicio_vigencia_ADT ADT

* PIB_percapita
rename PIB_percapita PIB_per


*-------------------------------------------------------------------------------
// Análisis de los datos

* Estadísticas descriptivas generales
summarize IED_0 PIB PIB_per IPC BC dif_imp SP_500 Brent Distancia_km

* Estadísticas descriptivas generales por país
tabstat IED_0 PIB PIB_per IPC BC dif_imp, by(País) stat(mean sd min max) save

* Estadísticas descriptivas por grupo de control y tratamiento
summarize IED_0 PIB PIB_per IPC BC dif_imp SP_500 Brent Distancia_km if Tratamiento == 1
summarize IED_0 PIB PIB_per IPC BC dif_imp SP_500 Brent Distancia_km if Tratamiento == 0

* Prueba t para comparación de medias de la IED entre los 2 grupos
ttest IED_0, by(Tratamiento) 
// Hay evidencia estadísitca para sugerir que la media de los flujos de IED
// de los países sin ADTs es menor que los países con ADTs

* Gráficos de comparación
graph bar (mean) IED_0, over(Tratamiento) ///
    ytitle("Media de IED") ///
    title("Medias de los flujos positivos de IED hacia Colombia") ///
	bar(1, color(gs8))

* Tabla de Correlación entre las variables
corr LN_IED ADT PIB PIB_per IPC BC dif_imp SP_500 Brent Distancia_km


*-------------------------------------------------------------------------------
// Pruebas de Normalidad de IED

* Prueba Shapiro-Wilk
swilk IED_0
qnorm IED_0
// Los datos no siguen una distribución normal

* Prueba Shapiro-Wilk sobre los datos transformados
swilk LN_IED
swilk LN_IED_nonzero
qnorm LN_IED
qnorm LN_IED_nonzero
// Los datos tampoco siguen una distribución normal

hist IED_0, normal
hist IED_nonzero, normal



*-------------------------------------------------------------------------------
// Gráficas de dispersión

* Gráfica general
graph matrix ADT PIB PIB_per IPC BC dif_imp Acuerdo_comercial Acuerdo_prot_inversión Distancia_km SP_500 Brent , title("Gráfico de Dispersión") name(scatter_matrix, replace)

* Gráfica con las variables de comparación y de instrumentos de política pública
graph matrix ADT dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km, title("Gráfico de dispersión variables de comparación") name(scatter_matrix1, replace)
graph export "Gráficas/Scatter1_var_comparacion.png", replace

* Gráfica con las variables de contexto económico local del país de origen de la IED
graph matrix ADT PIB PIB_per IPC, title("Gráfico de dispersión de variables de contexto económico local") name(scatter_matrix2, replace)
graph export "Gráficas/Scatter2_var_contexto_econ_local.png", replace

* Gráfica con las variables de efectos fijos - Contexto económico global
graph matrix ADT Brent SP_500, title("Gráfico de dispersión de variables de contexto económico global") name(scatter_matrix3, replace)
graph export "Gráficas/Scatter3_var_contexto_econ_global.png", replace


//graph export "C:\Users\migue\OneDrive - Universidad de los andes\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables finales\Gráficas\Scatter1_var_comparacion.png", replace


*-------------------------------------------------------------------------------
// Regresiones POLS

* Modelo simple
xtreg LN_IED ADT
eststo reg_pols_1

* Con variables macroeconómicas y de comparación
xtreg LN_IED ADT PIB PIB_per Var_inflacion dif_imp BC   
eststo reg_pols_2

* Con variables de promoción de inversión - acuerdos
xtreg LN_IED ADT PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión 
eststo reg_pols_3

* Con variables de efecto fijo y de contexto mundial
xtreg LN_IED ADT PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500
eststo reg_pols_4

* Con variables de contexto financiero y de riesgo
xtreg LN_IED ADT PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500 Tasa_prestamo DXY i.Calficacion_Moodys
eststo reg_pols_5

** Sin variable de calificación, mejor modelo
xtreg LN_IED ADT PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500 Tasa_prestamo DXY
eststo reg_pols_6


** Exportar principal
outreg2 [reg_pols_1 reg_pols_2 reg_pols_4 reg_pols_6] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables\Resultados\Resultados_iniciales\Regress_POLS.doc", replace 

** Exportar comparacion
outreg2 [reg_pols_5 reg_pols_6] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables\Resultados\Resultados_iniciales\Comparacion\Comparacion_POLS.doc", replace 




outreg2 [reg_pols_1 reg_pols_2 reg_pols_3 reg_pols_4 reg_pols_5 reg_pols_6] using "Resultados/Regress_POLS.doc", replace   // Repositorio


// Revisión de supuestos

* Grraficar los residuos - Parece pasar la prueba
predict residuos, e
predict yhat, xb  // Genera los valores ajustados

scatter residuos yhat, title("Gráfico de Residuos vs Valores ajustados")
histogram residuos, normal title("Histograma de Residuos")
qnorm residuos, title("Gráfico Q-Q de Residuos")
xtline residuos, i(País) t(Trimestre)

* Prueba de normalidad de los residuos
swilk residuos
sktest residuos

* Expectativa condicional cero del término de error
xtreg LN_IED ADT dif_imp BC PIB PIB_per Var_inflacion Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500, fe
estimates store fixed

xtreg LN_IED ADT dif_imp BC PIB PIB_per Var_inflacion Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500, fe
estimates store random

hausman fixed random



*-------------------------------------------------------------------------------
// Regresiones DID

gen ADT_Trat = ADT*Tratamiento
//label variable ADT_Trat "Interacción entre Tratamiento y el inicio de vigencia ADT"

* Modelo simple
xtreg LN_IED ADT_Trat, fe
eststo reg_did_1

* Con variables macroeconómicas y de comparación
xtreg LN_IED ADT_Trat PIB PIB_per Var_inflacion dif_imp BC, fe
eststo reg_did_2

* Con variables de promoción de inversión - acuerdos
xtreg LN_IED ADT_Trat PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión, fe
eststo reg_did_3

* Con variables de efecto fijo y de contexto mundial
xtreg LN_IED ADT_Trat PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500, fe
eststo reg_did_4

* Con variables de contexto financiero y de riesgo
xtreg LN_IED ADT_Trat PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500 Tasa_prestamo DXY i.Calficacion_Moodys, fe
eststo reg_did_5

** Sin variable de calificación, mitiga impacto de API
xtreg LN_IED ADT_Trat PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500 Tasa_prestamo DXY, fe
eststo reg_did_6


** Exportar principal
outreg2 [reg_did_1 reg_did_2 reg_did_4 reg_did_6] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables\Resultados\Resultados_iniciales\Regress_DID.doc", replace 

** Exportar comparacion
outreg2 [reg_did_5 reg_did_6] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables\Resultados\Resultados_iniciales\Comparacion\Comparacion_DID.doc", replace 



outreg2 [reg_did_1 reg_did_2 reg_did_3 reg_did_4 reg_did_5 reg_did_6] using "Resultados/Regress_DID.doc", replace   // Repositorio





*-------------------------------------------------------------------------------
// Regresiones DID con PSM


* Generar el puntaje de propensión (Regresion logistica)
logit ADT PIB PIB_per Var_inflacion BC Tasa_prestamo, robust
eststo reg_did_psm_et_1
outreg2 [reg_did_psm_et_1] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables\Resultados\Resultados_iniciales\PSM_1.doc", replace 
predict ps, p

* Crear el PSM (emparejamiento)
psmatch2 ADT ps, caliper(0.1)

* Creación de variables
gen tratamiento = (Tratamiento == 1)
gen tratado_tratamiento = tratamiento * _treated
gen tratado_control = tratamiento * (1 - _treated)


** Pruebas
egen min_t = min (tratado_tratamiento)
egen max_c = max (tratado_control)
gen soportecomun = 1 if ps >= min_t & ps <= max_c
replace soportecomun = 0 if soportecomun ==.

* label define Soporte_comun 0 "Fuera sop_comun" 1 "Dentro sop_comun"
label values soportecomun Soporte_comun
tab soportecomun
summ soportecomun

count if soportecomun == 1



** Regresiones

* Modelo simple
xtreg LN_IED tratado_tratamiento tratado_control _pscore, cluster(País) robust
eststo reg_did_psm_1

* Con variables macroeconómicas y de comparación
xtreg LN_IED tratado_tratamiento tratado_control _pscore PIB PIB_per Var_inflacion dif_imp BC, cluster(País) robust
eststo reg_did_psm_2

* Con variables de promoción de inversión - acuerdos
xtreg LN_IED tratado_tratamiento tratado_control _pscore PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión, cluster(País) robust
eststo reg_did_psm_3

* Con variables de efecto fijo y de contexto mundial
xtreg LN_IED tratado_tratamiento tratado_control _pscore PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500, cluster(País) robust
eststo reg_did_psm_4

* Con variables de contexto financiero y de riesgo
xtreg LN_IED tratado_tratamiento tratado_control _pscore PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500   Tasa_prestamo DXY i.Calficacion_Moodys, cluster(País) robust
eststo reg_did_psm_5

* Modelo depurado
xtreg LN_IED tratado_tratamiento tratado_control _pscore PIB PIB_per Var_inflacion dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500   Tasa_prestamo DXY, cluster(País) robust
eststo reg_did_psm_6

** Exportar principal
outreg2 [reg_did_psm_1 reg_did_psm_2 reg_did_psm_4 reg_did_psm_6] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables\Resultados\Resultados_iniciales\Regress_DID_PSM.doc", replace 

** Exportar comparacion
outreg2 [reg_did_psm_5 reg_did_psm_6] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables\Resultados\Resultados_iniciales\Comparacion\Comparacion_DID_PSM.doc", replace 


outreg2 [reg_did_psm_1 reg_did_psm_2 reg_did_psm_3 reg_did_psm_4 reg_did_psm_5 reg_did_psm_6] using "Resultados/Regress_DID_PSM.doc", replace   // Repositorio


*-------------------------------------------------------------------------------

*** Resultados finales
 
outreg2 [reg_pols_6 reg_did_6 reg_did_psm_6] using "C:\Users\migue\OneDrive - Universidad de los andes\MECA\Semestre_2\Seminario de Investigación\Tesis - MeCA\Entregables\Resultados\Resultados_iniciales\Resultado_final.doc", replace 
 
 
 
 outreg2 [reg_pols_4 reg_did_4 reg_did_psm_4] using "Resultados/Resultado_final1.doc", replace   // Repositorio
outreg2 [reg_pols_5 reg_did_5 reg_did_psm_5] using "Resultados/Resultado_final2.doc", replace   // Repositorio
outreg2 [reg_pols_6 reg_did_6 reg_did_psm_6] using "Resultados/Resultado_final3.doc", replace   // Repositorio
 
 
 
 
 
 
*-------------------------------------------------------------------------------
// Pruebas Regresiones PSM con modelo Probit 

global controles PIB PIB_per IPC dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500 Impago_deuda_sob Tasa_mer_monetario diftasa_mercmonetario Tasa_prestamo diftasa_prestamo DXY


probit Tratamiento $controles, robust
eststo psm_etapa1
outreg2 [psm_etapa1] using "Resultados/psm_etapa1_prob.doc", replace  // Repositorio

* Obtener el Propensity Score Matching
predict ps, p


gen prob_t = ps if Tratamiento==1
gen prob_c = ps if Tratamiento==0
sum prob_t prob_c

* Gráfica de la distribución del Propensity Score del grupo de tratados y del grupo de control
twoway (kdensity ps if Tratamiento==1, legend(label(1 "Tratamiento")))(kdensity ps if Tratamiento==0, legend(label(2 "Control")) xtitle(Probabilidad de ser tratado))

* Soporte Común
egen min_t = min (prob_t)
egen max_c = max (prob_c)
gen soportecomun = 1 if ps >= min_t & ps <= max_c
replace soportecomun = 0 if soportecomun ==.

label define Soporte_comun 0 "Fuera sop_comun" 1 "Dentro sop_comun"
label values soportecomun Soporte_comun
tab soportecomun
summ soportecomun

count if soportecomun == 1

// Regresiones

* Modelo Simple
reg LN_IED Tratamiento ps if soportecomun == 1, cluster(País) robust 
eststo reg_psm_1

* Con variables macroeconómicas y de comparación
xtreg LN_IED Tratamiento ps PIB PIB_per IPC dif_imp BC if soportecomun == 1, cluster(País) robust
eststo reg_psm_2

* Con variables de promoción de inversión - acuerdos
xtreg LN_IED Tratamiento ps PIB PIB_per IPC dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión if soportecomun == 1, cluster(País) robust
eststo reg_psm_3

* Con variables de efecto fijo y de contexto mundial
xtreg LN_IED Tratamiento ps PIB PIB_per IPC dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500 if soportecomun == 1, cluster(País) robust
eststo reg_psm_4

* Con variables de contexto financiero y de riesgo - sólo las diferencias de tasas
xtreg LN_IED Tratamiento ps PIB PIB_per IPC dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500 Impago_deuda_sob diftasa_mercmonetario diftasa_prestamo DXY if soportecomun == 1, cluster(País) robust
eststo reg_psm_5

* Con variables de contexto financiero y de riesgo
xtreg LN_IED Tratamiento ps PIB PIB_per IPC dif_imp BC Acuerdo_comercial Acuerdo_prot_inversión Distancia_km Brent SP_500 Impago_deuda_sob Tasa_mer_monetario diftasa_mercmonetario Tasa_prestamo diftasa_prestamo DXY if soportecomun == 1, cluster(País) robust
eststo reg_psm_6

** Exportar 

outreg2 [reg_psm_1 reg_psm_2 reg_psm_3 reg_psm_4 reg_psm_5 reg_psm_6] using "Resultados/Regress_PSM.doc", replace   // Repositorio
