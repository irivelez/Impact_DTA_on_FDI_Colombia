
*-------------------------------------------------------------------------------
///     Script Tesis MECA     ///
//     Análisis de Eventos    //
*-------------------------------------------------------------------------------

** Directorios

* Irina
cd "/Users/irina/Coding/Repositorios/Impact_DTA_on_FDI_Colombia"


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
// Ajuste de variables
* Tratamiento
rename ADT Tratamiento
label variable Tratamiento "1 si tiene acuerdo firmado, 0 en caso contrario"
label define Tratamiento 1 "Países con ADT Vigente" 0 "Países sin ADT"
label values Tratamiento Tratamiento

* ADT
rename Inicio_vigencia_ADT ADT
replace País = "Reino_Unido" if País == "Reino Unido"

*-------------------------------------------------------------------------------
// Análisis de eventos

* Identificar el trimestre de la firma del ADT
gen ADT_lag1 = L1.ADT
gen inicio_vigencia_ADT = (ADT == 1 & ADT_lag1 == 0)
order inicio_vigencia_ADT, after(ADT)

// Normalización de los datos -- Semestre anterior //

// Punto de Referencia semestre: 2 trimestres antes de la firma del ADT
by ID: gen punto_ref = F2.inicio_vigencia_ADT

* Crear factor de IED para el trimestre de referencia
by ID (trimestre_stata): gen ref_IED = IED / 100 if punto_ref == 1

* Factores IED para Bolivia, Ecuador, Perú
replace ref_IED = IED / 100 if ID == 10 & trimestre_stata == yq(2007,2) // Bolivia
replace ref_IED = IED / 100 if ID == 18 & trimestre_stata == yq(2007,1) // Ecuador
replace ref_IED = IED / 100 if ID == 35 & trimestre_stata == yq(2007,1) // Perú
replace ref_IED = 0 if ref_IED == .

* factor_6 que contiene el valor de referencia para todos los trimestres por país
by ID: egen factor_6 = max(ref_IED)
replace factor_6 = 1 if factor_6 == 0


* Variable con IED normalizada con referencia semestre
gen IED_norm6 = IED / factor_6
order IED_norm6, after(IED)


* Identificar el inicio de vigencia de los acuerdos comerciales y de prot_inversión
gen AC_lag1 = L1.Acuerdo_comercial
gen inicio_vigencia_AC = (Acuerdo_comercial == 1 & AC_lag1 == 0)
order inicio_vigencia_AC, after(Acuerdo_comercial)

gen API_lag1 = L1.Acuerdo_prot_inversión
gen inicio_vigencia_API = (Acuerdo_prot_inversión == 1 & API_lag1 == 0)
order inicio_vigencia_API, after(Acuerdo_prot_inversión)



levelsof País if Tratamiento == 1, local(paises)

foreach p in `paises' {
    * Encuentra el valor máximo de IED para ese país para establecer el eje Y
    summarize IED if País == "`p'" & Tratamiento == 1, detail
    local ymax = ceil(r(max))

    * Encuentra la fecha del primer trimestre en que el ADT está en vigor
    qui summarize trimestre_stata if País == "`p'" & inicio_vigencia_ADT == 1 & Tratamiento == 1
    local vigencia_date = r(min)
	

    * Solo si encontramos un valor para inicio_vigencia específico para el país
    if r(N) > 0 {
        * Grafica la serie de tiempo con una línea vertical donde el acuerdo entra en vigor
        twoway (line IED trimestre_stata if País == "`p'" & Tratamiento == 1) ///
            , name("IED_`p'") title("Flujo de IED en `p'") ///
            ytitle("IED Normalizada") ///
            yscale(range(0 `ymax')) ///
            xtitle("Trimestre") ///
            xline(`vigencia_date', lcolor(red) lpattern(dash)) ///
            legend(off)

        * Guarda el gráfico con un nombre basado en el país y reemplazo si ya existe
        graph save "Gráficas/IED6_`p'.gph", replace
    }
}

