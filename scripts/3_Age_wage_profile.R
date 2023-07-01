#         3.  Age wage profile

# - Daniel Casas Bautista
# - Lucia Fillippo
# - Miguel Angel Victoria Simbaqueva 
# - Irina Andrea Vélez López 

# Initial Configuration and packages insdtallment
if(!require(pacman)) install.packages("pacman")
require(pacman)
p_load(tidyverse, rvest, stargazer, broom, flextable, officer,boot)

# Configurando el espacio de trabajo 
getwd()
setwd("/Users/luciafillippo/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Big Data & Machine Learning/Problem Sets/Problem Set I/V.F Repositorio/Problem Set 1/stores")

# Data base load
load("data.Rdata")
glimpse("data.Rdata")

#Crear variable edad al cuadrado
db_geih_filtered$age_cuadradofinal <- db_geih_filtered$age^2
print (db_geih_filtered$age^2)

#Crear variable logaritmica para la y
db_geih_filtered$log_salarioreal <-log(db_geih_filtered$y_salary_m_hu)

#Correr regresion
regresion_salario_edad <- lm (log_salarioreal~age +age_cuadradofinal,db_geih_filtered)
stargazer(regresion_salario_edad,type="text")
str (regresion_salario_edad)
regresion_salario_edad$coefficients


#Exportar a word
tabla_resultados <- tidy(regresion_salario_edad)
tabla_word <- flextable(tabla_resultados)
doc <- read_docx()
doc <- body_add_flextable(doc, tabla_word)
ruta_archivo <-"/Users/luciafillippo/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Big Data & Machine Learning/Problem Sets/Problem Set I/resultados_regresionpunto3.docx"
print(doc, target = ruta_archivo)

#Bootstrap
##Boot 1
eta_fn<-function(data,index){
  coef(lm(log_salarioreal~age +age_cuadradofinal, data = data, subset = index))[2]}

eta_fn(db_geih_filtered,1:nrow(db_geih_filtered))
boot(db_geih_filtered, eta_fn, R = 1000)

##Boot 2
eta_fn2<-function(data,index){
  coef(lm(log_salarioreal~age +age_cuadradofinal, data = data, subset = index))[3]}

eta_fn2(db_geih_filtered,1:nrow(db_geih_filtered))
boot(db_geih_filtered, eta_fn2, R = 1000)

##Guardar resultados
boot_results1 <- boot(db_geih_filtered, eta_fn, R = 1000)
boot_results2 <- boot(db_geih_filtered, eta_fn2, R = 1000)

# Intervalos de confianza
boot_ci1 <- boot.ci(boot_results1, type = "basic")  
boot_ci2 <- boot.ci(boot_results2, type = "basic") 

## Grafica
# Verificar si hay filas con valores faltantes
length(y_estimados)
length(db_geih_filtered$age)
sum(!is.na(db_geih_filtered$age))

missing_data <- complete.cases(db_geih_filtered$age, db_geih_filtered$log_salarioreal)

# Eliminar filas con valores faltantes
db_geih_filtered <- db_geih_filtered[missing_data, ]

# Ajustar el modelo de regresión
modelo_p3 <- lm(log_salarioreal ~ age + age_cuadradofinal, data = db_geih_filtered)

# Obtener los valores ajustados 
y_estimados <- predict(modelo_p3)

# Crear un gráfico de dispersión
plot(db_geih_filtered$age, db_geih_filtered$log_salarioreal, 
     xlab = "Edad", ylab = "Log Salario Real",
     main = "Regresión de Salario Real en función de la Edad",
     pch = 16, col = "blue")

# Agregar los valores ajustados
points(db_geih_filtered$age, y_estimados, pch = 16, col = "green")



