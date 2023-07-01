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
setwd("/Users/luciafillippo/Library/CloudStorage/OneDrive-UniversidaddelosAndes/Big Data & Machine Learning/Problem Sets/Problem Set I/V.F Repositorio/PS1/stores")

# Data base load
load("data.Rdata")

#Crear variable logaritmica para la y
db_geih_filtered$log_salarioreal <-log(db_geih_filtered$y_salary_m_hu)

#Modificar los valores de la variable dummy para que 1=mujer
db_geih_filtered$female <- ifelse(db_geih_filtered$sex == 1, 0, 1)
head(db_geih_filtered$sex)
head(db_geih_filtered$female)

#Correr regresion
reg_gendergap <- lm(log_salarioreal~female p6210 Npr,db_geih_filtered)
stargazer(reg_gendergap,type="text")

