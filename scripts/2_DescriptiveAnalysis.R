#########################################################################################
##############          Taller 1 - Big data and Machine Learning           ##############
#########################################################################################

############################    Integrantes del grupo       #############################

# Irina Andrea Vélez López – Código:  
# Miguel Angel Victoria Simbaqueva – Código:  
# Daniel Casas Bautista – Código: 202120803
# Lucia Fillippo – Código: 202213187

#######################    Adecuación del espacio de trabajo      ########################

install.packages("pacman") #Instalar librería si no contamos con esta 
library(pacman) #Llamar librería
p_load("tidyverse","stargazer", "rvest")
rm(list = ls()) #Limpia las variables que existan al momento de correr el código

#######################    Scraping de las bases de datos a usar    ######################

#NOTA: La matriz queda guardada y la podemos llamar, para no tener que correr ese código demorado
DatosGEIH<-readRDS("Datos_GEIH.Rds") #Para cargar la base

#Importamos cada base de datos y la volvemos data.frame para poder convertirlos en una  matriz
Base1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")%>%
  html_table()
Base1 <- data.frame(Base1)
Base2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html")%>% 
  html_table()
Base2 <- data.frame(Base2)
Base3 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html")%>% 
  html_table()
Base3 <- data.frame(Base3)
Base4 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html")%>% 
  html_table()
Base4 <- data.frame(Base4)
Base5 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html")%>% 
  html_table()
Base5 <- data.frame(Base5)
Base6 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html")%>% 
  html_table()
Base6 <- data.frame(Base6)
Base7 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html")%>% 
  html_table()
Base7 <- data.frame(Base7)
Base8 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html")%>% 
  html_table()
Base8 <- data.frame(Base8)
Base9 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html")%>% 
  html_table()
Base9 <- data.frame(Base9)
Base10 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")%>% 
  html_table()
Base10<- data.frame(Base10)
#Al observar que cada una de las bases de datos si pudo ser importada, se procede a unir cada base de datos
#Con la fusión de todas las bases de datos, tendremos oficialmente los Datos completos de la GEIH de 2018
DatosGEIH<- rbind(Base1, Base2, Base3, Base4, Base5, Base6, Base7, Base8, Base9, Base10)

#Con este código guardamos y cargamos la base de datos para no tener que hacer siempre el Scraping

#DatosGEIH<-readRDS("Datos_GEIH.Rds") #Para cargar la base, en caso que no se tenga tiempo para cargar desde la página web-Solo se debe poner la base de datos en la ubicación de la sesión y carga esta tabla
#saveRDS(DatosGEIH, file = "Datos_GEIH.rds") #Crea el archivo RDS en el directorio de trabajo, en caso de necesitarse


#######################    Ordenar la base de datos      ########################

# Aquí incluimos solo a las personas con edad mayor o igual a 18 años
DatosGEIH_18<-DatosGEIH[DatosGEIH$age>=18,]

# Aquí hacemos la clasificación de variables
exp <- floor(c(DatosGEIH_18$p6426/12)) #Ponemos anual la variable de experiencia
View(exp)   #Vemos que hay un montón de observaciones "NA" que nos piden quitar
educ <- DatosGEIH_18$p6210 #Se asigna la variable educación
View(educ)
DGEIH<-subset(DatosGEIH_18, select = c( "ingtot", "pet", "mes", "age", "sex","ocu", "oficio") ) #Hacemos un subset con las variables a usar
DGEIH<-cbind(DGEIH, exp, educ) #Incluimos las variables calculadas que utilizaremos en el modelo
View(DGEIH) # Aquí aún no hemos quitado las observaciones "NA" 
DGEIH<- DGEIH[DGEIH$ingtot>0,]  #Este ingreso no tiene "NA" pero sí tiene ceros, aquí los limpiamos
summary(DGEIH) #Visualización general de la base de datos... peeero, no está horario el salario
length(DGEIH$ingtot)

# Aquí la idea es quitar las observaciones "NA"
cantidad_na <- sapply(DGEIH, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(DGEIH)
porcentaje_na <-porcentaje_na*100
porcentaje_na #Visualizamos el porcentaje de los datos que tienen NA
DGEIH$oficio[is.na(DGEIH$oficio)] = 100 #Se imputa la nueva categoría 1oo a Oficio

DGEIH <- DGEIH %>% #Se vuelven categóricas las variables que así lo sean en la BD
  mutate_at(.vars = c(
    "pet","sex", "ocu", "educ", "oficio"),
    .funs = factor)

DGEIH[is.na(DGEIH)] = 0 #Se asigna 0 a las NA de la variable "exp"
#DGEIH %>% subset(ingtot <= 2*iqr | is.na(ingtot)==T)
summary(DGEIH) #Se verifica que no existan NA
View(DGEIH) #Se verifica que no existan NAs

#######################    Análisis descriptivo      ########################

library(ggplot2)        #Cargaremos esta librería porque lo necesitaremos

# (1) Descripción general de la base
View(DGEIH)
nrow(DGEIH) #Número de filas
ncol(DGEIH) #Número de columnas
dim(DGEIH)  #Número de filas y columnas
head(DGEIH) #Esto muestra los primeros valores de la base... no sirve mucho pero me pareció cool
tail(DGEIH) #Esto muestra los últimos  valores de la base... no sirve mucho pero me pareció cool

# (2) Descripción de la variable de edad
Edad<- DGEIH$age
class(Edad)         #Aquí vemos la clase y nos dice que es de números enteros (integer)
mean(Edad)          #Edad media... nos dice que es 42.95
min(Edad)           #Edad mínima... como no incluimos a gente menor a 18 años, entonces obvio es 18
max(Edad)           #Edad máxima... 106 años, caramba... para ponerlo en el doc
modeEdad <- function(Edad){
  return(as.numeric(names(which.max(table(Edad)))))
}
modeEdad(Edad)      #La moda de la edad es 25 años

#Ahora haremos un diagrama de barras muy top

# Crearemos un data frame con los datos de edad
datos_edad <- data.frame(Edad)

# Crearemos el diagrama de barras
bar_chart <- ggplot(datos_edad, aes(x = Edad)) +
  geom_bar(fill = "#2E75B6", color = "white") +
  labs(x = "Edad", y = "Frecuencia", title = "Distribución de Edad") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# Que nos muestre el diagrama de barras
print(bar_chart)

# (3) Descripción de la Población en Edad de Trabajar (PET)
PET <- DGEIH$pet
View(DGEIH$pet)
class(PET)          #Aquí vemos la clase y nos dice que es categórica (factor)
levels(PET)         #Como es categórica, es de nivel 1
summary(PET)        #Todas las observaciones son 1 (esta es una dummy que dice que están en edad de trabajar, por eso es siempre 1)

# (4) Descripción de la variable de ocupación
ocu<- DGEIH$ocu
class(ocu)          #Aquí vemos la clase y nos dice que es categórica (factor)
levels(ocu)         #Como es categórica y tiene nivel 0 y 1         
summary(ocu)        #16.277 personas están ocupadas y 3.254 están desocupadas
pie(table(ocu))     #Aquí podemos poner un pie que se ve genial

# Pero ese pie no quedó tan top, entonces utilizaremos ggplot2 para que quede más estético:

#Creamos un data frame con los datos de ocupación
datos_ocu <- data.frame(
  Categoría = c("Ocupadas", "No Ocupadas"),
  Cantidad = c(16277, 3524)
)

#Calculamos los porcentajes
datos_ocu$Porcentaje <- round(datos_ocu$Cantidad / sum(datos_ocu$Cantidad) * 100, 1)

#Creamos el diagrama de pie
pie_chart <- ggplot(datos_ocu, aes(x = "", y = Cantidad, fill = Categoría)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0)) +
  geom_text(aes(label = paste0(Categoría, "\n", Porcentaje, "%")), 
            position = position_stack(vjust = 0.5), size = 3, color = "white")

#Que nos muestre el diagrama de pie
print(pie_chart)

# (5) Descripción de la variable de educación

#NOTA: Dejamos esto como molde, pero realmente tenemos que incluir la educación con "maxEducLevel" que tiene las siguientes opciones:
#1 "None"
#2 "preschool"
#3 "primary incomplete (1-4)"
#4 "primary complete (5)"
#5 "secondary incomplete (6-10)"
#6 "secondary complete (11)"
#7 "terciary"
#9 "N/A";
# Esto será clave de cara a la clasificación de la educación


View(educ)
class(educ)         #Aquí vemos la clase y nos dice que es de números enteros (integer)
mean(educ)          #Educación media... nos dice que es 4.87 (casi secundaria incompleta)
modeEduc <- function(educ){
  return(as.numeric(names(which.max(table(educ)))))
}
modeEduc(educ)      #La moda de la educación es 6 (secundaria completa)

## Ahora vamos a hacer una gráfica de barras que refleje la cantidad de personas que tienen cada nivel de educación
## Se utilizarán abreviaturas de las siguiente manera:
#1 "None"                         = "none
#2 "preschool"                    = "pre"
#3 "primary incomplete (1-4)"     = "pri_in"
#4 "primary complete (5)"         = "pri_com"
#5 "secondary incomplete (6-10)"  = "sec_in"
#6 "secondary complete (11)"      = "sec_com"
#7 "terciary"                     = "ter"
#9 "N/A";                         = "N/A"... aunque no hay en la base ya limpia

# Creamos el data frame
datos_educ <- data.frame(educ)

# Creamos el diagrama de barras con etiquetas de datos y demás
bar_chart <- ggplot(datos_educ, aes(x = as.factor(educ))) +
  geom_bar(fill = "#2E75B6", color = "white") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3, color = 'black') +  # Agregar etiquetas de datos
  labs(x = "Educ", y = "Frecuencia", title = "Distribución de Educación") +
  scale_x_discrete(labels = c("none", "pre", "prim_in", "prim_com", "sec_in", "sec_com", "ter", "N/A")) +  # Cambiar las categorías del eje x
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 8),
        axis.line.y = element_blank(),  # Eliminar el eje vertical izquierdo
        axis.ticks.y = element_blank(),  # Eliminar los ticks del eje vertical izquierdo
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# Hacemos que nos muestre el diagrama de barras
print(bar_chart)


# (6) Descripción de la variable de género
sex<- DGEIH$sex
class(sex)          #Aquí vemos la clase y nos dice que es categórica (factor)
levels(sex)         #Como es categórica y tiene nivel 0 y 1         
summary(sex)        #10.047 son hombres (1) y 9.754 son mujeres (0)... tener en cuenta si esto cambia

# Haremos un pie 

# Creamos un data frame con los datos de sexo
datos_sex <- data.frame(
  Categoría = c("Hombres", "Mujeres"),
  Cantidad = c(10.047, 9.754)
)

# Crear el diagrama de pie con etiquetas de datos
pie_chart <- ggplot(datos_sex, aes(x = "", y = Cantidad, fill = Categoría)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = Cantidad), position = position_stack(vjust = 0.5), size = 4) +  # Etiquetas de datos en valores enteros
  coord_polar("y", start = 0) +
  labs(title = "Distribución por género") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.position = "right")

# Mostrar el diagrama de pie
print(pie_chart)



# (7) Descripción de la variable de experiencia
expp<- DGEIH$exp
View(expp)
class(expp)         #Aquí vemos la clase y nos dice que es numérica
mean(expp)          #Aquí vemos la experiencia media y vemos es de 4.2 años
min(expp)           #Aquí vemos la experiencia mínima y vemos que como era de esperar es de 0 años
max(expp)           #Aquí vemos la experiencia máxima y vemos que es de 60 años
modeExp <- function(expp){
  return(as.numeric(names(which.max(table(expp)))))
}
modeExp(expp)       #Aquí vemos que la moda de la experiencia es de 0 años



###########

# Crear el data frame
datos_exp <- data.frame(expp)

# Crear el histograma
histograma <- ggplot(datos_exp, aes(x = expp)) +
  geom_histogram(binwidth = 1, fill = "#2E75B6", color = "white") +
  labs(x = "Experiencia", y = "Frecuencia", title = "Histograma de Experiencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Mostrar el histograma
print(histograma)



###########


plot(hist(expp))



# (8) Descripción de la variable de oficio
library(modeest)
Oficio_<- DGEIH$oficio
class(Oficio_)
levels(Oficio_)
summary(Oficio_)
table(Oficio_)
barplot(table((Oficio_)))
mlv(Oficio_, method = "mfv")

