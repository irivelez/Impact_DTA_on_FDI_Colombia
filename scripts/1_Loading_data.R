##########################################################
#         Problem Set 1. Predicting Income
#           Big data and Machine Learning
#             Universidad de los Andes
##########################################################
'''
Authors:

- Daniel Casas Bautista
- Lucia Fillippo
- Miguel Angel Victoria Simbaqueva 
- Irina Andrea Vélez López
'''

### Initial Configuration
if(!require(pacman)) install.packages("pacman")
require(pacman)
p_load(tidyverse, rvest)


### Loading data  
data_url = "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_"

# Loop
db_list <- lapply(1:10, function(i) {
  url <- paste0(data_url, "page_", i, ".html")
  page <- read_html(url) 
  tabla <- page %>% html_table
  db_geih <- data.frame(tabla)
})

db_geih <- do.call(rbind,db_list) # Concatenate the tables in a dataframe


### Transforming data

##Filtering the data to those over 18 and employed (ocu=1)
# Details of the variable age 
summary(db_geih$age)
sum(is.na(db_geih$age)) # Checking for missing values
sum(is.na(db_geih$ocu))

db_geih_filtered <- db_geih[db_geih$age > 18 & db_geih$ocu == 1, ]
save(db_geih_filtered, file = "/Users/irina/Documents/Repositorios/PS1_Predicting_income/stores/completedata.Rdata")

## Dealing with missing values
# Count missing values for each column and sorting
missing_counts <- sapply(db_geih_filtered, function(x) sum(is.na(x)))
sorted_missing <- sort(missing_counts, decreasing = TRUE)
top_missing <- head(sorted_missing, 20)
top_missing

# Deleting the top missing variables and all variables that start with cclasnr
delete_var <- c("p550", "p7310", "p7350", "p7422", "p7422s1", "p7472", "p7472s1", "ina", "imdi", "imdies", "y_gananciaNetaAgro_m", "iof3ies", "y_accidentes_m", "p6585s4a2", "y_subEducativo_m", "isaes", "iof2es", "dominio", "clase")
delete_var2 <- grep("^cclasnr", names(db_geih_filtered), value = TRUE)
delete_t <- c(delete_var, delete_var2)

geih_filtered <- db_geih_filtered %>%
  select(-one_of(delete_t))

## Dealing with 0 wages

# Verification of the min and max value of the main resultant variables, and their missing values
summary(geih_filtered$p6500)           # Monthly labor income. DANE Variable
summary(geih_filtered$y_ingLab_m)      # labor income salaried nominal monthly
summary(geih_filtered$y_ingLab_m_ha)   # labor income salaried nominal hourly
summary(geih_filtered$y_salary_m)      # salary - nominal monthly
summary(geih_filtered$y_salary_m_hu)   # salary - real hourly (usual)

# The 40.32% of the observations for the main resultant variables have missing values
missing_values <- is.na(geih_filtered$y_salary_m_hu)

missing_perc = (sum(missing_values)/nrow(geih_filtered))*100
missing_perc

missing_data <- geih_filtered[missing_values, c("estrato1", "maxEducLevel")]

missing_by_estrato <- table(missing_data$estrato1)
missing_by_educ <- table(missing_data$maxEducLevel)

missing_by_estrato <- sort(missing_by_estrato, decreasing = TRUE)
missing_by_educ <- sort(missing_by_educ, decreasing = TRUE)

missing_by_estrato_p <- missing_by_estrato / sum(missing_by_estrato) * 100
missing_by_educ_p <- missing_by_educ / sum(missing_by_educ) * 100

missing_by_estrato_p
missing_by_educ_p

# Data frame with missing values by estrato and educación
results <- data.frame(estrato = names(missing_by_estrato_p),
                      educacion = names(missing_by_educ_p),
                      p_missing_estrato = missing_by_estrato_p,
                      p_missing_educacion = missing_by_educ_p)

knitr::kable(results, caption = "Missing Values by Estrato and Educación")

'''
Finally, we decided to eliminate observations containing missing values 
for the resultant variable (y_salary_m_hu)
'''

geih_filtered <- geih_filtered[complete.cases(geih_filtered$y_salary_m_hu), ]
save(geih_filtered, file = "/Users/irina/Documents/Repositorios/PS1_Predicting_income/stores/data.Rdata")
