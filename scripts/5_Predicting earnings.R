##########################################################
#         Problem Set 1. Predicting Income
#           Big data and Machine Learning
#             Universidad de los Andes
##########################################################
### Initial Configuration
if(!require(pacman)) install.packages("pacman")
require(pacman)
p_load(tidyverse, rvest, stargazer)

### Loading data
setwd("/Users/irina/Documents/Repositorios/PS1_Predicting_income/stores")
load("data.Rdata")
glimpse(geih_filtered)

### Selecting some predictors
playgroundb <- geih_filtered %>% select (y_salary_m_hu, maxEducLevel,
                                        p6426, age,
                                        estrato1, sex,
                                        formal, informal,
                                        hoursWorkUsual, totalHoursWorked,
                                        hoursWorkActualSecondJob,
                                        oficio, relab,
                                        p6870, sizeFirm,
                                        p6610, p7040, p7160,
                                        p6920, regSalud,
                                        p7500s1, p7500s1a1,
                                        p7510s5, p7510s5a1,
                                        p7510s6, p7510s6a1,
                                        p7510s7, p7510s7a1
                                        )

### Creating and adjusting variables
# Create log variable for real hourly salary
playgroundb$log_salariorealh <-log(playgroundb$y_salary_m_hu)

# Rename p6426
playgroundb <- playgroundb %>% rename(exp = p6426)

# Create experience squared
playgroundb <- playgroundb %>% mutate(exp2 = exp^2)

# Create age squared
playgroundb <- playgroundb %>% mutate(age2 = age^2)

# Modify the values for dummy variable so that 1 = female
playgroundb$female <- ifelse(playgroundb$sex == 1, 0, 1)
playgroundb$female <- factor(playgroundb$female)

# Categorical variables
playgroundb$educ <- factor(playgroundb$maxEducLevel)
playgroundb$oficio <- factor(playgroundb$oficio)
playgroundb$relab <- factor(playgroundb$relab)
playgroundb$sizeFirm <- factor(playgroundb$sizeFirm)
playgroundb$p6920 <- factor(playgroundb$p6920)
playgroundb$regSalud <- factor(playgroundb$regSalud)
playgroundb$formal <- factor(playgroundb$formal)
playgroundb$informal <- factor(playgroundb$informal)
playgroundb$estrato1 <- factor(playgroundb$estrato1)
playgroundb$p7040 <- factor(playgroundb$p7040)
playgroundb$p7160 <- factor(playgroundb$p7160)

## Descriptive statistics
stargazer(data.frame(playgroundb), header=FALSE, type='text', title = 'Variables included in the Selected Data Set')

## Distribution of real hourly salary
plot(density(playgroundb$log_salariorealh))



### Linear Model Specification

# Model1. Linear Model using age
regresion_salario_edad <- lm(log_salariorealh~age + age2, playgroundb)
stargazer(regresion_salario_edad, type = "text")

playgroundb <- playgroundb %>% mutate(yhat = predict(regresion_salario_edad))

summ <- playgroundb %>%
  group_by(age, age2) %>%
  summarize(
    mean_y = mean(log_salariorealh),
    yhat_reg = mean(yhat), .groups = "drop"
  )

ggplot(summ) + geom_line(
  aes(x = age, y = yhat_reg),
  color = "blue", size = 1.5
) + 
  labs(
    title = "Ln wages by age in the EIGH",
    x = "Age",
    y = "Ln Real hourly wage"
  ) +
  theme_bw()

regresion_salario_edad$coefficients
-regresion_salario_edad$coefficients[2]/(2*regresion_salario_edad$coefficients[3])


### Validation Set Approach ###
### Split the sample into two: a training (70%) and a testing (30%) sample.
## Using the data.Rdata database. This db is without missing values for the resultant variable (y_salary_m_hu)

# Making this example reproducible
set.seed(10101)

# Create ID column with a row number
playgroundb$id <- 1:nrow(playgroundb)

# Use 70% of the dataset as a training set and 30% as a test set
train <- playgroundb %>% dplyr::sample_frac(0.70)
test <- dplyr::anti_join(playgroundb, train, by = 'id')

### Predicting and testing the different models
## Model 1: Linear relationship
model1 <- lm(log_salariorealh~age, data = train)
test$predict1 <- predict(model1, newdata = test)
mse1 <- with(test, mean((log_salariorealh - predict1)^2))

## Model 2: Quadratic relationship between wage and age
model2 <- lm(log_salariorealh~age + age2, data = train)
test$predict2 <- predict(model2, newdata = test)
mse2 <- with(test, mean((log_salariorealh - predict2)^2))

## Model 3: Adding level of education
model3 <- lm(log_salariorealh~age + age2 + educ, data = train)
test$predict3 <- predict(model3, newdata = test)
mse3 <- with(test, mean((log_salariorealh - predict3)^2))

## Model 4: Adding level of experience
model4 <- lm(log_salariorealh~age + age2 + educ + exp, data = train)
test$predict4 <- predict(model4, newdata = test)
mse4 <- with(test, mean((log_salariorealh - predict4)^2))

## Model 5: Testing adding squared experience just only in this model
model5 <- lm(log_salariorealh~ age + age2 + educ + exp + exp2, data = train)
test$predict5 <- predict(model5, newdata = test)
mse5 <- with(test, mean((log_salariorealh - predict5)^2))

## Model 6: Adding hours worked
model6 <- lm(log_salariorealh~age + age2 + educ + exp + hoursWorkUsual + totalHoursWorked, data = train)
test$predict6 <- predict(model6, newdata = test)
mse6 <- with(test, mean((log_salariorealh - predict6)^2))

## Model 7: Adding gender female
model7 <- lm(log_salariorealh~age + age2 + educ + exp + hoursWorkUsual + totalHoursWorked + female, data = train)
test$predict7 <- predict(model7, newdata = test)
mse7 <- with(test, mean((log_salariorealh - predict7)^2))

## Model 8: Adding other income
model8 <- lm(log_salariorealh~age + age2 + educ + exp + hoursWorkUsual + totalHoursWorked + female + p7500s1a1 + p7510s5a1 + p7510s6a1 + p7510s7a1, data = train)
test$predict8 <- predict(model8, newdata = test)
mse8 <- with(test, mean((log_salariorealh - predict8)^2))

## Model 9: Adding pension
model9 <- lm(log_salariorealh~age + age2 + educ + exp + hoursWorkUsual + totalHoursWorked + female + p7500s1a1 + p7510s5a1 + p7510s6a1 + p7510s7a1 + p6920, data = train)
test$predict9 <- predict(model9, newdata = test)
mse9 <- with(test, mean((log_salariorealh - predict9)^2))

## Model 10: Adding informal variable
model10 <- lm(log_salariorealh~age + age2 + educ + exp + hoursWorkUsual + totalHoursWorked + female + p7500s1a1 + p7510s5a1 + p7510s6a1 + p7510s7a1 + p6920 + informal, data = train)
test$predict10 <- predict(model10, newdata = test)
mse10 <- with(test, mean((log_salariorealh - predict10)^2))

## Model 11: Adding size firm variable
model11 <- lm(log_salariorealh~age + age2 + educ + exp + hoursWorkUsual + totalHoursWorked + female + p7500s1a1 + p7510s5a1 + p7510s6a1 + p7510s7a1 + p6920 + informal + sizeFirm, data = train)
test$predict11 <- predict(model11, newdata = test)
mse11 <- with(test, mean((log_salariorealh - predict11)^2))

## Model 12: Adding estrato1 variable
model12 <- lm(log_salariorealh~age + age2 + educ + exp + hoursWorkUsual + totalHoursWorked + female + p7500s1a1 + p7510s5a1 + p7510s6a1 + p7510s7a1 + p6920 + informal + sizeFirm + estrato1, data = train)
test$predict12 <- predict(model12, newdata = test)
mse12 <- with(test, mean((log_salariorealh - predict12)^2))

## Model 13: Adding p7040 variable (additional job)
model13 <- lm(log_salariorealh~age + age2 + educ + exp + hoursWorkUsual + totalHoursWorked + female + p7500s1a1 + p7510s5a1 + p7510s6a1 + p7510s7a1 + p6920 + informal + sizeFirm + estrato1 + p7040, data = train)
test$predict13 <- predict(model13, newdata = test)
mse13 <- with(test, mean((log_salariorealh - predict13)^2))

## Model 14: Adding squared experience again
model14 <- lm(log_salariorealh~ age + age2 + educ + exp + exp2 + hoursWorkUsual + totalHoursWorked + female + p7500s1a1 + p7510s5a1 + p7510s6a1 + p7510s7a1 + p6920 + informal + sizeFirm + estrato1 + p7040, data = train)
test$predict14 <- predict(model14, newdata = test)
mse14 <- with(test, mean((log_salariorealh - predict14)^2))

## Creating a MSE vector
mse <- c(mse1, mse2, mse3, mse4, mse5, mse6, mse7, mse8, mse9, mse10, mse11, mse12, mse13, mse14)

# Create a data frame
results <- data.frame(model = factor (c("Model1", "Model2",
                                        "Model3", "Model4",
                                        "Model5", "Model6",
                                        "Model7", "Model8",
                                        "Model9", "Model10",
                                        "Model11", "Model12",
                                        "Model13", "Model14"), ordered = TRUE),
                      MSE = mse)

### LOOCV ###
# Making this example reproducible
set.seed(10101)

# In LOOCV k = n
K <- 9700

# Vectors to store the predicted values and mse
predict_model12 <- vector("numeric", K)
predict_model13 <- vector("numeric", K)
mse_model12 <- vector("numeric", K)
mse_model13 <- vector("numeric", K)

# Perform LOOCV
for (i in 1:K) {
  # Training and testing set
  train_data <- playgroundb[-i, ]
  test_data <- playgroundb[i, ]
  
  # Fitting the models
  model14 <- lm(log_salariorealh ~ age + age2 + educ + exp + hoursWorkUsual + totalHoursWorked + female + p7500s1a1 + p7510s5a1 + p7510s6a1 + p7510s7a1 + p6920 + informal + sizeFirm + estrato1, data = train_data)
  model15 <- lm(log_salariorealh ~ age + age2 + educ + exp + exp2 + hoursWorkUsual + totalHoursWorked + female + p7500s1a1 + p7510s5a1 + p7510s6a1 + p7510s7a1 + p6920 + informal + sizeFirm + estrato1 + p7040, data = train_data)
  
  # Testing the models
  predict_model12[i] <- predict(model14, newdata = test_data)
  predict_model13[i] <- predict(model15, newdata = test_data)
  
  # Calculating the MSE
  mse_model12[i] <- with(test_data, (log_salariorealh - predict_model12[i])^2)
  mse_model13[i] <- with(test_data, (log_salariorealh - predict_model13[i])^2)
}

mean_mse_model12 <- mean(mse_model12)
mean_mse_model13 <- mean(mse_model13)

cat("LOOCV MSE for Model 12:", mean_mse_model12, "\n")
cat("LOOCV MSE for Model 14:", mean_mse_model13, "\n")
