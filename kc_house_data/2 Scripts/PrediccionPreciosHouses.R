#dataset obtenido de    https://www.kaggle.com/divan0/multiple-linear-regression/data
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
library(randomForest)

pricesHouse<- read_excel("1 Data/kc_house_data_limpia.xlsx")
selectVars <- c("price", "bedrooms", "bathrooms","floors","sqft_above","sqft_living",
                "sqft_basement", "yr_built", "condition","waterfront","grade")
df <- pricesHouse[,selectVars]

#######Análisis Exploratorio#########
#Revisión de los datos
  
#Ver mínimo y máximo del área de la casa
min(pricesHouse$sqft_living)
max(pricesHouse$sqft_living)

#Tablas y proporciones
table(pricesHouse$bedrooms)
table(pricesHouse$bathrooms)
table(pricesHouse$floors)

#Gráfica distribuciones


hist(df$price, xlab= "Precio", main = "Distribucion del precio de las casas", col="green")
hist(df$bathrooms, xlab= "Bathrooms", main = "Distribucion de la cantidad de baños", col="red")
hist(df$bedrooms, xlab= "Bedrooms", main = "Distribucion de la cantidad de cuartos", col="blue")
hist(df$grade, xlab= "Grade", main = "Distribucion del grado de calificación", col="orange")


######Transformación de los datos#########
  
####Categorización de la variable "Condition"

df %>% ggplot(aes(x = factor(condition), y = price))+geom_boxplot()
df %>%  group_by(condition) %>% summarise(n = n(),
                                          promedio = mean(price),
                                          ds = sd(price))
df$condition <-  ifelse(df$condition %in% c(3,4,5),df$condition,"Otros")
df$condition <- as.factor(df$condition)

###Categorización de la variable "Grade"
df %>% ggplot(aes(x = factor(grade), y = price))+geom_boxplot()
df %>%  group_by(grade) %>% summarise(n = n(),
                                      promedio = mean(price),
                                      ds = sd(price))
df$NameGrade <-  ifelse(df$grade %in% c(1,2,3,4,5),"LowGrade",
                        ifelse(df$grade %in% c(6,7,8,9),"GoodGrade","HighGrade"))
df$NameGrade <- as.factor(df$NameGrade)
df <- df %>% select(-grade)

###Categorización de la variable "Waterfront"

df$waterfront <- as.factor(df$waterfront)


###Evaluación de los Pies Cuadrados (sqft)

#correlaciones entre las variables para obser cual de ellas tiene una mejor correlación lineal con la variable objetivo
cor(df$sqft_above,df$sqft_basement)
cor(df$price,df$sqft_above)
cor(df$price,df$sqft_living)
ggplot(df, aes(x=sqft_basement, y=sqft_above)) +
  geom_point() +
  geom_smooth(alpha=0.4)
df <- df %>% select(-sqft_above)
df <- df %>% select(-sqft_basement)
#Se selecciona sqft_living, ya que las otras dos variables son codependientes de ésta

###Año de construcción a antiguedad de contrsucción
df$anti <- 2021 - df$yr_built
df <- df %>% select(-yr_built)

###Estado final de la base de datos después de las transformaciones realizadas:

str(df)
  
######Regresión Lineal#########
  
#Train y Test
set.seed(2021)
indice <- sample(1:nrow(df), 0.80*21613, replace = F)
df_train <- df[indice,]
df_test <- df[-indice,]

#Prueba 1: Con todas las variables seleccionadas<h4>

modLin <- lm(formula = price~., data = df_train)
summary(modLin)

df_test$price

pred <- predict(modLin, newdata = df_test)
Errores <- (df_test$price-pred)
hist(Errores)
plot(df_test$price, pred)

ECM <- mean(Errores^2)
RECM <- ECM^0.5
RECM
cor(df_test$price,pred)

#Prueba 2: Discriminando variables<h4>


modLin2 <- lm(formula = price~bedrooms+bathrooms+sqft_living+floors+waterfront+NameGrade+anti, 
              data = df_train)
summary(modLin2)

df_test$price

pred_2 <- predict(modLin2, newdata = df_test)
Errores_2 <- (df_test$price-pred_2)
hist(Errores_2)
plot(df_test$price, pred_2)

ECM_2 <- mean(Errores_2^2)
RECM_2 <- ECM_2^0.5
RECM
cor(df_test$price,pred_2)

"Como se puede observar, la Prueba 1 posee un mejor valor en su R2, así como en 
su correlación; sin embargo, el eror de ambas pruebas es el mismo, esto podría 
deberse a que las variables discriminadas no afectan mucho al modelo cuando son 
retiradas. Por esa razón, se selecciona la Prueba 1, debido a que en las pruebas
siguientes, se realizarán diferentes experimentos como el topeo u obtención de 
nuevas variables para observar como éstas afectan en la predicción de los precios"


##Prueba Topeando variables<h4>

#Topeamos los precios:
  

quantile(df$price, seq(0,1,0.1))
quantile(df$price, seq(0.9,1,0.01))

ggplot(df, aes(x = price))+
  geom_density(alpha = 0.4, color = "orange")

ggplot(df, aes(x = price))+
  geom_histogram(alpha = 0.4, color = "orange")


df_2 <- df
df_2$price <- ifelse(df_2$price>1964400, 1964400, df_2$price)

#Topeamos sqft_living:
  

quantile(df$sqft_living, seq(0,1,0.1))
quantile(df$sqft_living, seq(0.9,1,0.01))

df_2$sqft_living <- ifelse(df_2$sqft_living>4978.8, 4978.8, df_2$sqft_living)

ggplot(df_2, aes(x = sqft_living  ))+
  geom_density(alpha = 0.4, color = "orange")

ggplot(df_2, aes(x = sqft_living  ))+
  geom_histogram(alpha = 0.4, color = "orange")


#Topeamos bedrooms:

hist(df$bedrooms)
df %>%  group_by(df$bedrooms) %>% summarise(n = n(),
                                            promedio = mean(price),
                                            ds = sd(price))

df_2$bedrooms <- ifelse(df_2$bedrooms>7  , 7  , df_2$bedrooms)

#Topeamos bathrooms:

hist(df$bathrooms)
df %>%  group_by(df$bathrooms) %>% summarise(n = n(),
                                             promedio = mean(price),
                                             ds = sd(price))


df_2$bathrooms <- ifelse(df_2$bathrooms<1.5  , 1.5  , df_2$bathrooms)

#Prueba 3: Topeando sólo Prices

#Prueba 4: Topeando Prices y sqft_living

#Prueba 5: Topeando Prices, sqft_living y bedrooms

#Prueba 6: Topeando Prices, sqft_living, bedrooms y bathrooms

#solamente se debe seleccionar a que variables se le desea realizar el topping, 
#al momento de ejecutarlo, para obtener los resultados de la otra pruebas)
df_trainTOP <- df_2[indice,]
df_testTOP <- df_2[-indice,]
modLinTop <- lm(formula = price~., data = df_trainTOP)
summary(modLinTop)

predTOP <- predict(modLinTop, newdata = df_testTOP)
ErroresTOP <- (df_testTOP$price-predTOP)
hist(ErroresTOP)

ECMTOP <- mean(ErroresTOP^2)
RECMTOP <- ECMTOP^0.5
RECMTOP

cor(df_testTOP$price,predTOP)

"En conclusión, observando los resultados de las pruebas listadas en el cuadro 
superior, se selecciona la prueba 6. Esto se debe a que es la que tiene el mayor 
valor en su R2, y posee una gran caída en su error. Apesar de que los que tienen 
una mayor correlación de la variable objetivo con la predicción son las pruebas 
1 y 2, éstas tienen un alto mayor de error, por esa razónse seleccionó la }
transformación de los datos realizados en la prueba 6 para seguir realizando 
más experimentos con ellos."

##Prueba 7: Agregando una nueva variable<h4>

#Área en promedio por piso

"Se realiza todo el proceso de transformación desde el principio, debido a que
se utilizarán las variables sqft_above y sqft_basement, las cuales no fueron 
utilizadas en las pruebas anteriores. Así mismo, se eliminará la variable
sqft_living, ya que la suma de los valores de las dos variables ingresadas, 
es igual a los valores de la ultima mencionada en cada registro de la base de 
datos. Además, se muestra todo el proceso de transformación desde el comienzo,
para facilitar la comprensión y mostrar  el proceso que se está realizando en 
esta prueba, luego de realizar los experimentos del 1 al 6"


dfNewVar <- pricesHouse[,selectVars]

dfNewVar$condition <-  ifelse(dfNewVar$condition %in% c(3,4,5),dfNewVar$condition,"Otros")
dfNewVar$condition <- as.factor(dfNewVar$condition)
dfNewVar$NameGrade <-  ifelse(dfNewVar$grade %in% c(1,2,3,4,5),"LowGrade",
                              ifelse(dfNewVar$grade %in% c(6,7,8,9),"GoodGrade","HighGrade"))
dfNewVar$NameGrade <- as.factor(dfNewVar$NameGrade)
dfNewVar <- dfNewVar %>% select(-grade)
dfNewVar$waterfront <- as.factor(dfNewVar$waterfront)
dfNewVar$anti <- 2021 - dfNewVar$yr_built
dfNewVar <- dfNewVar %>% select(-yr_built)

dfNewVar$sqft_above <- ifelse(dfNewVar$sqft_above>4370, 4370, dfNewVar$sqft_above)

dfNewVar$sqft_basement <- ifelse(dfNewVar$sqft_basement>1660, 1660, dfNewVar$sqft_basement)

dfNewVar$bedrooms <- ifelse(dfNewVar$bedrooms>7  , 7  , dfNewVar$bedrooms)
dfNewVar$bathrooms <- ifelse(dfNewVar$bathrooms<1.5  , 1.5  , dfNewVar$bathrooms)

dfNewVar$price <- ifelse(dfNewVar$price>1964400, 1964400, dfNewVar$price)
#dataset para utilizar en el proximo experimento
dfNewVar2 <- dfNewVar

dfNewVar <- dfNewVar %>% select(-sqft_living)
dfNewVar <-  dfNewVar %>% mutate(areaXPiso = (sqft_above)/(floors) )

dfNewVar_train <- dfNewVar[indice,]
dfNewVar_test <- dfNewVar[-indice,]
modLin4 <- lm(formula = price~., data = dfNewVar_train)
summary(modLin4)


pred4 <- predict(modLin4, newdata = dfNewVar_test)
Errores4<- (dfNewVar_test$price-pred4)
ECM4 <- mean(Errores4^2)
RECM4  <- ECM4^0.5
RECM4

cor(dfNewVar_test$price,pred4)

##Prueba 8: Área promedio del ambiente<h4>

#Se asume que el basement también tiene habitaciones

dfNewVar2 <- pricesHouse[,selectVars]

dfNewVar2$condition <-  ifelse(dfNewVar2$condition %in% c(3,4,5),dfNewVar2$condition,"Otros")
dfNewVar2$condition <- as.factor(dfNewVar2$condition)
dfNewVar2$NameGrade <-  ifelse(dfNewVar2$grade %in% c(1,2,3,4,5),"LowGrade",
                               ifelse(dfNewVar2$grade %in% c(6,7,8,9),"GoodGrade","HighGrade"))
dfNewVar2$NameGrade <- as.factor(dfNewVar2$NameGrade)
dfNewVar2 <- dfNewVar2 %>% select(-grade)
dfNewVar2$waterfront <- as.factor(dfNewVar2$waterfront)
dfNewVar2$anti <- 2021 - dfNewVar2$yr_built
dfNewVar2 <- dfNewVar2 %>% select(-yr_built)

dfNewVar2$sqft_above <- ifelse(dfNewVar2$sqft_above>4370, 4370, dfNewVar2$sqft_above)

dfNewVar2$sqft_basement <- ifelse(dfNewVar2$sqft_basement>1660, 1660, dfNewVar2$sqft_basement)
dfNewVar2$bedrooms <- ifelse(dfNewVar2$bedrooms>7  , 7  , dfNewVar2$bedrooms)
dfNewVar2$bathrooms <- ifelse(dfNewVar2$bathrooms<1.5  , 1.5  , dfNewVar2$bathrooms)

dfNewVar2$price <- ifelse(dfNewVar2$price>1964400, 1964400, dfNewVar2$price)

dfNewVar2 <- dfNewVar2 %>% mutate(sumCuartos = bathrooms+bedrooms)
dfNewVar2$sumCuartos <-  ifelse(dfNewVar2$sumCuartos < 1,1,dfNewVar2$sumCuartos)
dfNewVar2 <- dfNewVar2 %>% mutate(areaAmb= (sqft_living)/sumCuartos)
dfNewVar2 <- dfNewVar2 %>% select(-sumCuartos)

dfNewVar_train <- dfNewVar2[indice,]
dfNewVar_test <- dfNewVar2[-indice,]
modLin5 <- lm(formula = price~., data = dfNewVar_train)
summary(modLin5)


pred3 <- predict(modLin5, newdata = dfNewVar_test)
Errores3 <- (dfNewVar_test$price-pred3)
ECM3 <- mean(Errores3^2)
RECM3  <- ECM3^0.5
RECM3

cor(dfNewVar_test$price,pred3)

"omo se puede observar, los mejores resultados se obtienen en el experimento 7.
Dicho experimento, aparte de tener el R2 más alto y el error más pequeño 
(el precio se encuentra en miles), su correlación de la predicción con la
variable objetivo tiene un valor de 0.802, el cual es el mayor valor obtenido
para dicha métrica dentro de todos los experimentos realizados. Por esta razón, 
se considera que el mejor proceso a utilizar para este caso con un modelo de 
regresión multiple, es el experimento 7. A continucación, se realizará una 
última prueba con el proceso del experimento 7, pero en este caso utilizando 
un Random Forest."


#Prueba 9: Utilizando el modelo Random Forest con el proceso de la prueba 7<h4>


dfRF <-  dfNewVar 

dfRF_train <- dfRF[indice,]
dfRF_test <- dfRF[-indice,]

mod_ba <- randomForest(price~., data = dfRF_train)
summary(mod_ba)


pred_ba <- predict(mod_ba, newdata = dfRF_test)
Errores_ba <- (dfRF_test$price-pred_ba)
hist(Errores_ba)
ECM_ba <- mean(Errores_ba^2)
RECM_ba <- ECM_ba^0.5


RECM_ba
cor(dfRF_test$price,pred_ba)

(cor(dfRF_test$price,pred_ba))^2
  

"Como se puede observar, el mejor resultado es obtenido por el modelo no lineal,
el Random Forest. Así mismo, este modelo logra reducir 
en una gran cantidad el error obtenido. "


######Conclusiones##########

"Después de realizar este trabajo, podemos concluir que antes de realizar un análisis
exploratorio, transformaciones de los datos o predicciones; es muy importante 
conocer muy bien el dataset y entender qué representa cada variable; 
como en este caso las variables sqft_above y sqft_basement, 
las cuales sumando sus valores resultaba en el valor de sqft_living
para cada registro, lo cual si no habríamos tomado en cuenta se habría tomado un
modelo en donde una variable tendría mas importancia que otras, cuando en 
realidad ese no es el caso. Luego, es importante realizar un análisis
exploratorio, en donde se puedan observar los patrones de la data y cómo los 
datos se comportan dentro de cada variable: si uno de ellos tiene demasiados 
valores atípicos, o si existen valores vacíos o incongruencia; para poder así 
realizar los procesos necesarios y mejorar el resultado obtenido al momento de 
la predicción. Por último, al momento de realizar las predicciones, es muy 
importante e interesante realizar diferentes pruebas con las variables que 
se tienen (topeandolas, convirtiendolas a categóricas, creando nuevas variables, 
utilizando diferentes variables, etc) debido a que se pueden obtener diferentes 
resultados y encontrar el mejor experimento para cada situación."
  
"Finalmente, con respecto a los resultados de los experimentos, en este caso con
las pruebas realizadas y las variables utilizadas, el mejor modelo es el random 
forest, debido a que es el que tiene una mejor performance y logra reducir 
bastante el error obtenido, el segundo mejor modelo o experimento es el número 7, 
debido a que comparándolo entre los resultados entre los modelos lineales,
éste es el que obtiene los mejores resultados"
  
