#ANÁLISIS DESCRIPTIVO DE LOS DATOS:

#DE LAS VARIABLES NUMÉRICAS HALLAR EL VALOR MÍNIMO, EL MÁXIMO, LA MEDIANA Y LA MEDIA.
#Cargamos nuestro data
housing_train <- read.csv("housing_train.csv")
summary(housing_train)
#obtenemos valor entre 0 y 1 de la columnas que son de tipo factor
colm.factor <- sapply(housing_train, function(x) sum(is.factor(x)))
valor <- 0
#borramos las columnas de tipo factor
columnas_a_borrar <- which((colm.factor != valor))
housing_train[,columnas_a_borrar] <- NULL
#obtenemos el valor mínimo, el máximo, la mediana y la media.
summary(housing_train)
housing_train <- data.frame(columnas_a_borrar,)

#DE LAS VARIABLES CATEGÓRICAS, LISTAR LAS DIFERENTES CATEGORÍAS Y HALLAR LA 
#FRECUENCIA DE CADA UNA DE ELLAS. 
housing_train <- read.csv("housing_train.csv")
summary(housing_train)
#obtenemos valor entre 0 y 1 de la columnas que son de tipo factor
colm.factor <- sapply(housing_train, function(x) sum(is.factor(x)))
valor <- 0
#borramor los datos que son de tipo int de nuestro data principal
columnas_a_borrar <- which((colm.factor==valor))
housing_train[,columnas_a_borrar] <- NULL
#obtenemos las diferentes categorías y la frecuencia de cada una de ellas.
summary(housing_train)


#CORRELACIONES EXISTENTES ENTRE LAS VARIABLES NUMÉRICAS DEL CONJUNTO DE DATOS.
housing_train <- read.csv("housing_train.csv")
summary(housing_train)
#obtenemos valor entre 0 y 1 de la columnas que son de tipo factor
colm.factor <- sapply(housing_train, function(x) sum(is.factor(x)))
#borramos las columnas que tiene la opcion de factor
columnas_a_borrar <- which((colm.factor!=valor))
housing_train[,columnas_a_borrar] <- NULL
#obtenemos la matriz de variables numericas
summary(housing_train)
#buscamos las relaciones de las variables 
correlaciones <- cor(x = housing_train, method = "pearson")
correlaciones <- format(correlaciones, digits = 0)
#observamos las relacion de las variables con valores de 1 las que tiene 
#relacion y con 0 las que no tienen relacion 
View(correlaciones)


#TRATAMIENTO DE MISSING 
housing_train <- read.csv("housing_train.csv")
summary(housing_train)
#obtenemos el porcentaje de columans que tienen valores nulos
#colMeans(is.na(housing_train))
#asignamos el valor que deseamos eliminar
#porcentaje <- .6
#borramos las columnas que tiene un % > 0.6
#columnas_a_borrar <- which(colMeans(is.na(housing_train))>porcentaje)
#housing_train[,columnas_a_borrar] <- NULL
fac <- read.csv("housing_train.csv")
int <- read.csv("housing_train.csv")
#obtenemos valor entre 0 y 1 de la columnas que son de tipo factor
colm.factor <- sapply(housing_train, function(x) sum(is.factor(x)))
valor <- 0
#borramos las columnas de tipo factor
fac.borrar <- which((colm.factor != valor))
int.borrar <- which((colm.factor == valor))
fac[,fac.borrar] <- NULL
int[,int.borrar] <- NULL
#asignamos valor de 0 a los NA
cero.na=function(x){
  ifelse(is.na(x),0,x)}
fac = data.frame(sapply(fac,cero.na))
summary(fac)
housing_train <- data.frame(int,fac)
summary(housing_train)
View(housing_train)
#segun lo observado en la data tome la decicionde del asignas un valor 
# de 0 para no perjudicar ninguna columan al eliminarla y asi
# no alterar la prediccion de la venta de los inmuebles.


#ARBOL DE REGRESION
install.packages("tree", dependencies = T)
library(tree)
set.seed(1)
housing_train <- read.csv("housing_train.csv")
fac <- read.csv("housing_train.csv")
int <- read.csv("housing_train.csv")
#obtenemos valor entre 0 y 1 de la columnas que son de tipo factor
colm.factor <- sapply(housing_train, function(x) sum(is.factor(x)))
valor <- 0
#borramos las columnas de tipo factor
fac.borrar <- which((colm.factor != valor))
int.borrar <- which((colm.factor == valor))
fac[,fac.borrar] <- NULL
int[,int.borrar] <- NULL
#asignamos valor de 0 a los NA
cero.na=function(x){
  ifelse(is.na(x),0,x)}
fac = data.frame(sapply(fac,cero.na))
int=as.matrix(int)
int[is.na(int)] <-"None"
int=as.data.frame(int)
summary(fac)
housing_train <- data.frame(int,fac)
#dividimo la mitad de los datos de forma aleatoria
train <- sample(1:nrow(housing_train), nrow(housing_train)/2)
#observamos lo que contiene train
summary(train)
tree.housing_train <- tree(SalePrice ~ . , housing_train[train, ])
summary(tree.housing_train)
plot(tree.housing_train)
text(tree.housing_train, pretty = 0)
#El precio de la vivienda depende de el material general y calidad de acabado
#tiene una calificacion entre 1 y 10, ademas se toma en cunta los m^2 del terreno
#para que el costo de la vivienda sea elevado
#Utilizacion del cvtree() para podar el arbol 
cv.housing_train <- cv.tree(tree.housing_train, K=10)
plot(cv.housing_train$size, cv.housing_train$dev, type = 'b')
#podamos el arbol
prune.housing_train <- prune.tree(tree.housing_train, best = 8)
plot(prune.housing_train)
text(prune.housing_train, pretty = 0)
#prediccion sin podar 
yhat <- predict(tree.housing_train, newdata = housing_train[-train, ])
housing_train.test <- housing_train[-train, "SalePrice"]
plot(yhat, housing_train.test, main = "Arbol sin podar")
abline(0,1)
#prediccion con el arbol podado 
yhat.prune <- predict(prune.housing_train, newdata = housing_train[-train, ])
plot(yhat.prune, housing_train.test, main = "Arbol podado")
abline(0,1)
#error cuadrático medio
mean((yhat - housing_train.test)^2)
mean((yhat.prune - housing_train.test)^2)
#las diferencia no es ta grande con el MSE del arbol podado tomado el MSE de 
# del arbol sin podar tenemos un dato de 1839728480 del cual sacamos 
#su raiz cudrada y no da como resultado de 42892.05  lo que nos idnica que 
#las prediciones con este modelo estan alrededor del 42 % del 
#valor mediano real por otro lado con el arbol podado observamos que el MSE 
#es mayor 2050058286  obteniebdo un % de 45% del valor mediano real 



#ARBOL DE CLASIFICACION 
library(tree)
set.seed(1)
housing_train <- read.csv("housing_train.csv")
fac <- read.csv("/housing_train.csv")
int <- read.csv("housing_train.csv")
#obtenemos valor entre 0 y 1 de la columnas que son de tipo factor
colm.factor <- sapply(housing_train, function(x) sum(is.factor(x)))
valor <- 0
#borramos las columnas de tipo factor
fac.borrar <- which((colm.factor != valor))
int.borrar <- which((colm.factor == valor))
fac[,fac.borrar] <- NULL
int[,int.borrar] <- NULL
summary(int)
summary(fac)
#asignamos valor de 0 a los NA
cero.na=function(x){
  ifelse(is.na(x),0,x)}
fac = data.frame(sapply(fac,cero.na))
int=as.matrix(int)
int[is.na(int)] <-"None"
int=as.data.frame(int)
housing_train <- data.frame(int,fac)
summary(housing_train)
#creamos los tres grupos dependiendo la caracteristicas
grupo1 <- ifelse(housing_train$SalePrice <= 100000, "Yes", "No") 
grupo2 <- ifelse(housing_train$SalePrice >= 101000  & housing_train$SalePrice<= 500000 , "Yes", "No") 
grupo3 <- ifelse(housing_train$SalePrice >= 501000, "Yes", "No")
grupo1_train <- data.frame(housing_train, grupo1)
grupo2_train <- data.frame(housing_train, grupo2)
grupo3_train <- data.frame(housing_train, grupo3)
summary(grupo1_train)
summary(grupo2_train)
summary(grupo3_train)
#Construccion del aarbol de Clasificacion
tree.1 <- tree(grupo1 ~ .-SalePrice, grupo1_train)
tree.2 <- tree(grupo2 ~ .-SalePrice, grupo2_train)
tree.3 <- tree(grupo3 ~ .-SalePrice, grupo3_train)
#El indice que toma como principal es el barrio
plot(tree.1)
text(tree.1, pretty = 0)
#El indice que toma como principal es el barrio
plot(tree.2)
text(tree.2, pretty = 0)
#El indice que toma como principal es el material general y calidad de acabado
plot(tree.3)
text(tree.3, pretty = 0)
#Evaluar los conjuntos de train con test tomando la mitad de los datos 
train1 <- sample(1:nrow(grupo1_train),730)
test1 <- grupo1_train[-train1,]
grupo1.test <- grupo1[-train1]
View(grupo1.test)
train2 <- sample(1:nrow(grupo2_train),730)
test2 <- grupo1_train[-train2,]
grupo2.test <- grupo2[-train2]
View(grupo2.test)
train3 <- sample(1:nrow(grupo3_train),730)
test3 <- grupo3_train[-train3,]
grupo3.test <- grupo3[-train3]
View(grupo3.test)
#creamor un arbol podado 
tree.11 <- tree(grupo1 ~ .-SalePrice, grupo1_train, subset = train1 )
plot(tree.11)
text(tree.11, pretty = 0)
tree.22 <- tree(grupo2 ~ .-SalePrice, grupo2_train, subset = train2 )
plot(tree.22)
text(tree.22, pretty = 0)
tree.33 <- tree(grupo3 ~ .-SalePrice, grupo3_train, subset = train3 )
plot(tree.33)
text(tree.33, pretty = 0)
tree.pred1 <- predict(tree.11, test1, type = "class")
table(tree.pred1, grupo1.test)
prop.table(table(tree.pred1, grupo1.test),margin = 2)
tree.pred2 <- predict(tree.22, test2, type = "class")
table(tree.pred2, grupo2.test)
prop.table(table(tree.pred2, grupo2.test),margin = 2)
tree.pred3 <- predict(tree.33, test3, type = "class")
table(tree.pred3, grupo3.test)
prop.table(table(tree.pred3, grupo3.test),margin = 2)
#Precision
Precision1 <- ((643+35)/730)
Precision1
Precision2 <- ((35+623)/730)
Precision2
Precision3 <- ((723+0)/730)
Precision3
#podamos el arbol paara ver si emjora la precision 
cv.grupo1_train <- cv.tree(tree.11, FUN = prune.misclass)
names(cv.grupo1_train)
cv.grupo1_train
par(mfrow = c(1,2))
plot(cv.grupo1_train$size, cv.grupo1_train$dev, type = "b")
plot(cv.grupo1_train$k, cv.grupo1_train$dev, type = "b")

cv.grupo2_train <- cv.tree(tree.22, FUN = prune.misclass)
names(cv.grupo2_train)
cv.grupo2_train
par(mfrow = c(1,2))
plot(cv.grupo2_train$size, cv.grupo2_train$dev, type = "b")
plot(cv.grupo2_train$k, cv.grupo2_train$dev, type = "b")

cv.grupo3_train <- cv.tree(tree.33, FUN = prune.misclass)
names(cv.grupo3_train)
cv.grupo3_train
par(mfrow = c(1,2))
plot(cv.grupo3_train$size, cv.grupo3_train$dev, type = "b")
plot(cv.grupo3_train$k, cv.grupo3_train$dev, type = "b")
#podamos el arbol para obtenre con 5 nodos 
prune.grupo1_train <- prune.misclass(tree.11, best = 5)
par(mfrow = c(1,1))
plot(prune.grupo1_train)
text(prune.grupo1_train, pretty = 0)
prune.grupo2_train <- prune.misclass(tree.22, best = 5)
par(mfrow = c(1,1))
plot(prune.grupo2_train)
text(prune.grupo2_train, pretty = 0)
prune.grupo3_train <- prune.misclass(tree.33, best = 2)
par(mfrow = c(1,1))
plot(prune.grupo3_train)
text(prune.grupo3_train, pretty = 0)
# en el caso del tercer grupo no podemos tomar 5 folds porque 
#los datos positivos son pequenos en compracion con los casos negativos 
# y al tomar la mitad de los datos estamos tomadno solo negativos

#comportamiento del arbol con el test data
tree.pred1 <- predict(prune.grupo1_train, test1, type = "class")
table(tree.pred1, grupo1.test)
prop.table(table(tree.pred1, grupo1.test),margin = 2)
Precision1 <- ((643+35)/730)
Precision1
tree.pred2 <- predict(prune.grupo2_train, test2, type = "class")
table(tree.pred2, grupo2.test)
prop.table(table(tree.pred2, grupo2.test),margin = 2)
Precision2 <- ((35+623)/730)
Precision2
tree.pred3 <- predict(prune.grupo3_train, test3, type = "class")
table(tree.pred3, grupo3.test)
prop.table(table(tree.pred3, grupo3.test),margin = 2)
Precision3 <- ((723+0)/730)
Precision3


#RANDOM FOREST REGRESION
install.packages("randomForest", dependencies = T)
install.packages("gmodels", dependencies = T)
library(randomForest)
library(gmodels)
housing_train <- read.csv("Desktop/Actividad 1/housing_train.csv")
fac <- read.csv("Desktop/Actividad 1/housing_train.csv")
int <- read.csv("Desktop/Actividad 1/housing_train.csv")
#obtenemos valor entre 0 y 1 de la columnas que son de tipo factor
colm.factor <- sapply(housing_train, function(x) sum(is.factor(x)))
valor <- 0
#borramos las columnas de tipo factor
fac.borrar <- which((colm.factor != valor))
int.borrar <- which((colm.factor == valor))
fac[,fac.borrar] <- NULL
int[,int.borrar] <- NULL
summary(int)
summary(fac)
#asignamos valor de 0 a los NA
cero.na=function(x){
  ifelse(is.na(x),0,x)}
fac = data.frame(sapply(fac,cero.na))
int=as.matrix(int)
int[is.na(int)] <-"None"
int=as.data.frame(int)
housing_train <- data.frame(int,fac)
summary(housing_train)
str(housing_train)
grupo1 <- housing_train
grupo1.rand <- grupo1[order(runif(1460)),]
grupo1.train <- grupo1.rand[1:1000,]
grupo1.test <- grupo1.rand[1001:1460,]
prop.table(table(grupo1.train$SalePrice))
prop.table(table(grupo1.test$SalePrice))

rf.model <- randomForest(x = grupo1.train[, -81],
                         y = grupo1.train$SalePrice,
                         data = grupo1.train,
                         ntree = 500,
                         do.trace = T
)
rf.model
plot(rf.model)
#el error disminuye a medida que se incrementan los arboles 
varImpPlot(rf.model)
#observamos la importancia de atributos entre la cual destcaa 
#la calidad en general de los acabados 
rf.predict <- predict(rf.model, grupo1.test)
head(rf.predict)
head(grupo1.test$SalePrice)
prop.table(table(grupo1.train$SalePrice))
set.seed(1234)
grupo1.train.bal <- rbind(grupo1.train)
rf.model.2 <- randomForest(SalePrice ~., data = grupo1.train.bal, ntree=500,
                           do.trace = T)
plot(rf.model.2)
varImpPlot(rf.model.2)
rf.predict.2 <- predict(rf.model.2, grupo1.test)
set.seed(1234)
grupo1.train.bal <- rbind(grupo1.train)
rf.model.3 <- randomForest(SalePrice ~., data = grupo1.train.bal, ntree=500,
                           do.trace = T)
grupo1.train.bal <- rbind(grupo1.train)
rf.model.4 <- randomForest(SalePrice ~., data = grupo1.train.bal, ntree=500,
                           do.trace = T)
grupo1.train.bal <- rbind(grupo1.train)
rf.model.5 <- randomForest(SalePrice ~., data = grupo1.train.bal, ntree=500,
                           do.trace = T)

plot(rf.model.3)
plot(rf.model.4)
plot(rf.model.5)
varImpPlot(rf.model.3)
varImpPlot(rf.model.4)
varImpPlot(rf.model.5)
rf.model.2
rf.model.3
rf.model.4
rf.model.5



#RANDOM FOREST CLASIFICACION
install.packages("randomForest", dependencies = T)
install.packages("gmodels", dependencies = T)
library(randomForest)
library(gmodels)
housing_train <- read.csv("housing_train.csv")
fac <- read.csv("housing_train.csv")
int <- read.csv("housing_train.csv")
#obtenemos valor entre 0 y 1 de la columnas que son de tipo factor
colm.factor <- sapply(housing_train, function(x) sum(is.factor(x)))
valor <- 0
#borramos las columnas de tipo factor
fac.borrar <- which((colm.factor != valor))
int.borrar <- which((colm.factor == valor))
fac[,fac.borrar] <- NULL
int[,int.borrar] <- NULL
summary(int)
summary(fac)
#asignamos valor de 0 a los NA
cero.na=function(x){
  ifelse(is.na(x),0,x)}
fac = data.frame(sapply(fac,cero.na))
int=as.matrix(int)
int[is.na(int)] <-"None"
int=as.data.frame(int)
housing_train <- data.frame(int,fac)
summary(housing_train)
str(housing_train)
grupo1 <- housing_train
grupo2 <- housing_train
grupo3 <- housing_train
grupo1$SalePrice <- ifelse(grupo1$SalePrice <= 100000, 1, 0)
grupo2$SalePrice <- ifelse(grupo2$SalePrice >= 101000  & grupo2$SalePrice <= 500000, 1, 0) 
grupo3$SalePrice <- ifelse(grupo3$SalePrice >= 500000, 1, 0) 
str(grupo1)
str(grupo2)
str(grupo3)
grupo1.rand <- grupo1[order(runif(1460)),]
grupo1.train <- grupo1.rand[1:1000,]
grupo1.test <- grupo1.rand[1001:1460,]
grupo2.rand <- grupo2[order(runif(1460)),]
grupo2.train <- grupo2.rand[1:1000,]
grupo2.test <- grupo2.rand[1001:1460,]
grupo3.rand <- grupo3[order(runif(1460)),]
grupo3.train <- grupo3.rand[1:1000,]
grupo3.test <- grupo3.rand[1001:1460,]

prop.table(table(grupo1.train$SalePrice))
prop.table(table(grupo1.test$SalePrice))
prop.table(table(grupo2.train$SalePrice))
prop.table(table(grupo2.test$SalePrice))
prop.table(table(grupo3.train$SalePrice))
prop.table(table(grupo3.test$SalePrice))

grupo1.train$SalePrice <- as.factor(grupo1.train$SalePrice)
grupo2.train$SalePrice <- as.factor(grupo2.train$SalePrice)
grupo3.train$SalePrice <- as.factor(grupo3.train$SalePrice)

rf.model1 <- randomForest(x = grupo1.train[, -81],
                         y = grupo1.train$SalePrice,
                         data = grupo1.train,
                         ntree = 500,
                         do.trace = T
                         )
rf.model1
plot(rf.model1)
varImpPlot(rf.model1)
rf.model2 <- randomForest(x = grupo2.train[, -81],
                          y = grupo2.train$SalePrice,
                          data = grupo1.train,
                          ntree = 500,
                          do.trace = T
)
rf.model2
plot(rf.model2)
varImpPlot(rf.model2)
rf.model3 <- randomForest(x = grupo3.train[, -81],
                          y = grupo3.train$SalePrice,
                          data = grupo1.train,
                          ntree = 500,
                          do.trace = T
)
rf.model3
plot(rf.model3)
varImpPlot(rf.model3)

rf.predict1 <- predict(rf.model1, grupo1.test)
head(rf.predict1)
head(grupo1.test$SalePrice)
rf.predict2 <- predict(rf.model2, grupo2.test)
head(rf.predict2)
head(grupo2.test$SalePrice)
rf.predict3 <- predict(rf.model3, grupo3.test)
head(rf.predict3)
head(grupo3.test$SalePrice)

CrossTable(grupo1.test$SalePrice, rf.predict1, prop.chisq = F, 
           prop.c = F, prop.r = T, prop.t = F,
           dnn = c("actual SalePrice", "predict SalePrice"))
CrossTable(grupo2.test$SalePrice, rf.predict2, prop.chisq = F, 
           prop.c = F, prop.r = T, prop.t = F,
           dnn = c("actual SalePrice", "predict SalePrice"))
CrossTable(grupo3.test$SalePrice, rf.predict3, prop.chisq = F, 
           prop.c = F, prop.r = T, prop.t = F,
           dnn = c("actual SalePrice", "predict SalePrice"))


prop.table(table(grupo1.train$SalePrice))
prop.table(table(grupo2.train$SalePrice))
prop.table(table(grupo3.train$SalePrice))

grupo1.train.pos <- grupo1.train[grupo1.train$SalePrice==1,]
grupo1.train.neg <- grupo1.train[grupo1.train$SalePrice==0,]
grupo2.train.pos <- grupo2.train[grupo2.train$SalePrice==1,]
grupo2.train.neg <- grupo2.train[grupo2.train$SalePrice==0,]
grupo3.train.pos <- grupo3.train[grupo3.train$SalePrice==1,]
grupo3.train.neg <- grupo3.train[grupo3.train$SalePrice==0,]

set.seed(1234)
grupo1.train.neg.bal <- grupo1.train.neg[sample(1:nrow(grupo1.train.neg),
                                                nrow(grupo1.train.neg)),]
grupo2.train.neg.bal <- grupo2.train.neg[sample(1:nrow(grupo2.train.neg),
                                                nrow(grupo2.train.neg)),]
grupo3.train.neg.bal <- grupo3.train.neg[sample(1:nrow(grupo3.train.neg),
                                                nrow(grupo3.train.neg)),]

grupo1.train.bal <- rbind(grupo1.train.pos, grupo1.train.neg.bal)
grupo2.train.bal <- rbind(grupo2.train.pos, grupo2.train.neg.bal)
grupo3.train.bal <- rbind(grupo3.train.pos, grupo3.train.neg.bal)

rf.model11 <- randomForest(SalePrice ~., data = grupo1.train.bal, ntree=500,
                           do.trace = T)
rf.model22 <- randomForest(SalePrice ~., data = grupo2.train.bal, ntree=500,
                           do.trace = T)
rf.model33 <- randomForest(SalePrice ~., data = grupo3.train.bal, ntree=500,
                           do.trace = T)
plot(rf.model11)
plot(rf.model22)
plot(rf.model33)

varImpPlot(rf.model11)
varImpPlot(rf.model22)
varImpPlot(rf.model33)

rf.predict11 <- predict(rf.model11, grupo1.test)
rf.predict22 <- predict(rf.model22, grupo2.test)
rf.predict33 <- predict(rf.model33, grupo3.test)

CrossTable(grupo1.test$SalePrice, rf.predict11, prop.chisq = F, 
           prop.c = F, prop.r = T, prop.t = F,
           dnn = c("actual SalePrice", "predict SalePrice"))
CrossTable(grupo2.test$SalePrice, rf.predict22, prop.chisq = F, 
           prop.c = F, prop.r = T, prop.t = F,
           dnn = c("actual SalePrice", "predict SalePrice"))
CrossTable(grupo3.test$SalePrice, rf.predict33, prop.chisq = F, 
           prop.c = F, prop.r = T, prop.t = F,
           dnn = c("actual SalePrice", "predict SalePrice"))
 
set.seed(1234)

grupo1.train.neg.bal <- grupo1.train.neg[sample(1:nrow(grupo1.train.neg),
                                                nrow(grupo1.train.neg)),]
grupo2.train.neg.bal <- grupo2.train.neg[sample(1:nrow(grupo2.train.neg),
                                                nrow(grupo2.train.neg)),]
grupo3.train.neg.bal <- grupo3.train.neg[sample(1:nrow(grupo3.train.neg),
                                                nrow(grupo3.train.neg)),]

grupo1.train.bal <- rbind(grupo1.train.pos, grupo1.train.neg.bal)
rf.model111 <- randomForest(SalePrice ~., data = grupo1.train.bal, ntree=500,
                            do.trace = T)
grupo1.train.bal <- rbind(grupo1.train.pos, grupo1.train.neg.bal)
rf.model1111 <- randomForest(SalePrice ~., data = grupo1.train.bal, ntree=500,
                           do.trace = T)
grupo1.train.bal <- rbind(grupo1.train.pos, grupo1.train.neg.bal)
rf.model11111 <- randomForest(SalePrice ~., data = grupo1.train.bal, ntree=500,
                           do.trace = T)

grupo2.train.bal <- rbind(grupo2.train.pos, grupo2.train.neg.bal)
rf.model222 <- randomForest(SalePrice ~., data = grupo2.train.bal, ntree=500,
                            do.trace = T)
grupo2.train.bal <- rbind(grupo2.train.pos, grupo2.train.neg.bal)
rf.model2222 <- randomForest(SalePrice ~., data = grupo2.train.bal, ntree=500,
                            do.trace = T)
grupo2.train.bal <- rbind(grupo2.train.pos, grupo2.train.neg.bal)
rf.model22222 <- randomForest(SalePrice ~., data = grupo2.train.bal, ntree=500,
                            do.trace = T)

grupo3.train.bal <- rbind(grupo3.train.pos, grupo3.train.neg.bal)
rf.model333 <- randomForest(SalePrice ~., data = grupo3.train.bal, ntree=500,
                            do.trace = T)
grupo3.train.bal <- rbind(grupo3.train.pos, grupo3.train.neg.bal)
rf.model3333 <- randomForest(SalePrice ~., data = grupo3.train.bal, ntree=500,
                            do.trace = T)
grupo3.train.bal <- rbind(grupo3.train.pos, grupo3.train.neg.bal)
rf.model33333 <- randomForest(SalePrice ~., data = grupo3.train.bal, ntree=500,
                            do.trace = T)

  
plot(rf.model111)
plot(rf.model1111)
plot(rf.model11111)
plot(rf.model222)
plot(rf.model2222)
plot(rf.model22222)
plot(rf.model333)
plot(rf.model3333)
plot(rf.model33333)

varImpPlot(rf.model111)
varImpPlot(rf.model1111)
varImpPlot(rf.model11111)
varImpPlot(rf.model222)
varImpPlot(rf.model2222)
varImpPlot(rf.model22222)
varImpPlot(rf.model333)
varImpPlot(rf.model3333)
varImpPlot(rf.model33333)

rf.model.all1 <- combine(rf.model111,rf.model1111,rf.model11111)
rf.model.all2 <- combine(rf.model222,rf.model2222,rf.model22222)
rf.model.all3 <- combine(rf.model333,rf.model3333,rf.model33333)
 
rf.predict.all1 <- predict(rf.model.all1, grupo1.test, )
rf.predict.all2 <- predict(rf.model.all2, grupo2.test, )
rf.predict.all3 <- predict(rf.model.all3, grupo3.test, )

CrossTable(grupo1.test$SalePrice, rf.predict.all1, prop.chisq = F, 
            prop.c = F, prop.r = T, prop.t = F,
            dnn = c("actual SalePrice", "predict SalePrice"))
CrossTable(grupo2.test$SalePrice, rf.predict.all2, prop.chisq = F, 
           prop.c = F, prop.r = T, prop.t = F,
           dnn = c("actual SalePrice", "predict SalePrice"))
CrossTable(grupo3.test$SalePrice, rf.predict.all3, prop.chisq = F, 
           prop.c = F, prop.r = T, prop.t = F,
           dnn = c("actual SalePrice", "predict SalePrice"))

 








