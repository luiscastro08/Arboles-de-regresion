install.packages("tree", dependencies = T)
library(tree)
set.seed(1)

help(read_csv, package = "readr")
housing_train <- read.csv("Desktop/Actividad 1/housing_train.csv")
View(housing_train)
str(housing_train)
summary(housing_train)
#obtenemos el porcentaje de columans que tienen valores nulos
colMeans(is.na(housing_train))
#asignamos el valor que deseamos eliminar
porcentaje <- .6
#borramos las columnas que tiene un % > 0.6
columnas_a_borrar <- which(colMeans(is.na(housing_train))>porcentaje)
housing_train[,columnas_a_borrar] <- NULL
summary(housing_train)
#dividimo la mitad de los datos de forma aleatoria
train <- sample(1:nrow(housing_train), nrow(housing_train)/2)
#observamos lo que contiene train
train
summary(train)
tree.housing_train <- tree(SalePrice ~ . , housing_train[train, ])
summary(tree.housing_train)
plot(tree.housing_train)
text(tree.housing_train, pretty = 0)
#el precio de la vivienda depende de la calidad general la cual si es mayo de 
#pero el precio de las viviendas mas caras dependen de barrio 
# y del area del jardin que posee la vivienda 

#utilizacion del cvtree() para podar el arbol 
cv.housing_train <- cv.tree(tree.housing_train, K=10)
plot(cv.housing_train$size, cv.housing_train$dev, type = 'b')
