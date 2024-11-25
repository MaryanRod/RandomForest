library(tidyverse)
library(haven)
library(randomForest)

MIGRACION <- read_sav("MIGRACION_BDP.sav")
MIGRACION
MIGRACION <- MIGRACION[,c("DEPARTAMENTO","AREA","ID_EMIGRACION","PEI3","PEI4")]

MIGRACION$DEPARTAMENTO <- as.factor(MIGRACION$DEPARTAMENTO)
set.seed(100)
MIGRACION <- MIGRACION[sample(1:nrow(MIGRACION)),]

index <- sample(1:nrow(MIGRACION),0.8*nrow(MIGRACION))

TRAIN <- MIGRACION[index,]
test <- MIGRACION[-index,]

bosque <- randomForest(DEPARTAMENTO ~ AREA+ID_EMIGRACION+PEI3+PEI4,
                       data = TRAIN,
                       ntree = 100,
                       mtry=10
                       )
prueba <- predict(bosque,test)
prueba

dato_nuevo <- data.frame(
  AREA = 2,
  ID_EMIGRACION = 1,
  PEI3 = 1,
  PEI4 = 35
)

prediccion <- predict(bosque, dato_nuevo)
prediccion

dato_nuevo2 <- data.frame(
  AREA = 1,
  ID_EMIGRACION = 1,
  PEI3 = 1,
  PEI4 = 35
)

prediccion2 <- predict(bosque, dato_nuevo2)
prediccion2
