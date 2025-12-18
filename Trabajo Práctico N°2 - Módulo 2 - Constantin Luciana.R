#EJERCICIO 1: SELECCIÓN DE DATASET --------------------------------------------

# Datset: prioridad_accidentes.csv

#EJERCICIO 2: PREGUNTAS DE INTERÉS -------------------------------------

# 1.¿Cómo se distribuyen los accidentes según el nivel de prioridad? 
# 2.¿Qué tipos de accidentes aparecen con mayor frecuencia?
# 3.¿Cómo se distribuye la cantidad de personas lesionadas por accidente?
# 4.¿Cómo varía la cantidad de accidentes a lo largo del año?
# 5.¿Existen valores atípicos en la cantidad de personas lesionadas por accidente?

#EJERCICIOS 3: CARGA DEL DATASET Y CHEQUEO/LIMPIEZA ---------------

accidentes <- read.csv("prioridad_accidentes.csv", stringsAsFactors = FALSE)

head(accidentes)
View(head(accidentes))

dim(accidentes) #para comprobar la cantidad de filas y columnas 
str(accidentes) #para ver la estructura general (tipos de datos)

colnames(accidentes)

colSums(is.na(accidentes))  #para detectar missing values

sum(duplicated(accidentes)) #para verificar duplicados

#EJERCICIO 4: ANÁLISIS EXPLORATORIO DE DATOS ----------------------------------

table(accidentes$prioridad)            #se analiza la variable prioridad a modo de ejemplo, por tratarse de una variable categórica, central en el dataset y relevante para las preguntas planteadas
prop.table(table(accidentes$prioridad)) 

sapply(accidentes, class) #para verificar tipos de variables

summary(accidentes) #resumen general de las variables para ver estadísticas descriptivas

summary(accidentes$total_lesionados) #se analiza la variable total_lesionados a modo de ejemplo, por tratarse de una variable numérica, relevante para evaluar la posibilidad de valores atípicos

#EJERCICIO 5: TABLAS DE CONTINGENCIAS Y GRÁFICOS ------------------------------

#Pregunta 1: ¿Cómo se distribuyen los accidentes según el nivel de prioridad? 

prioridad_filtrada <- accidentes$prioridad[
    accidentes$prioridad %in% c("ALTA", "MEDIA", "BAJA")
  ]

tabla_prioridad_filtrada <- table(prioridad_filtrada)

porcentajes <- round(100*tabla_prioridad_filtrada / sum (tabla_prioridad_filtrada), 1) 

etiquetas <- paste(names(tabla_prioridad_filtrada),
                   porcentajes, 
                   "%")

pie(tabla_prioridad_filtrada,
    etiquetas,
    main = "Distribución de la prioridad de los accidentes",
    col = c("red","blue", "yellow"))

#Pregunta 2: ¿Qué tipos de accidentes aparecen con mayor frecuencia?

tabla_evento <- table(accidentes$tipo_de_evento)

barplot(tabla_evento,
        main = "Frecuencia de tipos de accidentes",
        xlab = "Tipo de evento",
        ylab = "Cantidad",
        las = 2,
        col = "lightgreen")

#Pregunta 3: ¿Cómo se distribuye la cantidad de personas lesionadas por accidente?

hist(accidentes$total_lesionados,
     main = "Distribución total de lesionados por accidente",
     xlab = "Cantidad de lesionados",
     ylab = "Frecuencia",
     col = "gray")

#Pregunta 4: ¿Cómo varía la cantidad de accidentes a lo largo del año?

mes_filtrado <-accidentes$mes[
  accidentes$mes %in% c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
]

tabla_mes_filtrado <- table(mes_filtrado)

orden_meses <- c("enero", "febrero", "marzo", "abril",
                 "mayo", "junio", "julio", "agosto",
                 "septiembre", "octubre", "noviembre", "diciembre")

tabla_mes_filtrado <- tabla_mes_filtrado[orden_meses]

barplot(tabla_mes_filtrado,
        main = "Cantidad de accidentes por mes",
        xlab = "Mes",
        ylab = "Cantidad",
        las = 2, 
        col = "lightblue")

#Pregunta 5: ¿Existen valores atípicos en la cantidad de personas lesionadas por accidente?

boxplot(accidentes$total_lesionados,
        main = "Distribución del total de lesionados por accidente",
        ylab = "Cantidad de lesionados")

#Pregunta complementaria (para poder trabajar ggplot2): ¿Qué alcaldías concentran mayor cantidad de accidentes?

library(ggplot2)

ggplot(accidentes,aes(x = alcaldia)) +
  geom_bar() +
  labs(title = "Cantidad de accidentes por alcaldía",
       x = "Alcaldía",
       y = "Cantidad de accidentes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

