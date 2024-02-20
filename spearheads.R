#Ejercicio 1. Importar_tabla_de_datos_spearheads_como_data_frame
#Install_readxl
installed.packages("readxl")
library(readxl)
#La ruta no muy larga
spear <- read_excel("C:/spearheads/spearheads.xlsx")
View(spear) ##Muestra una tabla en el editor de registros
str(spear) ##Tipo de datos campos
class(spear) ##Tipo de estructura de datos del objeto

#Convertimos a data frame con la función de as.data.frame(spear)
spear <- as.data.frame(spear)
class(spear)


#Ejercicio 2 renombra_las_variables
#Ahora se cambian los valores a una abreviatura para que resulte más sencillo
#Con el doble igual == se comparan las variables
#Con la función View, se podrá ver el resultado en forma de tabla.
names(spear)[names(spear) == "Mat"] <- "Materiales"
names(spear)[names(spear) == "Con"] <- "Contexto"
names(spear)[names(spear) == "Cond"] <- "Conservacion"
names(spear)[names(spear) == "Loo"] <- "Loop"
names(spear)[names(spear) == "Peg"] <- "Remache"
names(spear)[names(spear) == "Date"] <- "Fecha"
names(spear)[names(spear) == "Maxle"] <- "Longitud_max"
names(spear)[names(spear) == "Socle"] <- "Longitud_encaje"
names(spear)[names(spear) == "Maxwi"] <- "Ancho_max"
names(spear)[names(spear) == "Upsoc"] <- "Ancho_encaje"
names(spear)[names(spear) == "Mawit"] <- "Ancho_max_encaje"
names(spear)[names(spear) == "Weight"] <- "Peso"
spear
View(spear)
#Para renombrar un valor, se debe poner la función "names" y 
#seguir la fórmula que se refleja arriba. En cuanto al doble igual "==", 
#esta es una función lógica que compara el data.frame. 
#Las funciones "names", "==" y "<-" permiten que se pueda modificar el 
#nombre que viene predeterminado en el excell para el dataframe del código.


#Ejercicio 3 asigna_las_etiquetas
spear$Contexto=factor(spear$Contexto, levels=c('1','2','3'), labels=c("s/c", "Habitacional", "Funerario"))
spear$Conservacion=factor(spear$Conservacion, levels=c(1,2,3,4), 
                          labels=c("Excelente", "Bueno", "Regular", "Malo"))
spear$Remache=factor(spear$Remache, levels=c(1,2), labels=c('Si', 'No'))
spear$Materiales=factor(spear$Materiales, levels=c(1,2), labels=c('Bronce', 'Hierro'))
View(spear)
#Ahora se han colocado cuatro etiquetas: Contexto, Conservación, Remache y Materiales. En base a estas cuatro etiquetas,
#se han ido colocando los valores asociados. No hay diferencia entre las dos comillas o la comilla única, algunas veces
#la ciencia es así (Galo, 2024).  


#Ejercicio 4 tablas_de_frecuencias_Materiales_Contextos_Conservación
#Esto se realizará a partir de la función table, que permite generar una tabla con los datos
#del data.set.
#Con la función $ se bloquean únicamente aquellos valores que queramos utilizar.
freq.mat=table(spear$Materiales)
View(freq.mat)
freq.con=table(spear$Contexto)
View(freq.con)
freq.cond=table(spear$Conservacion)
View(freq.cond)


#Ejercicio 5 tablas_cruzadas
#Se vuelve a emplear la función table para volver a generar otra tabla. Sin embargo,
#en esta ocasión se requiere que se generen tablas cruzadas y no de frecuencia.
#Por ello, se elimna la función freq. Además, en este caso se añaden dos valores 
#según la función, y por eso, se pone la función $ entre las dos. 
materiales_contexto <- table(spear$Materiales, spear$Contexto)
View(materiales_contexto)
materiales_conservacion <-table(spear$Materiales, spear$Conservacion)
View(materiales_conservacion)


#Ejercicio 6 tablas_de_porcentajes
#Para realizar tablas de porcentajes, se utiliza la función prop.table. 
#Los datos vuelven a seleccionarse específicamente con la $.
#Para poder obtener el porcentaje, se multiplica por 100 cada función.
procentaje_materiales <- prop.table(table(spear$Materiales)) * 100
View(procentaje_materiales)
procentaje_contexto <- prop.table(table(spear$Contexto)) * 100
View(procentaje_contexto)
procentaje_conservacion <- prop.table(table(spear$Conservacion)) * 100
View(procentaje_conservacion)


#Ejercicio 7 tablas_cruzadas_de_porcentajes
#Se vuelve a utilizar la función prop.table para hacer una tabla de frecuencia,
#en donde se vuelve a integrar la $ para identificar los valores que se quieren mantener.
#Como novedad, introducimos la función magin para especficar la dimensión sobre la 
#cual se va a realizar el calculo del porcentaje. 
porcentaje_materiales_contexto <- prop.table(table(spear$Materiales, 
                                                   spear$Contexto), margin = 1) * 100
View(porcentaje_materiales_contexto)
porcentaje_materiales_conservacion <- prop.table(table(spear$Materiales, 
                                                       spear$Conservacion), margin = 1) * 100
View(porcentaje_materiales_conservacion)


#Ejercicio 8 gráficos_barras_verticales
#Ahora se utiliza la función barplot para la elaboración de gráficos de barras verticales.
barras_verticales_conservacion <- barplot(table(spear$Conservacion), 
                                          main = "Frecuencia de Conservacion", 
                                          xlab = "Conservación")
barras_verticales_contexto <- barplot(table(spear$Contexto), 
                                      main = "Frecuencia de Contexto", 
                                      xlab = "Contexto")


#Ejercicio 9 gráficos_barras_horizontales
#Ahora se añade la función horiz = True para que los gráficos sean horizontales.
barras_horizontales_materiales <- barplot(table(spear$Materiales), 
                                          horiz = TRUE, main = "Frecuencia de Materiales", 
                                          ylab = "Materiales")
barras_horizontales_remache <- barplot(table(spear$Remache), 
                                       horiz = TRUE, main = "Frecuencia de Remache", 
                                       ylab = "Remache")


#Ejercicio 10 gráficos_barras_agrupados
#Ahora lo que hay que hacer es seleccionar los datos que se quieren agrupar: $
#Empleamos la función de gráficos de barras: barplot
#Incluimos leyenda con los valores otorgados a las categorías. 
barras_material_conservacion <- barplot.default(table(spear$Conservacion, 
                                              spear$Materiales), beside = TRUE, 
                                        legend = TRUE, main = "Conservacion por Material", 
                                        xlab = "Conservacion", col = c("purple", "blue", "pink", "orange"))

#Ejercicio 11 gráfico_sectores_variable_Conservación
#Para ello, se utiliza la función pie. 
sectores_conservacion <- pie(table(spear$Conservacion), main = "Porcentaje de Conservacion")


#Ejercicio 12 histograma_probabilidad_variables
#Para ello, se utiliza la función "hist". 
#Las variables continuas son el ancho máx y la long máx. 
hist(spear$Longitud_max, prob = TRUE, main = "Histograma de Probabilidad de Longitud Max", xlab = "Longitud Max")
hist(spear$Ancho_max, prob = TRUE, main = "Histograma de Probabilidad de Ancho Max", xlab = "Ancho Max")
 




