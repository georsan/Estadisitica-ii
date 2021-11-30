#importar datos
load("saber_pro.RData")

#variables
# peridodo:año en que presento el examen
# genero:genero de la persona que precento el examen
#codmpio:mucipio de recidencia del evaluado
#lc:lectura critica
#in:ingles

df <- data.frame(Periodo=character()) 

#clasificar los datos de los evaluados desde el 2012 hasta el 2018 

install.packages('magrittr')
install.packages('dplyr')

library(magrittr) # Contiene la función select
library(dplyr) # Permite leer la función %>%
 


datos <-  saber_pro %>% select(CODMPIO,GENERO,PERIODO,LC,IN) %>% filter(CODMPIO==68001)
datos

#muestreo aleatorio simple
library(samplingbook) # llamamos la librería

MAS <- 

