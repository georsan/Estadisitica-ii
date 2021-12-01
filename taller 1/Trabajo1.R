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
 


datos <-  saber_pro %>% select(CODMPIO,GENERO,PERIODO,LC,IN) %>% filter(substring(saber_pro$CODMPIO,1,2)==68)
datos

#muestreo aleatorio simple
library(samplingbook) # llamamos la librería
# muestra aleatoria simple para los datos LC
MAS <- sample.size.mean(e=0.5, S=sd(datos$LC), N = nrow(datos), level = 0.95)

#muestra 
muestra<- sample(datos$LC,size=MAS$n,replace=FALSE)

muestra

