#---------------------------------Cargue de archivos
library(readr)
saber_2016<-read_delim('C:/Taller_1_E2/SBPRO_Genericas_2016.txt',"|", escape_double = FALSE, trim_ws = TRUE)
saber_2017<-read_delim('C:/Taller_1_E2/SBPRO_Genericas_2017.txt',"|", escape_double = FALSE, trim_ws = TRUE)

##Selección de columnas que pueden ser estudiadas 
sp_2016<-data.frame(saber_2016[,c(1:4,6,9,11,19,20:23,53,55,56,57,63,76:98)])
sp_2017<-data.frame(saber_2017[,c(1:4,6,10,12,20,22:25,58,60:62,68,81:100,103,104,106)])

##Unión de datos en un solo datagrid
Pruebas<-rbind(sp_2017,sp_2016)


# ---------------------------------- Objetivos del trabajo ----------------------------
# ------- 1. Analizar como influye el estrato socioeconómico (ligado al pago de matrícula) a las diferente
#            Areas de estudio (Cuantitativo, Lectura, Ciudadanas, Inglés y com. Escrita)

# ------- 2. Analizar como influyen las horas de trabajo del estudiante en su rendimiento 
#            en las diferentes áreas

#------------------------------------------  Muestreo   ----------------------------------

# Revisión de la estructura de pago de matrícula y horas de trabajo
install.packages('magrittr')
install.packages('dplyr')

library(magrittr) # Contiene la función select
library(dplyr) # Permite leer la función %>%

Estratos<- Pruebas %>%
  select(estu_valormatriculauniversidad,mod_ingles_punt) %>%
  group_by(estu_valormatriculauniversidad) %>%
  summarise(n=n(),
            s=sd(mod_ingles_punt)) %>%
  routn(p=n/sum(n))

hrsTrabajo<- Pruebas %>%
  select(estu_horassemanatrabaja,mod_ingles_punt) %>%
  group_by(estu_horassemanatrabaja) %>%
  summarise(n=n(),
            s=sd(mod_ingles_punt)) %>%
  mutate(p=n/sum(n))

Estratos
hrsTrabajo


##-- Estimación del tamaño de muestra
# 1. Estimar media, varianza, desviación, confianza
# Confianza : 95%
# cuantil de distribución : 1.96

dataC1ingles <- data.frame(hrstrabajo=Pruebas$estu_horassemanatrabaja, vlrMatricula=Pruebas$estu_valormatriculauniversidad, ptje=Pruebas$mod_ingles_punt)
dataC1Lectura <- data.frame(hrstrabajo=Pruebas$estu_horassemanatrabaja, vlrMatricula=Pruebas$estu_valormatriculauniversidad, ptje=Pruebas$mod_lectura_critica_punt)
dataC1ciudadana <- data.frame(hrstrabajo=Pruebas$estu_horassemanatrabaja, vlrMatricula=Pruebas$estu_valormatriculauniversidad, ptje=Pruebas$mod_competen_ciudada_punt)
dataC1Cuantitativo <- data.frame(hrstrabajo=Pruebas$estu_horassemanatrabaja, vlrMatricula=Pruebas$estu_valormatriculauniversidad, ptje=Pruebas$mod_razona_cuantitat_punt)
dataC1Escrita <- data.frame(hrstrabajo=Pruebas$estu_horassemanatrabaja, vlrMatricula=Pruebas$estu_valormatriculauniversidad, ptje=Pruebas$mod_comuni_escrita_punt)


#---- Determinación de la media, desviació, Simetria y Kurtoise para las poblaciones
mystats <- function(x, na.omit=TRUE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c("tamaño"=n, "media"=m, "desviación estándar"=s, "simetría"=skew, "kurtosis"=kurt))
}

popIngles<-round(mystats(dataC1ingles[,'ptje']),1)
popLectura<-round(mystats(dataC1Lectura[,'ptje']),1)
popciudadana<-round(mystats(dataC1ciudadana[,'ptje']),1)
popCuantitativo<-round(mystats(dataC1Cuantitativo[,'ptje']),1)
popEscrita<-round(mystats(dataC1Escrita[,'ptje']),1)
data.frame(popIngles,popLectura,popciudadana,popCuantitativo,popEscrita)



#--- Gráficas de distribución 
par(mfrow=c(1,2))
with(dataC1ingles, hist(ptje, nclass = 100, main = 'POB- Inglés'))
with(dataC1Lectura, hist(ptje, nclass = 100, main = 'POB- Lectura'))
par(mfrow=c(1,2))
with(dataC1ciudadana, hist(ptje, nclass = 100, main = 'POB- Ciudadanas'))
with(dataC1Cuantitativo, hist(ptje, nclass = 100, main = 'POB- Cuantitativa'))
par(mfrow=c(1,1))
with(dataC1Escrita, hist(ptje, nclass = 100, main = 'POB- Escrita'))


##------Muestreo
install.packages('samplingbook')
library(samplingbook)
##Para determinar el tamaño de la muestra inicialmente se trabajó con un error a la media del 10%
## y luego del 5%, pero con un error del 2.5% la muestra es mas confiable
#Tamaño de la muestra para Inglés
nmIngles <- sample.size.mean(e=3.755, S=31.8, level = 0.95, N = nrow(Pruebas))
#Tamaño de la muestra para Lectura
nmLectura <- sample.size.mean(e=3.7525, S=31, level = 0.95, N = nrow(Pruebas))
#Tamaño de la muestra para Ciudadanas
nmCiudadanas <- sample.size.mean(e=3.65, S=32, level = 0.95, N = nrow(Pruebas))
#Tamaño de la muestra para Cuantitativo
nmCuantitativo <- sample.size.mean(e=3.7325, S=30.5, level = 0.95, N = nrow(Pruebas))
#Tamaño de la muestra para Escrita
nmEscrita <- sample.size.mean(e=3.7625, S=31.4, level = 0.95, N = nrow(Pruebas))


nmIngles
nmEscrita


library('SamplingUtil')
library(magrittr) # Contiene la función select
library(dplyr) # Permite leer la función %>%


##--- Análisis para inglés 
hrsTrabajo
Estratos
nmIngles

nsizePropIng<-nstrata(n=276,wh=Estratos[,4],method="proportional")
mtrIngles <- data.frame(Estratos$vlrMatricula,nsizePropIng)

nsizePropIng<-nstrata(n=276,wh=hrsTrabajo[,4],method="proportional")
mtr2Ingles <- data.frame(hrsTrabajo$estu_horassemanatrabaja,nsizePropIng)

mtrInglesData <- sample(1:nrow(dataC1ingles),size=276,replace=FALSE)
mtrInglesData <- dataC1ingles[mtrInglesData,]

mtrIngles2Data <- sample(1:nrow(dataC1ingles),size=276,replace=FALSE)
mtrIngles2Data <- dataC1ingles[mtrIngles2Data,]

par(mfrow=c(1,3))
with(dataC1ingles, hist(ptje, nclass = 100, main = 'POB- Inglés'))
with(mtrInglesData, hist(ptje, nclass = 50, main = 'MTRA 1 - Inglés'))
with(mtrIngles2Data, hist(ptje, nclass = 50, main = 'MTRA 2 - Inglés'))

install.packages('ggplot2')
library(ggplot2)

ggplot(data = dataC1ingles, aes(x = vlrMatricula, y = ptje)) + geom_boxplot() + theme_test() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 5, angle = 45),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 5),
        legend.position = 'none')

ggplot(data = mtrInglesData, aes(x = vlrMatricula, y = ptje)) + geom_boxplot() + theme_test() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 5,angle = 45),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 5),
        legend.position = 'none')


ggplot(data = dataC1ingles, aes(x = hrstrabajo, y = ptje)) + geom_boxplot() + theme_test() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 5,angle = 45),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 5),
        legend.position = 'none')


ggplot(data = mtrIngles2Data, aes(x = hrstrabajo, y = ptje)) + geom_boxplot() + theme_test() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 5,angle = 45),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 5),
        legend.position = 'none')

ptjeInglesMtr <- mtrInglesData$ptje
n <- length(ptjeInglesMtr)
varmediaIngMtra <- (1/(n^2)) * sum((ptjeInglesMtr - mean(ptjeInglesMtr))^2)
desvmediaIngMtra <- sqrt(varmediaIngMtra)


varmediaIngMtra
desvmediaIngMtra

# --- Cálculo de sesgo 

# Remuestreo
B <- 10000
media <- numeric(B)
mediana <- numeric(B)
for (k in 1:B) {
  remuestra <- sample(ptjeInglesMtr, n, replace = TRUE)
  media[k] <- mean(remuestra)
  mediana[k] <- median(remuestra)
}

sesgomediaboot <- mean(media) - mean(ptjeInglesMtr)
sesgomedianaboot <- mean(mediana) - ptjeInglesMtr[276]

sesgomediaboot
sesgomedianaboot # se debe empliar la media y no la mediana


# --- Cálculo de Consistencia 


muestra_1<- sample(1:nrow(dataC1ingles),size=100,replace=FALSE)
m_dataC1ingles_1 <- dataC1ingles[muestra_1,]

muestra_2<- sample(1:nrow(dataC1ingles),size=1000,replace=FALSE)
m_dataC1ingles_2 <- dataC1ingles[muestra_2,]

muestra_3<- sample(1:nrow(dataC1ingles),size=10000,replace=FALSE)
m_dataC1ingles_3 <- dataC1ingles[muestra_3,]

library(ggplot2)
multiplot(
  ggplot(data = dataC1ingles, aes(x = ptje)) + geom_histogram(bins = 100) + theme_test() + ggtitle('Población') +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.position = 'none'),
  ggplot(data = m_dataC1ingles_1, aes(x = ptje)) + geom_histogram(bins = 50) + theme_test() + ggtitle('n = 100') +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.position = 'none'),
  ggplot(data = m_dataC1ingles_2, aes(x = ptje)) + geom_histogram(bins = 50) + theme_test() + ggtitle('n = 1000') +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.position = 'none'),
  ggplot(data = m_dataC1ingles_3, aes(x = ptje)) + geom_histogram(bins = 100) + theme_test() + ggtitle('n = 10000') +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.position = 'none'),
  cols = 2)





