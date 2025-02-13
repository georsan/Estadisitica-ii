#importar datos
load("saber_pro.RData")

#variables
# peridodo:año en que presento el examen
# genero:genero de la persona que precento el examen
#codmpio:mucipio de recidencia del evaluado
#lc:lectura critica
#in:ingles



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
Mas <- sample.size.mean(e=0.5,S=sd(datos$IN),N=nrow(datos),level = 0.95)

#muestra 
muestra<- sample(1:nrow(datos),size=MAS$n,replace=FALSE)
head(muestra)
n_datos <-datos[muestra,]
head(n_datos)
muestra1=sample(1:nrow(datos),size = Mas$n,replace = FALSE)
head(muestra)
n_datos1 <-datos[muestra1,]
head(n_datos1)
#Graficas
install.packages("devtools")
devtools::install_github("IRkernel/repr")
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# para Redimensionar el espacio gráfico en esta aplicación, no se require en el software
install.packages('repr')
library(repr)
options(repr.plot.width=16, repr.plot.height=8)

install.packages('ggplot2')

library(ggplot2)

  install.packages('psych')
  library(psych)
  
  
n_datos$LCR <- ifelse(n_datos$PERIODO == '2012' |
                        n_datos$PERIODO == '2013' |
                        n_datos$PERIODO == '2014' |
                        n_datos$PERIODO == '2015',
                      rescale(x = n_datos$LC, mean = 150, sd = 30, df = T)[,1],n_datos$LC
                        )


  
  library(ggplot2)
  #multiplot(
  ggplot(data = n_datos, aes(x =(as.character(PERIODO)) , y = LCR)) + 
    geom_boxplot() + theme_test() + labs(x="Periodo")+
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.position = 'none')
  
  #)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
  PROM<-aggregate(LC~PERIODO,data=n_datos,FUN=mean)
  
  PROM$LCR <- ifelse(PROM$PERIODO == '2012' |
                       PROM$PERIODO == '2013' |
                       PROM$PERIODO == '2014' |
                       PROM$PERIODO == '2015',
                        rescale(x = PROM$LC, mean = 150, sd = 30, df = T)[,1],PROM$LC
  )
  
ggplot(data = PROM, aes(x = (as.character(PERIODO)),y=LCR)) + geom_bar(stat="identity") + theme_test() +labs(x="Periodo")+
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = 'none')

ggplot(data = n_datos, aes(x = LCR)) + geom_histogram(bins = 50) + facet_wrap(~GENERO) + theme_test() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = 'none')


n_datos1$INR <- ifelse(n_datos1$PERIODO == '2012' |
                        n_datos1$PERIODO == '2013' |
                        n_datos1$PERIODO == '2014' |
                        n_datos1$PERIODO == '2015',
                      rescale(x = n_datos1$IN, mean = 150, sd = 30, df = T)[,1],n_datos$IN
)






ggplot(data = n_datos1, aes(x =(as.character(PERIODO)) , y = INR)) + 
  geom_boxplot() + theme_test() + labs(x="Periodo")+
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = 'none')

#)

ggplot(data = PROM, aes(x = (as.character(PERIODO)),y=LCR)) + geom_bar(stat="identity") + theme_test() +labs(x="Periodo")+
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = 'none')


PROM1<-aggregate(IN~PERIODO,data=n_datos1,FUN=mean)

PROM1$INR <- ifelse(PROM$PERIODO == '2012' |
                     PROM$PERIODO == '2013' |
                     PROM$PERIODO == '2014' |
                     PROM$PERIODO == '2015',
                   rescale(x = PROM1$IN, mean = 150, sd = 30, df = T)[,1],PROM1$IN
)


ggplot(data = PROM1, aes(x = (as.character(PERIODO)),y=INR)) + geom_bar(stat="identity") + theme_test() +labs(x="Periodo")+
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = 'none')
  
  

ggplot(data = n_datos1, aes(x = INR)) + geom_histogram(bins = 50) + facet_wrap(~GENERO) + theme_test() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = 'none')

#Insesgamiento
#mues <- n_datos$LC

#n <- length(mues)
#varmedia <- (1/(n^2)) * sum((mues - mean(mues))^2)
#desviacion
#desvmedia <- sqrt(varmedia)

#B <- 1e+04
#media <- numeric(B)
#mediana <- numeric(B)
#for (k in 1:B) {
#  remuestra <- sample(mues, n, replace = TRUE)
 # media[k] <- mean(remuestra)
  # remordenada <- sort(remuestra)
  # mediana[k] <- remordenada[8]
  #mediana[k] <- median(remuestra)
#}
#sesgomediaboot <- mean(media) - mean(mues)
#sesgomedianaboot <- mean(mediana) - median(mues)

#muestra_1<- sample(1:nrow(n_datos),size=10,replace=FALSE)
#n_datos_1 <- n_datos[muestra_1,]

#muestra_2<- sample(1:nrow(n_datos),size=100,replace=FALSE)
#n_datos_2 <- n_datos[muestra_2,]

#muestra_3<- sample(1:nrow(n_datos),size=1000,replace=FALSE)
#n_datos_3 <- n_datos[muestra_3,]

#multiplot(
 # ggplot(data = n_datos, aes(x = LCR)) + geom_histogram(bins = 50) + theme_test() + ggtitle('Población') +
#    theme(axis.title.x = element_text(size = 14),
#          axis.text.x = element_text(size = 14),
#          axis.title.y = element_text(size = 14),
#          axis.text.y = element_text(size = 14),
#          strip.text = element_text(size = 14),
#          legend.position = 'none'),
#  ggplot(data = n_datos_1, aes(x = LCR)) + geom_histogram(bins = 50) + theme_test() + ggtitle('n = 10') +
#    theme(axis.title.x = element_text(size = 14),
#          axis.text.x = element_text(size = 14),
#          axis.title.y = element_text(size = 14),
#          axis.text.y = element_text(size = 14),
#          strip.text = element_text(size = 14),
#          legend.position = 'none'),
#  ggplot(data = n_datos_2, aes(x = LCR)) + geom_histogram(bins = 50) + theme_test() + ggtitle('n = 100') +
#    theme(axis.title.x = element_text(size = 14),
#          axis.text.x = element_text(size = 14),
#          axis.title.y = element_text(size = 14),
#          axis.text.y = element_text(size = 14),
#          strip.text = element_text(size = 14),
#          legend.position = 'none'),
#  ggplot(data = n_datos_3, aes(x = LCR)) + geom_histogram(bins = 50) + theme_test() + ggtitle('n = 1000') +
#    theme(axis.title.x = element_text(size = 14),
#          axis.text.x = element_text(size = 14),
#          axis.title.y = element_text(size = 14),
#          axis.text.y = element_text(size = 14),
#          strip.text = element_text(size = 14),
#          legend.position = 'none'),
#  cols = 2)

#Insesgamiento Ingles
#mues <- n_datos$IN

#n <- length(mues)
#varmedia <- (1/(n^2)) * sum((mues - mean(mues))^2)
#desviacion
#desvmedia <- sqrt(varmedia)

#B <- 1e+04
#media <- numeric(B)
#mediana <- numeric(B)
#for (k in 1:B) {
#  remuestra <- sample(mues, n, replace = TRUE)
#  media[k] <- mean(remuestra)
  # remordenada <- sort(remuestra)
  # mediana[k] <- remordenada[8]
 # mediana[k] <- median(remuestra)
#}
#sesgomediaboot <- mean(media) - mean(mues)
#sesgomedianaboot <- mean(mediana) - median(mues)

#muestra_1<- sample(1:nrow(n_datos1),size=10,replace=FALSE)
#n_datos_1 <- n_datos1[muestra_1,]

#muestra_2<- sample(1:nrow(n_datos1),size=100,replace=FALSE)
#n_datos_2 <- n_datos1[muestra_2,]

#muestra_3<- sample(1:nrow(n_datos1),size=1000,replace=FALSE)
#n_datos_3 <- n_datos1[muestra_3,]

#multiplot(
#  ggplot(data = n_datos1, aes(x = INR)) + geom_histogram(bins = 50) + theme_test() + ggtitle('Población') +
#    theme(axis.title.x = element_text(size = 14),
#          axis.text.x = element_text(size = 14),
#          axis.title.y = element_text(size = 14),
#          axis.text.y = element_text(size = 14),
#          strip.text = element_text(size = 14),
#          legend.position = 'none'),
#  ggplot(data = n_datos_1, aes(x = INR)) + geom_histogram(bins = 50) + theme_test() + ggtitle('n = 10') +
#    theme(axis.title.x = element_text(size = 14),
#          axis.text.x = element_text(size = 14),
#          axis.title.y = element_text(size = 14),
#          axis.text.y = element_text(size = 14),
#          strip.text = element_text(size = 14),
#          legend.position = 'none'),
#  ggplot(data = n_datos_2, aes(x = INR)) + geom_histogram(bins = 50) + theme_test() + ggtitle('n = 100') +
#    theme(axis.title.x = element_text(size = 14),
#          axis.text.x = element_text(size = 14),
#          axis.title.y = element_text(size = 14),
#          axis.text.y = element_text(size = 14),
#          strip.text = element_text(size = 14),
#          legend.position = 'none'),
#  ggplot(data = n_datos_3, aes(x = INR)) + geom_histogram(bins = 50) + theme_test() + ggtitle('n = 1000') +
 #   theme(axis.title.x = element_text(size = 14),
  #        axis.text.x = element_text(size = 14),
  #        axis.title.y = element_text(size = 14),
  #        axis.text.y = element_text(size = 14),
  #        strip.text = element_text(size = 14),
  #        legend.position = 'none'),
  #cols = 2)
  #
##########################################################################
#Intervalos de confianza
##########################################################################
#LC
datos$LCR <- ifelse(datos$PERIODO == '2012' |
                     datos$PERIODO == '2013' |
                     datos$PERIODO == '2014' |
                     datos$PERIODO == '2015',
                   rescale(x = datos$LC, mean = 150, sd = 30, df = T)[,1],datos$LC
)
datos$INR <- ifelse(datos$PERIODO == '2012' |
                      datos$PERIODO == '2013' |
                      datos$PERIODO == '2014' |
                      datos$PERIODO == '2015',
                    rescale(x = datos$IN, mean = 150, sd = 30, df = T)[,1],datos$IN
)
# Media Poblacional
Media_poblacional_LC=round(mean(datos$LCR),5)
#media muestral
Media_muestral_LC=round(mean(n_datos$LCR),5)
#varianza
Np<-nrow(datos)
varianzaP<-round(var(datos$LCR)*((Np-1)/Np),5)
#varianza muestral
varianzaM=round(var(n_datos$LCR),5)

#descacion estandar
desviacionE=round(sd(n_datos$LCR),5)

datafLC<-data.frame("MEDIA POBLACIONAL"=Media_poblacional_LC,"MEDIA MUESTRAL"=Media_muestral_LC,"VARIANZA POBLACIONAL"=varianzaP,"VARIANZA MUESTRAL"=varianzaM,"DESVIACION ESTANDAR"=desviacionE)

##########################IN
# Media Poblacional
Media_poblacional_IN=round(mean(datos$INR),5)
#media muestral
Media_muestral_IN=round(mean(n_datos1$INR),5)
#varianza
Np<-nrow(datos)
varianzaP1<-round(var(datos$INR)*((Np-1)/Np),5)
#varianza muestral
varianzaIN=round(var(n_datos1$INR),5)

#descacion estandar
desviacionEIN=round(sd(n_datos1$INR),5)


datafIN<-data.frame("MEDIA POBLACIONAL"=Media_poblacional_IN,"MEDIA MUESTRAL"=Media_muestral_IN,"VARIANZA POBLACIONAL"=varianzaP1,"VARIANZA MUESTRAL"=varianzaIN,"DESVIACION ESTANDAR"=desviacionEIN)


#Intervalo de confianza lc
media <- mean(na.omit(n_datos$LCR)) # Pedimos la media
desv <- sd(na.omit(n_datos$LCR)) # La desviación estándar
N <- length(na.omit(n_datos$LCR)) # El tamaño válido de la muestra
error.est <- desv/sqrt(N) # Calculamos el error estándar
error <- 2.57*error.est # Fijamos Z=2.57 para indicar un nivel de confianza de 99%
lim.inf <- media-error # Límite inferior del intervalo
lim.sup <- media+error # Límite superior del intervalo
resultado1 <- data.frame(media, desv, N, error.est, error, lim.inf, lim.sup)
round(resultado1,1)

#Intervalo de confianza RC
media <- mean(na.omit(n_datos1$INR)) # Pedimos la media
desv <- sd(na.omit(n_datos1$INR)) # La desviación estándar
N <- length(na.omit(n_datos1$INR)) # El tamaño válido de la muestra
error.est <- desv/sqrt(N) # Calculamos el error estándar
error <- 2.57*error.est # Fijamos Z=2.57 para indicar un nivel de confianza de 99%
lim.inf <- media-error # Límite inferior del intervalo
lim.sup <- media+error # Límite superior del intervalo
resultado1 <- data.frame(media, desv, N, error.est, error, lim.inf, lim.sup)
round(resultado1,1)
