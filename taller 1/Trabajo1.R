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
multiplot(
  ggplot(data = n_datos, aes(x = PERIODO, y = LC)) + geom_boxplot() + theme_test() +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.position = 'none'),cols = 1
 
)

ggplot(data = n_datos, aes(x = PERIODO,y=LC)) + geom_bar(stat="identity") + theme_test() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.position = 'none')

multiplot(
  ggplot(data = n_datos, aes(x = PERIODO, y = IN)) + geom_boxplot() + theme_test() +
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14),
          legend.position = 'none'),cols = 1
  
)

