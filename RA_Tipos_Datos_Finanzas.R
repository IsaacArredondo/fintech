#### Tutorial 1 de Analitica Financiera: Tipos de Datos

library(dygraphs) # para graficasinteractivas
library(xts) # para trabajar con series de tiempo
library(quantmod) # para acceder a los datos de yahoo
library(dplyr) # para operaciones

options(warm = -1)
#------Primero, generemos una función que ayude a simplificar los tipos de datos que deseamos de la fuente de
#------información financiera. En este ejemplo, los datos de Cierre y Volúmenes, que aprenderán del simbolo o ticker
#------del activo y a partir de qué año se consultan:
#------Obtención de Datos:

start<-format(as.Date("2014-01-01"),"%Y-%m-%d")
end<-format(as.Date("2020-07-01"),"%Y-%m-%d")

precios_volumenes <- function(simbolo)
{
  ##Obtener precios stocks de Yahoo Finance
  datos <- getSymbols(simbolo, from=start, to=end, auto.assign = FALSE)
  ##Elimina faltantes
  datos <- na.omit(datos)
  ##mantener columnas con precios cierre y volumenes: columnas 4 y 5
  datos <- datos[,4:5]
  ##Para hacerlo datos accesibles en el global environment:
  assign(simbolo, datos, envir = .GlobalEnv)
}

#-----Llamamos a la funcion para cada stock:
precios_volumenes("AMZN")
precios_volumenes("NFLX")
precios_volumenes("IBM")
precios_volumenes("SPY")

#-----Juntamos los datos y renombramos las columnas
PyV <- merge.xts(AMZN, NFLX, IBM, SPY)
colnames(PyV) <- c("Amazon P.Cierre", "Amazon Vol", "Netflix P.Cierre", "Netflix Vol",
                   "IBM P.Cierre", "IBM Vol", "SP500 P.Cierre", "SP500 Vol")

#------Serie De Tiempo:
# Podemos generar una gráfica interactiva las variables, en este caso de los precios:
Precios <- dygraph(PyV[,c(1,3,5,7)], main = "Precios de Amazon, Netflix, IBM y SP&500") %>%
  dyAxis("y", label = "Precios") %>%
  dyRangeSelector(dateWindow = c("2014-01-01", "2020-07-01")) %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set1"))
Precios

# Podemos ver los 5 ultimos datos redondeando hasta 3 decimales:
round(tail(PyV, n = 5), 3)

#-------------------------------------------------------------------------
# Ejemplo de Panel Data, generamos una list de objetos dygraphs, y para imprimirlos usamos htmltools:
library(dygraphs)
library(htmltools)
dy_graficos <- list(
  dygraphs::dygraph(PyV[,c(1,3,5,7)], main = "Precios de Amazon, Netflix, IBM y SP&500"),
  dygraphs::dygraph(PyV[,c(2,4,6,8)], main = "Volumenes de Amazon, Netflix, IBM y SP&500")
)

# Representamos los objetos dygraphs usando htmltools
htmltools::browsable(htmltools::tagList(dy_graficos))

#------------------------------------------------------------------------
#------Datos  tipo Transversales o Cross Sectional
# Seleccionaremos los datos de AMZN del 2014 y del 2020
# Empecemos seleccionando los años 2014 de AMZN que es a 1era columna
AMZN_2014 <- subset(PyV[,1], index(PyV) >= "2014-01-01" & index(PyV) <= "2014-12-31")
AMZN_2014[c(1:5, nrow(AMZN_2014))]
#Para el año 2020
AMZN_2020 <- subset(PyV[,1], index(PyV) >= "2020-01-01" & index(PyV) <= "2020-12-31")
AMZN_2020[c(1:5, nrow(AMZN_2020))]

# Ahora podemos visualizarlo, elegimos un histograma
par(mfrow=c(2,1))

hist(AMZN_2014, freq = FALSE, col = "yellow", border = "blue", main = "Densidades de los Precios en 2014", xlab = "Precio Cierre")
lines(density(AMZN_2014), lwd = 2, col = 'red')
hist(AMZN_2020, freq = FALSE, col = "green", border = "blue", main = "Densidades de los Precios en 2020", xlab = "Precios Cierr")
lines(density(AMZN_2020), lwd = 2, col = 'red')
