getOption("repos")
library(knitr)
library(plotly)
library(ggplot2)
library(ggbiplot)

dir <- "C:/Users/eldher.hernandez/Documents/promidat"

setwd(dir)

# 1. Explique en que consisten los datos
# R: Los datos fueron extraidos de la UCI "https://archive.ics.uci.edu/ml/datasets/seeds". Es un dataset que proviene de un estudio en campos experimentales, investigados en el Instituto de Agrociencias de la Academia Polaca de Ciencias. Cuenta con 210 observaciones con medidas geometricas de tres tipos de semillas de trigo: Kama, Rosa y Canadiense tomadas usando una técnica de Rayos X. Esta formado por 7 variables reales continuas, entre las cuales:

#Area, perimetro, compacidad, largo del grano, ancho del grano, coeficiente de asimetria, largo de la muesca del grano y tipo

plot(datos)

datos <- read.csv("UCI_clasificar_trigo.csv")

datos$tipo_grano[datos$tipo_grano == 1] <- "Kama"
datos$tipo_grano[datos$tipo_grano == 2] <- "Rosa"
datos$tipo_grano[datos$tipo_grano == 3] <- "Canadiense"



## 2. Relacion entre tres variables
## R: Al ser medidas de la forma geométrica del grano, al aumentar el ancho o el largo, aumenta el área y el largo de la muesca, y por ende el perímetro. Se observa una relación lineal entre el área del grano y largo de la muesca.


## AREA VS RANURA VS PERIMETRO
g <- ggplot(datos, aes(datos$area, datos$largo_ranura, color = datos$perimetro))
g <- g + geom_point(size = 2) + geom_count(aes(color = datos$perimetro)) 
g <- g + labs(title = "Relacion entre el área, longitud de muesca y perímetro por tipo de grano", x="Área del Grano (mm^2)", y="Perímetro del Grano (mm)" ) +  facet_wrap(~tipo_grano) 
(gg <- ggplotly(p =g, autosize = T) )
g


# La compacidad es una variable proporcional al área y al perimetro C = 4*pi*A/P^2.

# Area vs Ancho vs Compacidad
g <- ggplot(datos, aes(datos$area, datos$compacidad, color = datos$ancho_grano))
g <- g + geom_point(size = 2) + geom_count(aes(color =datos$ancho_grano)) 
g <- g + labs(title = "Relacion entre el área, ancho y compacidad por tipo de grano", x="Área del Grano (mm^2)", y="Ancho del Grano (mm)" ) +  facet_wrap(~tipo_grano) 
g
(gg <- ggplotly(p =g) )




## 3

cluster <- kmeans(datos[,1:7],3)

g <- ggplot(datos, aes(datos$area, datos$largo_ranura, color = cluster$cluster))
g <- g + geom_point(size = 2) + geom_count(aes(color =cluster$cluster)) 
g
(gg <- ggplotly(p =g) )






## PCA 
granos.pca <- prcomp(datos[,1:7], center = TRUE, scale. = FALSE) 
print(granos.pca)
plot(granos.pca, type="l")
summary(granos.pca)




### PCA BI PLOT

g <- ggbiplot(granos.pca, obs.scale = 3, var.scale = 3, 
              groups = cluster$cluster, ellipse = TRUE, ellipse.prob = .78, 
              circle = TRUE)
g
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
g <- g + geom_count(aes(color = cluster$cluster))
g <- g + labs(title = "Cluster de Médicos por Rx de Arahkor" )
g <- g + theme(legend.position = "right", legend.direction = "vertical")
g <- g + scale_colour_manual(values=cbPalette)
print(g)
