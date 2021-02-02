########CORRER POR PARTES#############

#TIPOS DE RELACIONES
rm(list=ls()) #Para limpiar el espacio de trabajo

x <- c(1,2,3,4,5,6,7,8,9,10) 
x_c <- c(-10:10) #Para las funciones cuadraticas y cubica es mejor fijar la variable x desde numeros negativos 
                #hasta positivos para poder visualizar por completo la forma que adoptan estas funciones.

#Funciones que van a generar los datos correspondientes a la variable y
lineal_positiva <- function (x) {x+1} 
grafica_a <- lineal_positiva(x)
lineal_negativa <- function(x){-x+10}
grafica_b <- lineal_negativa(x)
cuadratica_positiva <- function(x){x^2+3}
grafica_c <- cuadratica_positiva(x_c)
cuadratica_negativa <- function(x){-x^2+10}
grafica_d <- cuadratica_negativa(x_c)
cubica <- function(x){x^3+1}
grafica_e <- cubica(x_c)

#Código para gráfica con ausencia de relación 
eje_y <- c(3,5,1,7,3,8,9,3,4,5) #Podemos mantener la varible x que ya tenemos
                               #y sólo definimos los valores aleatorios de la variable y 

#Graficando
g <- c(1,2,3,4,5,6) #6 gráficas diferentes
m <- matrix(g, ncol = 3)
m
layout(m) 
nf <- layout(m)
layout.show(nf) #esta función permite visualizar la organización de las gráficas

#options(repr.plot.width = 16, repr.plot.height = 8)
#layout(matrix(3:2,nrow = 2, ncol=3 ))

  #Grafica a - Relación lineal positiva (1)
plot(NULL,xlim=c(0,10),ylim=c(0,11),axes=F,ann=F)
axis(1,pos=0,cex.axis=1.5)
axis(2,pos=0,cex.axis=1.5,las=1)
mtext('x',1,cex=1,line=2)
mtext('y',2,cex=1,line=2)
mtext('a) Relación lineal positiva',3,cex=1,font=1,line=0)
lines(x,grafica_a,lwd=3, col = '#D0D3D4')
points(x, grafica_a, lwd=2,cex=1.5,pch=1)

  #Grafica d Relación cuadratica negativa (2)
plot(NULL,xlim=c(-10,10),ylim=c(-100,20),axes=F,ann=F)
axis(1,pos=0,cex.axis=1.5)
axis(2,pos=0,cex.axis=1.5,las=1)
mtext('x',1,cex=1,line=2)
mtext('y',2,cex=1,line=2)
mtext('d) Relación cuadratica negativa',3,cex=1,font=1,line=0)
lines(x_c,grafica_d,lwd=3,  col = '#D0D3D4')
points(x_c, grafica_d, lwd=2,cex=1.5,pch=1)

  #Grafica b Relación lineal negativa (3)
plot(NULL,xlim=c(0,10),ylim=c(0,10),axes=F,ann=F)
axis(1,pos=0,cex.axis=1.5)
axis(2,pos=0,cex.axis=1.5,las=1)
mtext('x',1,cex=1,line=2)
mtext('y',2,cex=1,line=2)
mtext('b) Relación lineal negativa',3,cex=1,font=1,line=0)
lines(x,grafica_b,lwd=3, col = '#D0D3D4')
points(x, grafica_b, lwd=2,cex=1.5,pch=1)

  #Grafica e Relación cubica (4)
plot(NULL,xlim=c(-10,10),ylim=c(-1010,1010),axes=F,ann=F)
axis(1,pos=0,cex.axis=1.5)
axis(2,pos=0,cex.axis=1.5,las=1)
mtext('x',1,cex=1,line=2)
mtext('y',2,cex=1,line=2)
mtext('e) Relación cubica',3,cex=1,font=1,line=0)
lines(x_c,grafica_e,lwd=3, col = '#D0D3D4')
points(x_c, grafica_e, lwd=2,cex=1.5,pch=1)

  #Grafica c Relación cuadratica positiva (5)
plot(NULL,xlim=c(-10,10),ylim=c(0,110),axes=F,ann=F)
axis(1,pos=0,cex.axis=1.5)
axis(2,pos=0,cex.axis=1.5,las=1)
mtext('x',1,cex=1,line=2)
mtext('y',2,cex=1,line=2)
mtext('c) Relación cuadratica positiva',3,cex=1,font=1,line=0)
lines(x_c,grafica_c,lwd=3, col = '#D0D3D4')
points(x_c, grafica_c, lwd=2,cex=1.5,pch=1)

  #Grafica f Ausencia de relación (6)
plot(NULL,xlim=c(0,10),ylim=c(0,10),axes=F,ann=F)
axis(1,pos=0,cex.axis=1.5)
axis(2,pos=0,cex.axis=1.5,las=1)
mtext('x',1,cex=1,line=2)
mtext('y',2,cex=1,line=2)
mtext('f) Ausencia de Relación',3,cex=1,font=1,line=0)
points(x, eje_y, lwd=2,cex=1.5,pch=1)

###############################################
#DIAGRAMAS DE DISPERSIÓN PARA OBTENER COEFICIENTE

rm(list=ls())

x_a <- c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10) 
y_a <- c(-7,-5,-6,-4,-5,-3,-4,2,-2,1,0,1,-2,-3,2,3,5,5,6,5,8)
x_b <- c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10)
y_b <- c(8,5,6,5,5,3,2,-3,-2,1,0,1,-2,2,-4,-3,-5,-4,-6,-7,-5)


options(repr.plot.width = 16, repr.plot.height = 8)
layout(matrix(1:2,ncol=2))
#Esta es otra forma de poner varios graficos en un mismo espacio

#Gráfica a
plot(NULL,xlim=c(-10,10),ylim=c(-10,10),axes=F,ann=F)
box()
axis(1,pos=0,padj=0.5)
axis(2,pos=0,las=2)
mtext('x',1,cex=1,line=0.5)
mtext('y',2,cex=1,line=0.5)
mtext('a',3,cex=1,font=2,col='#000000')
points(x_a, y_a, lwd=2,cex=1.5,pch=1,bg='#000000',col='#000000')

#Gráfica b
plot(NULL,xlim=c(-10,10),ylim=c(-10,10),axes=F,ann=F)
box()
axis(1,pos=0,padj=0.5)
axis(2,pos=0,las=2)
mtext('x',1,line=0.5,cex=1)
mtext('y',2,line=0.5,cex=1)
mtext('b',3,cex=1,font=2,col='#000000')
points(x_b, y_b, lwd=2,cex=1.5,pch=1,bg='#000000',col='#000000')

  #*Falta programar elipse desde R

###############################################

#PUNTUACIONES DIFERENCIALES
rm(list=ls())

x_a <- c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7,8,9,10) 
y_a <- c(-7,-5,-6,-4,-5,-3,-4,2,-2,1,1,-2,-3,2,3,5,5,6,5,8)


plot(x_a,y_a,main='Diagrama de dispersión y puntos diferenciales',xlab='x',ylab='y',xlim=c(-10,10),ylim=c(-10,10),pch=1)
abline(h=0,lty=2) #Dibuja dos ejes que pasan por cero en X y Y
abline(v=0,lty=2)

###############################################
#CALCULO DE COVARIANZA
rm(list=ls())
x_c <- c(1,1,2,2,2,3,3,4,4,4)
y_c <- c(6,7,19,25,23,37,30,45,47,50)

par(cex.axis=1.5,fg='#000000',col.axis='#000000')
options(repr.plot.width = 12, repr.plot.height = 10) 
# Canvas
plot(NULL,xlim=c(0,4),ylim=c(0,50),axes=F,ann=F)
# Data
points(x_c, y_c, lwd=2,cex=1.5,pch=1,bg='#000000',col='#000000')
axis(1,pos=0,padj=0.5)
axis(2,pos=0,las=2)
mtext('Horas de sueño',1,line=3,cex=1)
mtext('Puntuación total de estrés',2,line=3,cex=1)
mtext('Sueño y estrés',3,cex=1,font=2,col='#000000')


###############################################
#Graficando datos de sueño y estrés
rm(list=ls())
#Leyendo base de datos 
datos <- read.csv('C:/Users/Xochitl Cárdenas/Desktop/JASP/datos.csv')
    #También es posible copiar los datos a la consola

par(cex.axis=1.5,fg='#000000',col.axis='#000000')
options(repr.plot.width = 12, repr.plot.height = 10) 
# Canvas
plot(datos,xlim=c(0,4),ylim=c(0,60),axes=F,ann=F)
# Data
points(datos$sueno, datos$estres, lwd=2,cex=1.5,pch=21,bg='#000000',col='#000000')
axis(1,pos=0,padj=0.5)
axis(2,pos=0,las=2)
mtext('Horas de sueño',1,line=3,cex=1)
mtext('Puntuación total de estrés',2,line=3,cex=1)
mtext('Sueño y estrés',3,cex=1,font=2,col='#000000')



###############################################
###############################################
###############################################
#Gráficas de diapositivas 
rm(list=ls())

x <- c(64,40,30,71,55,31,61,42,57,38)
y <- c(66,79,98,65,79,83,68,80,72,95)


par(cex.axis=1.5,fg='#000000',col.axis='#000000')
options(repr.plot.width = 12, repr.plot.height = 10) 
# Canvas
plot(NULL,xlim=c(0,100),ylim=c(0,100),axes=F,ann=F)
# Data
points(x, y, lwd=2,cex=1.5,pch=1,bg='#000000',col='#000000')
axis(1,pos=0,padj=0.5)
axis(2,pos=0,las=2)
mtext('Estrés',1,line=3,cex=1)
mtext('Memoria',2,line=3,cex=1)



