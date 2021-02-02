########CORRER POR PARTES!!!!!!!!#############
####REGRESION LINEAL

###############################################
#DIAGRAMAS DE DISPERSIÓN con medidas de minimos cuadrados [Figura 6]
rm(list=ls())

ansiedad <- c(49,41,31,29,27,24,17,15,14,6) #X
puntaje <- c(2,7,15,21,25,36,41,47,52,57) #Y

x_pa <- c(49,41,31,29,27,24,17,15,6)
y_pa <- c(2,7,15,21,25,36,41,47,57)

x_pb <- c(49,41,31,29,27,24,17,15,14)
y_pb <- c(2,7,15,21,25,36,41,47,52)

#Recta a
B1_a <- -1.431
B0_a <- 66.5
f_x_a <- function(x){
  return(x*B1_a+B0_a)
}
y_a <- f_x_a(ansiedad)

#Recta b
B1_b <- -0.4
B0_b <- 47.09 #mantener
f_x_b <- function(x){
  return(x*B1_b+B0_b)
}
y_b <- f_x_b(ansiedad)


#Graficando
options(repr.plot.width = 16, repr.plot.height = 8)
layout(matrix(1:2,ncol=2))


#Gráfica a
plot(NULL,xlim=c(-10,60),ylim=c(-10,60),axes=F,ann=F)
axis(1,pos=0,cex.axis=1.75,col='#aaaaaa',col.axis='#aaaaaa')
axis(2,pos=0,las=2,cex.axis=1.75,col='#aaaaaa',col.axis='#aaaaaa')
mtext('Ansiedad',1,cex=1,line=2)
mtext('Puntaje',2,cex=1,line=2)
mtext('a',3,cex=1.5,font=2,col='#000000')

segments(x0=c(14),x1=c(14), #no mover
         y0=c(46.466),y1=c(52),
         lty='dashed')
segments(x0=c(6,15,17,24,27,29,31,41,49),x1=c(6,15,17,24,27,29,31,41,49), #no mover
         y0=c(57,45.035,41,32.156,25,21,15,7,-3.619),y1=c(57.914,47,42.173,36,27.863,25.139,22.139,7.829,2),
         lty='dashed', col = '#8D9091')

text(20,52,'Y-\U0232=Error del sujeto 9',cex=1,adj=c(0.2,1.5))
text(10,10,'Y-\U0232=66.5 - 1.431x',cex=1,adj=c(0.2,1.5))

lines(ansiedad,y_a,lwd=6)
points(14,52, lwd=1.3, pch=20)
points(x_pa, y_pa, lwd=1.3, pch=20, col = '#8D9091')

#Gráfica b
plot(NULL,xlim=c(-10,60),ylim=c(-10,60),axes=F,ann=F)
axis(1,pos=0,cex.axis=1.75,col='#aaaaaa',col.axis='#aaaaaa')
axis(2,pos=0,las=2,cex.axis=1.75,col='#aaaaaa',col.axis='#aaaaaa')
mtext('Ansiedad',1,cex=1,line=2)
mtext('Puntaje',2,cex=1,line=2)
mtext('b',3,cex=1.5,font=2,col='#000000')

segments(x0=c(6),x1=c(6), #no mover
         y0=c(44.69),y1=c(57), #nomover
         lty='dashed') #nomover
segments(x0=c(14,15,17,24,27,29,31,41,49),x1=c(14,15,17,24,27,29,31,41,49), #no mover
         y0=c(41.49,41.09,40.29,37.49,25,21,15,7,2),y1=c(52,47,41,36,36.29,35.49,34.69,30.69,27.49),
         lty='dashed', col = '#8D9091')

text(12,56,'Y-\U0232=Error del sujeto 10',cex=1,adj=c(0.2,1.5))
text(10,10,'Y-\U0232=47.09 - 0.4x',cex=1,adj=c(0.2,1.5))

lines(ansiedad,y_b,lwd=6)
points(6,57, lwd=1.3, pch=20) #nomover
points(x_pb, y_pb, lwd=1.3, pch=20, col = '#8D9091')

###############################################
#HOMOSCEDASTICITY

rm(list=ls())

ansiedad <- c(49,41,31,29,27,24,17,15,14,6) #X
puntaje <- c(2,7,15,21,25,36,41,47,52,57) #Y

x_pa <- c(49,41,31,29,27,24,17,15,6)
y_pa <- c(2,7,15,21,25,36,41,47,57)

x_pb <- c(49,41,31,29,27,24,17,15,14)
y_pb <- c(2,7,15,21,25,36,41,47,52)

#Recta a
B1_a <- -1.431
B0_a <- 66.5
f_x_a <- function(x){
  return(x*B1_a+B0_a)
}
y_a <- f_x_a(ansiedad)

#Recta b
B1_b <- -0.4
B0_b <- 47.09 #mantener
f_x_b <- function(x){
  return(x*B1_b+B0_b)
}
y_b <- f_x_b(ansiedad)


#Graficando
options(repr.plot.width = 16, repr.plot.height = 8)
layout(matrix(1:2,ncol=2))


#Gráfica a
plot(NULL,xlim=c(-10,60),ylim=c(-10,60),axes=F,ann=F)
axis(1,pos=0,cex.axis=1.75,col='#aaaaaa',col.axis='#aaaaaa')
axis(2,pos=0,las=2,cex.axis=1.75,col='#aaaaaa',col.axis='#aaaaaa')
mtext('Ansiedad',1,cex=1,line=2)
mtext('Puntaje',2,cex=1,line=2)
mtext('a',3,cex=1.5,font=2,col='#000000')

segments(x0=c(14),x1=c(14), #no mover
         y0=c(46.466),y1=c(52),
         lty='dashed')
segments(x0=c(6,15,17,24,27,29,31,41,49),x1=c(6,15,17,24,27,29,31,41,49), #no mover
         y0=c(57,45.035,41,32.156,25,21,15,7,-3.619),y1=c(57.914,47,42.173,36,27.863,25.139,22.139,7.829,2),
         lty='dashed', col = '#8D9091')

text(20,52,'Y-\U0232=Error del sujeto 9',cex=1,adj=c(0.2,1.5))
text(10,10,'Y-\U0232=66.5 - 1.431x',cex=1,adj=c(0.2,1.5))

lines(ansiedad,y_a,lwd=6)
points(14,52, lwd=1.3, pch=20)
points(x_pa, y_pa, lwd=1.3, pch=20, col = '#8D9091')

#Gráfica b
plot(NULL,xlim=c(-10,60),ylim=c(-10,60),axes=F,ann=F)
axis(1,pos=0,cex.axis=1.75,col='#aaaaaa',col.axis='#aaaaaa')
axis(2,pos=0,las=2,cex.axis=1.75,col='#aaaaaa',col.axis='#aaaaaa')
mtext('Ansiedad',1,cex=1,line=2)
mtext('Puntaje',2,cex=1,line=2)
mtext('b',3,cex=1.5,font=2,col='#000000')

segments(x0=c(6),x1=c(6), #no mover
         y0=c(44.69),y1=c(57), #nomover
         lty='dashed') #nomover
segments(x0=c(14,15,17,24,27,29,31,41,49),x1=c(14,15,17,24,27,29,31,41,49), #no mover
         y0=c(41.49,41.09,40.29,37.49,25,21,15,7,2),y1=c(52,47,41,36,36.29,35.49,34.69,30.69,27.49),
         lty='dashed', col = '#8D9091')

text(12,56,'Y-\U0232=Error del sujeto 10',cex=1,adj=c(0.2,1.5))
text(10,10,'Y-\U0232=47.09 - 0.4x',cex=1,adj=c(0.2,1.5))

lines(ansiedad,y_b,lwd=6)
points(6,57, lwd=1.3, pch=20) #nomover
points(x_pb, y_pb, lwd=1.3, pch=20, col = '#8D9091')





