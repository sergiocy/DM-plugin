
########
# Funcion para plotear una matriz en 3d siendo; 
# - el eje x de la gráfica, el número de columnas de la matriz
# - el eje y de la gráfica, el número de filas de la matriz
# - el eje z (vertical), los elementos de matriz
####
#como argumento de entrada/llamada tiene una matriz (``frecuencias'')
########

   

#########################333
# PARA LA GRAFICACIÓN Y PRESENTACION DEL AJUSTE
##########################

#datos <- as.data.frame(matrix(c(-2,0.1,1,-1,0.8,2,0,2,3,1,0.8,4,2,0.1,5), nrow=5, ncol=3, byrow=TRUE))
#frecuencias <- datos

plotMatrix3d <- function(frecuencias){
  frecuencias <- as.matrix(frecuencias)
  
  #dim(frecuencias)
  #pixeles_x <- seq(0.5,NPIXX,by=1)         # Generamos una malla de puntos (x,y)
  pixeles_x <- seq(0.5,ncol(frecuencias)-0.5,by=1)#seq(0.5,10,by=0.5)
  #length(pixeles_x)
  #pixeles_x <- c(1:nPixImg_x)
  pixeles_y <- seq(0.5,nrow(frecuencias)-0.5,by=1)
  #pixeles_y <- seq(0.5,NPIXY,by=1)
  eventos <- matrix(nrow=nrow(frecuencias),ncol=ncol(frecuencias))
  eventos <- frecuencias[pixeles_y+0.5,pixeles_x+0.5]
  
  if(min(frecuencias)<0){minimo_matriz <- min(frecuencias)}
  else {minimo_matriz <- 0}
  
  persp(pixeles_y,pixeles_x,eventos, xlab="pixels y (rows)", ylab="pixels x (columns)", zlab="intensity", zlim=c(minimo_matriz,max(frecuencias)), phi=30, theta=60)  # Un gráfico en perspectiva
  #par(lty=3)
  #plot3d(pixeles_y,pixeles_x,eventos, zlim=c(0,max(frecuencias)), phi=30, theta=60)
  
}



plotPoints <- function(x,y){
  plot(x,y)
}



generateArrayOfData <- function(x, y, type="BCC"#, N_cellUnits_perSide=3
                                ){
  N_cellUnits_perSide <- ((1/2)*(1+sqrt((2*length(x))-1)))-1 
  
  #plot(M[,1],M[,2])
  #N_cellUnits_perSide <- 3
  #x <- ESTIM_FITTED[,1]
  #y <- ESTIM_FITTED[,2]
  
  if(type=="BCC"){
    #...generamos matriz que simula la estructura cristalina a tratar...
    #struc_crystall_BCC <- matrix(c(1,0,0,2),ncol=2,nrow=2)
    struc_crystall_aux <- rbind(c(rep(c(1,0), times=N_cellUnits_perSide),1),c(rep(c(0,2), times=N_cellUnits_perSide),0))
    struc_crystall <- struc_crystall_aux
    for(i in 1:(N_cellUnits_perSide-1)){struc_crystall <- rbind(struc_crystall,struc_crystall_aux)}
    struc_crystall <- rbind(struc_crystall, c(rep(c(1,0), times=N_cellUnits_perSide),1))
    
    #extendemos esta matriz a array de 3 dim. En la tercera dim, almacenaremos la posicion (x e y)
    struc_and_estimates <- array(NA ,c(nrow(struc_crystall),ncol(struc_crystall),3))
    struc_and_estimates[,,1] <- struc_crystall
    #dim(struc_and_estimates)
    #struc_crystall[1:2,1:2]
    
    #pr[1,1] <- list(p11=list(x=0,y=1), y=1)
    #pr[1,2] <- list(x=1, y=3)
    
    #...almacenamos los valores de x e y correspondientes...
    n <- 1
    for(i in 1:nrow(struc_crystall)){
      for(j in 1:ncol(struc_crystall)){
        if(struc_and_estimates[i,j,1] != 0){
          struc_and_estimates[i,j,2] <- x[n]
          struc_and_estimates[i,j,3] <- y[n]
          n <- n+1
        }    
      }
    }
    
    #struc_and_estimates[,,3] 
    
  } 
  return (struc_and_estimates)
}



#funcion plotear filas o columnas de una matriz como coordenadas
#IMPORTANTE!!!!! la imagen debe ser "cuadrada" (mismo)
plotSquareImageDistance <- function(arrayData, type="BCC", calibration=0.017455){
  #arrayData <- generateArrayOfData(ESTIM_FITTED[,1],ESTIM_FITTED[,2])
  
  if(type=="BCC"){
    
  #...representamos graficamente la estructura y distancias con los vecinos...
  win.graph()
  plot(arrayData[,,2],-arrayData[,,3], xlab="pixeles x", ylab="pixeles y")
  
  arrayData <- arrayData[3:(nrow(arrayData[,,1])-2),3:(ncol(arrayData[,,1])-2),]
  
  i <- 1
  j <- 1
  while((2*i+1)<=dim(arrayData[,,1])[1]){
    #while((2*j+1)<=dim(arrayData[,,1])[2]){
      
    array_cell_unit <- arrayData[(2*i-1):(2*i+1),(2*j-1):(2*j+1),]
    

    lines(x=c(array_cell_unit[1,1,2], array_cell_unit[1,3,2]),y=c(-array_cell_unit[1,1,3],-array_cell_unit[1,3,3]),type="l")#
    d1 <- calibration*sqrt(((array_cell_unit[1,3,2]-array_cell_unit[1,1,2])^2)+((array_cell_unit[1,3,3]-array_cell_unit[1,1,3])^2))
    #...distancia tipo "N"...
    text(x=array_cell_unit[1,1,2]+(array_cell_unit[1,3,2]-array_cell_unit[1,1,2])/2,y=-array_cell_unit[1,1,3]-(-array_cell_unit[1,3,3]+array_cell_unit[1,1,3])/2,pos=4,label=paste("d1 (N) = ",d1) )
    
    lines(x=c(array_cell_unit[1,3,2], array_cell_unit[3,3,2]),y=c(-array_cell_unit[1,3,3],-array_cell_unit[3,3,3]),type="l")#
    d2 <- calibration*sqrt(((array_cell_unit[1,3,2]-array_cell_unit[3,3,2])^2)+((array_cell_unit[1,3,3]-array_cell_unit[3,3,3])^2))
    #...distancia tipo "E"...
    text(x=array_cell_unit[1,3,2]+(array_cell_unit[1,3,2]-array_cell_unit[3,3,2])/2,y=-array_cell_unit[1,3,3]-(-array_cell_unit[1,3,3]+array_cell_unit[3,3,3])/2,pos=4,label=paste("d2 (E) = ",d2) )
    
    lines(x=c(array_cell_unit[3,3,2], array_cell_unit[3,1,2]),y=c(-array_cell_unit[3,3,3],-array_cell_unit[3,1,3]),type="l")#
    d3 <- calibration*sqrt(((array_cell_unit[3,3,2]-array_cell_unit[3,1,2])^2)+((array_cell_unit[3,3,3]-array_cell_unit[3,1,3])^2))
    #...distancia tipo "S"...
    text(x=array_cell_unit[3,3,2]-(array_cell_unit[3,3,2]-array_cell_unit[3,1,2])/2,y=-array_cell_unit[3,3,3]-(-array_cell_unit[3,3,3]+array_cell_unit[3,1,3])/2,pos=4,label=paste("d3 (S) = ",d3) )
    
    lines(x=c(array_cell_unit[1,1,2], array_cell_unit[3,1,2]),y=c(-array_cell_unit[1,1,3],-array_cell_unit[3,1,3]),type="l")#
    d4 <- calibration*sqrt(((array_cell_unit[1,1,2]-array_cell_unit[3,1,2])^2)+((array_cell_unit[1,1,3]-array_cell_unit[3,1,3])^2))
    #...distancia tipo "W"...
    text(x=array_cell_unit[1,1,2]+(array_cell_unit[3,1,2]-array_cell_unit[1,1,2])/2,y=-array_cell_unit[1,1,3]-(array_cell_unit[3,1,3]-array_cell_unit[1,1,3])/2,pos=4,label=paste("d4 (W) = ",d4) )
    
    
    
    lines(x=c(array_cell_unit[2,2,2], array_cell_unit[1,1,2]),y=c(-array_cell_unit[2,2,3],-array_cell_unit[1,1,3]),type="l")#
    d5 <- calibration*sqrt(((array_cell_unit[2,2,2]-array_cell_unit[1,1,2])^2)+((array_cell_unit[2,2,3]-array_cell_unit[1,1,3])^2))
    #...distancia tipo "NW"...
    text(x=-1+array_cell_unit[2,2,2]-(array_cell_unit[2,2,2]-array_cell_unit[1,1,2])/2,y=1-array_cell_unit[2,2,3]+(array_cell_unit[2,2,3]-array_cell_unit[1,1,3])/2,pos=4,label=paste("d5 (NW) = ",d5) )
    
    lines(x=c(array_cell_unit[2,2,2], array_cell_unit[1,3,2]),y=c(-array_cell_unit[2,2,3],-array_cell_unit[1,3,3]),type="l")#
    d6 <- calibration*sqrt(((array_cell_unit[2,2,2]-array_cell_unit[1,3,2])^2)+((array_cell_unit[2,2,3]-array_cell_unit[1,3,3])^2))
    #...distancia tipo "NE"...
    text(x=-1+array_cell_unit[2,2,2]+(-array_cell_unit[2,2,2]+array_cell_unit[1,3,2])/2,y=-1-array_cell_unit[2,2,3]+(array_cell_unit[2,2,3]-array_cell_unit[1,3,3])/2,pos=4,label=paste("d6 (NE) = ",d6) )
    
    lines(x=c(array_cell_unit[2,2,2], array_cell_unit[3,3,2]),y=c(-array_cell_unit[2,2,3],-array_cell_unit[3,3,3]),type="l")#
    d7 <- calibration*sqrt(((array_cell_unit[2,2,2]-array_cell_unit[3,3,2])^2)+((array_cell_unit[2,2,3]-array_cell_unit[3,3,3])^2))
    #...distancia tipo "SE"...
    text(x=-1+array_cell_unit[2,2,2]+(-array_cell_unit[2,2,2]+array_cell_unit[3,3,2])/2,y=1-array_cell_unit[2,2,3]+(array_cell_unit[2,2,3]-array_cell_unit[3,3,3])/2,pos=4,label=paste("d7 (SE) = ",d7) )
    
    lines(x=c(array_cell_unit[2,2,2], array_cell_unit[3,1,2]),y=c(-array_cell_unit[2,2,3],-array_cell_unit[3,1,3]),type="l")#
    d8 <- calibration*sqrt(((array_cell_unit[2,2,2]-array_cell_unit[3,1,2])^2)+((array_cell_unit[2,2,3]-array_cell_unit[3,1,3])^2))
    #...distancia tipo "SW"...
    text(x=-1+array_cell_unit[2,2,2]-(array_cell_unit[2,2,2]-array_cell_unit[3,1,2])/2,y=-1-array_cell_unit[2,2,3]-(-array_cell_unit[2,2,3]+array_cell_unit[3,1,3])/2,pos=4,label=paste("d8 (SW) = ",d8) )
    
    
    
    #}
    if((2*j+1)==dim(arrayData[,,1])[2]){
      i <- i+1
      j <- 1
    }
    else{
      j <- j+1
    }
  }
  #dim(arrayData)
  
  system("cmd PAUSE")
  }#...fin del if para type BCC...  
}




# plotMatrix3d(DATOS_IN-min(DATOS_IN))
# plotMatrix3d(DATOS_IN)
# plotMatrix3d(DATA_ORIGINAL)
# plotMatrix3d(DATA_FITTED)
# min(DATOS_IN)
# max(DATOS_IN)


#ESTIM_ORIGINAL <- read.table("C:\\Users\\Usuario\\Desktop\\plugInDM\\estimImgDM_OUT.txt", header=F)
#ESTIM_FITTED <- read.table("C:\\Users\\Usuario\\Desktop\\plugInDM\\estimImgDM_IN.txt", header=F)
#DATA_ORIGINAL <- read.table("C:\\Users\\Usuario\\Desktop\\plugInDM\\datosImgDM_OUT.txt", header=F)
#DATA_FITTED <- read.table("C:\\Users\\Usuario\\Desktop\\plugInDM\\datosImgDM_IN.txt", header=F)

#plotPoints(ESTIM_ORIGINAL[,1],ESTIM_ORIGINAL[,2])
#plotPoints(ESTIM_FITTED[,1],ESTIM_FITTED[,2])
#generateArrayOfData(ESTIM_FITTED[,1],ESTIM_FITTED[,2])
#plotSquareImageDistance(generateArrayOfData(ESTIM_FITTED[,1],ESTIM_FITTED[,2]))




#par(lab=c(1,1,7))
#dev.off()
#win.graph()
#plotPoints(ESTIM_FITTED[,2],-ESTIM_FITTED[,3])
#lines(x=ESTIM_FITTED[,2],y=-ESTIM_FITTED[,3])#
#text(x=10,y=-10,pos=4,label = "hello")
#jpeg("grafico.jpg")
#system("cmd PAUSE")

#text(locator(), labels = "algo")
#par(lab=c(1,1,7))
#par()



#plotPoints(ESTIM_ORIGINAL[,1],ESTIM_ORIGINAL[,2])

