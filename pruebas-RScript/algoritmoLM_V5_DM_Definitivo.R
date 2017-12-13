###########################################
# script para algoritmo Levenberg-Marquardt
# version V5 para DM  (comenzado: 5/08/2015)
##########################################

library(minpack.lm)

##################################
#FUNCION PARA LEER DATOS DE UN TXT
##################################
datosEntrada <- function(){
  txtDatosIN <- "C:\\Users\\Usuario\\Desktop\\plugInDM\\datosImgDM_OUT.txt"
  txtEstimadoresIni <- "C:\\Users\\Usuario\\Desktop\\plugInDM\\estimImgDM_OUT.txt"
  
  DATOS_IN <<- read.table(txtDatosIN)
  #DATOS_IN <<- read.table("C:\\Users\\Usuario\\Desktop\\TFM\\MiTrabajoV1\\ImagenReal\\datosImgDM_OUT.txt")
  ESTIMADORES_INICIALES <<- read.table(txtEstimadoresIni)
  
  #definimos el offset
  offs <<- min(DATOS_IN)
  
}


#############################
#FUNCION PARA CONVERTIR LA MATRIZ DE DATOS EN UNA TABLA
#################################
convierteMatrizATabla <- function(matrizImagen){
  #escribimos los datos en forma de tabla en vez de como matriz...
  tablaDatos <- data.frame(x=numeric(0),y=numeric(0),z=numeric(0))
  #...los cogemos y organizamos de la matriz ``frecuencias''...
  for(col in 1:ncol(matrizImagen)){
    for(fil in 1:nrow(matrizImagen)){
      
      tablaDatos <- rbind(tablaDatos,c(col-0.5,fil-0.5,matrizImagen[fil,col]))
    }
  }
  names(tablaDatos) <- c("x","y","z") 
  return(tablaDatos)
}


######################################
#FUNCION PARA RESTAR EL OFFSET Y NORMALIZAR LOS DATOS (TABLA)
#########################################
procesaTablaEntrada <- function(tabla){
  #restamos offset a los datos...
  tabla[,3] <- tabla[,3]-offs
  
  #Normalizamos...
  sumaDatosEntrada <<- sum(tabla[,3])
  tabla[,3] <- tabla[,3]/sumaDatosEntrada
  
  ##############
  ####prueba normalizacion###
  #diferencia <<- max(tabla[,3])-min(tabla[,3])
  #tabla[,3] <- tabla[,3]/diferencia
  #####################
  
  return(tabla)
}


##############################################
#FUNCION PARA ALGORITMO DE LEVENBERG-MARQUARDT
##############################################
algoritmoLMLibreria <- function(tablaDatos){
  
  x <- tablaDatos$x
  y <- tablaDatos$y
  z <- tablaDatos$z
  
  ################
  #generamos la formula completa
  estimParametros <- ESTIMADORES_INICIALES
  #y el número de componentes de la mixtura
  nComp <- nrow(ESTIMADORES_INICIALES)
  cotaSup <- c()
  cotaInf <- c()
  
  for(componente in 1:nComp){
    
    valorMx <- estimParametros[componente,1] 
    valorMy <- estimParametros[componente,2]
    valorSx <- estimParametros[componente,3]
    valorSy <- estimParametros[componente,4]
    valorRho <- estimParametros[componente,5]
    valorPeso <- estimParametros[componente,6]
    mx <- paste("mx",componente, sep="")
    my <- paste("my",componente, sep="")
    sx <- paste("sx",componente, sep="")
    sy <- paste("sy",componente, sep="")
    rho <- paste("rho",componente, sep="")
    peso <- paste("peso",componente, sep="")
    
    #la funcion expresada como cadena de caracteres
    funcionNormal <- paste("(",paste(peso),"*( 1/(2*pi*",paste(sx,"*",sy),"*sqrt(1-",paste(rho),"^2)) )*( exp( (-1/(2*(1-",paste(rho),"^2)))*(((x-",paste(mx),")^2/",paste(sx),"^2)+((y-",paste(my),")^2/",paste(sy),"^2)-(2*",paste(rho),"*(x-",paste(mx),")*(y-",paste(my),")/(",paste(sx,"*",sy),"))) ) ))" )
    
    
    ini <- list(mx=valorMx,my=valorMy,sx=valorSx,sy=valorSy,rho=valorRho,peso=valorPeso)
    names(ini)[1]<-paste(mx)
    names(ini)[2]<-paste(my)
    names(ini)[3]<-paste(sx)
    names(ini)[4]<-paste(sy)
    names(ini)[5]<-paste(rho)
    names(ini)[6]<-paste(peso)
    
    cotaSup <- c(cotaSup, valorMx+7,valorMy+7,valorSx+7,valorSy+7,0.99,5)
    cotaInf <- c(cotaInf,valorMx-7,valorMy-7,valorSx-7,valorSy-7,-0.99,0)
    
     
    if(componente==1){
      formula <- paste("z ~",paste("of"),"+",funcionNormal)
      
      inicio <- c(of=min(tablaDatos[,3]),ini)
      cotaSup <- c(min(tablaDatos[,3])+(max(tablaDatos[,3])/2), cotaSup)
      cotaInf <- c(min(tablaDatos[,3])-(max(tablaDatos[,3])/2), cotaInf)
      
    }
    if(componente>1){
      #sumamos las demás componentes al modelo completo
      formula <- paste(formula,"+",funcionNormal)
      inicio <- c(inicio,ini)
    }  
    
    
  }#fin del for 
  
  
  #...lanzamos el ajuste con la fórmula generada...
  ajuste <- nlsLM(formula,
                  data=tablaDatos,
                  start=inicio,
                  #par=list(mx=5,my=3,sx=1,sy=1,rho=0.8),
                  upper=cotaSup,
                  lower=cotaInf,
                  control=nls.lm.control(maxiter=70,nprint=0),#ptol, ftol, factor...
                  trace=T
                  #jac=as.expression(calculaGradiente()) 
                  )
  
  #print(summary(ajuste))
  
  
  tablaDatosAjustados <- data.frame(tablaDatos$x,tablaDatos$y,predict(ajuste))
  estimParametros <- matrix(coef(ajuste)[-1],nrow=nComp,byrow=T)
  offsAjustado <- coef(ajuste)[1]
  errorOffsAjustado <- summary(ajuste)$coefficients[1,2]
  errorEstimParametros <- matrix(summary(ajuste)$coefficients[-1,2],nrow=nComp,byrow=T)
  
  cat("parámetros ajustados: \n")
  cat("media x   media y    sigma x    sigma y    coefCorr    peso")
  print(estimParametros)
  cat("offset para la mixtura \n")
  print(offsAjustado)
  
  Sys.sleep(5)
  
  valoresAjuste <- list(tablaDatosAjustados,estimParametros,errorEstimParametros,offsAjustado,errorOffsAjustado)
  
  return(valoresAjuste)
  
}#fin de la funcion ``algoritmoLMLibreria''



#############################################
#FUNCION PARA PROCESAR LA TABLA DE DATOS DE SALIDA
#############################################
procesaTablaSalida <- function(tablaDatosAjuste){
  #``desnormalizamos'' los valores ajustados y sumamos el offset que
  # habíamos restado en ``procesaTablaEntrada''
  tablaDatosAjuste[,3] <- tablaDatosAjuste[,3]*sumaDatosEntrada
  
  #########################
  #prueba desnormalizacion
  #tablaDatosAjuste[,3] <- tablaDatosAjuste[,3]*diferencia
  #########################
  
  tablaDatosAjuste[,3] <- round(tablaDatosAjuste[,3]+offs)
  
  matrizAjuste <- matrix(data=tablaDatosAjuste[,3],nrow=nrow(DATOS_IN),ncol=ncol(DATOS_IN),byrow = FALSE)
  return(matrizAjuste)
}


#############################################
#funcion para escribir datos
##############################
datosSalida <- function(estimAjustados,datosAjustados,errorEstimAjustados){
  txtDatosOUT <- "C:\\Users\\Usuario\\Desktop\\plugInDM\\datosImgDM_IN.txt"
  txtEstimadoresOUT <- "C:\\Users\\Usuario\\Desktop\\plugInDM\\estimImgDM_IN.txt"
  txtErrorEstimadoresOUT <- "C:\\Users\\Usuario\\Desktop\\plugInDM\\errorEstimImgDM_IN.txt"
  
  write.table(estimAjustados,txtEstimadoresOUT, row.names=F, col.names=F)
  write.table(datosAjustados,txtDatosOUT, row.names=F, col.names=F)
  write.table(errorEstimAjustados,txtErrorEstimadoresOUT, row.names=F, col.names=F)
  
  
  #esribimos datos y estimadores en una columna, habiendolos tomado de la matriz por filas
  txtDatosOUTColumna <- "C:\\Users\\Usuario\\Desktop\\plugInDM\\ColumnaDatosImgDM_IN.txt"
  txtEstimadoresOUTColumna <- "C:\\Users\\Usuario\\Desktop\\plugInDM\\ColumnaEstimImgDM_IN.txt"
  txtErrorEstimadoresOUTColumna <- "C:\\Users\\Usuario\\Desktop\\plugInDM\\ColumnaErrorEstimImgDM_IN.txt"
  vectorDatos <- numeric(0)
  vectorEstim <- numeric(0)
  vectorErrorEstim <- numeric(0)
  
  for(fil in 1:nrow(datosAjustados)){
    for(col in 1:ncol(datosAjustados)){
      vectorDatos <- c(vectorDatos,datosAjustados[fil,col])
    }
  }
  write.table(as.matrix(vectorDatos),txtDatosOUTColumna, row.names=F, col.names=F)
  
  for(fil in 1:nrow(estimAjustados)){
    for(col in 1:ncol(estimAjustados)){
      vectorEstim <- c(vectorEstim,estimAjustados[fil,col])
      vectorErrorEstim <- c(vectorErrorEstim,errorEstimAjustados[fil,col])
    }
  }
  write.table(as.matrix(vectorEstim),txtEstimadoresOUTColumna, row.names=F, col.names=F)
  write.table(as.matrix(vectorErrorEstim),txtErrorEstimadoresOUTColumna, row.names=F, col.names=F)
}




##################################################################
####  MAIN  ######################################################
##################################################################
#RECIBIMOS DATOS Y ESTIMADORES
datosEntrada()
#PONEMOS LA MATRIZ DE ENTRADA COMO TABLA
tablaDatosIn <- convierteMatrizATabla(DATOS_IN)
#PROCESAMOS LA TABLA
tablaDatosInProc <- procesaTablaEntrada(tablaDatosIn)

#EJECUTAMOS EL ALGORITMO 
t <- proc.time()
nuevosDatos <- algoritmoLMLibreria(tablaDatosInProc)
proc.time()-t

tablaDatosAjustados <- as.data.frame(nuevosDatos[1])
estimadoresAjustados <- as.data.frame(nuevosDatos[2])
errorEstimadoresAjustados <- as.data.frame(nuevosDatos[3])
offsetAjustado <- as.numeric(nuevosDatos[4])
errorOffsetAjustado <- as.numeric(nuevosDatos[5])


#PROCESAMOS LA TABLA DE DATOS AJUSTADOS Y LA ESCRIBIMOS COMO MATRIZ
DATOS_OUT <- procesaTablaSalida(tablaDatosAjustados)


#ESCRIBIMOS LOS DATOS AJUSTADOS...
datosSalida(estimadoresAjustados,DATOS_OUT,errorEstimadoresAjustados)


