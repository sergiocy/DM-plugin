
###########################################
# script para algoritmo Levenberg-Marquardt mejorado
# (calculo de la expresión analitica del gradiente)
# nueva version para DM  (comenzado: Feb. 2016)
##########################################

# source("C:\\Users\\Usuario\\Desktop\\plugInDM\\scriptsR_support\\plotMatrix.R")
#ESTIM_ORIGINAL <- read.table("C:\\Users\\Usuario\\Desktop\\plugInDM\\estimImgDM_OUT.txt", header=F)
#ESTIM_FITTED <- read.table("C:\\Users\\Usuario\\Desktop\\plugInDM\\estimadores_IN.txt", header=T)



# plotMatrix3d(DATOS_IN-min(DATOS_IN))
# plotMatrix3d(DATOS_IN)
# min(DATOS_IN)
# max(DATOS_IN)

#ESTIM_FITTED <- read.table("C:\\Users\\Usuario\\Desktop\\plugInDM\\estimadores_IN.txt", header=T)
# m <- read.table("C:\\Users\\Usuario\\Desktop\\plugInDM\\estimImgDM_OUT.txt")
# plotMatrixPoints(m)



library(minpack.lm)

##################################
#FUNCION PARA LEER DATOS DE UN TXT
datosEntrada <- function(){
  txtDatosIN <- "C:\\Users\\Usuario\\Desktop\\plugInDM\\datosImgDM_OUT.txt"
  txtEstimadoresIni <- "C:\\Users\\Usuario\\Desktop\\plugInDM\\estimImgDM_OUT.txt"
  
  DATOS_IN <<- read.table(txtDatosIN)
  #DATOS_IN <<- read.table("C:\\Users\\Usuario\\Desktop\\TFM\\MiTrabajoV1\\ImagenReal\\datosImgDM_OUT.txt")
  ESTIMADORES_INICIALES <<- read.table(txtEstimadoresIni)
  NUM_COMPONENTES <<- nrow(ESTIMADORES_INICIALES)
  
  #definimos el offset
  OFFS <<- min(DATOS_IN)
}


#############################
#FUNCION PARA CONVERTIR LA MATRIZ DE DATOS EN UNA TABLA
convierteMatrizATabla2 <- function(matrizImagen){
  num_cols <- ncol(matrizImagen)
  num_rows <- nrow(matrizImagen)
  
  #escribimos los datos en forma de tabla en vez de como matriz...
  tablaDatos <- cbind(as.vector(sapply(seq(from=0.5, to=num_cols-0.5, by=1), function(j) rep(j,times=num_rows))),
                      rep(seq(from=0.5, to=num_rows-0.5, by=1), times=num_cols),
                      as.vector(as.matrix(matrizImagen)))
  tablaDatos <- as.data.frame(tablaDatos)
  
  names(tablaDatos) <- c("x","y","z") 
  return(tablaDatos)
}


######################################
#FUNCION PARA RESTAR EL OFFSET Y NORMALIZAR LOS DATOS (TABLA)
procesaTablaEntrada <- function(tabla){
  #restamos offset a los datos...
  tabla[,3] <- tabla[,3]-OFFS
  
  #Normalizamos...
  SUMA_DATOS_ENTRADA <<- sum(tabla[,3])
  tabla[,3] <- tabla[,3]/SUMA_DATOS_ENTRADA
  
  #M <- max(tabla[,3])
  #tabla[,3] <- tabla[,3]/M
  
  return(tabla)
}


###################################################################
###FUNCION PARA GENERAR MODELO DE MIXTURAS DE NORMALES DE N COMPONENTES
generaModelo <- function(n){    
  modelo_temp <- ""
  for(comp in 1:n){
    if(comp==1){
      modelo_temp <- paste("of+(",paste("peso",comp,sep=""),"*( 1/(2*pi*",paste("sx",comp,sep=""),"*",paste("sy",comp,sep=""),"*sqrt(1-",paste("rho",comp,sep=""),"^2)) )*( exp( (-1/(2*(1-",paste("rho",comp,sep=""),"^2)))*(((x-",paste("mx",comp,sep=""),")^2/",paste("sx",comp,sep=""),"^2)+((y-",paste("my",comp,sep=""),")^2/",paste("sy",comp,sep=""),"^2)-(2*",paste("rho",comp,sep=""),"*(x-",paste("mx",comp,sep=""),")*(y-",paste("my",comp,sep=""),")/(",paste("sx",comp,sep=""),"*",paste("sy",comp,sep=""),"))) ) ))" )
    }
    else{
      modelo_temp <- paste(modelo_temp,"+(",paste("peso",comp,sep=""),"*( 1/(2*pi*",paste("sx",comp,sep=""),"*",paste("sy",comp,sep=""),"*sqrt(1-",paste("rho",comp,sep=""),"^2)) )*( exp( (-1/(2*(1-",paste("rho",comp,sep=""),"^2)))*(((x-",paste("mx",comp,sep=""),")^2/",paste("sx",comp,sep=""),"^2)+((y-",paste("my",comp,sep=""),")^2/",paste("sy",comp,sep=""),"^2)-(2*",paste("rho",comp,sep=""),"*(x-",paste("mx",comp,sep=""),")*(y-",paste("my",comp,sep=""),")/(",paste("sx",comp,sep=""),"*",paste("sy",comp,sep=""),"))) ) ))" )
    } 
  }
  modelo <- modelo_temp
  MODELO <<- parse(text=modelo)
}


#######################################################3
###FUNCION PARA CALCULAR EL VALOR DEL MODELO CON LOS PARAMETROS DADOS
calculaModelo <- function(param, x, y){
  eval(MODELO, param)
}


########################################################
###FUNCION PARA GENERAR LA EXPRESION ANALÍTICA DEL GRADIENTE ASOCIADO AL MODELO
generaGradiente <- function(n){
  
  modelo_mixtura <- parse(text=MODELO)
  gradiente_mixtura <- c()
  gradiente_mixtura[1] <- 1
  
  for(componente in 1:n) {
    gradiente_componente <- c()
    
    parcialmx <- D(modelo_mixtura, paste("mx",componente, sep=""))
    parcialmy <- D(modelo_mixtura, paste("my",componente, sep=""))
    parcialsx <- D(modelo_mixtura, paste("sx",componente, sep=""))
    parcialsy <- D(modelo_mixtura, paste("sy",componente, sep=""))
    parcialrho <- D(modelo_mixtura, paste("rho",componente, sep=""))
    parcialpeso <- D(modelo_mixtura, paste("peso",componente, sep=""))
    
    gradiente_componente <- c(parcialmx,parcialmy,parcialsx,parcialsy,parcialrho,parcialpeso)
    
    gradiente_mixtura <- c(gradiente_mixtura,gradiente_componente)
  }
  GRADIENTE <<- as.expression(gradiente_mixtura)
} 


#######################################################
###FUNCION PARA CALCULAR EL VALOR DEL GRADIENTE PARA PARAMETROS Y PUNTOS DADOS
calculaGradiente2 <- function(param,x,y,z){
  l <- length(GRADIENTE)

  j1 <- rep(-1,times=length(x))
  j2 <- vapply(GRADIENTE[2:l], function(k) -eval(k,param), FUN.VALUE=numeric(length(x)))
  
  c(j1,j2)
}


###############################
###FUNCION PARA CALCULAR LOS RESIDUOS
residuales <- function(param,x,y,z){
  z-calculaModelo(param,x,y)
}


###############################
###FUNCION QUE EJECUTA EL ALGORITMO
algoritmoLM <- function(tablaDatos){ 
 param_iniciales <- list(of=0)
 cotaSuperior <- c(max(tablaDatos[,3])/2)
 cotaInferior <- c(-max(tablaDatos[,3])/2)                 
 
 for(i in 1:NUM_COMPONENTES){ 
   
  aux <- list(mx=ESTIMADORES_INICIALES[i,1],my=ESTIMADORES_INICIALES[i,2],sx=ESTIMADORES_INICIALES[i,3],sy=ESTIMADORES_INICIALES[i,4],rho=ESTIMADORES_INICIALES[i,5],peso=ESTIMADORES_INICIALES[i,6]) 
  names(aux) <- c(paste("mx",i,sep=""),paste("my",i,sep=""),paste("sx",i,sep=""),paste("sy",i,sep=""),paste("rho",i,sep=""),paste("peso",i,sep="")) 
  param_iniciales <- c(param_iniciales, aux)

 
  cotaSuperior <- c(cotaSuperior,ESTIMADORES_INICIALES[i,1]+7,ESTIMADORES_INICIALES[i,2]+7,ESTIMADORES_INICIALES[i,3]+10,ESTIMADORES_INICIALES[i,4]+10,0.99,5)
  cotaInferior <- c(cotaInferior,ESTIMADORES_INICIALES[i,1]-7,ESTIMADORES_INICIALES[i,2]-7,0.1,0.1,-0.99,0)
 }
 
 
 ajuste <- nls.lm(par=param_iniciales, upper=cotaSuperior, lower=cotaInferior, 
                  fn=residuales, x=tablaDatos$x, y=tablaDatos$y, z=tablaDatos$z,
                  control=nls.lm.control(nprint=1),
                  jac=calculaGradiente2
                  )
 
  
 tablaDatosAjustados <- data.frame(tablaDatos$x,tablaDatos$y,calculaModelo(as.list(coef(ajuste)),tablaDatos$x,tablaDatos$y))
 #View(tablaDatosAjustados)
 estimParametros <- matrix(coef(ajuste)[-1],nrow=NUM_COMPONENTES,byrow=T)
 #View(estimParametros)
 offsAjustado <- coef(ajuste)[1]
 errorOffsAjustado <- summary(ajuste)$coef[1,2]
 errorEstimParametros <- matrix(summary(ajuste)$coef[-1,2],nrow=NUM_COMPONENTES,byrow=T)
 #View(errorEstimParametros)
 
 
 cat("parámetros ajustados: \n")
 cat("media x   media y    sigma x    sigma y    coefCorr    peso")
 print(estimParametros)
 cat("offset para la mixtura \n")
 print(offsAjustado)
 
 Sys.sleep(5)
 
 valoresAjuste <- list(tablaDatosAjustados,estimParametros,errorEstimParametros,offsAjustado,errorOffsAjustado)
 
 return(valoresAjuste)
}


#############################################
#FUNCION PARA PROCESAR LA TABLA DE DATOS DE SALIDA
#############################################
procesaTablaSalida <- function(tablaDatosAjuste){
  #``desnormalizamos'' los valores ajustados y sumamos el offset que
  # habíamos restado en ``procesaTablaEntrada''
  tablaDatosAjuste[,3] <- tablaDatosAjuste[,3]*SUMA_DATOS_ENTRADA
  
  #########################
  #prueba desnormalizacion
  #tablaDatosAjuste[,3] <- tablaDatosAjuste[,3]*diferencia
  #########################
  
  tablaDatosAjuste[,3] <- round(tablaDatosAjuste[,3]+OFFS)
  
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




#####################################
######### MAIN ########################
principal <- function(){
  t_total <- proc.time()
  datosEntrada()
  #PONEMOS LA MATRIZ DE ENTRADA COMO TABLA
  tablaDatosIn <- convierteMatrizATabla2(DATOS_IN)
  #PROCESAMOS LA TABLA
  tablaDatosInProc <- procesaTablaEntrada(tablaDatosIn)
  
  generaModelo(NUM_COMPONENTES)
  generaGradiente(NUM_COMPONENTES)
  
  t_alg <- proc.time()
  nuevosDatos <- algoritmoLM(tablaDatosInProc)
  proc.time()-t_alg
  

  tablaDatosAjustados <- as.data.frame(nuevosDatos[1])
  estimadoresAjustados <- as.data.frame(nuevosDatos[2])
  errorEstimadoresAjustados <- as.data.frame(nuevosDatos[3])
  offsetAjustado <- as.numeric(nuevosDatos[4])
  errorOffsetAjustado <- as.numeric(nuevosDatos[5])


  #PROCESAMOS LA TABLA DE DATOS AJUSTADOS Y LA ESCRIBIMOS COMO MATRIZ
  DATOS_OUT <- procesaTablaSalida(tablaDatosAjustados)


  #ESCRIBIMOS LOS DATOS AJUSTADOS...
  datosSalida(estimadoresAjustados,DATOS_OUT,errorEstimadoresAjustados)

  proc.time()-t_total
}
principal()





