
library(minpack.lm)
datos <- as.data.frame(matrix(c(-2,0.1,-1,0.8,0,2,1,0.8,2,0.1), nrow=5, ncol=2, byrow=TRUE))
num_componentes <- 1


###FUNCION PARA GENERAR MODELO DE MIXTURAS DE NORMALES DE N COMPONENTES
generaModelo <- function(n){    
  modelo_temp <- ""
  for(comp in 1:n){
    if(comp==1){
      #modelo_temp <- paste("x+of+((",paste("mx",comp,sep=""),"^2)+(",paste("sx",comp,sep=""),"^2))")
      #modelo_temp <- paste("of+((1/(",paste("sx",comp,sep=""),"*sqrt(2*pi)))*( exp((-1/2)*(((",paste("x"),"-",paste("mx",comp,sep=""),")/",paste("sx",comp,sep=""),")^2)) ) )")
      modelo_temp <- paste( "of+( (1/(",paste("sx",comp,sep=""),"*sqrt(2*pi)))*(exp( (-1/2)*(((x-",paste("mx",comp,sep=""),")/",paste("sx",comp,sep=""),")^2) )) )" )
    }
    else{
      #modelo_temp <- paste(modelo_temp,"+((",paste("mx",comp,sep=""),"^2)+(",paste("sx",comp,sep=""),"^2))")
      #modelo_temp <- paste(modelo_temp,"+((1/(",paste("sx",comp,sep=""),"*sqrt(2*pi)))*( exp((-1/2)*(((",paste("x"),"-",paste("mx",comp,sep=""),")/",paste("sx",comp,sep=""),")^2)) ))")
      modelo_temp <- paste(modelo_temp,"+( (1/(",paste("sx",comp,sep=""),"*sqrt(2*pi)))*(exp( (-1/2)*(((x-",paste("mx",comp,sep=""),")/",paste("sx",comp,sep=""),")^2) )) )" )
    } 
  }
  modelo <- modelo_temp
  modelo <<- parse(text=modelo)
}
#generaModelo(1)


###FUNCION PARA CALCULAR EL VALOR DEL MODELO CON LOS PARAMETROS DADOS
calculaModelo <- function(param, x){
  #exp1 <- parse(text=modelo)
  eval(modelo, param)
}
#calculaModelo(list(of=0,mx1=0, sx1=1),datos$V1)
#calculaModelo(list(of=0, mx1=1, sx1=1, mx2=1, sx2=1),datos$V1)


###FUNCION PARA GENERAR LA EXPRESION ANALÍTICA DEL GRADIENTE ASOCIADO AL MODELO
generaGradiente <- function(n){
  
  #n <- nrow(ESTIMADORES_INICIALES)
  
  #gradiente_mixtura <- rep(NA,6*n+1)
  #length(gradiente_mixtura)
  
  modelo_mixtura <- parse(text=modelo)
  gradiente_mixtura <- c()
  gradiente_mixtura[1] <- 1
  
  for(componente in 1:n) {
    gradiente_componente <- c()
    
    parcialmx <- D(modelo_mixtura, paste("mx",componente, sep=""))
    #parcialmy <- D(modelo_mixtura, paste("my",componente, sep=""))
    parcialsx <- D(modelo_mixtura, paste("sx",componente, sep=""))
    #parcialsy <- D(modelo_mixtura, paste("sy",componente, sep=""))
    #parcialrho <- D(modelo_mixtura, paste("rho",componente, sep=""))
    #parcialpeso <- D(modelo_mixtura, paste("peso",componente, sep=""))
    
    gradiente_componente <- c(parcialmx,parcialsx)#,parcialsx,parcialsy,parcialrho,parcialpeso)
    
    gradiente_mixtura <- c(gradiente_mixtura,gradiente_componente)
  }
  #return(gradiente_mixtura)
  gradiente <<- as.expression(gradiente_mixtura)
} 
#generaGradiente(1)
#is.expression(generaGradiente(2))


###FUNCION PARA CALCULAR EL VALOR DEL GRADIENTE PARA PARAMETROS Y PUNTOS DADOS
calculaGradiente <- function(param,x_vector,z_vector){
  
  #g <- c()
  #for(i in 1:length(gradiente)){
  #  g <- c(g,-eval(gradiente[i],param)) 
  #}
  #g
  #################
  j <- c()
  for(i in 1:length(x_vector)){
    g <- c()
    x <- x_vector[i]
    z <- z_vector[i]
    #g <- unlist(lapply(gradiente, function(k) -eval(k,param)))
    for(k in 1:length(gradiente)){
      g <- c(g,-eval(gradiente[k],param)) 
    }
    j <- rbind(j,g)
    
  } 
  #print(param)
  as.vector(j) 
  
  #is.list(g)
  #-c(eval(parcialmx,param),eval(parcialsx,param))
  #-(eval(gradiente[1],param))
  #-c(eval(gradiente,param))
}
#calculaGradiente(list(of=0, mx1=1, sx1=1, mx2=1, sx2=1),datos$V1,datos$V2)
#calculaGradiente(list(of=0, mx1=1, sx1=2, mx2=1, sx2=1),1,1)
#calculaGradiente(list(of=0, mx1=1, sx1=1),c(0,1,2),c(0,1,2))
#length(gradiente)


###FUNCION PARA CALCULAR LOS RESIDUOS
residuales <- function(param,x,z){
  #print(z)
  #print(calculaModelo(param,x))
  #print(z-calculaModelo(param,x))
  z-calculaModelo(param,x)
}
#residuales(list(of=0, mx1=0.2, sx1=1),datos$V1,datos$V2)


inicio <- list(of=0, mx1=0.2, sx1=1)
cotaSup <- c(1,10,50)
cotaInf <- c(-1,-10,-50)
ajuste <- nls.lm(par=inicio, upper=cotaSup, lower=cotaInf, fn=residuales, z=datos$V2, x=datos$V1,
                 control=nls.lm.control(nprint=1, factor=100),
                 jac=calculaGradiente
                 )

summary(ajuste)
#plot(datos$V1,datos$V2)
#plot(datos$V1,fitted(ajuste))



######### MAIN ########################
generaModelo(num_componentes)
generaGradiente(num_componentes)
#predicciones <- calculaModelo(ajuste$par,datos$V1)
#plot(datos$V1,predicciones)



##########################
########################




#####################################
#######################################
fcn <- function(p, T, N, fcall, jcall)
  (N - do.call("fcall", c(list(T = T), as.list(p))))



#######################################



inicio <- list(mx1=0.2, sx1=1)
cotaSup <- c(3,5)
cotaInf <- c(-3,-5)
x <- datos$V1
z <- datos$V2

#form <- paste("z ~",modelo)

ajuste <- nlsLM(paste("z ~",modelo),
                data=datos,
                start=inicio,
                #jac=calculaGradiente,
                #par=list(mx=5,my=3,sx=1,sy=1,rho=0.8),
                upper=cotaSup,
                lower=cotaInf,
                control=nls.lm.control(maxiter=70,nprint=0),#ptol, ftol, factor...
                trace=T
                #jac=as.expression(calculaGradiente()) 
                )

summary(ajuste)



