
library(minpack.lm)

datos <- as.data.frame(matrix(c(-2,0,-1,1,0,2,1,1,2,0), nrow=5, ncol=2, byrow=TRUE))

####################################################
#Funcion para generar el modelo
############################################
generaFuncion <- function(n_componentes){
  
  #modelo <- expression( (1/sqrt(2*pi))*( exp() ) )
  modelo <<- paste("(1/(",paste("sx1"),"*sqrt(2*pi)))*( exp((-1/2)*(((",paste("x"),"-",paste("mx1"),")/",paste("sx1"),")^2)) )")
  
  #return(modelo)
  
}

##############################################
#Funcion para generar el gradiente
#########################################
generaGradiente <- function(param,x,z){
  #exp1 <- modelo
  exp1 <- parse(text=modelo)
  #exp1 <- expression(( 1/(2*pi*sx*sy*sqrt(1-rho^2)) )*( exp( (-1/(2*(1-rho^2)))*(((x-mx)^2/sx^2)+((y-my)^2/sy^2)-(2*rho*(x-mx)*(y-my)/(sx*sy))) ) ))
  #expression(x/(y + exp(z)))
  #eval(exp1)
  #mx1 <- 1; sx1 <- 1
  mx1 <- as.numeric(param[1])
  sx1 <- as.numeric(param[2])
  
  parcialmx <- D(exp1, "mx1")
  #parcialmy <- D(exp1, "my")
  parcialsx <- D(exp1, "sx1")
  #parcialsy <- D(exp1, "sy")
  #parcialrho <- D(exp1, "rho")
  
  -c(eval(parcialmx),eval(parcialsx))
}
#generaGradiente(list(mx1=0.2, sx1=1),x,z)

######################### FUNCIONES PARA EL ALGORITMO #########################
##############################
#funcion para evaluar funcion
###############################
f <- function(x, mx1, sx1){
  exp1 <- parse(text=modelo)
  return(eval(exp1))
}
##################################
#funcion para evaluar gradiente
##############################
j <- function(x, mx1, sx1) 
{
  exp1 <- parse(text=modelo)
  
  parcialmx <- D(exp1, "mx1")
  #parcialmy <- D(exp1, "my")
  parcialsx <- D(exp1, "sx1")
  #parcialsy <- D(exp1, "sy")
  #parcialrho <- D(exp1, "rho")
  
  gradiente <- c(eval(parcialmx),eval(parcialsx))
  
  return(gradiente)
  
}
###############################
fcn <- function(p, x, z, fcall, jcall){
  (datos$V2 - do.call("fcall", c(list(x = datos$V1), as.list(p))))
}

fcn.jac <- function() {
  -do.call("jcall", c(list(x = x), as.list(p)))
}  

#fcn.jac <- function(p, x, N, fcall, jcall) {
#  -do.call("jcall", c(list(x = x), as.list(p)))
#}  

###############################################################################
##############################################################################

#----------------- using nlsLM() -------------------------
inicio <- list(mx1=0.2, sx1=1)
cotaSup <- c(3,5)
cotaInf <- c(-3,-5)
x <- datos$V1
z <- datos$V2

#form <- paste("z ~",modelo)

ajuste <- nlsLM(paste("z ~",modelo),
                data=datos,
                start=inicio,
                jac=generaGradiente(coef,x,z),
                #par=list(mx=5,my=3,sx=1,sy=1,rho=0.8),
                upper=cotaSup,
                lower=cotaInf,
                control=nls.lm.control(maxiter=70,nprint=0),#ptol, ftol, factor...
                trace=T
                #jac=as.expression(calculaGradiente()) 
                )

summary(ajuste)
#ajuste$call



#--------------- using nls.lm() ----------------

out <- nls.lm(par = inicio, lower=cotaInf, upper=cotaSup,
              fn = fcn, jac = fcn.jac,
              fcall = f, jcall = j,
              T = T, N = N, control = nls.lm.control(nprint=1))


#################### MAIN ########################
modelo_expresion <- generaFuncion(1)#devuelve el modelo como cadena de texto
gradiente_expresion <- generaGradiente(modelo_expresion)
#is.formula(gradiente_expresion[1])

