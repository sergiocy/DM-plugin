
library(minpack.lm)

datos <- as.data.frame(matrix(c(-2,0,-1,1,0,2,1,1,2,0), nrow=5, ncol=2, byrow=TRUE))

calculaGradiente <- function(paramet,x,z){
  #exp1 <- modelo
  #paramet <- inicio
  exp1 <- parse(text=modelo)
  #exp1 <- expression(( 1/(2*pi*sx*sy*sqrt(1-rho^2)) )*( exp( (-1/(2*(1-rho^2)))*(((x-mx)^2/sx^2)+((y-my)^2/sy^2)-(2*rho*(x-mx)*(y-my)/(sx*sy))) ) ))
  #expression(x/(y + exp(z)))
  #eval(exp1)
  #mx1 <- 1; sx1 <- 1
  #mx1 <- as.numeric(paramet[1])
  #sx1 <- as.numeric(paramet[2])
  #substitute(exp1,paramet)
  
  parcialmx <- D(exp1, "mx1")
  #parcialmy <- D(exp1, "my")
  parcialsx <- D(exp1, "sx1")
  #parcialsy <- D(exp1, "sy")
  #parcialrho <- D(exp1, "rho")
  
  -c(eval(parcialmx,paramet),eval(parcialsx,paramet))
  
  #return(gradiente)
}
#calculaGradiente(inicio,x,z)
    
#modelo <- expression( (1/sqrt(2*pi))*( exp() ) )
modelo <- paste("(1/(",paste("sx1"),"*sqrt(2*pi)))*( exp((-1/2)*(((",paste("x"),"-",paste("mx1"),")/",paste("sx1"),")^2)) )")

inicio <- list(mx1=0.2, sx1=1)
cotaSup <- c(3,5)
cotaInf <- c(-3,-5)
x <- datos$V1
z <- datos$V2

#form <- paste("z ~",modelo)

ajuste <- nlsLM(formula=paste("z ~",modelo),
                data=datos,
                start=inicio,
                jac=calculaGradiente,
                #par=list(mx=5,my=3,sx=1,sy=1,rho=0.8),
                upper=cotaSup,
                lower=cotaInf,
                control=nls.lm.control(maxiter=70,nprint=0),#ptol, ftol, factor...
                #x=datos$V1,
                #z=datos$V2,
                trace=T
                #jac=as.expression(calculaGradiente()) 
                )

summary(ajuste)
