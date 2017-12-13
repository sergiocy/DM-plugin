library(minpack.lm)

#fonction pour simulation des données de conductivité hydraulique
f.k <- function(h, alp, n, L, ksat){
  ksat <- 0.000108
  k.mod <-expression(ksat+((((1+(alp*h)^n)^(n-(1/n))-((alp*h)^(n-1)))^2)/((1+(alp*h)^n)^((n-(1/n))*(L+2)))))
  eval (k)
  }

>  > #fonction d'aide pour le gradient analytique
  >  > j.k <- function(h, alp, n, L, ksat){
    >  >  ksat <- 0.000108
    >  >  k.mod <-expression(ksat+((((1+(alp*h)^n)^(n-(1/n))-((alp*h)^(n-1)))^2)/((1+(alp*h)^n)^((n-(1/n))*(L+2)))))
    >  >
      >  >  c(eval(D(k.mod, "alp")), eval(D(k.mod, "n")), eval(D(k.mod, "L")))
    >  > }


>  > h <- hydro$h
>  > # Paramètre surestimé pour démarer la simulation
  >  > p.k <- c(alp = 0.04, n = 1.6, L = 0.5)
>  > ## get data with noise
  >  > k <- hydro$k
>  > ## plot the data to fit
  >  > par(mfrow=c(2,1), mar = c(3,5,2,1))
>  > plot(h, k, bg = "black", cex = 0.5, main="data", 
          > ylab=expression(K(h)), log="y")
>  > ## define a residual function
  >  > fcn.k <- function(p.k, h, k, fcall, jcall)
    >  >     (k - do.call("fcall", c(list(h = h), as.list(p.k))))

>  > ##define analytical expression for the gradient
  >  > fcn.jac.k <- function(p.k, h, k, fcall, jcall)
    >  >     -do.call("jcall", c(list(h = h), as.list(p.k)))
>  > ##starting values (alp = 0.04, n = 1.6, L = 0.5)
  >  > guess.k <- c(alp = 0.04, n = 1.6, L = 0.5)
>  > ## to use an analytical expression for the gradient found in fcn.jac
  >  > ## uncomment jac = fcn.jac
  >  > out.k <- nls.lm(par = guess.k, fn = fcn.k, jac = fcn.jac.k,
                                      fcall = f.k, jcall = j.k,
                                      h = h, k = k, control = nls.lm.control(gtol = 1, 
                                                                                epsfcn = 0, factor = 100,
                                                                                maxiter = 100, nprint = 50))
>  > ## get the fitted values
  >  > N1.k <- do.call("f.k", c(list(h = h), out.k$par))
>  > ## add a blue line representing the fitting values to the plot of data
  >  > lines(h, N1.k, col="blue", lwd=2)
>  > summary(out.k)
>  >