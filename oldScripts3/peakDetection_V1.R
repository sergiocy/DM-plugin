

estimaParamInicio <- function(DATOS_IN, OFFS, MAXIMO, txtTiempoRuta){
  #MAXIMO <- max(DATOS_IN); txtTiempoRuta <-  txtTiempoEstimaPicos
  ESTIMADORES_I <- c()
  R <- 5
  
  
  t_estimaciones <- proc.time()
  
  
  datosImg <- as.matrix(DATOS_IN)
  fil <- 1 #fil<-6 #fil<-nrow(datosImg)-5   #DATOS_IN[36,37] #fil<-27;col<-29
  #col<-6 #col<-ncol(datosImg)-5
  while(fil <= nrow(datosImg)){
    col <- 1 
    while(col <= ncol(datosImg)){
      
      centro <- as.numeric(datosImg[fil,col])
      entorno <- c()#is.numeric(centro) #which(datosImg==489722,arr.ind=TRUE)
      
      if(fil>R & col>R & fil<=(nrow(datosImg)-R) & col<=(ncol(datosImg)-R)){
        
        entorno <- datosImg[(fil-R):(fil+R),(col-R):(col+R)] 
        if(max(entorno)==centro & 
             as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[1]==fil &
             as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[2]==col){
          ESTIMADORES_I <- rbind(ESTIMADORES_I,c(col,fil,4,4,0,(centro-OFFS)/(MAXIMO-OFFS)))
        }
        
      }else{
        #esquina superior-izda...
        if(fil<=R & col<=R){
          entorno <- datosImg[1:(fil+R),1:(col+R)]
          if(max(entorno)==centro & 
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[1]==fil &
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[2]==col){
            ESTIMADORES_I <- rbind(ESTIMADORES_I,c(col,fil,4,4,0,(centro-OFFS)/(MAXIMO-OFFS)))
          }
        }
        
        #marco superior...
        if(fil<=R & col>R & col<=(ncol(datosImg)-R)){
          entorno <- datosImg[1:(fil+R),(col-R):(col+R)]
          if(max(entorno)==centro & 
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[1]==fil &
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[2]==col){
            ESTIMADORES_I <- rbind(ESTIMADORES_I,c(col,fil,4,4,0,(centro-OFFS)/(MAXIMO-OFFS)))
          }  
        }
        
        #esquina superior-dcha...
        if(fil<=R & col>(ncol(datosImg)-R)){
          entorno <- datosImg[1:(fil+R),(col-R):(ncol(datosImg))]
          if(max(entorno)==centro & 
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[1]==fil &
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[2]==col){
            ESTIMADORES_I <- rbind(ESTIMADORES_I,c(col,fil,4,4,0,(centro-OFFS)/(MAXIMO-OFFS)))
          }  
        }
        
        #marco derecho...
        if(fil>R & fil<=(nrow(datosImg)-R) & col>(ncol(datosImg)-R)){
          entorno <- datosImg[(fil-R):(fil+R),(col-R):(ncol(datosImg))]
          if(max(entorno)==centro & 
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[1]==fil &
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[2]==col){
            ESTIMADORES_I <- rbind(ESTIMADORES_I,c(col,fil,4,4,0,(centro-OFFS)/(MAXIMO-OFFS)))
          }  
        }
        
        #esquina inferior-dcho...
        if(fil>(nrow(datosImg)-R) & col>(ncol(datosImg)-R)){
          entorno <- datosImg[(fil-R):(nrow(datosImg)),(col-R):(ncol(datosImg))]
          if(max(entorno)==centro & 
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[1]==fil &
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[2]==col){
            ESTIMADORES_I <- rbind(ESTIMADORES_I,c(col,fil,4,4,0,(centro-OFFS)/(MAXIMO-OFFS)))
          }  
        }
        
        #marco inferior...
        if(col>R & fil>(nrow(datosImg)-R) & col<=(ncol(datosImg)-R)){
          entorno <- datosImg[(fil-R):(nrow(datosImg)),(col-R):(col+R)]
          if(max(entorno)==centro & 
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[1]==fil &
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[2]==col){
            ESTIMADORES_I <- rbind(ESTIMADORES_I,c(col,fil,4,4,0,(centro-OFFS)/(MAXIMO-OFFS)))
          }  
        }
        
        #esquina inferior-izda...
        if(col<=R & fil>(nrow(datosImg)-R)){
          entorno <- datosImg[(fil-R):(nrow(datosImg)),1:(col+R)]
          if(max(entorno)==centro & 
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[1]==fil &
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[2]==col){
            ESTIMADORES_I <- rbind(ESTIMADORES_I,c(col,fil,4,4,0,(centro-OFFS)/(MAXIMO-OFFS)))
          }  
        }
        
        #marco izda...
        if(fil>R & col<=R & fil<=(nrow(datosImg)-R)){
          entorno <- datosImg[(fil-R):(fil+R),1:(col+R)]
          if(max(entorno)==centro & 
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[1]==fil &
               as.vector(which(datosImg==max(entorno),arr.ind=TRUE))[2]==col){
            ESTIMADORES_I <- rbind(ESTIMADORES_I,c(col,fil,4,4,0,(centro-OFFS)/(MAXIMO-OFFS)))
          }  
        }
        
      }#...fin del else...
      
      
      col <- col+1
    }
    fil <- fil+1
    #cat(fil,"   ",col,"\n")
  }
  
  tiempo <- proc.time()-t_estimaciones #typeof(proc.time())
  write.table(as.matrix(tiempo),txtTiempoRuta)
  
  return(ESTIMADORES_I)
  
}#...fin de la funcion estimaParamInicio()...