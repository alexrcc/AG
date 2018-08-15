#' Función SIR() 
#' @param p : tiempo incial y final
#' @param m : amplitud del intervalo en el tiempo
#' @param n : numero de iteraciones
#' @return dataframe con los valores de la función SRI
#' 
AG<-function(p,m,n){
  
  #'Matriz de resultados (tiempo, Sanas, Infectadas, Recuperadas)
  w <- matrix(0,nrow = n,ncol = (length(p)*2)+5)
  band=0
  
  for(i in 1:5){
    w[1,i] <- p[i] 
  }
  i=6;
  for(j in 1:5){
    w[1,i] <- m[j]
    i=i+1
  }
  suma=0;
  for(j in 1:5){
    suma = suma + w[1,j] 
  }
  producto=1;
  for(z in 6:10){
    producto = producto * w[1,z] 
  }
  
  w[1,11]=suma
  w[1,12]=producto
  if(suma==36&&producto==360)
    w[1,15]=1
  else
    if((suma>30&&suma<40)&&(producto>300&&producto<450)){
      w[1,15]=2
    }
  
  producto2 = 1;
  for(j in 1:5){
    producto2 = producto2 * w[1,j] 
  }
  suma2 = 0;
  for(z in 6:10){
    suma2 = suma2 + w[1,z] 
  }
  
  w[1,14]=producto2
  w[1,13]=suma2
  
  if(suma2==36&&producto2==360)
    w[1,15]=1
  else
    if((suma2>30&&suma2<40)&&(producto2>300&&producto2<450)){
      w[1,15]=2
    }
  
  
  for(i in 2:n){
    band=0
    
    N<-sample(1:4,1);
    h1<-c(p[c(1:N)],m[c((N+1):5)]);
    h2<-c(p[c((N+1):5)],m[c(1:N)]);
    p<-h1
    m<-h2
    
    for(j in 1:5){
      w[i,j] <- h1[j] 
    }
    aux=6;
    for(j in 1:5){
      w[i,aux] <- h2[j]
      aux=aux+1
    }
    suma=0;
    for(j in 1:5){
      suma = suma + w[i,j] 
    }
    producto=1;
    for(z in 6:10){
      producto = producto * w[i,z] 
    }
    if(suma==36&&producto==360){
      w[i,15]=1
      
    }else
      if((suma>30&&suma<40)&&(producto>300&&producto<450)){
        w[i,15]=2
      }
    
    w[i,11]=suma
    w[i,12]=producto
    
    producto2 = 1;
    for(j in 1:5){
      producto2 = producto2 * w[i,j] 
    }
    suma2 = 0;
    for(z in 6:10){
      suma2 = suma2 + w[i,z] 
    }
    
    w[i,14]=producto2
    w[i,13]=suma2
    
    if(suma2==36&&producto2==360)
      w[i,15]=1
    else
      if((suma2>30&&suma2<40)&&(producto2>300&&producto2<450)&&w[i,15]!=1){
        w[i,15]=2
      }
    
    
  }
  return(w)
  
}

