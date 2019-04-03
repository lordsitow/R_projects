
GausianJordan<-function(m){
  if(det(m)==0){
    return("Cant determinant is 0")
  }
  ####  Preparation
  rang<-dim(m)
  m<-cbind(m,diag(rang[1]))
  print(m)
  ####   For loops to get 0 below diag
  for(i in 1:rang[1]){
    if(m[i,i]==0){       #####    check 0 in diag
      for(k in (i+1):rang[1]){
        if(m[k,i]!=0)
          temp <- m[i,] 
        m[i,]<-m[k,]
        m[k,]<-temp
      }
    }
    
    
    m[i,]<-m[i,]/m[i,i]
    
    for(j in (i+1):rang[1]){
      if(j<=rang[1]&&j!=i){
        m[j,]<-m[j,]-m[i,]*m[j,i]
      }
    }
  }

  
  print(m)    #     0's below diag
  ####   0 over diagonal
  for(j in rang[1]:2){
    for(i in (i-1):1){
      m[i,]<-m[i,]-m[i,j]*m[j,]
    }
  }
  
  
  return(m)
}
#######################
rank<-function(m){
  if(det(m)==0){
    return("Cant determinant is 0")
  }
  a<-dim(m)
  a<-a[2]
  ####  Preparation
  rang<-dim(m)
  m<-cbind(m,diag(rang[1]))
  ####   For loops to get 0 below diag
  for(i in 1:rang[1]){
    if(m[i,i]==0){       #####    check 0 in diag
      a<-a-1
      for(k in (i+1):rang[1]){
        if(m[k,i]!=0)
          temp <- m[,i] 
        m[,i]<-m[,k]
        m[,k]<-temp
      }
    }
    
    
    m[i,]<-m[i,]/m[i,i]
    
    for(j in (i+1):rang[1]){
      if(j<=rang[1]&&j!=i){
        m[j,]<-m[j,]-m[i,]*m[j,i]
      }
    }
  }
  return(a)
}
########################
Matr<-matrix(c(1,2,2,4),2,2)
print(rank(Matr))
print(GausianJordan(Matr))
