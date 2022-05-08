pacman::p_load(pacman, expm)

inventori <- function(k,x,lambd){
   k1 <- k+1
   P <- matrix(0, nrow = k1-x, ncol = k1-x)
   first <- x-1
   for(i in 1:(k-1)){
     for(j in 1:(k-1)){
       logic1 <- ((first+i)-(first+j)) >= 0
       logic2 <- j != (k-1)
       if(logic1 & logic2){
         P[i,j] <- dpois(((first+i)-(first+j)),lambd)
       }
     }
   }
   for(i in 1:(k-1)){
     for(j in 1:(k-1)){
       logic1 <- j == k-1
       logic2 <- i != k-1
       if(logic1 & logic2){
         P[i,j] <- ppois((first+i-x), lambd, lower.tail = FALSE)
       }
     }
   }
   
   P[k-1,k-1] <- (dpois(0,lambd))+(ppois((k-2),lambd,lower.tail = FALSE))
   
   print(P)
   
   print("Matrix Occupancy")
   
   M <- matrix(0, nrow = k1-x, ncol = k1-x)
   
   for(i in 0:(k-x-1)){
     Z <- P%^%i
     M <- M+Z
   }
   print(M)
   
   print(colSums(P))
   
 }
inventori(k=10,x=2,lambd=1.4875)