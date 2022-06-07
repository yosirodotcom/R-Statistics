setwd("C:/Users/YOSIRONADI/Dropbox/R Programming/R-Statistics/MatKul")

pacman::p_load(pacman, expm, matlib, MASS)

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
   
   
   
 }
inventori(k=10,x=2,lambd=1.4875)

library(expm)

A <- matrix(c(1/6,4/6,0,1/6,2/6,0,2/6,0,3/6,2/6,1/6,3/6,0,0,2/6,2/6), nrow=4)
C <- matrix(c(0,2/3,1,1/3), nrow=2)
D <- matrix(c(15/25,15/25,0,0,0,0,
              10/25,0,15/25,0,0,0,
              0,10/25,0,15/25,0,0,
              0,0,10/25,0,15/25,0,
              0,0,0,10/25,0,15/25,
              0,0,0,0,10/25,10/25), nrow=6)

normalization <- function(mat, t, r){
        
        k <- as.integer(round(r*t+5*sqrt(r*t)+1))
        print(k)
        B <- (exp(-r*t)*((r*t)^0)/factorial(0))*mat%^%0
        
        for(i in 1:k){
                B <- B + (exp(-r*t)*((r*t)^i)/factorial(i))*mat%^%i
        }
        
        print(B)
        
}

normalization(mat = D,t = 1, r = 25)
