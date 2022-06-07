pacman::p_load(pacman, expm, matlib, MASS)
# Matrix Occupancy

occupancy <- function(x, t){
        M <- x%^%0
        for(i in 1:t){
                Z <- x%^%i
                M <- M+Z
        }
        print(M)
}


A <- matrix(c(0.97,0.008,0.02,0.1,0.3,0.982,0,0,0,0.01,0.975,0,0,0,0.005,0.99),nrow=4)


occupancy(A, 100)



# Distribusi Stationer


stationary <- function(x){
        r=eigen(x)
        lvec=ginv(r$vectors)
        pi_eig<-lvec[1,]/sum(lvec[1,])
        print(pi_eig)
}

stationary(A)