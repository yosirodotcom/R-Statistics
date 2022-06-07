# Inverse Umum

pacman::p_load(matlib, MASS) #https://cran.r-project.org/web/packages/matlib/vignettes/ginv.html

A <- matrix(c(3,-4,1,8,1,1,1,1,0), nrow = 3)
matlib::Ginv(A, fractions = TRUE)

# Cek apakah Matrix Hermite
B <- matrix(c(1,2,1,2,3,1,1,1,0), nrow = 3)
echelon(B)
isSymmetric(B)

# Moore Penrose
C <- matrix(c(1,0,-1,0,2,4,-1,2,5,1,2,3), nrow = 3)
Cmp <- matlib::MoorePenrose(C)
MASS::fractions(Cmp)

D <- matrix(c(1,2,0,-1,-1,1), nrow = 3)
(D%*%Ginv(D))%*%D%*%Ginv(D)

E <- matrix(c(1,4,7,2,5,8,3,6,9),nrow=3)
MASS::fractions(Ginv(E))
Inverse(E)
echelon(E)

e <- MASS::fractions((matrix(c(-5/3,4/3,0,2/3,-1/3,0,0,0,0),nrow=3))%*%(matrix(c(1,0,0,0,1,6,-1,2,12),nrow=3)))
E%*%e%*%E
