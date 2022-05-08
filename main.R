# Demographic https://github.com/timriffe/DemoTools

#  *** Karup-King Interpolation  ***

x <- c(0.2, 0.4, 0.6, 0.8)
pengali_1 <- ((x^3)-(x^2))/2
pengali_2 <- (x-(2*(x^2)-(x^3)))/2

pacman::p_load('tidyverse',
               'magrittr',
               'datapasta', 
               'esquisse')