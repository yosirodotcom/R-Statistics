pacman::p_load(pacman, dplyr, skedastic)

df <- as_tibble(skedastic::T_alpha)

head(df[,1])

ncol <- length(names(df))


names(df)<-sprintf("x%d",0:ncol)
colnames(df)[1] <- "y"

names(df)
