beg <- Sys.time()

joe <-  function(x, ncols) {
    y <- which(x)
    nR <- nrow(x)
    myR <- y %% nR
    myR[myR==0] <- nR
    myNames <- (1:ncols)[ceiling(y/nR)]
    myCols <- which(!(duplicated(myR)))
    myNames[myCols][order(myR[myCols])]

    res <- rep(NA, nR)
    res[myR[myCols]] <- myNames[myCols]
    res
}

M <- 10^4
ncols <- 5
xx <- runif(M)
res <- rep(NA, M)

# find the first true for each row
i <- 0
while (any(is.na(res)) && i < 1000) {
    na_idx <- which(is.na(res))
    y <- matrix(runif(length(na_idx) * ncols), nrow = length(na_idx))
    x <- xx[na_idx]
    res[na_idx] <- joe(y < x, ncols) + i * ncols
    i <- i + 1
}

end <- Sys.time()
end - beg

########################
sum(is.na(res))
mean(res)

i <- 0
print(i)
na_idx <- which(is.na(res))
y <- matrix(runif(length(na_idx) * ncols), nrow = length(na_idx))
x <- x[na_idx]
res[na_idx] <- joe(y < x, ncols) + i * ncols
i <- i + 1
i
res
res[is.na(res)]

