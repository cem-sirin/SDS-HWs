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

set.seed(123)
M <- 10^6
ncols <- 8
xx <- runif(M)
res <- rep(NA, M)

# find the first true for each row
i <- 0
while (any(is.na(res))) {
    na_idx <- which(is.na(res))
    y <- matrix(runif(length(na_idx) * ncols), nrow = length(na_idx))
    x <- xx[na_idx]
    res[na_idx] <- joe(y < x, ncols) + i * ncols
    i <- i + 1
    ncols <- ncols * 2
}

sum(is.na(res))
end <- Sys.time()
end - beg

########################

