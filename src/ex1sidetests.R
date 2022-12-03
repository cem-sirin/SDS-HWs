firstcrosss <-  function(x) {
    # x: a matrix of boolean values
    Z  <- which(x)
    n  <- nrow(x)
    m  <- ncol(x)

    # Ys crossing which X
    crossx   <- (Z - 1) %% n + 1

    # Which Y's crossing
    crossy   <- (1:m)[ceiling(Z / n)]

    # index of first crossing
    idx <- which(!(duplicated(crossx)))

    # na vector
    res <- rep(NA, n)
    res[crossx[idx]] <- crossy[idx]
    res
}

set.seed(123)
M <- 10^5
ncols <- 8

# Initialize T as a NA vector
T <- rep(NA, M)

# Generate X ~ Uniform(0,1)
X <- runif(M)

# find the first true for each row
s <- 0
while (any(is.na(T))) {
    # Indexes where T is NA
    na_idx <- which(is.na(T)) 

    # Filter X to only include the NA values
    XF <- X[na_idx]

    # Generate Y ~ Uniform(0,1)
    Y <- matrix(runif(length(na_idx) * ncols), nrow = length(na_idx))
    
    T[na_idx] <- firstcrosss(Y < XF) + s

    s <- s + ncols
    ncols <- ncols * 2
}


h <- hist(T, breaks = 0:25, plot = FALSE)
h

# count the number of times each value occurs
table(T)[1:30] / M

errors
