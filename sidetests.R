# Set seed and generate Beta(10,10) distruution
set.seed(123)
x <- rbeta(200, 10, 10)
hist(x, breaks = 20)

rlaplace <- function(n, mu = 0, b = 1) {
  r <- runif(n, 0, 1)
  r <- mu - b * sign(r - 0.5) * log(1 - 2 * abs(r - 0.5))
  return(r)
}

perturb <- function(x, epsilon, m) {
  n <- length(x)
  bin <- cut(x, m)
  bin <- as.numeric(bin)
  bin <- bin + rlaplace(n, 0, epsilon / n)
  hist(bin, breaks = m)
}

## Probability of a Beta(10,10) being between 0.4 and 0.6
pbeta(0.6, 10, 10) - pbeta(0.4, 10, 10)

## Probability of a Beta(10,10) being between in each bin
pbeta(3:5 / 7, 10, 10)

m <- 20

# Real probability of a Beta(10,10) being in each bin
pbeta(1:m / m, 10, 10) - pbeta((1:m - 1) / m, 10, 10)

real_hist <- function(pdist, m) {
  pdist(1:m / m) - pdist((1:m - 1) / m)
}

pdist <- function(x) pbeta(x, 10, 10)
yo <- real_hist(pdist, m)
yo
