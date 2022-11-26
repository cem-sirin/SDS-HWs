set.seed(123) # for reproducibility
n <- 100      # number of observations
m <- 50       # number of bins

options(scipen = 999, digits = 5)

x <- rbeta(n, 10, 10)
hist(x, breaks = m, xlim = c(0,1), col = "lightblue", main = "Beta(10,10) distribution")

rlaplace <- function(n, mu = 0, b = 1) {
  r <- runif(n, 0, 1)
  r <- mu - b * sign(r - 0.5) * log(1 - 2 * abs(r - 0.5))
  return(r)
}

perturb <- function(x, epsilon, n_bins) {
  n <- length(x)
  bin <- cut(x, n_bins)
  bin <- as.numeric(bin)
  # count the number of elements in each bin
  count <- table(bin)
  p_hat <- count / n

  # add noise to each bin
  count <- count + rlaplace(n_bins, 0, 8 / epsilon^2)
  # count 0 or max
  count[count < 0] <- 0
  q <- count / sum(count)

  # return $p_hat and $q
  return(list(p_hat = p_hat, q = q))
}

eps <- 0.1

n_bins <- m

n <- length(x)
bin <- cut(x, n_bins)
bin <- as.numeric(bin)
# count the number of elements in each bin


count <- table(factor(bin, levels = 1:n_bins))
p_hat <- count / n
count
# add noise to each bin
count <- count + rlaplace(n_bins, 0, 8 / 1^2)

res <- perturb(x, eps, m)
# print res$p_hat and res$q
res$p_hat
res$q

# plot the empircal histogram and the privatized histogram
barplot(res$p_hat, names.arg = 1:m, col = "lightblue", ylim = c(0, max(res$p_hat) * 1.1), main = "Empirical histogram")

# plot the perturbed histogram
barplot(res$q, col = "lightblue", ylim = c(0, max(res$q) + 0.01), main = "Perturbed Histogram")