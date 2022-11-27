## FUNCTIONS

rlaplace <- function(n, mu = 0, b = 1) {
  r <- runif(n, 0, 1)
  r <- mu - b * sign(r - 0.5) * log(1 - 2 * abs(r - 0.5))
  return(r)
}

perturb <- function(x, epsilon, breaks) {
  
  h <- hist(x, breaks = breaks, plot = FALSE)
  counts <- h$counts
  p_hat <- h$density
  
  # add noise to each bin
  counts <- counts + rlaplace(length(counts), 0, 8 / epsilon^2)
  # count 0 or max
  counts[counts < 0] <- 0 
  q_hat <- counts / sum(counts) * length(counts)
  
  # return $p_hat and $q
  return(list(p_hat = p_hat, q_hat = q_hat))
}

ePDF <- function(x, hist_prob, breaks) {
  # An m by n matrix to compare x with break points
  breaks_matrix <- matrix(breaks, nrow = length(x), ncol = length(breaks), byrow = TRUE)
  
  # An n-dimensional vector to store which bin each x falls into
  bins <- rowSums(breaks_matrix < x)
  
  # Assign the probability of each bin to the corresponding x
  d <- hist_prob[bins]
  
  # if x is outside the range of breaks, then d = 0
  d[is.na(d)] <- 0 
  
  return(d)
}

mise <- function(original, approx, start, end) {
  integrand <- function(x) (original(x) - approx(x))^2
  return(integrate(integrand, start, end, subdivisions = 1000, stop.on.error = FALSE)$value)
}

sim_mise <- function(S, eps, m, n, rdist, ddist) {
  # S: number of simulations
  # eps: epsilon
  # m: number of bins
  # n: sample size
  # rdist: random distribution
  # ddist: true distribution
  breaks <- (1:(m + 1) - 1) / m
  
  p_mises <- rep(NA, S)
  q_mises <- rep(NA, S)
  for (i in 1:S) {
    # generate data
    x <- rdist(n)
    # get densities
    res <- perturb(x, eps, breaks)
    # compute MISE
    p_mises[i] <- mise(ddist, function(x) ePDF(x, res$p_hat, breaks), 0, 1)
    q_mises[i] <- mise(ddist, function(x) ePDF(x, res$q_hat, breaks), 0, 1)
  }
  return(list(p_mises = mean(p_mises), q_mises = mean(q_mises)))
}

library(readr)
daily_steps <- read_csv("data/daily_steps.csv", 
                        col_types = cols(date = col_date(format = "%Y-%m-%d"),
                                         value = col_integer()))


x <- daily_steps$value
# Number of bins
m      <- 10
breaks <- (0:m / m) * (max(x) - min(x)) + min(x)

# Let's see the distrubution
hist(x, breaks=breaks, main="Djem's daily steps after his arrival in Rome",
     col = 'wheat1')

eps <- 1
res <- perturb(x, eps, breaks)

# The values of the bins
barplot(res$p_hat, col = 'wheat1', main = 'Values of p-hat', space=0)
barplot(res$q_hat, col = 'wheat2', main = 'Values of q-hat', space=0)


