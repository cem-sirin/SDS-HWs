# make sure that you're working directory (wd) is the homework2 folder
getwd() # this is you're current wd

library(dplyr)
load("hw2_data.RData")
load("neuro.aal.rda")

asd <- lapply(asd_sel, function(x) as.data.frame(x))
td  <- lapply(td_sel, function(x) as.data.frame(x))

d <- asd$caltech_0051472
d <- asd$trinity_0050234


asd



# Correlation assuming 0 mean
my_cor <- function(x) {
  x <- as.matrix.data.frame(x)
  x <- t(x) %*% x
  x <- x / sqrt(diag(x) %o% t(diag(x)))
  return(x)
}

neuro.aal <- levels(neuro.aal$desc$label)
superregion <- sapply(neuro.aal, function(x) strsplit(x, "_")[[1]][1])
superregion

R.asd <- lapply(asd_sel, cor)
R.td  <- lapply(td_sel, cor)

# Take the mean of large list R.asd
R.asd.mean <- matrix(0, nrow = 116, ncol = 116)
for (r in R.asd) {
  R.asd.mean <- R.asd.mean + r
}

R.asd.mean <- R.asd.mean / length(R.asd)

# Take the mean of large list R.td
R.td.mean <- matrix(0, nrow = 116, ncol = 116)
for (r in R.td) {
  R.td.mean <- R.td.mean + r
}

R.td.mean <- R.td.mean / length(R.td)

sum(abs(R.asd.mean) > 0.5)
sum(abs(R.td.mean)  > 0.5)

neuro.aal$desc

asd <- lapply(asd_sel, function(x) as.data.frame(scale(as.data.frame(x))))
class(asd$caltech_0051472)

library(data.table)
d <- as.data.frame(rbindlist(asd))
class(d)
