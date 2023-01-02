# make sure that you're working directory (wd) is the homework2 folder
getwd() # this is you're current wd

library(dplyr)
load("hw2_data.RData")

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


tanh(R.td.mean)

