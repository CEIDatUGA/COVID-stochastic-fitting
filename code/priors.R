# priors.R

rev_logistic <- function(x) {
  log((1/x)-1)
}

logistic <- function(x) {
  1 / (1 + exp(x))
}

# log_beta_s
ntot <- 106000000
mu <- log(0.658/ntot)
sigma <- 0.2
norms <- rnorm(10000, mu, sigma)
params <- exp(norms) * ntot
hist(params, main = "beta_s")


# beta_reduce
mu <- rev_logistic(0.65)  # from literature
sigma <- 0.5  # assumptions
norms <- rnorm(10000, mu, sigma)
ps <- logistic(norms)
hist(ps)

