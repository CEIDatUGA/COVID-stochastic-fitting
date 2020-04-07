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

# log_g_c
par(mfrow = c(1,3))
a <- 2.5
b <- 0.5
gs <- rgamma(100000, a, b)
hist(1/gs*4, breaks = 500, xlim = c(0, 4))
mu <- a/b
sigma <- sqrt(a/(b^2))
mul <- log(mu) - 0.5*log((sigma/mu)^2 + 1)
sigmal <- sqrt(log((sigma/mu)^2) + 1)
norms <- rnorm(100000, mul, sigmal)
hist(1/exp(norms)*4)
m1 <- mean(1/exp(norms)*4)
s1 <- sd(1/exp(norms)*4)
hist(rnorm(100000, m1, s1))

