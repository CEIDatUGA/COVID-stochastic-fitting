# run-particle-mcmc.R


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(pomp)


# Load the pomp object ----------------------------------------------------

pomp_object <- readRDS("../output/covid-ga-pomp-object.RDS")


# Define summary statistic (probes) functions -----------------------------

get_stat_times <- function(obs_cases) {
  x <- obs_cases
  ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}
  stat_times <- ma(diff(log(x)), n = 5)
  d0 <- min(which(stat_times > 0.1))
  suppressWarnings(
    d1 <- d0 + min(which(stat_times[d0:length(stat_times)] < -0.01))
  )
  return(c(d0 = d0, d1 = d1))
}

ds <- get_stat_times(as.numeric(pomp_object@data))
d0 <- ds["d0"]
d1 <- ds["d1"]
if(is.infinite(d1)) d1 <- ncol(pomp_object@data) - 1
d2 <- ncol(pomp_object@data)

max1 <- eval(parse(text = paste0("function(x){ max(log(x[1, 1:(", d0, "-1)] + 10))  }")))
max2 <- eval(parse(text = paste0("function(x){ max(log(x[1,", d0, ":", d1, "] + 10))  }")))
max3 <- eval(parse(text = paste0("function(x){ max(log(x[1,(", d1, "+1):", d2, "] + 10))  }")))
maxall <- function(x){ max(log(x[1,])) }
maxday <- function(x){ which.max(x[1,]) }

cumi1 <- eval(parse(text = paste0("function(x){ sum(log(x[1, 1:(", d0, "-1)] + 10))  }")))
cumi2 <- eval(parse(text = paste0("function(x){ sum(log(x[1, ", d0, ":", d1, "] + 10))  }")))
cumi3 <- eval(parse(text = paste0("function(x){ sum(log(x[1, (", d1, "+1):", d2, "] + 10))  }")))

exp1 <- eval(parse(text = paste0("function(x) { max(x[1, ]) / which.max(x[1, ]) -", d0, "}")))
regcoef <- function(x) { as.numeric(coef(lm(x[1,] ~ seq_along(x[1,])))[2]) }


# Define the prior density ------------------------------------------------     

prior_dens <- Csnippet(
  "
  lik = dnorm(beta_d,log(2e-7), 0.8, 1) +
    dnorm(beta_u, log(5e-8), 0.4, 1) +
    dunif(beta_red_factor, 0.01, 1, 1) +
    dunif(detect_frac_0, 0.01, 0.6, 1) +
    dnorm(beta_e, log(5e-8), 0.4, 1) +
    dlnorm(gamma_u, log(0.5), 1, 1) +
    dlnorm(gamma_d, log(0.5), 1, 1);
  if (!give_log) lik = exp(lik);
"
)

# dunif(beta_red_factor, 0.01, 1, 1) +
#   dlnorm(gamma_u, log(0.5), 0.2, 1) +
#   dlnorm(gamma_d, log(0.5), 0.2, 1) +
#   dunif(detect_frac_0, 0.01, 0.6, 1);


# Run ABC-MCMC ------------------------------------------------------------     

n <- 1   # number of mcmc chains

estpars <- c("beta_d", "beta_u",  "beta_e", "beta_red_factor", 
             "gamma_u", "gamma_d", "detect_frac_0", "theta") 

# Set noise level for parameter random walk for proposals
rw.sd <- rep(0.1, length(estpars))
names(rw.sd) <- estpars
rw.sd["theta"] <- 1
# rw.sd <- c(beta_d = 0.05, beta_u = 0.05, beta_e = 0.05,
#            beta_red_factor = 0.005, gamma_u = 0.1, gamma_d = 0.1,
#            detect_frac_0 = 0.005, theta = 0.5)



plist <- list(
  max1, max2, max3, maxall,
  cumi1, cumi2, cumi3, exp1, regcoef
)
psim <- probe(pomp_object, probes = plist, nsim = 1000)
plot(psim)
scale.dat <- apply(psim@simvals, 2, sd)

out_abc <- abc(
  pomp(
    pomp_object,
    dprior = prior_dens,
    paramnames = c("beta_d", "beta_u", "beta_e", "beta_red_factor", 
                   "gamma_u", "gamma_d", "detect_frac_0", "theta")
  ),
  Nabc = 100000,
  epsilon = 5,
  scale = scale.dat,
  proposal = mvn.diag.rw(rw.sd),
  probes = plist,
  verbose = TRUE
)

# plot(out_abc)


# saveRDS(object = out_mcmc, file = "../output/pomp-pmcmc-object.RDS")

# chain <- as.data.frame(out_abc@traces)[100000:200000,]
# chain <- chain[seq(1, nrow(chain), by = 10),]

# par(mfrow = c(4,2))
# plot(exp(chain$beta_d)*10600000, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(beta[d]))
# plot(exp(chain$beta_u)*10600000, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(beta[u]))
# plot(exp(chain$beta_e)*10600000, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(beta[e]))
# plot(chain$beta_red_factor, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(xi))
# plot(chain$gamma_d, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(gamma[d]))
# plot(chain$gamma_u, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(gamma[u]))
# plot(chain$detect_frac_0, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = "detect_frac_0")
# plot(chain$theta, type = "l", bty = "n",
#      xlab = "MCMC iteration", ylab = expression(theta))


chain <- as.data.frame(out_abc@traces)
par(mfrow = c(4, 2))
plot(density(exp(chain$beta_d)*10600000, adjust = 1), bty = "n",
     ylab = "Density", xlab = expression(beta[d]), main = "")
lines(density(exp(rnorm(100000, log(2e-7), 0.8))*10600000, adjust = 1), 
      col = "red", lty = 2)
plot(density(exp(chain$beta_u)*10600000, adjust = 1), bty = "n",
     ylab = "Density", xlab = expression(beta[u]), main = "")
lines(density(exp(rnorm(100000, log(5e-8), 0.4))*10600000, adjust = 1), 
      col = "red", lty = 2)
plot(density(exp(chain$beta_e)*10600000, adjust = 1), bty = "n",
     ylab = "Density", xlab = expression(beta[e]), main = "")
lines(density(exp(rnorm(100000, log(5e-8), 0.4))*10600000, adjust = 1), 
      col = "red", lty = 2)
plot(density(chain$beta_red_factor, adjust = 1), bty = "n",
     ylab = "Density", xlab = expression(xi), main = "")
lines(x = seq(0, 1, by = 0.01), dunif(x = seq(0, 1, by = 0.01), 0.01, 1), 
      col = "red", lty = 2)
plot(density(chain$gamma_u), bty = "n", ylab = "Density", 
     xlab = expression(gamma[u]), main = "")
lines(density(rlnorm(100000, log(0.5), 1)), 
      col = "red", lty = 2)
plot(density(chain$gamma_d), bty = "n", ylab = "Density", 
     xlab = expression(gamma[d]), main = "")
lines(density(rlnorm(100000, log(0.5), 1)), 
      col = "red", lty = 2)
plot(density(chain$detect_frac_0, adjust = 1), bty = "n",
     ylab = "Density", xlab = "detect_frac_0", main = "")
lines(x = seq(0, 1, by = 0.01), dunif(x = seq(0, 1, by = 0.01), 0.01, 0.6), 
      col = "red", lty = 2)
plot(density(chain$theta), bty = "n", xlab = expression(theta), main= "")

      