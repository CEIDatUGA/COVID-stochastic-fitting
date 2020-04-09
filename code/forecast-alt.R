# forecast-alt.R
# Alternate forecasting script using the King et al. ebola example
# approach. It is empirical Bayes, simulating from parameter values
# around the MLEs identified by MIF.


# Clear the decks ---------------------------------------------------------

rm(list = ls(all.names = TRUE))


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(pomp)
library(here)
library(foreach)
library(doParallel)
library(iterators)


# Load the MIF results ----------------------------------------------------

mifs <- readRDS(here("output/mif-results.RDS"))
logliks <- mifs$loglik_dfs


# Define some global functions and settings -------------------------------

# Forecast horizon
horizon <- 7 * 2  # 7 days * 7 weeks

# Weighted quantile function (from King et al. 2015)
wquant <- function (x, weights, probs = c(0.025,0.5,0.975)) {
  idx <- order(x)
  x <- x[idx]
  weights <- weights[idx]
  w <- cumsum(weights)/sum(weights)
  rval <- approx(w,x,probs,rule=1)
  rval$y
}



# Define parameter vectors around the MLEs --------------------------------


logliks  %>%
  select(-MIF_ID,-LogLik_SE) %>%
  # filter(LogLik > max(LogLik) - 0.5*qchisq(df=1, p=0.99)) %>%
  gather(parameter,value) %>%
  group_by(parameter) %>%
  summarize(min=min(value),max=max(value)) %>%
  ungroup() %>%
  filter(parameter!="LogLik") %>%
  column_to_rownames("parameter") %>%
  as.matrix() -> ranges

sobolDesign(lower=ranges[,'min'],
            upper=ranges[,'max'],
            nseq=10) -> params

params <- logliks[5,4:ncol(logliks)] %>%
  as.data.frame()

# Run the simulations -----------------------------------------------------
# 
# registerDoParallel()
# registerDoRNG(887851050L)

foreach(p=iter(params,by='row'),
        .inorder=FALSE,
        .combine=bind_rows
) %do% {
  
  library(pomp)
  
  M1 <- readRDS(here("output/pomp-model.RDS"))
  
  M1 %>% pfilter(params=p,Np=2000,save.states=TRUE) -> pf
  
  pf@saved.states %>%               # latent state for each particle
    tail(1) %>%                     # last timepoint only
    melt() %>%                      # reshape and rename the state variables
    spread(variable,value) %>%
    group_by(rep) %>%
    summarize(
      S_0=S,
      E1_0=E1, E2_0=E2, E3_0=E3, E4_0=E4,
      Ia1_0=Ia1, Ia2_0=Ia2, Ia3_0=Ia3, Ia4_0=Ia4,
      Isu1_0=Isu1, Isu2_0=Isu2, Isu3_0=Isu3, Isu4_0=Isu4,
      Isd1_0=Isd1, Isd2_0=Isd2, Isd3_0=Isd3, Isd4_0=Isd4,
      C1_0 = C1, C2_0 = C2, C3_0 = C3, C4_0 = C4,
      H1_0 = H1, H2_0 = H2, H3_0 = H3, H4_0 = H4,
      R_0=R, D_0 = D
    ) %>%
    gather(variable,value,-rep) %>%
    spread(rep,value) %>%
    column_to_rownames("variable") %>%
    as.matrix() -> x
  ## the final states are now stored in 'x' as initial conditions
  
  ## set up a matrix of parameters
  pp <- parmat(unlist(p),ncol(x))
  
  ## generate simulations over the interval for which we have data
  M1 %>%
    simulate(params=pp,format="data.frame") %>%
    select(.id,time,H1,hosps, cases, C1, D, deaths) %>%
    mutate(
      period="calibration",
      loglik=logLik(pf)
    ) -> calib
  
  ## make a new 'pomp' object for the forecast simulations
  M2 <- M1
  time(M2) <- max(time(M1))+seq_len(horizon)
  timezero(M2) <- max(time(M1))
  
  ## set the initial conditions to the final states computed above
  pp[rownames(x),] <- x
  
  ## perform forecast simulations
  M2 %>%
    simulate(params=pp,format="data.frame") %>%
    select(.id,time,H1,hosps, cases, C1, D, deaths) %>%
    mutate(
      period="projection",
      loglik=logLik(pf)
    ) -> proj
  
  bind_rows(calib,proj)
} -> out
out %>%
  # filter(loglik != -Inf) %>%
  # mutate(weight=exp(loglik-mean(loglik))) %>%
  arrange(time,.id) -> sims


## look at effective sample size
# ess <- with(subset(sims,week==max(week)),weight/sum(weight))
# ess <- 1/sum(ess^2); ess



## compute quantiles of the forecast incidence
sims %>%
  # mutate(weight = ifelse(weight == Inf, 1, weight)) %>%
  group_by(time,period) %>%
  summarize(
    lower = quantile(cases, probs = 0.01),
    median = median(cases),
    upper = quantile(cases, probs = 0.9)
  ) %>%
  # summarize(
  #   lower=wquant(H1,weights=weight,probs=0.025),
  #   median=wquant(H1,weights=weight,probs=0.5),
  #   upper=wquant(H1,weights=weight,probs=0.975)
  # ) %>%
  ungroup() -> simq

# thedata <- readRDS(here("output/pomp-model.RDS"))@data
ggplot(simq, aes(x = time, y = median, color = period)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = period), 
              alpha = 0.2, color = NA) +
  geom_line() +
  xlab("Days since March 1") + ylab("New hospitalizations")

start_date <- as.Date("2020-03-01")
end_date <- start_date + max(sims$time) - 1
dates <- seq.Date(start_date, end_date, "days") 
dates_df <- data.frame(Time = c(1:length(dates)), Date = dates)
sims %>%
  rename("Time" = time, "H_new" = H1, "rep" = .id) %>%
  dplyr::select(Time, H_new, rep) %>%
  left_join(dates_df, by = "Time") %>%
  dplyr::select(Date, H_new, rep) -> forecasts

saveRDS(object = forecasts, file = here("output/forecasts.RDS"))
