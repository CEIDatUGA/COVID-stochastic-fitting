# This fuction takes the dates and the dataframe of parameters in natural units, 
# and returns a vector for detection probability (q).

get_q <- function(t, params) {
  
  # detection probability # check against pomp code
  q_min <- params$base_detect_frac
  q_max <- params$max_detect_frac
  q_n <- params$detect_rampup # Hill coefficient
  q_th <- params$t_half_detect 
  # as defined in states-model.rmd:
  q <- scales::rescale(
    t^q_n / (q_th^q_n + t^q_n), 
    from = c(0,1), 
    to = c(q_min, q_max)
  )
  # as defined in makepompmodel:
  # 1/(1+exp(max_detect_par)) * exp(log_detect_inc_rate)^t / (exp(log_detect_inc_rate)^exp(log_half_detect) + exp(log_detect_inc_rate)^t) + base_detect_frac
  # q <- 1/(1 + q_max) * q_n ^ t / (q_n ^ q_th + q_n ^ t) + q_min # does not jive with q caluclation above
  
  return(q)
}

# This fuction takes the dates and the dataframe of parameters in natural units, 
# and returns a vector for diagnosis rate (s).

get_s <- function(t, params) {
  
  # diagnosis rate 
  s_max <- params$max_diag_factor
  s_n <- params$diag_rampup # Hill coefficient
  s_th <- params$t_half_diag 
  # as defined in makepompmodel:
  # 1 + exp(log_max_diag) * exp(log_diag_inc_rate)^t / ( exp(log_diag_inc_rate)^exp(log_half_diag) +   exp(log_diag_inc_rate)^t)
  s <- 1 + s_max * s_n^t / ( s_n ^ s_th + s_n ^ t )
  
  return(s)
}

# This fuction takes the dates, number of susceptibles, population, omega, 
# and the dataframe of parameters in natural units, and returns a vector for R effective.
# Transition times between classes are expected to be per class, and not per sub-compartment.

getReff <- function(S, omega, q, s, params) {
  
  # variables
  S <- S # number of susceptibles
  
  # constants
  a <- params$frac_asym
  h <- params$frac_hosp
  
  # relative transmissibility
  b_L <- params$frac_trans_e
  b_I_a <- params$frac_trans_a
  b_I_su <- 1
  b_I_sd <- 1
  b_C <- params$frac_trans_c
  b_H <- params$frac_trans_h
  
  # rates of movement between compartments
  # rates are per class, not per sub-compartment
  gamma_L <- 1/params$time_e
  gamma_I_a <- 1/params$time_a
  gamma_I_su <- 1/params$time_su
  gamma_I_sd <- 1/params$time_sd
  gamma_C <- 1/params$time_c
  gamma_H <- 1/params$time_h
  
  R_e <- S * omega * (
    b_L / gamma_L +  (1 - a) * (
      q * ( 
        b_I_sd / (s * gamma_I_sd) 
        + b_C * s / gamma_C 
        + h * b_H / gamma_H 
      )
      + (1 - q) * b_I_su / gamma_I_su
    ) 
    + a * b_I_a / gamma_I_a 
  )
  
  return(R_e)
}