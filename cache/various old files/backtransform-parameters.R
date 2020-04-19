

backtransform <- function(param_df) {
  out <- length(nrow(param_df))
  for(i in 1:nrow(param_df)) {
    trans <- param_df[i,"trans"]
    x <- param_df[i,"value"]
    if(trans == "log") {
      out[i] <- exp(x)
    } else {
      out[i] <- 1/(1+exp(x))
    }
  }
  return(out)
} 

param_names <- c("log_beta_s", "trans_e", "trans_a", "trans_c", "trans_h", 
                 "log_g_e", "log_g_a", "log_g_su", "log_g_sd", "log_g_c", "log_g_h", 
                 "log_max_diag", "log_diag_inc_rate", "max_detect_par", 
                 "log_detect_inc_rate", "frac_asym", "frac_hosp", "frac_dead", 
                 "log_theta_cases", "log_theta_hosps", "log_theta_deaths", 
                 "log_sigma_dw", "E1_0", "Ia1_0", "Isu1_0", "Isd1_0")

param_trans <- c("log", "logis", "logis", "logis", "logis",
                 "log", "log", "log", "log", "log", "log",
                 "log", "logis", "logis",
                 "log", "logis", "logis", "logis", 
                 "log", "log", "log",
                 "log", "log", "log", "log", "log")

param_df <- data.frame(param_name = param_names,
                       trans = param_trans)

ests <- allparvals %>%
  as.data.frame() %>%
  gather(key = "param_name")

param_df <- param_df %>%
  left_join(ests)
param_df$transforms <- backtransform(param_df)
param_df$transforms[param_df$param_name == "log_beta_s"] <- param_df$transforms[param_df$param_name == "log_beta_s"]*10600018


param_df$transforms <- round(param_df$transforms,3)
comparment_rates <- c("log_g_e", "log_g_a", "log_g_su", 
                      "log_g_sd", "log_g_c", "log_g_h")
param_df <- param_df %>%
  mutate(transforms = ifelse(param_name %in% comparment_rates, transforms/4, transforms))

param_df[ , c("param_name", "transforms")] %>% deframe()
