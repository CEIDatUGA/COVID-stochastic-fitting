# quick script that plots processed results for all states
# only for testing, real plotting will happen inside shiny app


library('dplyr')
library('here')
library('ggplot2')


# Load the data/output -----------------------------------
filename = here('output','results_for_shiny.rds')

df = readRDS(filename)

p1 <- df %>% 
      filter(Variable == "C_new") %>%
      filter(Var_Type == "ptvalue") %>%
      ggplot() +
      geom_line(alpha=1, aes(x = Date, y = Value, group = Scenario)) +
      facet_wrap(Scenario ~ Location, ncol = 3, scales = "free_y") 
                  
                         

