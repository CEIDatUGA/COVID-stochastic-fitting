library(tidyverse)
library(here)

fig_outpath <- here("output/figures/")

simfiles <- list.files(path = here('output/current'), "COV.csv")
covarfiles <- list.files(path = here('output/current'), "params.rds")
# miffile <- here("output/current", filename_mif)


## simulations
simfile <- paste0(here('output/current/'),simfiles[1])
out_sims <- read_csv(simfile)

for(i in 2:length(simfiles)) {
  simfile <- paste0(here('output/current/'),simfiles[i])
  o <- read_csv(simfile)
  out_sims <- bind_rows(out_sims, o)
}

# Cum Sums

## group by location, simtype, and variable, make sure the dates are ordered correctly, and then cumsum. 
# out_sims <- out_sims %>% 
#   group_by(location,sim_type,variable) # etc

# Dates
foredate <- out_sims %>% filter(period == "Past") %>% pull(date) %>% max()
end_date <- max(out_sims$date)
start_date <- min(out_sims$date)

# Visual Variables ----------------------------------------------------------------------------

mycols <- c("#5798d1", "#319045", "#e2908c", "#a11c3e", "#226e83", "#5e2b7b", "#252525")
names(mycols) <- c('lightblue', 'green', 'pink', 'red', 'blue', 'purple', 'black')
mycols.vec <- mycols
names(mycols.vec) <- NULL
mycols.vec.filt <- mycols[c('lightblue','green','red')]
names(mycols.vec.filt) <- NULL

# labels
scen_labs <- c("1. Increase social distancing",
               "2. Maintain social distancing (status quo)",
               "3. Return to normal")


# plot cum cases overview -----------------------------------------------------------------------------------

cumcases <- out_sims %>% filter(variable == 'cumulative_cases')

cumcases.maxy <- cumcases %>% 
  pull(mean_value) %>% max() %>% 
  plyr::round_any(accuracy = 20000, f = ceiling)
cases.ylim <- c(0,cumcases.maxy)

cumcases.lp <- cumcases %>% 
  ggplot(aes(x = date, y = mean_value)) +
  geom_line(data = filter(cumcases, sim_type == 'return_normal'),
            color = alpha(mycols['red'], .7), size = 1) +
  geom_line(data = filter(cumcases, sim_type == 'status_quo'),
            color = alpha(mycols['green'], .7), size = 1) +
  geom_line(data = filter(cumcases, sim_type == 'linear_increase_sd'),
            color = alpha(mycols['lightblue'], .7), size = 1) +
  geom_line(data = filter(cumcases, sim_type == 'return_normal' & period == 'Past'),
            color = mycols['black'], size = 1) +
  geom_vline(aes(xintercept = as.numeric(foredate)), color = "grey35", linetype = 2) +
  facet_wrap(~location, ncol = 10) +
  ylab("") +
  scale_y_continuous(labels = scales::comma, limits = cases.ylim) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Total number of confirmed cases")

# plot daily cases overview -----------------------------------------------------------------------------------

dailycases <- out_sims %>% filter(variable == 'daily_cases')

up_popsize <- readRDS(here('data/us_popsize.rds'))
idx <- match(x = dailycases$location, table = up_popsize$state_full)
dailycases$pop <- up_popsize$total_pop[idx]

dailycases <- dailycases %>% 
  mutate(mean_value_per100k = 100000*mean_value/pop)

# select 3 states
# dailycases <- dailycases %>% 
#   filter(location %in% c('California','New York','Georgia'))

# states with max daily cases / 100 k > 250
# topstates <- dailycases[dailycases$mean_value_per100k > 250,]$location %>% unique
# dailycases <- dailycases %>%
#   filter(location %in% topstates)

# states with max daily cases / 100 k > 250
# topstates <- dailycases[dailycases$mean_value_per100k > 250,]$location %>% unique
# dailycases <- dailycases %>%
#   filter(location %in% topstates)

# states with max daily cases / 100 k > 100 in status quo scenario
topstates <- dailycases[dailycases$sim_type == 'status_quo' & dailycases$mean_value_per100k > 100,]$location %>% unique
dailycases <- dailycases %>%
  filter(location %in% topstates)


dailycases.maxy <- dailycases %>% 
  pull(mean_value_per100k) %>% max() %>% 
  plyr::round_any(accuracy = 10, f = ceiling)
dailycases.ylim <- c(0,dailycases.maxy)

dailycases.lp <- dailycases %>% 
  ggplot(aes(x = date, y = mean_value_per100k)) +
  geom_line(data = filter(dailycases, sim_type == 'return_normal'),
            color = alpha(mycols['red'], .7), size = 1) +
  geom_line(data = filter(dailycases, sim_type == 'status_quo'),
            color = alpha(mycols['green'], .7), size = 1) +
  geom_line(data = filter(dailycases, sim_type == 'linear_increase_sd'),
            color = alpha(mycols['lightblue'], .7), size = 1) +
  geom_line(data = filter(dailycases, sim_type == 'return_normal' & period == 'Past'),
            color = mycols['black'], size = 1) +
  geom_vline(aes(xintercept = as.numeric(foredate)), color = "grey35", linetype = 2) +
  facet_wrap(~location, ncol = ceiling(length(topstates)/2)) +
  ylab("") +
  scale_y_continuous(labels = scales::comma, limits = dailycases.ylim) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Daily confirmed cases per 100,000 people")

p_dailycases.lp <- dailycases.lp %>% 
  plotly::ggplotly()

fig_outpath <- here("output/figures/")
p_dailycases.lp %>% htmlwidgets::saveWidget(file = paste0(fig_outpath, "topstates.html"))

# plot transmission rates over time -----------------------------------------------------------------------------------

# ggplot(out_sims, aes(date, omega)) +
#   geom_line(color = "salmon") +
#   facet_wrap(~location, ncol = 10) +
#   coord_cartesian(ylim = c(0, 1e-06)) +
#   theme_minimal(base_size = 10) +
#   ylab(expression(omega)) +
#   theme(axis.text.x = element_text(angle = 45)) +
#   ggtitle("Transmission rates over time")
