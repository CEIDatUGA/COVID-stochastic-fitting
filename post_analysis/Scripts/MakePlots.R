rm(list = ls())
require(ggplot2)
require(dplyr)
require(viridis)
require(lubridate)
require(westutils)
require(tidyr)
path <- "C:/Users/mknoedler/Desktop/COVID-stochastic-fitting/"
outPath <- file.path(path, "Data Analysis")
workspacePath <- file.path(outPath, "Workspaces")
forecast <- readRDS(file = file.path(workspacePath, "forecastMerged.RDS"))
jhu_long <- readRDS(file = file.path(workspacePath, "jhu_long.RDS"))
tabulate <- read.csv(file = file.path(outPath, "Tables/tabulate_long.csv"))
WIS <- readRDS(file = file.path(workspacePath, "WIS.RDS"))
MWIS <- readRDS(file = file.path(workspacePath, "MWIS.RDS"))

westColors <- westutils::ReturnWESTColors()


# Set up labels
states <- sort(unique(forecast$location))
states <- states[states != "Overall"]
simLabels <- c("Increasing Social Distancing", "Status Quo", "Return to Normal")
names(simLabels) <- c("linear_increase_sd", "status_quo", "return_normal")
rollLabels <- c("7 Day Rolling Average", "Daily Value")
names(rollLabels) <- c("1", "0")
varLabels <- c("Daily Cases", "Daily Deaths")
names(varLabels) <- c("daily_cases", "daily_deaths")
daysAheadLabels <- c(7, 14, 28)
daysAheadLabels <- paste0(daysAheadLabels, " days ahead")
names(daysAheadLabels) <-  c(7, 14, 28)
confidence <- unique(tabulate$conf)
#QAQC plots
# for(state in states) {
#   p <- jhu_long %>%
#     filter(Date <= "2021-07-23",
#            Date >= "2020-06-09",
#            Location %in% state) %>%
#     ggplot(aes(x = Date, y = ground_truth)) +
#     geom_line() +
#     facet_wrap(~variable, nrow = 2, scales = "free")
#   ggsave(file = file.path(outPath, "Figures/JHU Time Series/", paste0(state, "_jhuTimeSeries.png")), plot = p, width = 15, height = 10)
# }
# 
# for(state in states) {
#   p <- forecast %>%
#     filter(location == state,
#            days_ahead <= 28,
#            variable == "daily_cases",
#            conf == 95) %>%
#     ggplot(aes(x = days_ahead, y = intervalSize, color = as.factor(day))) +
#     geom_line() +
#     facet_grid(vars(month_year), vars(sim_type))
#   ggsave(file = file.path(outPath, "Figures/QAQC/", paste0(state, "_QAQC.png")), plot = p, width = 10, height = 20)
# }

#Exploration
#Make tabulate longer for ggplot
tabulate$main <- 0.75
tabulate$main[tabulate$location == "Overall"] <- 1
tabulate$region[tabulate$location %in% c('Washington',
                                         'Oregon',
                                         'California',
                                         'Idaho',
                                         'Nevada',
                                         'Montana',
                                         'Wyoming',
                                         'Alaska',
                                         'Hawaii')] <- "West"
tabulate$region[tabulate$location %in% c('Utah',
                                         'Arizona',
                                         'Colorado',
                                         'New Mexico',
                                         'Oklahoma',
                                         'Texas')] <- "Southwest"
tabulate$region[tabulate$location %in% c('North Dakota',
                                         'South Dakota',
                                         'Nebraska',
                                         'Kansas',
                                         'Minnesota',
                                         'Iowa',
                                         'Missouri',
                                         'Wisconsin',
                                         'Illinois',
                                         'Michigan',
                                         'Indiana',
                                         'Ohio')] <- "Midwest"
tabulate$region[tabulate$location %in% c('Arkansas',
                                         'Louisiana',
                                         'Mississippi',
                                         'Tennessee',
                                         'Alabama',
                                         'Kentucky',
                                         'Georgia',
                                         'West Virginia',
                                         'Virginia',
                                         'North Carolina',
                                         'South Carolina',
                                         'Florida')] <- 'Southeast'
tabulate$region[tabulate$location %in% c('Maine',
                                         'Vermont',
                                         'New Hampshire',
                                         'New York',
                                         'Pennsylvania',
                                         'Maryland',
                                         'Delaware',
                                         'Massachusetts',
                                         'Connecticut',
                                         'Rhode Island',
                                         'New Jersey')] <- 'Northeast'
tabulate$location <- factor(tabulate$location,
                             levels = c(states, "Overall"))
tabulate$region[tabulate$location == "Overall"] <- "Overall"
regions <- unique(tabulate$region)
for (confLevel in confidence) {
  for (areas in regions) {
    if (areas == "Overall") {
      sub <- tabulate %>%
        filter(conf == confLevel)
    }
    else {
      sub <- tabulate %>%
        filter(conf == confLevel,
               region %in% c(areas, "Overall"))
    }
    uniqueStates <- length(unique(sub$location)) - 1
    stateCols <- grDevices::rainbow(uniqueStates)
    p <- sub %>%
      ggplot(aes(x = days_ahead, y = percent_coverage_roll, color = location, alpha = main)) +
      #geom_smooth(se = FALSE) +
      geom_line() +
      scale_color_manual(values = c(stateCols, "#000000")) +
      facet_wrap(vars(variable, sim_type), nrow = 2, ncol = 3,
                 labeller = labeller(variable = varLabels,
                                     sim_type = simLabels)) +
      coord_cartesian(ylim = c(0, 1)) +
      scale_size_manual(values = c(rep(0.1, uniqueStates), 1)) +
      scale_alpha(range = c(0.5, 1)) +
      xlab("Days Ahead") +
      ylab("Forecast Coverage Portion") +
      guides(color=guide_legend("Location"), alpha = "none") +
      theme(text=element_text(size=20))
    ggsave(file = file.path(outPath, "Figures/Percent Coverage Rolling/", paste0(areas, confLevel, ".png")), plot = p, width = 20, height = 10)
  }
}
# for (state in states) {
#   p <- forecast %>%
#     filter(location == state,
#            days_ahead < 12) %>%
#     ggplot(aes(x = mean_value, y = ground_truth, color = days_ahead)) +
#     geom_point() +
#     geom_abline(intercept = 0, slope = 1) +
#     facet_wrap(vars(variable, sim_type), nrow = 2, ncol = 3, scales = "free") +
#     scale_color_viridis() +
#     xlab('Prediction')
#   ggsave(file = file.path(outPath, "Figures/Mean v Ground Truth", paste0(state, ".png")), plot = p, width = 15, height = 10)
# }
# Plot Prediction vs Rolling truth by state
for (state in states) {
  p <- forecast %>%
    filter(location == state,
           days_ahead %in% c(7, 14, 28)) %>%
    ggplot(aes(x = log(mean_value + 1), y = log(rolling_truth + 1), color = sim_type)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap(vars(variable, days_ahead), nrow = 2, ncol = 3, scales = "free",
               labeller = labeller(variable = varLabels,
                                   days_ahead = daysAheadLabels)) +
    scale_color_manual(values = westColors$HEXCode[1:3], 
                       breaks = names(simLabels),
                       labels = simLabels) +
    theme(text=element_text(size=20)) +
    labs(color = "Model",
         x = "Predicted Value (log scale)",
         y = "7 Day Rolling Average (log scale)")
  ggsave(file = file.path(outPath, "Figures/Mean v Rolling Truth", paste0(state, ".png")), plot = p, width = 15, height = 10)
}
# Plot prediction vs rolling truth for all states at once
p <- forecast %>%
  filter(location != "Overall",
         days_ahead %in% c(7, 14, 28)) %>%
  ggplot(aes(x = log(mean_value + 1), y = log(rolling_truth + 1), color = sim_type)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(vars(variable, days_ahead), nrow = 2, ncol = 3, scales = "free",
             labeller = labeller(variable = varLabels,
                                 days_ahead = daysAheadLabels)) +
  scale_color_manual(values = westColors$HEXCode[1:3], 
                     breaks = names(simLabels),
                     labels = simLabels) +
  theme(text=element_text(size=20)) +
  labs(color = "Model",
       x = "Predicted Value (log scale)",
       y = "7 Day Rolling Average (log scale)")
ggsave(file = file.path(outPath, "Figures/Mean v Rolling Truth/Overall.png"), plot = p, width = 15, height = 10)




#MWIS plots
# for (state in states) {
#   p <- MWIS %>%
#     filter(location == state,
#            days_ahead <=28) %>%
#     ggplot(aes(x = days_ahead, y = MWIS, color = sim_type)) +
#     geom_line() +
#     facet_wrap(~variable, nrow = 2, scales = "free")
#   ggsave(file = file.path(outPath, "Figures/MWIS", paste0(state, ".png")), plot = p, width = 15, height = 10)
# }
#WIS plots
# for (state in states) {
#   p <- WIS %>%
#     filter(location == state,
#            days_ahead <= 28) %>%
#     ggplot(aes(x = days_ahead, y = WIS, color = sim_type)) +
#     geom_point() +
#     facet_wrap(~variable, nrow = 2, scales = "free")
#   ggsave(file = file.path(outPath, "Figures/WIS", paste0(state, ".png")), plot = p, width = 15, height = 10)
# }
# WIS <- forecast %>%
#   group_by(location, days_ahead, sim_type, variable, mean_value, rolling_truth, month_year) %>%
#   summarize(summation = sum(weighted)) %>%
#   data.frame()
# 
# w_0 <- 1 / 2
# m <- WIS$mean_value
# y <- WIS$rolling_truth
# K <- 3
# WIS$WIS <- 1 / (K + 1/2) * (w_0 * abs(y - m) + WIS$summation)
# 
# MWIS <- WIS %>%
#   group_by(location, days_ahead, sim_type, variable, month_year) %>%
#   summarize(MWIS = mean(WIS, na.rm = TRUE)) %>%
#   data.frame()
# for(state in states[1:5]) {
#   for (var in c("daily_cases", "daily_deaths")) {
#     p <- MWIS %>%
#       filter(location == state,
#            days_ahead <= 28,
#            variable == var) %>%
#     ggplot(aes(x = days_ahead, y = MWIS, color = as.factor(sim_type))) +
#     geom_line() +
#     facet_wrap(~month_year, ncol = 1, scales = "free")
#   ggsave(file = file.path(outPath, "Figures/MWIS/", paste0(state, var, "_byMonth.png")), plot = p, width = 5, height = 20)
#   }
# }
# 
# 
# WIS <- forecast %>%
#   group_by(location, date, days_ahead, sim_type, variable, mean_value, rolling_truth, sim_date) %>%
#   summarize(summation = sum(weighted)) %>%
#   data.frame()
# 
# w_0 <- 1 / 2
# m <- WIS$mean_value
# y <- WIS$rolling_truth
# K <- 3
# WIS$WIS <- 1 / (K + 1/2) * (w_0 * abs(y - m) + WIS$summation)
# 
# MWIS <- WIS %>%
#   group_by(location, date, days_ahead, sim_type, variable, sim_date) %>%
#   summarize(MWIS = mean(WIS, na.rm = TRUE)) %>%
#   data.frame()
# 
# for(state in states[1:5]) {
#   for (var in c("daily_cases", "daily_deaths")) {
#     p <- MWIS %>%
#       filter(location == state,
#              days_ahead <= 28,
#              variable == var) %>%
#       ggplot(aes(x = date, y = MWIS, color = as.factor(sim_date))) +
#       geom_line() +
#       facet_wrap(~sim_type, nrow = 3)
#     ggsave(file = file.path(outPath, "Figures/MWIS/", paste0(state, var, "_bySimDate.png")), plot = p, width = 20, height = 20)
#   }
# }

#plot time series
long <- forecast[forecast$location == "Overall", c('location', 'sim_type',
                                                   'date', 'variable', 
                                                   'mean_value', 'days_ahead',
                                                   'rolling_truth')]
long_rolling <- long %>%
  gather(key = "sim_type", value = "mean_value", rolling_truth)
long_rolling$days_ahead <- "actual"
long_rolling <- unique(long_rolling)
long_rolling$sim_type <- 'linear_increase_sd'
long <- rbind(long[,c(1:6)], long_rolling)
long_rolling$sim_type <- 'return_normal'
long <- rbind(long, long_rolling)
long_rolling$sim_type <- 'status_quo'
long <- rbind(long, long_rolling)
p <- long %>%
  filter(days_ahead %in% c("actual", "7", "14", "28")) %>%
  ggplot(aes(x = date, y = mean_value, color = days_ahead)) +
  geom_line() +
  facet_wrap(vars(variable, sim_type), nrow = 2, ncol = 3, scales = "fixed",
             labeller = labeller(variable = varLabels,
                                 sim_type = simLabels))
p

#plot time series a different way
long <- forecast[forecast$location == "Overall", c('location', 'sim_type',
                                                   'date', 'variable', 
                                                   'mean_value', 'days_ahead',
                                                   'rolling_truth')]
long_rolling <- long %>%
  gather(key = "sim_type", value = "mean_value", rolling_truth)
plotDaysAhead <- c(5, 10, 14, 28)
for (day in plotDaysAhead) {
  long_rolling$days_ahead <- day
  long <- rbind(long[,c(1:6)], long_rolling)
}
long <- unique(long)
plotDaysAheadLabels <- paste0(plotDaysAhead, " days ahead")
names(plotDaysAheadLabels) <- plotDaysAhead
sim_truth <- c(simLabels, "True rolling average")
names(sim_truth) <- c(names(simLabels), "rolling_truth")
long$sim_type <- factor(long$sim_type,
                            levels = c(names(sim_truth)))
p <- long %>%
  filter(days_ahead %in% plotDaysAhead,
         as.Date(date) >= "2020-10-31",
         as.Date(date) <= "2021-01-31") %>%
  ggplot(aes(x = date, y = mean_value, color = sim_type)) +
  geom_line() +
  facet_wrap(vars(variable, days_ahead), nrow = 2, ncol = length(plotDaysAhead), scales = "free_y",
             labeller = labeller(variable = varLabels,
                                 days_ahead = plotDaysAheadLabels)) +
  scale_color_manual(values = c(westColors$HEXCode[1:3], "#000000"), 
                     breaks = names(sim_truth),
                     labels = sim_truth) +
  theme(text=element_text(size=20)) +
  labs(color = "Model",
       x = "Date",
       y = "Mean prediction")
ggsave(file = file.path(outPath, "Figures/DaysAhead.png"), plot = p, width = 20, height = 10)

#plot time series another different way
long <- forecast[forecast$days_ahead <= 14, ]
long <- long[long$location == "Overall", c('location', 'sim_type',
                                                   'date', 'variable', 
                                                   'mean_value', 'sim_date',
                                                   'rolling_truth')]
long_rolling <- long %>%
  gather(key = "sim_type", value = "mean_value", rolling_truth)
long_rolling$sim_date <- "2022-04-07"
for (type in names(simLabels)) {
  long_rolling$sim_type <- type
  long <- rbind(long[,c(1:6)], long_rolling)
}
# sim_truth <- c(unique(long$sim_date), "True rolling average")
# names(sim_truth) <- c(unique(long$sim_date), "rolling_truth")
# long$sim_type <- factor(long$sim_type,
#                         levels = c(names(sim_truth)))
sub <- long %>%
  filter(as.Date(date) >= "2020-11-04",
         as.Date(date) <= "2021-01-31")
nSims <- length(unique(sub$sim_date)) - 1
start <- sub %>%
  group_by(location, sim_type, variable, sim_date) %>%
  filter(date == min(date))
start <- start[start$sim_date != "2022-4-07", ]
p <- sub %>%
  ggplot(aes(x = date, y = mean_value, color = as.factor(sim_date))) +
  geom_point(data = start,
             inherit.aes = FALSE,
             aes(x= date, y= mean_value),
             size=2, 
             shape=16, 
             color="red",
             alpha = 0.25) +
  geom_line() +
  facet_grid(vars(variable), vars(sim_type), scales = "free_y",
             labeller = labeller(variable = varLabels,
                                 sim_type = simLabels)) +
  scale_color_manual(values = c(rep("#808080", nSims), "#000000")) +
  theme(text=element_text(size=20),
        legend.position = "none") +
  labs(color = "Model run date",
       x = "Date",
       y = "Median prediction")
ggsave(file = file.path(outPath, "Figures/SimulationsVTruth.png"), plot = p, width = 20, height = 10)
