rm(list = ls())
require(dplyr)
require(tidyr)
path <- "C:/Users/mknoedler/Desktop/COVID-stochastic-fitting/"
dataPath <- file.path(path, "output")
outPath <- file.path(path, "Data Analysis")
tablePath <- file.path(outPath, "Tables")
forecast <- readRDS(file = file.path(outPath, "Workspaces/forecastMerged.RDS"))
tabulate <- forecast %>%
  filter(location != "Overall",
         days_ahead <= 28) %>%
  group_by(variable, sim_type, location, days_ahead, conf) %>%
  summarise(percent_coverage = mean(withinCI),
            percent_coverage_roll = mean(withinCI_roll),
            n = n())
# Calculate overall percent coverage as median of each state rather than using
# the summed overall location from the forecast data frame
tabulate_overall <- tabulate %>%
  group_by(variable, sim_type, days_ahead, conf) %>%
  summarise(percent_coverage = median(percent_coverage),
            percent_coverage_roll = median(percent_coverage_roll),
            n = sum(n))
tabulate_overall$location <- "Overall"
tabulate_overall <- tabulate_overall[, names(tabulate)]
tabulate <- rbind(tabulate, tabulate_overall)

tabulate_wide <- spread(tabulate[, !names(tabulate) %in% c("percent_coverage_roll", "n")], days_ahead, percent_coverage)
tabulate_wide_roll <- spread(tabulate[, !names(tabulate) %in% c("percent_coverage", "n")], days_ahead, percent_coverage_roll)

write.csv(tabulate, file = file.path(tablePath, "tabulate_long.csv"), row.names = FALSE)
write.csv(tabulate_wide, file = file.path(tablePath, "tabulate_wide.csv"), row.names = FALSE)
write.csv(tabulate_wide_roll, file = file.path(tablePath, "tabulate_wide_roll.csv"), row.names = FALSE)
