# Riley Knoedler
# 3/21/2022
# Forecast Model Retrospective
rm(list = ls())
require(dplyr)
require(tidyr)
require(zoo)
require(lubridate)
path <- "C:/Users/mknoedler/Desktop/COVID-stochastic-fitting/"
dataPath <- file.path(path, "output")
outPath <- file.path(path, "Data Analysis")

folders <- list.files(dataPath)
# only the first 100 folders are relevant
folders <- folders[1:100]
uniqueNames <- readRDS(file.path(outPath, "Workspaces/uniqueNames.RDS"))
forecast <- data.frame(location = character(),
                  sim_type = character(),
                  date = character(),
                  variable = character(),
                  lower_95 = double(),
                  upper_95 = double(),
                  lower_90 = double(),
                  upper_90 = double(),
                  lower_80 = double(),
                  upper_80 = double(),
                  mean_value = double(),
                  median_value = double(),
                  sim_date = character(),
                  stringsAsFactors=FALSE)
for (date in folders) {
  data <- list.files(file.path(dataPath, date), pattern = "\\.csv$")
  print(date)
  for (file in data) {
    temp <- read.csv(file.path(dataPath, date, file))
    #only take relevant files
    if (list(names(temp)) %in% uniqueNames) {
      keep <- temp[temp$sim_type != "data" & 
                     temp$period == "Future" &
                     temp$variable %in% c("daily_cases", "daily_deaths"), ]
      keep <- keep[, c("location", "sim_type", "date", "variable", "lower_95", "upper_95",
                       "lower_90", "upper_90", "lower_80", "upper_80",
                       "mean_value", "median_value")]
      keep$sim_date <- date
      forecast <- rbind(forecast, keep)
    }
  }
}
# saveRDS(forecast, file = file.path(outPath, "Workspaces/allCsvData.RDS"))
forecast <- readRDS(file.path(outPath, "Workspaces/allCsvData.RDS"))
# There are many forecast values that have nonsensical confidence intervals
temp <- forecast[forecast$mean_value > forecast$upper_95, ]
unique(temp$mean_value)
#Subsetting out these records
forecast <- forecast %>%
  filter(mean_value < upper_95,
         mean_value > lower_95)
# Does the final df make sense?
# table(forecast$location)
# table(forecast$sim_type)
# table(forecast$date)
# table(forecast$variable)

#Get Johns Hopkins actual cases and deaths data
us_jhu_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
us_jhu_deaths <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
# Clean cases
us_jhu_cases <- us_jhu_cases %>% filter(iso3 == "USA") %>%
  dplyr::select(c(-Country_Region, -Lat, -Long_, -UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Combined_Key)) %>%
  rename(Location = Province_State)
us_jhu_cases <- aggregate(. ~ Location, us_jhu_cases, FUN = sum)
us_jhu_cases_clean <- tidyr::gather(us_jhu_cases, Date, Cases, -Location)
# Clean deaths
us_jhu_deaths <- us_jhu_deaths %>% filter(iso3 == "USA") %>%
  dplyr::select(c(-Country_Region, -Lat, -Long_, -UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Combined_Key, -Population)) %>%
  rename(Location = Province_State)
us_jhu_deaths <- aggregate(. ~ Location, us_jhu_deaths, FUN = sum)
us_jhu_deaths_clean <- gather(us_jhu_deaths, Date, Deaths, -Location)
us_jhu_combined <- merge(us_jhu_cases_clean, us_jhu_deaths_clean)
us_jhu_clean <- us_jhu_combined %>% mutate(Date = as.Date(as.character(Date),format="%m/%d/%y")) %>%
  group_by(Location) %>% arrange(Date) %>%
  mutate(daily_cases = c(0,diff(Cases))) %>%
  mutate(daily_deaths = c(0,diff(Deaths))) %>%
  ungroup() %>%
  rename(Total_Deaths = Deaths, Total_Cases = Cases) %>%
  data.frame()
#Add rolling 7-day average
us_jhu_rolled <- us_jhu_clean %>%
  group_by(Location) %>%
  mutate(daily_deaths = zoo::rollmean(daily_deaths, k = 7, fill = NA),
         daily_cases = zoo::rollmean(daily_cases, k = 7, fill = NA)
  )
jhu_long_left <- gather(us_jhu_clean, variable, ground_truth, daily_cases:daily_deaths)
jhu_long_right <- gather(us_jhu_rolled, variable, rolling_truth, daily_cases:daily_deaths)
jhu_long <- left_join(jhu_long_left, jhu_long_right,
                 by = c("Location" = "Location",
                        "Date" = "Date",
                        "variable" = "variable")) %>%
  select(c(Location, Date, variable, ground_truth, rolling_truth))
#some of the ground truth is negative which doesn't make sense
nrow(jhu_long[jhu_long$ground_truth < 0, ])
jhu_long$ground_truth[jhu_long$ground_truth < 0] <- NA
jhu_long$rolling_truth[jhu_long$rolling_truth < 0] <- NA
#add overall column for matching with forecast
jhu_overall <- jhu_long %>%
  group_by(Date, variable) %>%
  summarise(ground_truth = sum(ground_truth),
            rolling_truth = sum(rolling_truth))
jhu_overall$Location <- 'Overall'
jhu_overall <- jhu_overall[,names(jhu_long)]
jhu_long <- rbind(jhu_long, jhu_overall)
saveRDS(jhu_long, file = file.path(outPath, "Workspaces/jhu_long.RDS"))
jhu_long <- readRDS(file = file.path(outPath, "Workspaces/jhu_long.RDS"))


#Add actual data to forecasts
forecast$sim_date <- as.Date(forecast$sim_date)
forecast$date <- as.Date(forecast$date)
forecast$days_ahead <- as.numeric(forecast$date - forecast$sim_date)
# 5 of the simulations have "future" dates before the simulation date
exclude <- unique(forecast$sim_date[forecast$days_ahead < 0])
forecast <- forecast[!forecast$sim_date %in% exclude, ]
forecast_overall <- forecast %>%
  group_by(sim_type, date, variable, days_ahead, sim_date) %>%
  summarise(across(c("lower_95", "upper_95",
                     "lower_90", "upper_90",
                     "lower_80", "upper_80",
                     "mean_value", "median_value"), sum))
forecast_overall$location <- "Overall"
forecast_overall <- forecast_overall[, names(forecast)]
forecast <- rbind(forecast, forecast_overall)
forecast <- left_join(forecast, jhu_long,
                      by = c("location" = "Location",
                             "date" = "Date",
                             "variable" = "variable"))
forecast <- forecast %>%
  gather(key = "upConf", value = "upper", upper_95, upper_90, upper_80) %>%
  gather(key = "lowConf", value = "lower", lower_95, lower_90, lower_80) %>%
  filter(substring(upConf, 7, 8) == substring(lowConf, 7, 8)) %>%
  mutate(conf = as.numeric(substring(upConf, 7, 8))) %>%
  select(-upConf, -lowConf)
# Does this df make sense? Should be 0
forecast[forecast$upper < forecast$lower, ]
# Should exclude predictions where upper = lower = 0 as that will artificially
# deflate MWIS
forecast <- forecast %>%
  filter(upper != lower)

forecast$withinCI <- 0
forecast$withinCI[forecast$ground_truth < forecast$upper &
                    forecast$ground_truth > forecast$lower] <- 1

forecast$withinCI_roll <- 0
forecast$withinCI_roll[forecast$rolling_truth < forecast$upper &
                    forecast$rolling_truth > forecast$lower] <- 1
forecast$over_predict_roll <- 0
forecast$over_predict_roll[forecast$rolling_truth < forecast$lower] <- 1
forecast$under_predict_roll <- 0
forecast$under_predict_roll[forecast$rolling_truth > forecast$upper] <- 1

alpha <- 1 - forecast$conf / 100
u <- forecast$upper
l <- forecast$lower
y <- forecast$rolling_truth
indLow <- forecast$over_predict_roll
indUpp <- forecast$under_predict_roll
forecast$intervalSize <- u-l
forecast$intervalScore <- (u - l) + 2 / alpha * (l - y) * indLow + 2 / alpha * (y - u) * indUpp
forecast$weighted <- alpha/2 * forecast$intervalScore
forecast$month <- month(as.POSIXlt(forecast$sim_date, format="%Y-%m-%d"))
forecast$day <- day(as.POSIXlt(forecast$sim_date, format="%Y-%m-%d"))
forecast$year <- year(as.POSIXlt(forecast$sim_date, format="%Y-%m-%d"))
forecast$month_year <- paste0(forecast$month, "-", forecast$year)
  
saveRDS(forecast, file = file.path(outPath, "Workspaces/forecastMerged.RDS"))

WIS <- forecast %>%
  group_by(location, days_ahead, sim_type, variable, mean_value, rolling_truth) %>%
  summarize(summation = sum(weighted))
  
w_0 <- 1 / 2
m <- WIS$mean_value
y <- WIS$rolling_truth
K <- 3
WIS$WIS <- 1 / (K + 1/2) * (w_0 * abs(y - m) + WIS$summation)

MWIS <- WIS %>%
  group_by(location, days_ahead, sim_type, variable) %>%
  summarize(MWIS = mean(WIS, na.rm = TRUE)) %>%
  data.frame()
saveRDS(WIS, file = file.path(outPath, "Workspaces/WIS.RDS"))
saveRDS(MWIS, file = file.path(outPath, "Workspaces/MWIS.RDS"))
