
x=read_csv('data/applemobilitytrends-2020-04-27.csv')
xx <- x %>% dplyr::filter(region == "Georgia") %>%
            select(-geo_type, -region, -alternative_name) %>%
            pivot_longer(- transportation_type, names_to = "Date", values_to = "Value") %>%
            mutate(Date = as.Date(Date))

cv <- readRDS(here("output/rel-beta-change-covar.RDS"))
start_date = "2020-03-01"
cv$Date = seq.Date(from = as.Date(start_date), length = 84, by = "day")

plot(xx$Date,xx$Value,type='b')
lines(cv$Date,cv$rel_beta_change*100,col='red')
