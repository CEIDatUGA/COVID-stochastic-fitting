# Render diagnostic plots over time by state


# Enter states and range of dates -------------------------------------------------------------

states <- c("Indiana", "Maryland", "New Jersey", "Ohio", "Washington")
startdate <- "2020-09-01" # Y-m-d
enddate <- format(Sys.time(), '%Y-%m-%d')

# Don't alter the code below ------------------------------------------------------------------

library(here)

for(s in states) {
  rmarkdown::render(here("docs/plots_singlestate.Rmd"), 
                    output_file = paste0(here("docs/plots/"),"plots_",s,format(Sys.time(), '%Y%m%d'),".html"),
                    params = list(
                      statename = s,
                      start = startdate,
                      end = enddate)
                    )
}
