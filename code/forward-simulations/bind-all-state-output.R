# This script binds the individual state-level output files
# into one large data frame that can be accessed for web
# deployment and other reports.


# Load libraries ----------------------------------------------------------

library(tidyverse)
library(here)


# Loop over files and row bind --------------------------------------------

all_files <- list.files(here("output/current/"), ".csv")
us_output <- tibble()  # empty storage tibble
for(do_file in all_files) {
  tmp_file <- paste0(here("output/current/"), "/", do_file)
  tmp <- read.csv(tmp_file, stringsAsFactors = FALSE)
  tmp <- tmp %>%
    mutate(median_value = ifelse(is.na(sim_type), mean_value, median_value))
  us_output <- bind_rows(us_output, tmp)
}

write_csv(us_output, here("output/", "us_current_results.csv"))
