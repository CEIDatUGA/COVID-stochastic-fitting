# script to automatically render files from crontab (MacOS)
library(rmarkdown)
library(here)

# pandoc location must be hard coded. Find with Sys.getenv("RSTUDIO_PANDOC")
Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/MacOS/pandoc")

source(here::here("code/forward-simulations/bind-all-state-output.R"))

path <- paste0(here::here(),"/docs/")
rmarkdown::render(paste0(path,'overview.Rmd'),output_file=paste0(path,'overview.html'))
# setwd("docs")
rmarkdown::render(paste0(path,'model-details.Rmd'),output_file=paste0(path,'model-details.html'))
rmarkdown::render(paste0(path,'states-model.Rmd'),output_file=paste0(path,'states-model.html'))
rmarkdown::render(paste0(path,'plots.Rmd'),output_file=paste0(path,'plots.html'))
