# Johns Hopkins Dataset updates at around 00:50
# https://github.com/CSSEGISandData/COVID-19
system("git ls-remote https://github.com/CSSEGISandData/COVID-19 refs/heads/master")

# Run script to download latest data from Johns Hopkins then rebuild files
library(tidyverse)
library(here)
ibrary(rmarkdown)

# List all the data prep files
files <- list.files("data", pattern = "*.Rmd", recursive = T, full.names = T)
lapply(files, rmarkdown::render)

# Build the website
siteFiles <- list.files(here("docs"), pattern = ".Rmd")

for(i in siteFiles){
  rmarkdown::render(input = file.path("docs", i))
}

# Load All Countries
countries <- read_csv(here("data/global/covid_data_global.csv"))
countries_list <- unique(countries$region)
count_num <- 1

for(i in c(countries_list)){

  filename <- glue::glue("docs/countries/{region}.html", region = i)

  message(count_num, "/", length(countries_list), ": Rendering ", filename)

  rmarkdown::render(input = here("docs/countries/nationalReportTemplate.Rmd"),
                    output_file =  here(filename),
                    params = list(country = i),
                    quiet = T)

  count_num <- count_num + 1

}

# Commit to github
system("git add -A")
system(glue::glue('git commit -m ":rocket: Automatic Update {time}"',
                  time = format(Sys.time(), '%d %B %Y %H:%M')))
system('git push')

# Put computer to sleep
system('pmset sleepnow')