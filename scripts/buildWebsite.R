# Script Checks if Johns Hopkins repository has updated and waits if not,
# then will recompile files before pushing the changes to GitHub
# After compiling puts computer to sleep
#
# Probably could do something a bit more sophisticated that this but it works!

library(tidyverse)
library(here)
library(rmarkdown)

# Johns Hopkins Dataset updates at around 00:50
# https://github.com/CSSEGISandData/COVID-19

# 1 --- Checking if data has updated

# Load file of the previous hash
hash_previous <- read_file("scripts/hash.txt") %>%
  stringr::str_replace_all("\n", "")

# Function to extract the commit ID of the latest committ to Johns Hopkins
checkHash <- function(){
  system("git ls-remote https://github.com/CSSEGISandData/COVID-19 refs/heads/master", intern = T) %>%
    stringr::str_replace("\trefs/heads/master", "")}

hash_current <- checkHash()

# Checks if hash as updated. If not will wait for 5 minutes
while(hash_current == hash_previous){

  message(format(Sys.time(), "%H:%M"), " Hash Unchanged, Waiting")
  Sys.sleep(5 * 60)
  hash_current <- checkHash()

}

# Save new hash to file for future reference
hash_current %>%
  write(file = "scripts/hash.txt", append = F)

# 2 --- Running Analysis

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

# 3 --- Saving to GitHub

# Commit to github
system("git add -A")
system(glue::glue('git commit -m ":rocket: Automatic Update {time}"',
                  time = format(Sys.time(), '%d %B %Y %H:%M')))
system('git push')

# Put computer to sleep
system('pmset sleepnow')
