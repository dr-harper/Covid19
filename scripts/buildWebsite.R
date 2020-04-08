
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

for(i in c(countries_list)){

  filename <- glue::glue("docs/countries/{region}.html", region = i)

  rmarkdown::render(input = here("docs/countries/nationalReportTemplate.Rmd"),
                    output_file =  here(filename),
                    params = list(country = i))

}



