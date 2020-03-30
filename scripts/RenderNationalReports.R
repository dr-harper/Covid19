library(tidyverse)
library(here)

countries <- read_csv(here("data/global/covid_data_global.csv"))
countries_list <- unique(countries$region)

library(rmarkdown)

for(i in c(countries_list)){

  filename <- glue::glue("docs/countries/{region}.html", region = i)

  rmarkdown::render(input = here("reports/national/nationalReportTemplate.Rmd"),
                    output_file =  here(filename),
                    params = list(country = i))

}


global <- here("reports/global/dailyOverview.Rmd")

rmarkdown::render(global,
                  output_file = here("docs/global.html"))
