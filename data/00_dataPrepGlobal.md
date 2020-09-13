Data Preparation for Global COVID-19 Cases
================
Michael Harper

# About

This document loads and preprocesses the data which is used for further
analysis:

  - Merges John Hopkins into single dataset
  - Resolves name differences with spatial dataset and other files
  - Enriches data by calculating additional metrics such as cases per
    million, rate of change

<!-- end list -->

``` r
knitr::opts_chunk$set(echo = TRUE)

library(readr)
library(tidyverse)
library(sf)
```

    ## Linking to GEOS 3.8.1, GDAL 3.1.2, PROJ 7.1.0

# Specify Global Naming

The analysis uses different datasets which slightly different naming
conventions. For example, some datasets had `United States` while other
had `US`. A dictionary of replacement names was therefore created which
is later used to recode the data. The `recode` function is used
throughout the example referring to the following list:

``` r
# Resolves inconsistencies between naming in data sources
# Not all datasets have inconsistences but set here and applied to all
countryNameDict <- c("Czech Republic" = "Czechia",
                     "United Kingdom" = "UK",
                     "Bosnia Herzegovina" = "Bosnia and Herzegovina",
                     "Iran, Islamic Rep." = "Iran",
                     "Macedonia" = "North Macedonia",
                     "United States" = "US",
                     "Russian Federation" = "Russia",
                     "Dem. Rep. Korea" = "North Korea",
                     "Congo (Kinshasa)" = "DRC",
                     "Democratic Republic of the Congo" = "DRC",
                     "Congo (Brazzaville)" = "Congo",
                     "Republic of the Congo" = "Congo",
                     "Côte d'Ivoire" = "Cote d'Ivoire",
                     "Bahamas, The" = "Bahamas",
                     "Taiwan*" = "Taiwan",
                     "Korea, South" = "South Korea")
```

# Loading John Hopkins Dataset

<!---
# Potentially could use this API in future?
# Note, does not provide province level data
# https://github.com/pomber/covid19
--->

For the analysis, I am using the COVID-19 dataset produced by John
Hopkins University. They provide a daily updating dataset at a national
level, which provides the cases, recoveries and deaths associated with
the virus. The data can be accessed through the GitHub [repository
here](https://github.com/CSSEGISandData/COVID-19), and they also offer a
great dashboard for visualising the data
[here](https://coronavirus.jhu.edu/map.html).

There are three separate files for different records stored within the
following directory:

``` r
# Using the John hopkins dataset which updates daily
rootDir <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

files <- c("time_series_covid19_confirmed_global.csv", "time_series_covid19_deaths_global.csv", "time_series_covid19_recovered_global.csv")
```

When the data is loaded, some basic data cleaning to make the data
usable for graphing. For those familiar with the tidyverse in R, most
functions prefer to have data in [long
format](http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/).

``` r
# Function clean data for each of the files and aggregate
dataToLong <- function(filename, var){
  
  df <- read_csv(file.path(rootDir, filename), col_types = cols())
  
  # Convert to long data
  dataLong <- 
    df %>%
    tidyr::gather(key = "date", "cases", -c("Province/State",  "Country/Region", "Lat", "Long"))
  
  # Aggregate to only have a single region per country
  df_all <-
    dataLong %>%
    set_names(c("province", "region", "lat", "long", "date", "value")) %>%
    group_by(region, date) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(date = lubridate::parse_date_time(date, "%m%d%y"),
           region = recode(region, !!!countryNameDict), 
           type = var)
  
  return(df_all)
} 
```

This function is run across all three input dataframes and then merged
into a single dataframe:

``` r
# Combine data
df_all <- map2_df(.x = files,
                  .y = c("cases", "deaths", "recovered"),
                  .f = function(x,y) dataToLong(x,y))
```

# Calculate Additional Metrics

Some additional metrics are calculated from the raw data:

  - `rate of change`: how many new cases have there been since the day
    before and 7 days previous estimates.
  - `days Since outbreak` calculates how long it has been since the
    first reported case in a country

<!-- end list -->

``` r
# calculate when the first reported case was
firstOutbreak <- 
  df_all %>%
  filter(type == "cases") %>%
  filter(value != 0) %>%
  group_by(region) %>%
  filter(date == min(date)) %>%
  mutate(firstOutbreak = date) %>%
  select(-c(date, value, type))

# do the same as above, but this time work out when they passed the 200th case
first200Outbreak <- 
  df_all %>%
  filter(type == "cases") %>%
  filter(value > 200) %>%
  group_by(region) %>%
  filter(date == min(date)) %>%
  mutate(first200Outbreak = date) %>%
  select(-c(date, value, type))

firstDeath <- 
  df_all %>%
  filter(type == "deaths") %>%
  filter(value > 0) %>%
  group_by(region) %>%
  filter(date == min(date)) %>%
  mutate(firstDeath = date) %>%
  select(-c(date, value, type))

first10Deaths <- 
  df_all %>%
  filter(type == "deaths") %>%
  filter(value > 10) %>%
  group_by(region) %>%
  filter(date == min(date)) %>%
  mutate(first10Deaths = date) %>%
  select(-c(date, value, type))

# Calculate additional metrics such as rate of contagion
df_cases_extra <-
  df_all %>%
  left_join(firstOutbreak) %>%
  left_join(first200Outbreak) %>%
  left_join(firstDeath) %>%
  left_join(first10Deaths) %>%  
  group_by(region, type) %>%
  arrange(region, type, date) %>%
  mutate(daysSinceOutbreak = as.numeric(date - firstOutbreak)/86400 + 1,
         daysSinceOutbreak = ifelse(daysSinceOutbreak < 0, NA, daysSinceOutbreak),
         daysSinceFirstDeath = as.numeric(date - firstDeath)/86400 + 1,
         daysSinceFirstDeath = ifelse(daysSinceFirstDeath < 0, NA, daysSinceFirstDeath),
         daysSince10Deaths = as.numeric(date - first10Deaths)/86400 + 1,
         daysSince10Deaths = ifelse(daysSince10Deaths < 0, NA, daysSince10Deaths),
         daysSince200Outbreak = as.numeric(date - first200Outbreak)/86400 + 1,
         daysSince200Outbreak = ifelse(daysSince200Outbreak < 0, NA, daysSince200Outbreak),
         changeDaily = value - lag(value, n = 1),
         changeWeekly = value - lag(value, n = 7),
         changeDailyPerc = round(changeDaily/value * 100, 0),
         changeDailyPerc = ifelse(is.nan(changeDailyPerc), NA, changeDailyPerc),
         changeWeeklyPerc = round(changeWeekly/value * 100, 0),
         changeWeeklyPerc = ifelse(is.nan(changeWeeklyPerc), NA, changeWeeklyPerc)) %>%
  select(-c(firstOutbreak, first200Outbreak, firstDeath, first10Deaths))
```

    ## Joining, by = "region"
    ## Joining, by = "region"
    ## Joining, by = "region"
    ## Joining, by = "region"

# Load Population Data

To calculate normalised infection rates, we will use the population
dataset from the
[WorldBank](https://data.worldbank.org/indicator/SP.POP.TOTL):

``` r
# World bank population estimates
pop <- read_csv("dataIn/worldBankPop.csv",
                col_types = cols()) %>%
  select(c("Country Name", "2018")) %>%
  set_names("region", "pop") %>%
  mutate(pop = round(pop/10^6, 2),
         region = recode(region, !!!countryNameDict))
```

    ## Warning: Missing column names filled in: 'X65' [65]

This is merged with the full dataset and then used to calculate the
infection rates:

``` r
# Merge data with shapefile
# Normalise rates
df_all_extra <- 
  df_cases_extra %>%
  left_join(pop) %>%
  mutate(value = replace_na(value, 0),
         casespermillion = round(value/pop, 2)) %>%
  arrange(region, date)
```

    ## Joining, by = "region"

# Income Groups

``` r
income_group <- read_csv(here::here("data/dataIn/WorldIncomeGroups.csv"), col_types = cols()) %>%
  filter(Year == "Fiscal Year 2020") %>%
  select(c("Economy", "Income Group")) %>%
  set_names("region", "Income") %>%
  mutate(region = recode(region, !!!countryNameDict))


df_all_extra <- 
  df_all_extra %>%
  left_join(income_group)
```

    ## Joining, by = "region"

# Spatial Data

Spatial data is useful for visualising the datasets. These are formatted
to be consistent with the second dataset, making renaming the fields to
match and altering names to match to enable an easy spatial join:

``` r
# Load global boundaries
world_map <- 
  spData::world %>%
  st_as_sf() %>%
  select(name_long, continent) %>%
  rename("region" = "name_long") %>%
  filter(region != "Antarctica") %>% # Not useful display antartica
  mutate(region = recode(region, !!!countryNameDict))
```

These can be merged into a single long data format. Note this format is
not very efficient for storage as it duplicates the spatial data, but is
required to make it easy to use within the **tidyverse** workflow:

``` r
# This is not saved as it takes up more space 70+ MBs
df_all_spatial <- 
  df_all_extra %>%
  left_join(world_map)
```

    ## Joining, by = "region"

``` r
# Retain the continent variable
df_all_extra <- 
df_all_spatial %>%
  st_as_sf() %>%
  sf::st_drop_geometry()
```

# Save Data

``` r
write_csv(df_all_extra, "../data/global/covid_data_global.csv", append = FALSE)

sf::write_sf(world_map, "../data/global/covid_data_global_boundaries.geojson", delete_dsn = T)
```

f

# Session Info

``` r
sessionInfo()
```

    ## R version 4.0.2 (2020-06-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.6
    ## 
    ## Matrix products: default
    ## BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] sf_0.9-5        rmarkdown_2.3   here_0.1        forcats_0.5.0   stringr_1.4.0   dplyr_1.0.2     purrr_0.3.4     readr_1.3.1     tidyr_1.1.2    
    ## [10] tibble_3.0.3    ggplot2_3.3.2   tidyverse_1.3.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.5         lattice_0.20-41    lubridate_1.7.9    class_7.3-17       assertthat_0.2.1   rprojroot_1.3-2    digest_0.6.25     
    ##  [8] packrat_0.5.0      R6_2.4.1           cellranger_1.1.0   backports_1.1.9    reprex_0.3.0       evaluate_0.14      e1071_1.7-3       
    ## [15] httr_1.4.2         pillar_1.4.6       rlang_0.4.7        curl_4.3           readxl_1.3.1       rstudioapi_0.11    raster_3.3-13     
    ## [22] blob_1.2.1         munsell_0.5.0      broom_0.7.0        compiler_4.0.2     modelr_0.1.8       xfun_0.16          pkgconfig_2.0.3   
    ## [29] htmltools_0.5.0    tidyselect_1.1.0   codetools_0.2-16   fansi_0.4.1        crayon_1.3.4       dbplyr_1.4.4       withr_2.2.0       
    ## [36] grid_4.0.2         spData_0.3.8       jsonlite_1.7.1     gtable_0.3.0       lifecycle_0.2.0    DBI_1.1.0          magrittr_1.5      
    ## [43] units_0.6-7        scales_1.1.1       KernSmooth_2.23-17 cli_2.0.2          stringi_1.4.6      fs_1.5.0           sp_1.4-2          
    ## [50] xml2_1.3.2         ellipsis_0.3.1     generics_0.0.2     vctrs_0.3.4        tools_4.0.2        glue_1.4.2         hms_0.5.3         
    ## [57] yaml_2.2.1         colorspace_1.4-1   classInt_0.4-3     rvest_0.3.6        knitr_1.29         haven_2.3.1
