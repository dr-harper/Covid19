#' Produce summary indicators for COVID-19
#'
#' This calculates total deaths, how many people have recovered etc
#'
#'
summaryIndicators <- function(df){

  # Calculate metrics
  deaths <-
    df %>%
    filter(date == max(date)) %>%
    filter(type == "deaths") %>%
    pull(value)

  deaths_7_days_ago <-
    df %>%
    filter(date == max(df$date) - lubridate::ddays(7)) %>%
    filter(type == "deaths") %>%
    pull(value)

  recovered <-
    df %>%
    filter(date == max(date)) %>%
    filter(type == "recovered") %>%
    pull(value)

  cases <-
    df %>%
    filter(date == max(date)) %>%
    filter(type == "cases") %>%
    pull(value)

  cases_7_days_ago <-
    df %>%
    filter(date == max(df$date) - lubridate::ddays(7)) %>%
    filter(type == "cases") %>%
    pull(value)

  increasePercentageCases <- scales::percent(cases/cases_7_days_ago)
  increasePercentageDeaths <- scales::percent(deaths/deaths_7_days_ago)

  # Return object
  return(list(deaths = deaths,
         deaths_7_days_ago = deaths_7_days_ago,
         recovered = recovered,
         cases = cases,
         cases_7_days_ago = cases_7_days_ago,
         increasePercentageCases = increasePercentageCases,
         increasePercentageDeaths = increasePercentageDeaths))

}
