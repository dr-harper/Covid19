
#' Download the latest data from GitHub
#'
loadData <- function(spatial = T){

  df <- readr::read_csv(here::here("data/global/covid_data_global.csv"),
                        col_types = readr::cols())

  # Load Spatial data
  if(spatial){

    world_map <- loadWorldMap()

    df_all_spatial <-
      df %>%
      dplyr::left_join(world_map, by = c("region", "continent")) %>%
      sf::st_as_sf()

    return(df_all_spatial)

  }else{

    return(df)

  }


}


loadWorldMap <- function(){
  return(sf::read_sf(here::here("data/global/covid_data_global_boundaries.geojson")))
}


