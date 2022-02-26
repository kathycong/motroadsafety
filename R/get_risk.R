#' Calculating the risk from diving the total crashes in a given polygon by
#' the total distance travelled by travellers in th esame polygon
#'
#' This function will calculate the risk measure as indicated in the research
#' paper
#'
#' @param person_travelled The distance travelled by the person aggregated
#'
#' @param crash_lat The latitude coordinate of crash point. Needs to be a vector.
#'
#' @param crash_lng The longitude coordinate of crash point. Needs to be a vector.
#'
#' @param crash_weight The number of crashes in a given lat/lng points.
#'
#' @return This function returns a dataframe with new column called
#' risk_per_m as
#'
#'

get_risk <- function(person_travelled, crash_lat, crash_lng, crash_weight){

  #create a dataframe
  crash_data <-  data.frame(crash_weight = crash_weight,
                            crash_lat = crash_lat,
                            crash_lng = crash_lng)

  #change lat and lng as sf objects
  crash_data_sf <- st_as_sf(crash_data, coords = c("crash_lng", "crash_lat"),
                            crs = 4326)

  #calculating the risk exposure
  st_join(person_travelled, crash_data_sf, left = FALSE) %>%
    group_by(polygon_id, person_travelled_m, total_weight) %>%
    summarise(total_crash_weight = sum(crash_weight)) %>%
    mutate(risk_per_m = total_crash_weight/person_travelled_m)
}

########################



get_risk <- function(crash_lat, crash_lng, crash_weight, polygon){

  #create a dataframe
  crash_data <-  data.frame(total_crash_weight = crash_weight,
                            crash_lat = crash_lat,
                            crash_lng = crash_lng)

  #change lat and lng as sf objects
  crash_data_sf <- st_as_sf(crash_data, coords = c("crash_lng", "crash_lat"),
                            crs = 4326)

  #calculating the risk exposure
  output <- st_join(polygon, crash_data_sf)

  output <- st_drop_geometry(output) %>%
    group_by_at(vars(-total_crash_weight)) %>%
    summarise(total_crash_weight = sum(total_crash_weight))



  cbind(polygon['geometry'], output)
}


