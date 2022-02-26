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

  output  %>%
    group_by_at(vars(-total_crash_weight, -geometry)) %>%
    summarise(total_crash_weight = sum(total_crash_weight))

}


