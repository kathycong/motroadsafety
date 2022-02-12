#' Getting the distance travelled by a traveller per polygon
#'
#' This function returns a dataframe of the total distance travelled by
#' travellers  for each polygon
#'
#' @param routes The routes travelled by the traveller with the associated
#' weight or number of travellers
#'
#' @param polygon The polygon

get_dist_travel <- function(routes, polygon){

  # get or set relation_to_geometry attribute of an sf object to constant to
  # to avoid any warnings

  st_agr(routes) <- "constant"
  st_agr(polygon) <- "constant"

  # getting the intersection of polygons and routes provided
  output <- st_intersection(polygon, routes)

  #total distance travelled times the number of traveler
  output$total_dist <- (st_length((output)) * output$travelers)
  st_geometry(output) <- NULL

  #aggregating the total distance travelled by the traveller agains the polygon code
  output <- output %>%
    group_by(SA22021_V1) %>% #this needs to be more generic as this code varies for different dataset
    summarise(exposure_m = sum(total_dist), total_travelers = sum(travelers)) %>%
    mutate(exposure_m = as.numeric(exposure_m))

  #adding polygon info
  output <- left_join(polygon, output)

  #returns only polygons that intersects with the routes, ensure no NAs are returned
  output[which(!is.na(output$exposure_m)), ]

}
