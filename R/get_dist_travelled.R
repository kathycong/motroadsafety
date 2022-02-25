#' Getting the distance travelled by a traveller per polygon
#'
#' This function returns a dataframe of the total distance travelled by
#' travellers  for each polygon.
#'
#' @param routes The routes travelled by the traveller with the associated
#' weight or number of travellers.
#'
#' @param polygon An sfc class object. The polygon we are trying to intersect
#' with teh routes.
#'
#' @param polygon_id A unique polygon identifier used for groupby and
#' joining geometry data.
#'
#' @param weight The weight for each line segment or route. This is optional.

get_dist_travelled <- function(routes, polygon, polygon_id, weight){

  #weight is an optional argument

  #if weight param is missing then default to 1
  if(missing(weight)){
    weight <- rep(1, nrow(routes))
  } else weight


  #############checks#########
  # 1. input has the correct class
  # 2. input has the correct length i.e. length(routes) == weight
  # 3. Data has no NaNs

  #binding routes and weight as its easier for aggregation later on
  routes <- cbind(routes, weight = weight)

  #merging polygon_id and polygon for ease in aggregation later on
  polygon_id_df <- data.frame(polygon_id = polygon_id)
  polygon <- st_sf(polygon_id_df, geometry = polygon)

  #avoiding errors
  st_agr(routes) <- "constant"
  st_agr(polygon) <- "constant"

  # getting the intersection of polygons and routes provided
  output <- st_intersection(polygon, routes)

  #total distance travelled times the number of weight
  output$total_dist <- (st_length((output)) * output$weight) #/1000 as recommended by simon to do this later to avoid machine truncation

  st_geometry(output) <- NULL

  #aggregating the total distance travelled by the traveller agains the polygon code
  output <- output %>%
    group_by(polygon_id) %>%
    summarise(person_travelled_m = sum(total_dist), total_weight = sum(weight)) %>%
    mutate(person_travelled_m = as.numeric(person_travelled_m))

  #adding geometric info
  output <- left_join(polygon, output)

  #returns only polygons that intersects with the routes, ensure no NAs are returned
  output[which(!is.na(output$person_travelled_m)), ]

}
