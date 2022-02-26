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

get_dist_travelled <- function(polygon, routes, weight){

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

  #avoiding errors
  st_agr(routes) <- "constant"
  st_agr(polygon) <- "constant"

  # getting the intersection of polygons and routes provided
  output <- st_intersection(polygon, routes)

  #getting distance travelled
  output$distance_travelled <- st_length((output))

  #total distance travelled times the number of weight
  output$total_dist <- (output$distance_travelled * output$weight)

  #add polygon data
  st_join(polygon, output[, c('total_dist', 'weight',
                              'distance_travelled', 'geometry')])

}
