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

get_dist_travel <- function(polygon, routes, weight){

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
  output$total_dist<- st_length((output)) * output$weight

  output <- st_join(polygon, output[, c('total_dist', 'weight', 'geometry')])

  #grouping by aggregated data
  final_output<- st_drop_geometry(output) %>%
    group_by_at(vars(-total_dist, -weight)) %>%
    summarise(total_dist  = sum(total_dist),
              weight = sum(weight))

  #adding polygon data
  cbind(final_output[, 'geometry'], output)

}



############################

get_dist_travel <- function(polygon, routes, weight){

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
  sf::st_agr(routes) <- "constant"
  sf::st_agr(polygon) <- "constant"

  # getting the intersection of polygons and routes provided
  output <- sf::st_intersection(polygon, routes)

  #getting distance travelled
  output$total_dist<- sf::st_length((output)) * output$weight

  output <- sf::st_join(polygon, output[, c('total_dist', 'weight', 'geometry')])

  #grouping by aggregated data
  get_dist_travelled_test  %>%
    group_by_at( vars(-total_dist, -weight, -geometry)) %>%
    summarise(total_dist  = sum(total_dist),
              weight = sum(weight))

}

