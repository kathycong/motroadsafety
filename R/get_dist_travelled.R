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
#'
#' @param weight The weight for each line segment or route. This is optional.

get_dist_travel <- function(polygon, routes, weight = NULL){

  #weight is an optional argument

  ##check that polygon is an sf dataframe or sfc
  if (!(inherits(polygon, "data.frame") & inherits(polygon, "sf")) | inherits(polygon, "sfc"))
    stop("polygon input needs to be either an sf dataframe or sfc")

  ##check that routes is an sf dataframe or sfc
  if (!(inherits(routes, "data.frame") & inherits(routes, "sf")) | inherits(routes, "sfc"))
    stop("routes input needs to be either an sf dataframe or sfc")

  ##check that routes is an sfc LINESTRING
  if (!(inherits(sf::st_geometry(routes), "sfc_LINESTRING")))
    stop("routes need to be an sfc_LINESTRING")

  ##check that polygon is is either an sfc_POLYGON or sfc_MULTIPOLYGON
  if (!(inherits(sf::st_geometry(polygon), "sfc_MULTIPOLYGON") | inherits(sf::st_geometry(polygon), "sfc_POLYGON")))
    stop("routes need to be an sfc_MULTIPOLYGON or sfc_POLYGON")

  ##check that weight if not null is numeric
  if (!is.null(weight) & !is.numeric(weight))
    stop("weight needs to be numeric")

  ##check that weight does not contain any NAs
  if (!is.null(weight) & any(is.na(weight)))
    stop("weight should not contain any NAs")

  ##check that weight does not contain any NAs
  if (!is.null(weight) & length(weight) != nrow(routes))
    stop("weight and routes should have the same length or rows")


  #if weight param is missing then default to 1
  if(is.null(weight)){
    weight <- rep(1, nrow(routes))
  } else weight


  #binding routes and weight as its easier for aggregation later on
  routes <- cbind(routes, weight)

  #avoiding errors
  #sf::st_agr(routes) <- "constant"
  #sf::st_agr(polygon) <- "constant"

  # getting the intersection of polygons and routes provided
  output <- sf::st_intersection(polygon, routes)

  #getting distance traveled
  output$total_dist<- sf::st_length((output)) * output$weight

  output <- sf::st_join(polygon, output[, c('total_dist', 'weight', 'geometry')])

  #grouping by aggregated data
  output <- output  %>%
    group_by_at( vars(-total_dist, -weight, -geometry)) %>%
    summarise(total_dist  = sum(total_dist),
              weight = sum(weight))

  #tidying up sf dataframe
  output <- sf::st_sf(output[!(names(output) %in% 'geometry')],
                      geometry = output$geometry)

  return(output)


  #output %>% mapview(zcol = 'total_dist',
     #     layer.name = "total distance travelled")

  }

