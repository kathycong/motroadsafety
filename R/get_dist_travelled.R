#' Getting the distance travelled by a commuter
#'
#' @param routes The routes travelled by the commuter with the associated
#' weight or number of travellers. This is an sf dataframe where the geometry a
#' sfc LINESTRING.
#'
#' @param polygon The polygon we are trying to intersect
#' with the routes. It is an sf dataframe where the geometry is either an
#' sfc POLYGON or sfc MULTIPOLYGON. It is important that the sf dataframe
#' includes a unique ID for each polygon that will be used to aggregate and
#' calculate the total distance travelled.
#'
#' @param weight The weight for each line segment or route. This is optional.
#'
#' @return This function returns an sf dataframe of the total distance
#' travelled by the commuter within each polygon.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' data(jtw_driving_akl)
#' data(sa2_clipped_geo_akl)
#'
#' ## initialising the router for New Zealand
#' ghroute::router(osm.file = "osm/new-zealand-latest.osm.pbf")
#'
#' ##getting the routes
#' jtw_akl_routes <- get_routes(as.matrix(jtw_driving_akl[c("start_lat", "start_lon", "end_lat", "end_lon")]))
#'
#' ##transforming routes from lat/lon to projected crs
#' jtw_driving_akl_with_routes_proj <- sf::st_transform(jtw_driving_akl_with_routes, crs = 2193)
#' sa2_clipped_proj_akl <- sf::st_transform(sa2_clipped_geo_akl, crs  = 2193)
#'
#' ## getting distance travelled per SA2 polygon
#' akl_dist_travel <- get_dist_travel(sa2_clipped_proj_akl, jtw_driving_akl_with_routes_proj, weight = jtw_driving_akl_with_routes_proj$total_driving)
#' }
#'

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

  all(sf::st_geometry_type(routes) == "LINESTRING")

  ##check that polygon is is either an sfc_POLYGON or sfc_MULTIPOLYGON
  if (!(inherits(sf::st_geometry(polygon), "sfc_MULTIPOLYGON") | inherits(sf::st_geometry(polygon), "sfc_POLYGON")))
    stop("routes need to be an sfc_MULTIPOLYGON or sfc_POLYGON")

  ##ensure that the crs is the same
  if (sf::st_crs(polygon) != sf::st_crs(routes))
    stop("routes and polygon needs to have the same crs")

  ##check that weight if is numeric
  if (!is.null(weight) & !is.numeric(weight))
    stop("weight needs to be numeric")

  ##check that weight does not contain any NAs
  if (!is.null(weight) & any(is.na(weight)))
    stop("weight should not contain any NAs")

  ##check that weight and routes has the same length
  if (!is.null(weight) & length(weight) != nrow(routes))
    stop("weight and routes should have the same length or rows")

  #if weight param is missing then default to 1
  if(is.null(weight)){
    weight <- rep(1, nrow(routes))
  } else weight

  #binding routes and weight as its easier for aggregation later on
  routes <- cbind(routes, weight)

  # getting the intersection of polygons and routes provided
  output <- sf::st_intersection(polygon, routes)

  #getting distance traveled
  output$total_dist<- sf::st_length((output)) * output$weight

  output <- sf::st_join(polygon, output[, c('total_dist', 'weight', 'geometry')])

  #grouping by aggregated data
  output <- output  %>%
    dplyr::group_by_at(dplyr::vars(-total_dist, -weight, -geometry)) %>%
    dplyr::summarise(total_dist  = sum(total_dist),
                     weight = sum(weight))

  #tidying up sf dataframe
  output <- sf::st_sf(output[!(names(output) %in% 'geometry')],
                      geometry = output$geometry)

  return(output)

}

