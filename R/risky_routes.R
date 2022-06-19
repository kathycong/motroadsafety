#' Calculating the risk per route
#'
#' @param routes The routes travelled by the commuter with the associated
#' weight or number of travellers. This is an sf dataframe where the geometry a
#' sfc LINESTRING.
#'
#' @param crash_data An sf dataframe containing the geometry of the crash sites.
#' Geometry requires a projected coordinate system.
#'
#' @param radius The distance between the route and crash point. The default is 10.
#' The unit of measure is dependent on input sf dataframes CRS.
#'
#' @return returns an sf dataframe with linestring geometry from routes input
#' and a new column, crash_within_5m, the total number of crash sites encountered
#' by each route.
#'
#' @export
#' @examples
#' \dontrun{
#'
#' data(jtw_driving_akl)
#' data(cas_data_akl_2018_proj)
#'
#' ## initialising the router for New Zealand
#' ghroute::router(osm.file = "osm/new-zealand-latest.osm.pbf")
#'
#'##getting the routes
#'jtw_akl_routes <- get_routes(as.matrix(jtw_driving_akl[c("start_lat",
#'"start_lon", "end_lat", "end_lon")]), crs = 2193)
#'
#'route_riskiness_d_proj <- route_risk(jtw_akl_routes, cas_data_akl_2018_proj)
#'
#'}
#'
#'

route_risk <- function(routes, crash_data, radius = 5){

  ##check that routes is an sf dataframe or sfc
  if (!(inherits(routes, "data.frame") & inherits(routes, "sf")) | inherits(routes, "sfc"))
    stop("routes input needs to be either an sf dataframe or sfc")

  ##check that routes is an sfc LINESTRING
  if (!(inherits(sf::st_geometry(routes), "sfc_LINESTRING")))
    stop("routes need to be an sfc_LINESTRING")

  ##check that crash_data is an sf dataframe or sfc
  if (!(inherits(crash_data, "data.frame") & inherits(crash_data, "sf")) | inherits(crash_data, "sfc"))
    stop("crash_data input needs to be either an sf dataframe or sfc")

  ##check that crash_data is an sf dataframe or sfc
  if (!all(sf::st_geometry_type(crash_data) == "POINT"))
    stop("crash_data geometry type needs to be a POINT")

  ##check that radius is numeric
  if (!is.null(radius) & !is.numeric(radius) & !is.na(radius))
    stop("radius needs to be numeric")

  ##check that crs of routes and crash_data are the same
  if (sf::st_crs(routes) != sf::st_crs(crash_data))
    stop("crash_data and routes needs to have the same crs")

  ##check that routes and crash_data are both projected
  if (sf::st_is_longlat(routes) | sf::st_is_longlat(crash_data))
    stop("crash_data and routes needs to be projected CRS")

  #calculating the total crash points encountered during a commuters' journey
  routes$crash_within_5m <- lengths(sf::st_is_within_distance(routes,
                                                              crash_data,
                                                              dist = radius))

  return(routes)
}

