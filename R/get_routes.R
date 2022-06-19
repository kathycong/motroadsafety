#' Getting the routes of multiple pairs of geographic coordinate systems
#' (lat/lon) coordinates.
#'
#' It is important that the router is initialised before using this function
#' refer to \url{https://www.rforge.net/doc/packages/ghroute/router.html}
#' for more details.
#'
#' Below is an example of initialising the router, osm.file is a pbf file.
#' The New Zealand pbf file can be located from here
#' \url{https://download.geofabrik.de/australia-oceania/new-zealand.html}
#'
#' router(osm.file, path = "graphhopper-cache", profiles = "car", open = TRUE,
#' make.default = TRUE)
#'
#' @param matrix A 4 column matrix with the order start_latitude,start_longitude,
#' end_latitude, end_longitude. Values in the matrix need to be numeric.
#'
#' @param crs Target coordinate reference system: object of class 'crs', or
#' input string for st_crs.
#'
#' @return This function returns an sf dataframe with an sf geometry containing
#' the simulated route as an sf linestring. It also includes the distance
#' travelled in meters (m) and travel time in seconds.
#'
#' @export
#' @examples
#' \dontrun{
#'
#'data(jtw_driving_akl)
#'
#'## initialising the router for New Zealand
#'ghroute::router(osm.file = "osm/new-zealand-latest.osm.pbf")
#'
#'##getting the routes
#'jtw_akl_routes <- get_routes(as.matrix(jtw_driving_akl[c("start_lat", "start_lon", "end_lat", "end_lon")]))
#'
#' }


get_routes <- function(matrix, crs = NULL){

  ##error handling

  ##need to check that router has been initialised

  ##ensure that input is matrix
  if (!inherits(matrix, "matrix") | !inherits(matrix, "array"))
    stop("input needs to be a matrix")

  ##ensure that input has 4 columns
  if (ncol(matrix) != 4)
    stop("input requires 4 columns")

  ##ensure that matrix is numeric
  if (!is.numeric(matrix))
    stop("matrix contains non-numeric value")

  ##ensure that matrix has no NA values
  if (any(is.na(matrix)))
    stop("matrix contains NAs")

  ## get the routes using ghroute
  r <- ghroute::route(matrix, output = "sf")
  sf::st_crs(r) = 4326

  ## transform the crs if specified
  if(!is.null(crs)){
    r <- sf::st_transform(r, crs = crs)}

  ## binding input matrix with routes and returns a dataframe
  r <- cbind(r, as.data.frame(matrix))

  return(r)

}


