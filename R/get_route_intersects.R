#' Getting the polygons or route intersects
#'
#' This function gets the vector of the polygon ids or codes where a given route
#' is intersected
#'
#' @param routes An sf linesegment object representing routes of multiple
#' source-destination points
#'
#' @param polygon The polygon as geographic coordinate system we wanted to find
#' whether the routes have intersected or not
#'
#' @param inverse The default is FALSE. If FALSE, returns a list of routes length
#' and contains the indices of the polygon that intersected with the routes. If
#' TRUE then returns a list of polygon and contains the indices of the routes that
#' intersected with polygon.
#'
#' @return A list of vectors where a polygon has intersected with a given route.
#' Each vector in the list represents the series of polygons that has  route has
#' intersected and vice versa for routes.
#'
#' @export
#' @examples
#' \dontrun{
#'
#'data(sa2_clipped_geo_akl)
#'data(jtw_akl_routes)
#'
#'## initialising the router for New Zealand
#'ghroute::router(osm.file = "osm/new-zealand-latest.osm.pbf")
#'
#'##getting the routes
#'jtw_akl_routes <- get_routes(as.matrix(jtw_driving_akl[c("start_lat", "start_lon", "end_lat", "end_lon")]))
#'
#'get_route_intersects_test <-  get_route_intersects(jtw_akl_routes, sa2_clipped_geo_akl)
#'
#' }
#'

get_route_intersects <- function(routes, polygon, inverse = FALSE){

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

  ##ensure that the crs is the same
  if (sf::st_crs(polygon) != sf::st_crs(routes))
    stop("routes and polygon needs to have the same crs")


  if(inverse) {

    lapply(seq.int(nrow(polygon)),
           function(i) which(lengths(sf::st_intersects(routes,
                                                       polygon[i, ])) > 0))
  }

  else {
    lapply(seq.int(nrow(routes)),
           function(i) which(lengths(sf::st_intersects(polygon, routes[i, ])) > 0))
  }
}
