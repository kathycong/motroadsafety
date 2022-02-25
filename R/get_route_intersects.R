#' Getting the polygons the route intersected
#'
#' This function gets the vector of the polygon ids or codes where a given route
#' is intersected
#'
#' @param routes An sf linesegment object representing routes of multiple
#' source-destination points
#' @param polygon The polygon as geographic coordinate system we wanted to find
#' whether the routes have intersected or not
#' @return A list of vectors where a polygon has intersected with a given route.
#' Each vector in the list represents the series of polygons the route has
#' intersected.
#'

get_route_intersects <- function(routes, polygon){

  lapply(seq.int(nrow(routes)),

  function(i) which(lengths(sf::st_intersects(polygon, routes[i, ])) > 0))
}
