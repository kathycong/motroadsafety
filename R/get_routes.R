#' Getting the routes of multiple geographic coordinate system (lat/lon) points
#'
#' This function returns a dataframe of where each row an sf linesegment object from the lat/lon points
#'
#' @param lat_source The latitude coordinates of the source point, needs to be a vector
#' @param lng_aource The longitude coodinates of the source point, needs to be a vector
#' @param lat_dest The latitude coordinates of the destination point, needs to be a vector
#' @param lng_dest The longitude coodinates of the destination point, needs to be a vector


get_routes <- function(lat_source, lng_source, lat_dest, lng_dest, weight){
##note that gh::route uses the parameter "vehicle" to specify the transportation
##mode

  ##Error handling on inputs
  ##check that lengths for all inputs are the same
  if (!all(sapply(list(length(lng_source), length(lat_dest),
                       length(lng_dest)), FUN = identical,
                  length(lat_source))))
    stop("input lengths are not the same")

  ##check that all input are numeric
  if (!all(sapply(list(is.numeric(lng_source), is.numeric(lat_dest),
                       is.numeric(lng_dest)), FUN = identical,
                  is.numeric(lat_source))))
    stop("inputs must be a numeric vector")

  ##check that all inputs are not NAs
  if (!all(sapply(list(is.na(lng_source), is.na(lat_dest),
                       is.na(lng_dest)), FUN = identical,
                  is.na(lat_source))))
    stop("inputs must not contain NAs")


  ##get list of routes
  routes_list <- lapply(seq.int(length(lat_source)), function(i) {

    ##change the route to an sf linesegment and bundle it into a list
    sf::st_as_sf(sf::st_sfc(sf::st_linestring(ghroute::route(lat_source[i],
                                                 lng_source[i],
                                                 lat_dest[i],
                                                 lng_dest[i])[[1]][[1]][ ,2:1]),
                            crs = 4326))})

  ##transforms the nested list of sf linesegments or routes into a dataframe
  routes <- do.call(rbind, routes_list)

  cbind(routes, travelers = weight)
}
