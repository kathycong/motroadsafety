#' Getting the routes of multiple geographic coordinate system (lat/lon) points
#'
#' This function returns a dataframe of where each row an sf linesegment object from the lat/lon points
#'
#' @param lat_source The latitude coordinates of the source point, needs to be a vector
#' @param lng_aource The longitude coodinates of the source point, needs to be a vector
#' @param lat_dest The latitude coordinates of the destination point, needs to be a vector
#' @param lng_dest The longitude coodinates of the destination point, needs to be a vector

get_routes <- function(lat_source, lng_source, lat_dest, lng_dest, osm_dir){
  ##note that gh::route uses the parameter "vehicle" to specify the transportation
  ##mode


  if(missing(osm_dir)){
    if (!file.exists("new-zealand-latest.osm.pbf")){
      download.file("https://download.geofabrik.de/australia-oceania/new-zealand-latest.osm.pbf", "new-zealand-latest.osm.pbf")
      warning("Downloading osm pbf file from https://download.geofabrik.de/australia-oceania/new-zealand-latest.osm.pbf")
    }
    osm_dir <- "new-zealand-latest.osm.pbf"   }


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

  #initialise router
  ghroute::router(osm.file = osm_dir)

  ##get list of routes
  routes_list <- lapply(seq.int(length(lat_source)), function(i) {

    ##change the route to an sf linesegment and bundle it into a list
    tryCatch(ghroute::route(lat_source[i],
                                 lng_source[i],
                                 lat_dest[i],
                                 lng_dest[i])[[1]][[1]][ ,2:1],
             error = function(e) NA)})

  ##transforms the nested list of sf linesegments or routes into a dataframe

  output <- purrr::map_dfr(routes_list, function(x){ sf::st_as_sf(

    if(any(is.na(x))){
      sf::st_sfc(x, crs = 4326)}
    else {
      sf::st_sfc(sf::st_linestring(x), crs = 4326)})
  })

  #https://stackoverflow.com/questions/21937640/handling-java-lang-outofmemoryerror-when-writing-to-excel-from-r
  gc()
 rJava::.jcall("java/lang/System", method = "gc")

 output
}



