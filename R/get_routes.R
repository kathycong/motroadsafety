#' Getting the routes of multiple geographic coordinate system (lat/lon) points
#'
#' This function returns a dataframe where each row is an sf line segment object
#'
#' @param matrix A 4 column matrix with the order start_latitude, start_longitude, end_latitude,
#' end_longitude. Values in the matrix needs to be numeric.
#' @param crs Target coordinate reference system: object of class 'crs', or input string for st_crs


get_routes <- function(matrix, crs = NULL){

  ##error handling

  ##need to check that router has been initialised

  ##ensure that input is matrix
  if (!inherits(matrix, "matrix") | !inherits(matrix, "array"))
    stop("input needs to be a matrix")

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


