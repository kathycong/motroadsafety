#' Calculating Road Safety Risk
#'
#' @param crash_data An sf dataframe containing the geometry of the crash sites.
#' Geometry requires a projected coordinate system.
#'
#' @param crash_weight A vector, the latitude coordinate of crash point.
#'
#' @param exposure An sf dataframe of the total distance travelled by the
#' commuter within each polygon. The sf dataframe needs to contain 'total_dist'
#' column, it is calculated in get_dist_travel() function.
#'  It is important that the sf dataframe includes a unique ID for each polygon
#'  that will be used to aggregate and calculate risk in total crashes per meters.
#'  Geometry requires a projected coordinate system.
#'
#' @param buffer Increasing the exposure polygon size. The default is 10 meters.
#' The unit is dependent on the crash_data and exposure crs provided.
#'
#' @param crs Target coordinate reference system: object of class `crs`, or
#' input string for st_crs, It uses sf::st_transform to change the crs.
#' Refer to \url{https://r-spatial.github.io/sf/reference/st_transform.html}
#'
#' @return This function returns an sf dataframe with new column called
#' risk, the total crashes in a given polygon divided by the total distance
#' travelled in the said polygon.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'data(jtw_driving_akl)
#'data(sa2_clipped_geo_akl)
#'data(cas_data_akl_2018_proj)
#'
#'## initialising the router for New Zealand
#'ghroute::router(osm.file = "osm/new-zealand-latest.osm.pbf")
#'
#'##getting the routes
#'jtw_akl_routes <- get_routes(as.matrix(jtw_driving_akl[c("start_lat", "start_lon", "end_lat", "end_lon")]))
#'
#'##transforming routes from lat/lon to projected crs
#'jtw_driving_akl_with_routes_proj <- sf::st_transform(jtw_driving_akl_with_routes, crs = 2193)
#'sa2_clipped_proj_akl <- sf::st_transform(sa2_clipped_geo_akl, crs  = 2193)
#'
#'## getting distance travelled per SA2 polygon
#'akl_dist_travel <- get_dist_travel(sa2_clipped_proj_akl, jtw_driving_akl_with_routes_proj,
#'weight = jtw_driving_akl_with_routes_proj$total_driving)
#'
#'## getting the risk
#'risk_proj <-  get_risk(cas_data_akl_2018_proj,
#'cas_data_akl_2018_proj$seriousInjuryCount + cas_data_akl_2018_proj$fatalCount,
#'akl_dist_travel)
#'
#' }

get_risk <- function(crash_data, crash_weight, exposure, buffer = 10, crs = 2193){

  ##check that crash_data is an sf dataframe or sfc
  if (!(inherits(crash_data, "data.frame") & inherits(crash_data, "sf")) | inherits(crash_data, "sfc"))
    stop("crash_data input needs to be either an sf dataframe or sfc")

  ##check that polygon is an sf dataframe or sfc
  if (!all(sf::st_geometry_type(crash_data) == "POINT"))
    stop("crash_data geometry type needs to be a POINT")

  ##checks that crash_data has the same number of rows as crash weight
  if (length(crash_weight) != nrow(crash_data))
    stop("crash_data and crash_weight needs to have the same length or number of rows")

  ##check that weight is numeric
  if (!is.null(crash_weight) & !is.numeric(crash_weight))
    stop("crash_weight needs to be numeric")

  ##check that weight does not contain any NAs
  if (!is.null(crash_weight) & any(is.na(crash_weight)))
    stop("crash_weight should not contain any NAs")

  ##check that exposure is an sf dataframe or sfc
  if (!(inherits(exposure, "data.frame") & inherits(exposure, "sf")) | inherits(exposure, "sfc"))
    stop("exposure input needs to be either an sf dataframe or sfc")

  ##check that buffer is numeric
  if (!is.null(buffer) & !is.numeric(buffer) & !is.na(buffer))
    stop("buffer needs to be numeric")

  ##check that exposure and crash_data are both projected
  if (sf::st_is_longlat(exposure) | sf::st_is_longlat(crash_data))
    stop("crash_data and exposure needs to be projected CRS")

  ##check that 'total_dist' column exists in exposure sf df
  if (!any(names(exposure) %in% c("total_dist")))
    stop("exposure dataframe needs to contain 'total_dist' column")

  ##check that crs of exposure and crash_data are the same
  if (sf::st_crs(exposure) != sf::st_crs(crash_data))
    stop("crash_data and exposure needs to have the same crs")

  #create a dataframe
  crash_data$total_crash_weight <-  crash_weight

  exposure <- sf::st_buffer(exposure, buffer)

  #calculating the risk exposure
  output <- sf::st_join(exposure, crash_data)

  output <- output  %>%
    dplyr::group_by_at(dplyr::vars(-total_crash_weight, -geometry)) %>%
    dplyr::summarise(total_crash_weight = sum(total_crash_weight)) %>%
    dplyr::mutate(risk = total_crash_weight/total_dist)

  #tidying up sf dataframe
  output <- sf::st_sf(output[!(names(output) %in% 'geometry')],
                      geometry = output$geometry)

  return(sf::st_transform(output, crs = crs))

}





