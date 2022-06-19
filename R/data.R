#' 2018 Auckland Crash Analysis (CAS) Data
#'
#' This is a dataset containing traffic crashes that occurred in 2018 in Auckland Region
#' reported by New Zealand Police.
#'
#' @format An sf dataframe with 12,631 rows and 73 variables.
#' Below are the variables used in road safety analysis :
#' \describe{
#'  \items{crashSeverity}{}
#'  \items{crashYear}{}
#'  \items{minorInjuryCount}{}
#'  \items{seriousInjuryCount}{}
#'  \items{non_injury_crash}{}
#'  \items{geometry}{an sf point geometry type with crs 2193}
#'
#' }
#' @source \url{https://www.nzta.govt.nz/safety/partners/crash-analysis-system/}
"cas_data_akl_2018_proj"


#' 2018 Auckland Driving Journey to Work Data
#'
#' The is a dataset containing coordinate pairs of start and end of a
#' drivers commute to usual residence. The dataset was extracted from Statistics
#' New Zealand 2018 Census main means of work.
#'
#' @format A dataframe with 16035 rows and 24 variables.
#' Below are the variables used in road safety analysis :
#' \describe{
#'  \items{SA2_code_usual_residence_address}{}
#'  \items{SA2_name_usual_residence_address}{}
#'  \items{SA2_usual_residence_easting}{}
#'  \items{SA2_usual_residence_northing}{}
#'  \items{SA2_code_workplace_address}{}
#'  \items{SA2_name_workplace_address}{}
#'  \items{SA2_workplace_easting}{}
#'  \items{SA2_workplace_northing}{}
#'  \items{Drive_a_private_car_truck_or_van}{}
#'  \items{Drive_a_company_car_truck_or_van}{}
#'  \items{start_lat}{}
#'  \items{start_lon}{}
#'  \items{end_lat}{}
#'  \items{end_lon}{}
#'  \items{total_driving}{total count of drivers}
#'
#'
#' }
#' @source \url{https://datafinder.stats.govt.nz/table/104720-2018-census-main-means-of-travel-to-work-by-statistical-area-2/}
"jtw_driving_akl"

#' Statistics Area 2 clipped of Auckland
#'
#' This dataset contains the clipped statistics area 2 boundaries in Auckland from
#' Statistics New Zealand.
#'
#' @format An sf dataframe with 564 rows and 7 variables:
#' \describe{
#'  \items{SA22021_V1}{}
#'  \items{SA22021__1}{}
#'  \items{SA22021__2}{}
#'  \items{LAND_AREA_}{}
#'  \items{Shape_Leng}{}
#'  \items{Shape_Leng}{}
#'  \items{geometry}{ Multipolygon geometry type with lat/lon (EPSG code 4326) crs}
#'}
#' @source \url{https://catalogue.data.govt.nz/dataset/statistical-area-2-2018-clipped-generalised}
"sa2_clipped_geo_akl"
