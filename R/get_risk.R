#' Calculating the risk from diving the total crashes in a given polygon by
#' the total distance travelled by travellers in th esame polygon
#'
#' This function will calculate the risk measure as indicated in the research
#' paper
#'
#' @param exposure The exposure is the distance travelled by the
#' @param crashes A dataframe containing the spfpoint objects where the crashes
#' occurred in lat/lng
#'
#' @return returns dataframe with new column
#'
#'


get_risk <- function(exposure, crashes){

  st_join(exposure, crashes, left = FALSE) %>%
    group_by(SA22021_V1, exposure_m, total_travelers) %>% #need to generalise this
    summarise(injuries = sum(seriousInjuryCount)) %>%
    mutate(risk_per_bkm = injuries/(exposure_m/1000000000))  #converting meters to billion meters

}
