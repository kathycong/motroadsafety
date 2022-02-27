route_risk <- function(routes, crash_lat, crash_lng, crash_weight){

  #create a dataframe
  crash_data <-  data.frame(total_crash_weight = crash_weight,
                            crash_lat = crash_lat,
                            crash_lng = crash_lng)

  #change lat and lng as sf objects
  crash_data_sf <- st_as_sf(crash_data, coords = c("crash_lng", "crash_lat"),
                            crs = 4326)

  #sum the number of crash points within 5 meters from the route
  output <- lapply(seq.int(nrow(routes)), function(i){

    sum(crash_data_sf[lengths(st_is_within_distance(crash_data_sf,
                                                       routes[i, ],
                                                       dist = 5)) >0,
    ]$total_crash_weight)
  })

  cbind(routes, total_crash_weight = unlist(output))

}
