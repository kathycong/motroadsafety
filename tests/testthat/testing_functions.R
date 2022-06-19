
test_that("get_dist_travelled works", {

  jtw_akl_routes_proj_sample  =  readRDS("../data/jtw_akl_routes_proj_sample.rds")
  sa2_clipped_geo_akl_sample =  readRDS("../data/sa2_clipped_geo_akl_sample.rds")
  sa2_clipped_proj_akl_sample = sf::st_transform(sa2_clipped_geo_akl_sample, crs = 2193)

  #wrong geometry type
  expect_error(get_dist_travel(jtw_akl_routes_proj_sample, sa2_clipped_geo_akl_sample))

  #crs is not the same
  expect_error(get_dist_travel(sa2_clipped_geo_akl_sample, jtw_akl_routes_proj_sample))

  #warning is due to groupby
  expect_warning({dist_travel = get_dist_travel(sa2_clipped_proj_akl_sample, jtw_akl_routes_proj_sample)})

  #ensure that output should have the same rows as input polygons
  expect_true(nrow(dist_travel) == nrow(sa2_clipped_proj_akl_sample))

  #result should be a data.frame
  expect_s3_class(dist_travel, "data.frame")

  #result should be an sf object
  expect_true(is(dist_travel, "sf"))
})


test_that("get_risk works", {

  dist_travel_proj_sample  =  readRDS("../data/dist_travel_sample.rds")
  cas_data_akl_2018_proj_sample =  readRDS("../data/cas_data_akl_2018_proj_sample.rds")
  dist_travel_geo_sample = sf::st_transform(dist_travel_proj_sample, crs = 4326)

  #wrong geometry type
  expect_error(get_risk(dist_travel_geo_sample, cas_data_akl_2018_proj_sample$seriousInjuryCount + cas_data_akl_2018_proj_sample$fatalCount, cas_data_akl_2018_proj_sample))

  #crs is not the same
  expect_error(get_risk(cas_data_akl_2018_proj_sample, cas_data_akl_2018_proj_sample$seriousInjuryCount + cas_data_akl_2018_proj_sample$fatalCount, dist_travel_geo_sample))

  risk_d =  get_risk(cas_data_akl_2018_proj_sample, cas_data_akl_2018_proj_sample$seriousInjuryCount + cas_data_akl_2018_proj_sample$fatalCount, dist_travel_proj_sample)

  #ensure that output should have the same rows as input routes
  expect_true(nrow(risk_d) == nrow(dist_travel_proj_sample))

  #result should be a data.frame
  expect_s3_class(risk_d, "data.frame")

  #result should be an sf object
  expect_true(is(risk_d, "sf"))
})


test_that("test if risky_routes work", {

  jtw_akl_routes_proj_sample  =  readRDS("../data/jtw_akl_routes_proj_sample.rds")
  cas_data_akl_2018_proj_sample =  readRDS("../data/cas_data_akl_2018_proj_sample.rds")
  cas_data_akl_2018_geo_sample = sf::st_transform(cas_data_akl_2018_proj_sample, crs = 4326)

  #wrong geometry type
  expect_error(route_risk(cas_data_akl_2018_proj_sample, jtw_akl_routes_proj_sample))

  #crs is not the same
  expect_error(route_risk(jtw_akl_routes_proj_sample, cas_data_akl_2018_geo_sample))

  risk_routes_d = route_risk(jtw_akl_routes_proj_sample, cas_data_akl_2018_proj_sample)

  #ensure that output should have the same rows as input polygons
  expect_true(nrow(jtw_akl_routes_proj_sample) == nrow(risk_routes_d))

  #result should be a data.frame
  expect_s3_class(risk_routes_d, "data.frame")

  #result should be an sf object
  expect_true(is(risk_routes_d, "sf"))
})


test_that("get_route_intersects works", {

  jtw_akl_routes_proj_sample  =  readRDS("../data/jtw_akl_routes_proj_sample.rds")
  sa2_clipped_geo_akl_sample =  readRDS("../data/sa2_clipped_geo_akl_sample.rds")
  sa2_clipped_proj_akl_sample = sf::st_transform(sa2_clipped_geo_akl_sample, crs = 2193)

  #wrong geometry type
  expect_error(get_route_intersects(sa2_clipped_proj_akl_sample, jtw_akl_routes_proj_sample))

  #crs is not the same
  expect_error(get_route_intersects(jtw_akl_routes_proj_sample, sa2_clipped_geo_akl_sample))

  route_intersects_d = get_route_intersects(jtw_akl_routes_proj_sample, sa2_clipped_proj_akl_sample)

  #ensure that output should have the same rows as input routes
  expect_true(length(route_intersects_d) == nrow(jtw_akl_routes_proj_sample))

  route_intersects_inv_d = get_route_intersects(jtw_akl_routes_proj_sample, sa2_clipped_proj_akl_sample, inverse = TRUE)

  #ensure that output should have the same rows as input routes
  expect_true(length(route_intersects_inv_d) == nrow(sa2_clipped_proj_akl_sample))

  #result should be an sf object
  expect_type(route_intersects_d, "list")
})
