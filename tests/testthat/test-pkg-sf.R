
test_that("crs_proj_definition() works for st_crs objects", {
  crs_mock <- structure(list(input = 4326, epsg = 4326), class = "crs")
  expect_identical(crs_proj_definition(crs_mock), "EPSG:4326")
  crs_mock <- structure(list(input = "OGC:CRS84", epsg = NA), class = "crs")
  expect_identical(crs_proj_definition(crs_mock), "OGC:CRS84")
})
