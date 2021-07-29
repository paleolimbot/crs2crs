
test_that("crs_proj_definition() works", {
  expect_identical(crs_proj_definition("EPSG:1234"), "EPSG:1234")
  expect_identical(crs_proj_definition(1234), "EPSG:1234")
  expect_identical(crs_proj_definition(1234L), "EPSG:1234")
})
