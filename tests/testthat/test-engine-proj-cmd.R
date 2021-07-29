
test_that("PROJ command-line interface works", {
  skip_if_not(crs_has_default_proj_cmd())
  engine <- crs_engine_proj_cmd(quiet = TRUE)
  expect_true(is_crs_engine(engine))
  expect_equal(
    crs_engine_transform(engine, wk::xy(-64, 45, crs = "OGC:CRS84"), "EPSG:3857"),
    wk::xy(-7124447.41076951, 5621521.48619207, crs = "EPSG:3857")
  )
})
