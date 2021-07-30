
test_that("the sf engine works", {
  skip_if_not_installed("sf")
  engine <- crs_engine_sf()
  expect_identical(
    crs_transform(wk::xy(-64, 45, crs = "EPSG:4326"), "OGC:CRS84", engine = engine),
    wk::xy(-64, 45, crs = "OGC:CRS84")
  )
  expect_identical(
    crs_transform(sf::st_sfc(sf::st_point(c(-64, 45)), crs = "EPSG:4326"), "OGC:CRS84", engine = engine),
    sf::st_sfc(sf::st_point(c(-64, 45)), crs = "OGC:CRS84")
  )
})
