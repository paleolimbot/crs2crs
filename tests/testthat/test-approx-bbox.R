
test_that("crs_approx_bbox() works", {
  engine <- crs_engine_fun()
  engine <- crs_engine_fun_define(engine, "EPSG:3857", "OGC:CRS84", function(coords) {
    r <- 6378137
    coords$x <- coords$x * pi / 180 * r
    coords$y <- log(tan(pi / 4 + coords$y * pi / 180 / 2)) * r
    coords
  })

  trans <- crs_approx_bbox(
    wk::rct(-180, -1, 180, 1),
    "EPSG:3857", "OGC:CRS84",
    engine = engine
  )

  expect_equal(unclass(trans)$xmin, -pi * 6378137)
  expect_equal(unclass(trans)$xmax, pi * 6378137)
})
