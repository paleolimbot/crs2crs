
test_that("crs_engine_fun() works", {
  engine <- crs_engine_fun()
  expect_true(is_crs_engine(engine))
  engine <- crs_engine_fun_define(engine, "EPSG:3857", "OGC:CRS84", function(coords) {
    r <- 6378137
    coords$x <- coords$x * pi / 180 * r
    coords$y <- log(tan(pi / 4 + coords$y * pi / 180 / 2)) * r
    coords
  })

  expect_equal(
    crs_engine_transform(engine, wk::xy(-64, 45, crs = "OGC:CRS84"), "EPSG:3857"),
    wk::xy(-7124447.41076951, 5621521.48619207, crs = "EPSG:3857")
  )

  expect_error(
    crs_engine_transform(engine, wk::xy(-64, 45, crs = "OGC:CRS84"), "def not a CRS"),
    "no transform defined"
  )

  expect_error(crs_engine_fun_define(engine, "a", "b", NULL), "must be a function")
})

test_that("crs_transform_fun() works", {
  expect_identical(
    crs_transform_fun(wk::xy(1, 2), function(coords) {
      coords$x <- 6
      coords$y <- 12
      coords
    }),
    wk::xy(6, 12)
  )

  # check that chunking worked
  chunk_count <- 0L
  expect_identical(
    crs_transform_fun(wk::xy(1:65537, 1:65537), function(coords) {
      chunk_count <<- chunk_count + 1L
      coords$x <- coords$x * 2
      coords$y <- coords$y * 3
      coords
    }),
    wk::xy((1:65537) * 2, (1:65537) * 3)
  )
  expect_identical(chunk_count, 2L)
})
