
test_that("crs_transform() works", {
  expect_identical(
    crs_transform(wk::xy(1, 2), NULL, engine = crs_engine_identity()),
    wk::xy(1, 2)
  )
})

test_that("crs_transform_pipeline() works", {
  stopifnot(crs_has_default_proj_cmd())
  expect_identical(
    crs_transform_pipeline(
      wk::xy(1, 2), "+proj=axisswap +order=2,1",
      engine = crs_engine_proj_cmd(quiet = TRUE)
    ),
    wk::xy(2, 1)
  )
})

test_that("crs_set() works", {
  expect_identical(
    crs_set(wk::xy(1, 2), 1234),
    wk::xy(1, 2, crs = 1234)
  )
})

test_that("crs_get() works", {
  expect_identical(
    crs_get(wk::xy(1, 2, crs = 1234)),
    1234
  )
})

