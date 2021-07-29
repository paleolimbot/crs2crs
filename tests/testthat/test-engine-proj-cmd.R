
test_that("PROJ command-line interface works", {
  skip_if_not(crs_has_default_proj_cmd())
  engine <- crs_engine_proj_cmd(quiet = TRUE)
  expect_true(is_crs_engine(engine))
  expect_equal(
    crs_engine_transform(engine, wk::xy(-64, 45, crs = "OGC:CRS84"), "EPSG:3857"),
    wk::xy(-7124447.41076951, 5621521.48619207, crs = "EPSG:3857")
  )
})

test_that("The spatial_test argument works for the command-line interface", {
  skip_if_not(crs_has_default_proj_cmd())
  engine <- crs_engine_proj_cmd(quiet = TRUE)

  pipe <- crs_engine_proj_cmd_pipeline(
    engine,
    wk::xy(33.88199, -84.32385, crs = "NAD27"),
    crs_to = "NAD83"
  )
  expect_match(pipe, "hgridshift")

  pipe <- crs_engine_proj_cmd_pipeline(
    engine,
    wk::xy(33.88199, -84.32385, crs = "NAD27"),
    crs_to = "NAD83",
    bbox = NULL
  )
  expect_equal(pipe, "+proj=noop")

  engine$spatial_test <- "none"
  pipe <- crs_engine_proj_cmd_pipeline(
    engine,
    wk::xy(33.88199, -84.32385, crs = "NAD27"),
    crs_to = "NAD83"
  )
  expect_equal(pipe, "+proj=noop")

  pipe <- crs_engine_proj_cmd_pipeline(
    engine,
    wk::xy(33.88199, -84.32385, crs = "NAD27"),
    crs_to = "NAD83",
    extra_args = c("--area", "USA - North Carolina")
  )
  expect_match(pipe, "hgridshift")
})

test_that("PROJ command-line cct interface works", {
  skip_if_not(crs_has_default_proj_cmd())
  engine <- crs_engine_proj_cmd(quiet = TRUE)
  expect_equal(
    crs_cct_proj_cmd(wk::xy(1, 2), "+proj=axisswap +order=2,1", engine = engine),
    wk::xy(2, 1)
  )
})
