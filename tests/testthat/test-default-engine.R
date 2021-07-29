
test_that("setting the default engine works", {
  random_engine <- structure(list(), class = "random_engine")
  crs_engine_transform.random_engine <- function(...) NULL
  expect_true(is_crs_engine(random_engine))

  prev_engine <- crs_default_engine()
  with_crs_default_engine(random_engine, {
    expect_identical(crs_default_engine(), random_engine)
  })
  expect_identical(crs_default_engine(), prev_engine)
})

test_that("the default engine can be set from options() or env var", {
  prev_opt <- getOption("crs2crs.default_engine")
  prev_env <- Sys.getenv("R_CRS2CRS_DEFAULT_ENGINE")

  random_engine <- function() structure(list(), class = "random_engine")
  crs_engine_transform.random_engine <- function(...) NULL

  Sys.setenv(R_CRS2CRS_DEFAULT_ENGINE = "crs2crs::crs_engine_identity")
  expect_identical(crs_default_engine_if_unset(), crs_engine_identity())
  Sys.setenv(R_CRS2CRS_DEFAULT_ENGINE = "random_engine")
  expect_identical(crs_default_engine_if_unset(), random_engine())

  options(crs2crs.default_engine = "crs2crs::crs_engine_identity")
  expect_identical(crs_default_engine_if_unset(), crs_engine_identity())
  options(crs2crs.default_engine = "random_engine")
  expect_identical(crs_default_engine_if_unset(), random_engine())
  options(crs2crs.default_engine = random_engine)
  expect_identical(crs_default_engine_if_unset(), random_engine())
  options(crs2crs.default_engine = random_engine())
  expect_identical(crs_default_engine_if_unset(), random_engine())

  options(crs2crs.default_engine = "not a function")
  expect_warning(
    expect_identical(crs_default_engine_if_unset(), crs_engine_null()),
    "Can't find default"
  )

  options(crs2crs.default_engine = 1234)
  expect_warning(
    expect_identical(crs_default_engine_if_unset(), crs_engine_null()),
    "FALSE for engine computed"
  )

  Sys.setenv(R_CRS2CRS_DEFAULT_ENGINE = prev_env)
  options(crs2crs.default_engine = prev_opt)
})
