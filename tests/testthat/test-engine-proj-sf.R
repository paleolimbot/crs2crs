
test_that("PROJ sf interface fails appropriately when PROJ <= 7.1", {
  sf_proj_version <- package_version(sf::sf_extSoftVersion()["PROJ"])
  skip_if(sf_proj_version >= "7.1")
  expect_error(crs_engine_proj_sf(), "must be built and run")
})

test_that("PROJ sf interface works", {
  skip_if_not(crs_has_proj_sf())

  engine <- crs_engine_proj_sf()
  expect_true(is_crs_engine(engine))
  expect_equal(
    crs_engine_transform(engine, wk::xy(-64, 45, crs = "OGC:CRS84"), "EPSG:3857"),
    wk::xy(-7124447.41076951, 5621521.48619207, crs = "EPSG:3857")
  )
  expect_equal(
    crs_transform(sf::st_sfc(sf::st_point(c(-64, 45)), crs = "OGC:CRS84"), "EPSG:3857", engine = engine),
    sf::st_sfc(sf::st_point(c(-7124447.41076951, 5621521.48619207)), crs = "EPSG:3857")
  )
})

test_that("PROJ sf interface works with all authority compliant values", {
  skip_if_not(crs_has_proj_sf())

  engine <- crs_engine_proj_sf(authority_compliant = TRUE)
  expect_equal(
    crs_engine_transform(engine, wk::xy(45, -64, crs = "EPSG:4326"), "OGC:CRS84"),
    wk::xy(-64, 45, crs = "OGC:CRS84")
  )
  expect_identical(
    crs_transform(sf::st_sfc(sf::st_point(c(45, -64)), crs = "EPSG:4326"), "OGC:CRS84", engine = engine),
    sf::st_sfc(sf::st_point(c(-64, 45)), crs = "OGC:CRS84")
  )

  engine <- crs_engine_proj_sf(authority_compliant = FALSE)
  expect_equal(
    crs_engine_transform(engine, wk::xy(-64, 45, crs = "EPSG:4326"), "OGC:CRS84"),
    wk::xy(-64, 45, crs = "OGC:CRS84")
  )
  expect_identical(
    crs_transform(sf::st_sfc(sf::st_point(c(-64, 45)), crs = "EPSG:4326"), "OGC:CRS84", engine = engine),
    sf::st_sfc(sf::st_point(c(-64, 45)), crs = "OGC:CRS84")
  )
})

test_that("The spatial_test argument works for the PROJ sf interface", {
  skip_if_not(crs_has_proj_sf())
  skip("for now")

  engine <- crs_engine_proj_sf()

  pipe <- crs_engine_proj_pipeline(
    engine,
    wk::xy(33.88199, -84.32385, crs = "NAD27"),
    crs_to = "NAD83"
  )
  expect_match(pipe, "hgridshift")

  pipe <- crs_engine_proj_pipeline(
    engine,
    wk::xy(33.88199, -84.32385, crs = "NAD27"),
    crs_to = "NAD83",
    bbox = NULL
  )
  expect_equal(pipe, "+proj=noop")

  engine$spatial_test <- "none"
  pipe <- crs_engine_proj_pipeline(
    engine,
    wk::xy(33.88199, -84.32385, crs = "NAD27"),
    crs_to = "NAD83"
  )
  expect_equal(pipe, "+proj=noop")

  pipe <- crs_engine_proj_pipeline(
    engine,
    wk::xy(33.88199, -84.32385, crs = "NAD27"),
    crs_to = "NAD83",
    extra_args = c("--area", "USA - North Carolina")
  )
  expect_match(pipe, "hgridshift")
})

test_that("PROJ sf cct interface works", {
  skip_if_not(crs_has_proj_sf())

  engine <- crs_engine_proj_sf()
  expect_equal(
    crs_engine_proj_pipeline_apply(engine, wk::xy(1, 2), "+proj=axisswap +order=2,1"),
    wk::xy(2, 1)
  )
})
