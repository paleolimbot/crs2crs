
#' Use sf to transform coordinates
#'
#' The sf engine is a thin wrapper around [sf::st_transform()] but preserving
#' the output type to mach that of the input. Using the sf engine allows you
#' to use [crs_transform()] as a drop-in replacement for [sf::st_transform()].
#' The sf engine is not authority compliant (i.e., geodedic coordinates are
#' assumed to have lon,lat axis order).
#'
#' @inheritParams crs_engine_null
#' @param authority_compliant Use `TRUE` or `FALSE` to apply [sf::st_transform()]
#'   with [sf::st_axis_order()] temporarily set. The default `NA` uses whatever
#'   the current value of [sf::st_axis_order()] happens to be at the time of
#'   the transform.
#' @param ... Arguments passed on to [sf::st_transform()]
#'
#' @return
#'   - `crs_engine_sf()` returns an engine that can be used to transform coordinates
#'
#' @export
#'
#' @examples
#' if (crs_has_sf()) {
#'   engine <- crs_engine_sf()
#'   crs_transform(
#'     wk::xy(-64, 45, crs = "OGC:CRS84"), "EPSG:3857",
#'     engine = engine
#'   )
#' }
#'
crs_engine_sf <- function(authority_compliant = NA) {
  structure(list(authority_compliant = authority_compliant), class = "crs2crs_engine_sf")
}

#' @rdname crs_engine_sf
#' @export
crs_has_sf <- function() {
  requireNamespace("sf", quietly = TRUE)
}

#' @rdname crs_engine_sf
#' @export
crs_engine_transform.crs2crs_engine_sf <- function(engine, handleable, crs_to, crs_from = wk::wk_crs(handleable), ...) {
  if (!identical(engine$authority_compliant, NA)) {
    old_value <- sf::st_axis_order(authority_compliant = engine$authority_compliant)
    on.exit(sf::st_axis_order(old_value))
  }

  if (inherits(handleable, "sf") || inherits(handleable, "sfc")) {
    suppressWarnings(sf::st_crs(handleable) <- sf::st_crs(crs_from))
    result <- sf::st_transform(handleable, sf::st_crs(crs_to), ...)
  } else {
    sf_obj <- wk::wk_handle(handleable, wk::sfc_writer())
    suppressWarnings(sf::st_crs(sf_obj) <- sf::st_crs(crs_from))
    result <- wk::wk_handle(
      sf::st_transform(sf_obj, sf::st_crs(crs_to), ...),
      wk::wk_writer(handleable)
    )
  }

  wk::wk_set_crs(result, crs_to)
}
