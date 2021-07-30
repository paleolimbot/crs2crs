
#' Use sf to transform coordinates
#'
#' The sf engine is a thin wrapper around [sf::st_transform()] but preserving
#' the output type to mach that of the input. Using the sf engine allows you
#' to use [crs_transform()] as a drop-in replacement for [sf::st_transform()].
#' The sf engine is not authority compliant (i.e., geodedic coordinates are
#' assumed to have lon,lat axis order).
#'
#' @inheritParams crs_engine_null
#' @param ... Arguments passed on to [sf::st_transform()]
#'
#' @return
#'   - `crs_engine_sf()` returns an engine that can be used to transform coordinates
#'
#' @export
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   engine <- crs_engine_sf()
#'   crs_transform(
#'     wk::xy(-64, 45, crs = "OGC:CRS84"), "EPSG:3857",
#'     engine = engine
#'   )
#' }
#'
crs_engine_sf <- function() {
  structure(list(authority_compliant = FALSE), class = "crs2crs_engine_sf")
}

#' @rdname crs_engine_sf
#' @export
crs_engine_transform.crs2crs_engine_sf <- function(engine, handleable, crs_to, crs_from = wk::wk_crs(handleable), ...) {
  old_value <- sf::st_axis_order(authority_compliant = engine$authority_compliant)
  on.exit(sf::st_axis_order(old_value))

  if (inherits(handleable, "sf") || inherits(handleable, "sfc")) {
    sf::st_crs(handleable) <- sf::st_crs(crs_from)
    result <- sf::st_transform(handleable, sf::st_crs(crs_to), ...)
  } else {
    sf_obj <- wk::wk_handle(handleable, wk::sfc_writer())
    sf::st_crs(sf_obj) <- sf::st_crs(crs_from)
    result <- wk::wk_handle(
      sf::st_transform(sf_obj, sf::st_crs(crs_to), ...),
      wk::wk_writer(handleable)
    )
  }

  wk::wk_set_crs(result, crs_to)
}
