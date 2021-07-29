
#' Transform Coordinate Reference Systems
#'
#' @inheritParams crs_engine_null
#'
#' @return A modified `handleable` with `crs_to`.
#' @export
#'
#' @examples
#' engine <- crs_engine_fun()
#' engine <- crs_engine_fun_define(engine, "EPSG:3857", "OGC:CRS84", function(coords) {
#'   r <- 6378137
#'   coords$x <- coords$x * pi / 180 * r
#'   coords$y <- log(tan(pi / 4 + coords$y * pi / 180 / 2)) * r
#'   coords
#' })
#'
#' obj <- wk::xy(-64, 45, crs = "OGC:CRS84")
#' crs_transform(obj, "EPSG:3857", engine = engine)
#'
#' with_crs_default_engine(engine, {
#'   crs_transform(obj, "EPSG:3857")
#' })
#'
crs_transform <- function(handleable, crs_to, crs_from = wk::wk_crs(handleable),
                          engine = crs_default_engine(), ...) {
  stopifnot(is_crs_engine(engine, env = parent.frame()))
  crs_engine_transform(engine, handleable, crs_to, crs_from, ...)
}
