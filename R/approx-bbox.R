
#' Approximate bounding box transform
#'
#' @inheritParams crs_transform
#' @param n The number of points in a grid used to approximate the transform
#'
#' @return A [wk::rct()] representing the transformed bounding box
#' @export
#'
#' @examples
#' crs_approx_bbox(
#'   wk::rct(-180, -1, 180, 1, crs = "OGC:CRS84"), "OGC:CRS84",
#'   engine = crs_engine_identity()
#' )
#'
crs_approx_bbox <- function(handleable, crs_to, crs_from = wk::wk_crs(handleable),
                            engine = crs_default_engine(), n = 20, ...) {
  bbox <- unclass(wk::wk_bbox(handleable))
  seq_x <- seq(bbox$xmin, bbox$xmax, length.out = n)
  seq_y <- seq(bbox$ymin, bbox$ymax, length.out = n)
  pts <- wk::xy(
    rep(seq_x, n),
    rep(seq_y, each = n),
    crs = wk::wk_crs(handleable)
  )
  pts_trans <- crs_transform(pts, crs_to, crs_from, engine = engine, ...)
  wk::wk_bbox(pts_trans)
}
