
#' CRS object generic methods
#'
#' @param crs An arbitrary R object
#'
#' @return
#'   - `crs_hash_prepare()` Returns an object that will be passed to
#'     [rlang::hash()] to identify a CRS object. The object returned by
#'     this method should be canonical.
#' @export
#'
#' @examples
#' rlang::hash(crs_hash_prepare(1234))
#'
crs_hash_prepare <- function(crs) {
  UseMethod("crs_hash_prepare")
}

#' @rdname crs_hash_prepare
#' @export
crs_hash_prepare.default <- function(crs) {
  crs
}
