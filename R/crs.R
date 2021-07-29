
#' CRS object generic methods
#'
#' @param crs An arbitrary R object
#' @param proj_version A [package_version()] of the PROJ version, or
#'   `NULL` if the PROJ version is unknown.
#'
#' @return
#'   - `crs_hash_prepare()` Returns an object that will be passed to
#'     [rlang::hash()] to identify a CRS object. The object returned by
#'     this method should be canonical.
#'   - `crs_proj_definition()` Returns a string used to represent the
#'     CRS in PROJ. For recent PROJ version you'll want to return WKT2; however
#'     you should check `proj_version` if you want this to work with older
#'     versions of PROJ.
#' @export
#'
#' @examples
#' crs_proj_definition("EPSG:4326")
#'
crs_proj_definition <- function(crs, proj_version = NULL) {
  UseMethod("crs_proj_definition")
}

#' @rdname crs_proj_definition
#' @export
crs_proj_definition.character <- function(crs, proj_version = NULL) {
  stopifnot(length(crs) == 1)
  crs
}

#' @rdname crs_proj_definition
#' @export
crs_proj_definition.double <- function(crs, proj_version = NULL) {
  stopifnot(length(crs) == 1)
  paste0("EPSG:", crs)
}

#' @rdname crs_proj_definition
#' @export
crs_proj_definition.integer <- function(crs, proj_version = NULL) {
  stopifnot(length(crs) == 1)
  paste0("EPSG:", crs)
}
