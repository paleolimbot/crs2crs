
#' @export
crs_proj_definition.crs <- function(crs, proj_version = NULL) {
  if (isTRUE(is.na(crs$epsg))) {
    crs_proj_definition(crs$input)
  } else {
    paste0("EPSG:", crs$epsg)
  }
}
