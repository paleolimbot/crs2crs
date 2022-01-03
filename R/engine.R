
#' Generic Coordinate Transform Engines
#'
#' @inheritParams wk::wk_transform
#' @param engine A transform engine such as [crs_engine_null()]
#' @param pipeline A character vector representing a PROJ coordinate operation
#' @param crs_from,crs_to Source and destination coordinate reference systems
#' @param datum A crs or datum (e.g., WGS84) to use for a long/lat definition
#' @param quiet Use `TRUE` to silence warnings about the dummy identity transform
#' @param ... engine-specific transformation options
#'
#' @return
#'   - `crs_engine_null()` returns an engine that errors on any attempt to perform
#'     a coordinate transform.
#'   - `crs_get_wk_trans()` returns a [wk_trans][wk::wk_transform] that can be used to
#'     transform `handleable`.
#'   - `crs_engine_transform()` returns a modified version of `handleable` with the
#'     transform applied.
#'   - `crs_engine_proj_pipeline()` returns a string representing the pipeline
#'     transformation that can be passed to `crs_engine_proj_pipeline_apply()`
#'   - `crs_engine_proj_pipeline_apply()` Returns a transformed version of
#'     `handleable` with a `NULL` CRS.
#' @export
#'
#' @examples
#' crs_engine_null()
#' crs_transform(
#'   wk::xy(1, 1), "some_crs",
#'   engine = crs_engine_identity()
#' )
#'
crs_engine_null <- function() {
  structure(list(), class = "crs2crs_engine_null")
}

#' @rdname crs_engine_null
#' @export
crs_engine_identity <- function(quiet = FALSE) {
  structure(list(quiet = quiet), class = "crs2crs_engine_identity")
}

#' @rdname crs_engine_null
#' @export
crs_engine_get_wk_trans <- function(engine, handleable, crs_to, crs_from, ...) {
  UseMethod("crs_engine_get_wk_trans")
}

#' @rdname crs_engine_null
#' @export
crs_engine_transform <- function(engine, handleable, crs_to, crs_from = wk::wk_crs(handleable), ...) {
  UseMethod("crs_engine_transform")
}

#' @rdname crs_engine_null
#' @export
crs_engine_proj_pipeline <- function(engine, handleable, crs_to, crs_from = wk::wk_crs(handleable), ...) {
  UseMethod("crs_engine_proj_pipeline")
}

#' @rdname crs_engine_null
#' @export
crs_engine_proj_pipeline_apply <- function(engine, handleable, pipeline, ...) {
  UseMethod("crs_engine_proj_pipeline_apply")
}

#' @rdname crs_engine_null
#' @export
crs_engine_set_longlat <- function(engine, handleable, datum = NULL) {
  UseMethod("crs_engine_set_longlat")
}

#' @rdname crs_engine_null
#' @export
crs_engine_transform.default <- function(engine, handleable, crs_to, crs_from = wk::wk_crs(handleable), ...) {
  if (inherits(crs_to, "wk_crs_inherit") || wk::wk_crs_equal(crs_to, crs_from)) {
    return(handleable)
  } else if (inherits(crs_from, "wk_crs_inherit")) {
    return(wk::wk_set_crs(handleable, crs_to))
  }

  trans <- crs_engine_get_wk_trans(engine, handleable, crs_to, crs_from, ...)
  result <- wk::wk_transform(handleable, trans = trans)
  wk::wk_set_crs(result, crs_to)
}

#' @rdname crs_engine_null
#' @export
crs_engine_transform.crs2crs_engine_null <- function(engine, handleable, crs_to, crs_from = wk::wk_crs(handleable), ...) {
  stop(
    sprintf(
      "crs_engine_null() can't transform from...\n%s\n...to...\n%s",
      format(crs_from), format(crs_to)
    ),
    call. = FALSE
  )
}

#' @rdname crs_engine_null
#' @export
crs_engine_get_wk_trans.crs2crs_engine_identity <- function(engine, handleable, crs_to, crs_from = wk::wk_crs(handleable), ...) {
  if (!isTRUE(engine$quiet)) {
    message(
      sprintf(
        "crs_engine_identity(): Applying a dummy identity transform from...\n%s\n...to...\n%s",
        format(crs_from), format(crs_to)
      )
    )
  }

  wk::wk_affine_identity()
}

#' @rdname crs_engine_null
#' @export
crs_engine_set_longlat.default <- function(engine, handleable, datum = NULL) {
  crs <- wk::wk_crs(handleable)
  if ((is.null(crs) || inherits(crs, "wk_crs_inherit")) && (is.null(datum) || identical(datum, "WGS84"))) {
    return(wk::wk_set_crs(handleable, "OGC:CRS84"))
  }

  if (!is.null(datum)) {
    crs <- datum
  }

  crs_proj <- wk::wk_crs_proj_definition(crs)
  switch(
    crs_proj,
    "OGC:CRS84" =,
    "EPSG:4326" =,
    "WGS84" = wk::wk_set_crs(handleable, "OGC:CRS84"),
    "OGC:CRS27" =,
    "EPSG:4267" =,
    "NAD27" = wk::wk_set_crs(handleable, "OGC:CRS27"),
    "OGC:CRS83" =,
    "EPSG:4269" =,
    "NAD83" = wk::wk_set_crs(handleable, "OGC:CRS83"),
    stop(sprintf("Can't guess authority-compliant long/lat definition from CRS '%s'", format(crs)))
  )
}
