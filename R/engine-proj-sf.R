
#' Use PROJ via sf to transform coordinates
#'
#' The PROJ sf engine is a thin wrapper around [sf::st_transform()]
#' using [sf::sf_proj_pipelines()] to query the most appropriate transformation.
#' The interface is similar to that of [crs_engine_proj_cmd()] to allow a
#' drop-in replacement for most use-cases.
#'
#' @inheritParams crs_engine_proj_cmd
#' @inheritParams crs_engine_sf
#'
#' @return
#'   - `crs_engine_proj_sf()` returns an engine that can be used to transform coordinates
#'
#' @export
#'
#' @examples
#' if (crs_has_proj_sf()) {
#'   engine <- crs_engine_proj_sf()
#'   crs_transform(
#'     wk::xy(-64, 45, crs = "OGC:CRS84"), "EPSG:3857",
#'     engine = engine
#'   )
#' }
#'
crs_engine_proj_sf <- function(authority_compliant = TRUE,
                               spatial_test = "intersects") {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' required to use crs_engine_proj_sf()")
  }

  sf_proj_version <- package_version(sf::sf_extSoftVersion()["PROJ"])
  if (sf_proj_version < "7.1") {
    stop(
      "sf must be built and run against PROJ >= 7.1 to use crs_engine_proj_sf()",
      call. = FALSE
    )
  }

  structure(
    list(
      authority_compliant = authority_compliant,
      spatial_test = spatial_test
    ),
    class = "crs2crs_engine_proj_sf"
  )
}

#' @rdname crs_engine_proj_sf
#' @export
crs_has_proj_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(FALSE)
  }

  sf_proj_version <- package_version(sf::sf_extSoftVersion()["PROJ"])
  sf_proj_version >= "7.1"
}

#' @rdname crs_engine_proj_sf
#' @export
crs_engine_proj_pipeline.crs2crs_engine_proj_sf <- function(engine, handleable, crs_to,
                                                       crs_from = wk::wk_crs(handleable),
                                                       bbox = wk::wk_bbox(handleable),
                                                       ...) {
  if (!is.null(bbox) && (engine$spatial_test != "none")) {
    # don't pass extra arguments for transformed bbox
    engine2 <- engine
    engine2$spatial_test <- "none"
    bbox_lonlat <- unclass(crs_approx_bbox(handleable, "OGC:CRS84", crs_from, engine = engine2))
    AOI <- c(bbox_lonlat$xmin, bbox_lonlat$ymin, bbox_lonlat$xmax, bbox_lonlat$ymax)
  } else {
    AOI <- numeric(0)
  }

  use_lookup <- c("none" = "NONE", "contains" = "INTERSECTION", "intersets" = "INTERSECTION")

  pipelines_df <- sf::sf_proj_pipelines(
    sf::st_crs(crs_from),
    sf::st_crs(crs_to),
    Use = unname(use_lookup[engine$spatial_test]),
    strict_containment = identical(engine$spatial_test, "contains"),
    axis_order_authority_compliant = engine$authority_compliant,
  )

  if (length(pipelines_df$definition) == 0) {
    stop(
      paste0(
        "crs_engine_proj_sf() can't calculate transformation between\n",
        format(crs_from),
        "...and...\n",
        format(crs_to)
      ),
      call. = FALSE
    )
  }

  pipelines_df$definition[1]
}

#' @rdname crs_engine_proj_sf
#' @export
crs_engine_proj_pipeline_apply.crs2crs_engine_proj_sf <- function(engine, handleable, pipeline, ...) {
  if (inherits(handleable, "sf") || inherits(handleable, "sfc")) {
    sf::st_crs(handleable) <- sf::st_crs("OGC:CRS84")
    result <- suppressWarnings(sf::st_transform(
      handleable,
      sf::st_crs("OGC:CRS84"),
      pipeline = pipeline,
      ...
    ))
  } else {
    sf_obj <- wk::wk_handle(handleable, wk::sfc_writer())
    sf::st_crs(sf_obj) <- sf::st_crs("OGC:CRS84")
    result <- wk::wk_handle(
      suppressWarnings(sf::st_transform(
        sf_obj,
        sf::st_crs("OGC:CRS84"),
        pipeline = pipeline,
        ...
      )),
      wk::wk_writer(handleable)
    )
  }

  wk::wk_set_crs(result, NULL)
}

#' @rdname crs_engine_proj_sf
#' @export
crs_engine_transform.crs2crs_engine_proj_sf <- function(engine, handleable, crs_to, crs_from = wk::wk_crs(handleable), ...) {
  pipeline <- crs_engine_proj_pipeline(engine, handleable, crs_to, crs_from, ...)
  result <- crs_engine_proj_pipeline_apply(engine, handleable, pipeline[1])
  wk::wk_set_crs(result, crs_to)
}
