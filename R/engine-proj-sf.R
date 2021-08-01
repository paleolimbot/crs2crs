
#' Use PROJ via sf to transform coordinates
#'
#' The PROJ sf engine is a thin wrapper around [sf::st_transform()]
#' using [sf::sf_proj_pipelines()] to query the most appropriate transformation.
#' The interface is similar to that of [crs_engine_proj_cmd()] to allow a
#' drop-in replacement for most use-cases.
#'
#' @inheritParams crs_engine_proj_cmd
#' @param ... Arguments passed on to [sf::st_transform()]
#'
#' @return
#'   - `crs_engine_proj_sf()` returns an engine that can be used to transform coordinates
#'
#' @export
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   engine <- crs_engine_proj_sf()
#'   crs_transform(
#'     wk::xy(-64, 45, crs = "OGC:CRS84"), "EPSG:3857",
#'     engine = engine
#'   )
#' }
#'
crs_engine_proj_sf <- function() {
  structure(
    list(
      authority_compliant = FALSE,
      spatial_test = "intersects"
    ),
    class = "crs2crs_engine_proj_sf"
  )
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

  pipelines_df$definition[1]
}

#' @rdname crs_engine_proj_sf
#' @export
crs_engine_proj_pipeline_apply.crs2crs_engine_proj_sf <- function(engine, handleable, pipeline, ...) {
  if (inherits(handleable, "sf") || inherits(handleable, "sfc")) {
    result <- suppressWarnings(sf::st_transform(
      handleable,
      sf::st_crs("OGC:CRS84"),
      pipeline = pipeline,
      ...
    ))
  } else {
    sf_obj <- wk::wk_handle(handleable, wk::sfc_writer())
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
