
#' PROJ command-line transformation engine
#'
#' @param projinfo Path to the projinfo executable
#' @param cct Path to the cct executable
#' @param pipeline A PROJ coordinate transformation pipeline definition
#' @param env A list of environment variables to be applied during calls
#'   to projinfo and proj
#' @param extra_args Extra args to pass to `projinfo` (e.g., to use specific
#'   areas or grid options)
#' @param spatial_test Use "none" to skip querying coordinate operations based
#'   on `bbox`. Use "contains" to include only operations that completely
#'   contain `handleable`, or "intersects" for.
#' @param bbox The optional bounding box of the object.
#'   Defaults to [wk::wk_bbox()] of `handleable` forced to `crs_from`.
#'   Use `NULL` to skip bounding box selectio for a single transformation.
#' @param quiet Use `TRUE` to suppress output.
#' @inheritParams crs_engine_null
#'
#' @return
#'   - `crs_engine_proj_cmd()` returns an engine that uses command-line
#'     PROJ utilities to transform coordinates
#'   - `crs_has_default_proj_cmd()`: `TRUE` if the default arguments
#'     for this engine can transform coordinates
#'   - `crs_engine_proj_pipeline()`: Returns the pipeline definition
#'     that will be used to apply the transform.
#'   - `crs_engine_proj_cmd_trans()`: Returns a modified version of `coords`
#'     after running cct
#'   - `crs_cct_proj_cmd()`: Just run the `cct` tool (e.g., to execute a
#'     predefined pipeline).
#' @export
#'
#' @examples
#' if (crs_has_default_proj_cmd()) {
#'   engine <- crs_engine_proj_cmd()
#'   crs_transform(
#'     wk::xy(-64, 45, crs = "OGC:CRS84"), "EPSG:3857",
#'     engine = engine
#'   )
#' }
#'
crs_engine_proj_cmd <- function(projinfo = getOption("crs2crs.projinfo", "projinfo"),
                                cct = getOption("crs2crs.cct", "cct"),
                                spatial_test = "intersects",
                                env = "current", quiet = FALSE) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("crs_engine_proj_cmd() requires package 'processx'", call. = FALSE)
  }

  engine <- structure(
    list(
      projinfo = projinfo,
      cct = cct,
      spatial_test = spatial_test,
      env = env,
      quiet = quiet
    ),
    class = "crs2crs_engine_proj_cmd"
  )

  # test cct version info
  result <- processx::run(
    engine$cct[1],
    args = c(engine$cct[-1], "--version"),
    env = engine$env,
    echo_cmd = !engine$quiet,
    stderr_callback = if (!engine$quiet) function(x, ...) cat(x, file=stderr())
  )

  ver_match <- regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", result$stdout)
  engine$version <- package_version(
    substr(
      result$stdout,
      ver_match,
      ver_match + attr(ver_match, "match.length") - 1
    )
  )

  # need this for -d flag on cct
  if (engine$version < "5.2.0") {
    stop("crs_engine_proj_cmd() requires PROJ >= 5.2.0")
  }

  result <- processx::run(
    engine$projinfo[1],
    args = c(
      engine$projinfo[-1],
      "-q", "-s", "EPSG:4326", "-t", "OGC:CRS84", "-o", "PROJ"
    ),
    env = engine$env,
    echo_cmd = !engine$quiet,
    stderr_callback = if (!engine$quiet) function(x, ...) cat(x, file=stderr())
  )

  if (!grepl("\\+proj", result$stdout)) {
    stop("testing projinfo resulted in unexpected output!", call. = FALSE)
  }

  engine
}

#' @rdname crs_engine_proj_cmd
#' @export
crs_has_default_proj_cmd <- function() {
  tryCatch(
    {crs_engine_proj_cmd(quiet = TRUE); TRUE},
    error = function(e) {
      FALSE
    }
  )
}

#' @rdname crs_engine_proj_cmd
#' @export
crs_engine_proj_pipeline.crs2crs_engine_proj_cmd <- function(engine, handleable, crs_to,
                                                             crs_from = wk::wk_crs(handleable),
                                                             bbox = wk::wk_bbox(handleable),
                                                             extra_args = character()) {

  if (!is.null(bbox) && (engine$spatial_test != "none")) {
    # don't pass extra arguments for transformed bbox
    engine2 <- engine
    engine2$spatial_test <- "none"
    bbox_lonlat <- unclass(crs_approx_bbox(handleable, "OGC:CRS84", crs_from, engine = engine2))
    bbox_args <- c(
      "--bbox",
      paste(
        bbox_lonlat$xmin, bbox_lonlat$ymin,
        bbox_lonlat$xmax, bbox_lonlat$ymax,
        sep = ","
      ),
      "--spatial-test", engine$spatial_test
    )
  } else {
    bbox_args <- character()
  }

  # projinfo -q -s EPSG:25832 -t EPSG:4093 -o PROJ
  result <- processx::run(
    engine$projinfo[1],
    args = c(
      engine$projinfo[-1],
      "-q", "--single-line",
      "-o", "PROJ",
      "-s", crs_proj_definition(crs_from, engine$version),
      "-t", crs_proj_definition(crs_to, engine$version),
      bbox_args,
      extra_args
    ),
    env = engine$env,
    echo_cmd = !engine$quiet,
    stderr_callback = if (!engine$quiet) function(x, ...) cat(x, file=stderr())
  )

  strsplit(result$stdout, "\r?\n")[[1]]
}

#' @rdname crs_engine_proj_cmd
#' @export
crs_cct_proj_cmd <- function(handleable, pipeline, engine = crs_default_engine(), ...) {
  stopifnot(inherits(engine, "crs2crs_engine_proj_cmd"))
  trans <- crs_engine_proj_cmd_get_trans(engine, handleable, pipeline[1])
  wk::wk_transform(handleable, trans)
}

crs_engine_proj_cmd_trans <- function(engine, pipeline, coords) {
  # project a million coords at a time in chunks
  chunk_size <- 2 ^ 20
  n <- nrow(coords)
  n_chunks <- ((n - 1) %/% chunk_size) + 1

  for (i in seq_len(n_chunks)) {
    coord_i_start <- (i - 1) * chunk_size + 1
    coord_i_end <- min(coord_i_start + chunk_size - 1, n)
    coord_i <- coord_i_start:coord_i_end
    coords[coord_i, ] <-
      crs_engine_proj_cmd_trans_chunk(engine, pipeline, coords[coord_i, , drop = FALSE])
  }

  coords
}

crs_engine_proj_cmd_trans_chunk <- function(engine, pipeline, coords) {
  # the IO here is highly inefficient but it's really hard to work
  # around the constraints of the PROJ cct tool
  tmp_in <- tempfile()
  tmp_out <- tempfile()
  on.exit(unlink(c(tmp_in, tmp_out)))

  # cct doesn't handle non-finite values
  dims <- intersect(c("x", "y", "z", "m"), names(coords))
  xyzt_order <- match(dims, c("x", "y", "z", "m"))
  coords_in <- coords
  coords_in[setdiff(c("x", "y", "z", "m"), dims)] <- 0

  utils::write.table(
    lapply(coords_in[c("x", "y", "z", "m")], function(x) {
      x[!is.finite(x)] <- "nan"
      as.character(x)
    }),
    tmp_in,
    col.names = FALSE, row.names = FALSE, quote = FALSE
  )

  pipeline_split <- strsplit(gsub("^\\+", "", pipeline), "\\s\\+")[[1]]

  result <- processx::run(
    engine$cct[1],
    args = c(
      engine$cct[-1],
      "-d", "16",
      paste0("+", pipeline_split)
    ),
    env = engine$env,
    echo_cmd = !engine$quiet,
    stdin = tmp_in,
    stdout = tmp_out,
    stderr_callback = if (!engine$quiet) function(x, ...) cat(x, file=stderr())
  )

  # error transforms are commented out with a '#' and followed by ((null))
  coords_out_lines <- readLines(tmp_out)
  coords_out_lines <- coords_out_lines[!grepl("^#", coords_out_lines)]
  coords_out_lines <- coords_out_lines[coords_out_lines != ""]
  coords_out_lines[grepl("\\(\\(null\\)\\)", coords_out_lines)] <- " inf inf inf inf"
  writeLines(coords_out_lines, tmp_out)

  coords_out <- utils::read.table(
    tmp_out, header = FALSE, colClasses = "double",
    col.names = c("x", "y", "z", "m")
  )
  for (dim in dims) {
    coords_out[is.na(coords[[dim]]), dim] <- NA_real_
  }
  coords[dims] <- coords_out[dims]
  coords
}

#' @rdname crs_engine_proj_cmd
#' @export
crs_engine_get_wk_trans.crs2crs_engine_proj_cmd <- function(engine, handleable, crs_to, crs_from, ...) {
  pipeline <- crs_engine_proj_pipeline(engine, handleable, crs_to, crs_from)
  crs_engine_proj_cmd_get_trans(engine, handleable, pipeline[1])
}

crs_engine_proj_cmd_get_trans <- function(engine, handleable, pipeline) {
  coords_original <- wk::wk_coords(handleable)
  coords <- crs_engine_proj_cmd_trans(engine, pipeline, coords_original)
  crs_trans_explicit(
    wk::as_xy(coords),
    use_z = "z" %in% names(coords),
    use_m = "m" %in% names(coords)
  )
}
