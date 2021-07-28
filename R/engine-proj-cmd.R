
#' PROJ command-line transformation engine
#'
#' @param projinfo Path to the projinfo executable
#' @param cct Path to the cct executable
#' @param env A list of environment variables to be applied during calls
#'   to projinfo and proj
#' @param quiet Use `TRUE` to suppress output.
#' @param pipeline A PROJ pipeline definition
#' @param coords The output of [wk::wk_coords()]
#' @inheritParams crs_engine_null
#'
#' @return
#'   - `crs_engine_proj_cmd()` returns an engine that uses command-line
#'     PROJ utilities to transform coordinates
#'   - `crs_has_default_proj_cmd()`: `TRUE` if the default arguments
#'     for this engine can transform coordinates
#'   - `crs_engine_proj_cmd_pipeline()`: Returns the pipeline definition
#'     that will be used to apply the transform.
#'   - `crs_engine_proj_cmd_trans()`: Returns a modified version of `coords`
#'     after running cct
#' @export
#'
#' @examples
#' if (crs_has_default_proj_cmd()) {
#'   engine <- crs_engine_proj_cmd()
#'   crs_engine_transform(engine, wk::xy(-64, 45, crs = "OGC:CRS84"), "EPSG:3857")
#' }
#'
crs_engine_proj_cmd <- function(projinfo = getOption("crs2crs.projinfo", "projinfo"),
                                cct = getOption("crs2crs.cct", "cct"),
                                env = character(), quiet = FALSE) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("crs_engine_proj_cmd() requires package 'processx'", call. = FALSE)
  }

  engine <- structure(
    list(
      projinfo = projinfo,
      cct = cct,
      env = env,
      quiet = quiet
    ),
    class = "crs2crs_engine_proj_cmd"
  )

  # test cct version info
  result <- processx::run(
    engine$cct[1],
    args = c(engine$cct[-1], "--version"),
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
crs_engine_proj_cmd_pipeline <- function(engine, handleable, crs_to,
                                         crs_from = wk::wk_crs(handleable)) {
  # projinfo -q -s EPSG:25832 -t EPSG:4093 -o PROJ
  result <- processx::run(
    engine$projinfo[1],
    args = c(
      engine$projinfo[-1],
      "-q",
      "-o", "PROJ",
      "-s", crs_proj_definition(crs_from, engine$version),
      "-t", crs_proj_definition(crs_to, engine$version)
    ),
    env = engine$env,
    echo_cmd = !engine$quiet,
    stderr_callback = if (!engine$quiet) function(x, ...) cat(x, file=stderr())
  )

  strsplit(result$stdout, "\n", fixed = TRUE)[[1]]
}

#' @rdname crs_engine_proj_cmd
#' @export
crs_engine_proj_cmd_trans <- function(engine, pipeline, coords) {
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
      paste0("+", pipeline_split),
      tmp_in
    ),
    env = engine$env,
    echo_cmd = !engine$quiet,
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
crs_engine_get_wk_trans.crs2crs_engine_proj_cmd <- function(engine, handleable, crs_to, crs_from) {
  pipeline <- crs_engine_proj_cmd_pipeline(engine, handleable, crs_to, crs_from)
  coords_original <- wk::wk_coords(handleable)
  coords <- crs_engine_proj_cmd_trans(engine, pipeline[1], coords_original)
  crs_trans_explicit(
    wk::as_xy(coords),
    use_z = "z" %in% names(coords),
    use_m = "m" %in% names(coords)
  )
}
