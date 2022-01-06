
#' Transform coordinates using R functions
#'
#' @param engine A [crs_engine_fun()]
#' @param prepare A function that converts CRS objects to a canonical string
#'   definition. This reduces the number of function definitions that might
#'   otherwise be required to capture the transforms. The default is to
#'   use [wk::wk_crs_proj_definition()], which returns the shortest possible
#'   definition of a CRS that can be used as a PROJ definition
#'   (usually something like "EPSG:32620"), falling back to a more verbose
#'   definition if an authority string is not found.
#' @param fun A function that accepts the output of [wk::wk_coords()] and modifies
#'   the `x`, `y`, `z`, and/or `m` columns. These columns can also be added
#'   or removed to set or drop the dimensions of the output.
#' @inheritParams crs_engine_null
#'
#' @return
#'   - `crs_engine_fun()`: An engine that can be used for transforms
#'   - `crs_engine_fun_define()`: Returns `engine`, modified mutably
#'   - `crs_engine_fun_get()`: A [function()] that accepts a single argument
#'     that is the output of [wk::wk_coords()].
#'   - `crs_transform_fun()`: Returns a modified `handleable` with `fun` applied
#'     to the coordinates
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
#' crs_transform(
#'   wk::xy(-64, 45, crs = "OGC:CRS84"),
#'   "EPSG:3857",
#'   engine = engine
#' )
#'
#' # can also use a function to apply a generic transform
#' crs_transform_fun(wk::xy(1, 2), function(coords) {
#'   coords$x <- coords$x * 2
#'   coords$y <- coords$y * 2
#'   coords
#' })
#'
crs_engine_fun <- function(prepare = wk::wk_crs_proj_definition) {
  engine <- list(fun = list(), prepare = match.fun(prepare))
  structure(engine, class = "crs2crs_engine_fun")
}

#' @rdname crs_engine_fun
#' @export
crs_engine_fun_define <- function(engine, crs_to, crs_from, fun) {
  if (!is.function(fun)) {
    stop("`fun` must be a function")
  }

  crs_to_hash <- engine$prepare(crs_to)
  crs_from_hash <- engine$prepare(crs_from)

  if (!(crs_to_hash %in% engine$fun)) {
    engine$fun[[crs_to_hash]] <- list()
  }

  engine$fun[[crs_to_hash]][[crs_from_hash]] <- fun

  if (!(crs_to_hash %in% names(engine$crs))) {
    engine$crs[[crs_to_hash]] <- crs_to
  }

  if (!(crs_from_hash %in% names(engine$crs))) {
    engine$crs[[crs_from_hash]] <- crs_from
  }

  invisible(engine)
}

#' @rdname crs_engine_fun
#' @export
crs_engine_fun_get <- function(engine, crs_to, crs_from) {
  crs_to_hash <- engine$prepare(crs_to)
  crs_from_hash <- engine$prepare(crs_from)
  result <- engine$fun[[crs_to_hash]][[crs_from_hash]]

  if (is.null(result)) {
    stop(
      sprintf(
        "crs_engine_fun(): no transform defined from...\n%s\n...to...\n%s",
        format(crs_from), format(crs_to)
      ),
      call. = FALSE
    )
  }

  result
}

#' @rdname crs_engine_fun
#' @export
crs_transform_fun <- function(handleable, fun,
                              strategy = wk::wk_chunk_strategy_coordinates(chunk_size = 65536)) {
  if (inherits(handleable, "sf")) {
    proxy <- sf::st_geometry(handleable)
    n <- nrow(handleable)
  } else if (is.data.frame(handleable)) {
    col_is_handleable <- vapply(handleable, wk::is_handleable, logical(1))
    proxy <- handleable[[which(col_is_handleable)[1]]]
    n <- nrow(handleable)
  } else {
    proxy <- handleable
    n <- length(handleable)
  }

  if (n == 0) {
    return(handleable)
  }

  wk::wk_crs(proxy) <- NULL
  if (wk::wk_is_geodesic(proxy)) {
    wk::wk_is_geodesic(proxy) <- FALSE
  }

  chunks <- strategy(list(proxy), n)
  for (chunk_i in seq_len(nrow(chunks))) {
    slice <- (chunks$from[chunk_i]):(chunks$to[chunk_i])
    chunk <- proxy[slice]
    trans <- crs_transform_fun_trans(chunk, fun)
    proxy[slice] <- wk::wk_transform(chunk, trans)
  }

  wk::wk_restore(handleable, proxy)
}

#' @rdname crs_engine_fun
#' @export
crs_engine_get_wk_trans.crs2crs_engine_fun <- function(engine, handleable, crs_to, crs_from, ...) {
  fun <- crs_engine_fun_get(engine, crs_to, crs_from)
  crs_transform_fun_trans(handleable, fun)
}

crs_transform_fun_trans <- function(handleable, fun) {
  coords_original <- wk::wk_coords(handleable)
  coords <- fun(coords_original)

  stopifnot(
    is.data.frame(coords),
    nrow(coords) == nrow(coords_original),
    all(c("x", "y") %in% names(coords))
  )

  crs_trans_explicit(
    wk::as_xy(coords),
    use_z = "z" %in% names(coords),
    use_m = "m" %in% names(coords)
  )
}
