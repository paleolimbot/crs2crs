
crs_engine_env <- new.env(parent = emptyenv())


#' Default coordinate transform engine
#'
#' @inheritParams crs_engine_null
#' @param env The environment to look for named functions if the engine
#'   option is a string.
#' @param expr The expression to evaluate with the transform engine
#'
#' @return
#'   - `crs_engine()` returns the current default transform engine
#'   - `crs_set_engine()` returns the previously set default transform
#'     engine
#'   - `with_crs_engine()` returns the result of `expr`
#' @export
#'
#' @examples
#' crs_engine()
#' prev_engine <- crs_set_engine(crs_engine_identity())
#' # ...
#' crs_set_engine(prev_engine)
#'
#' with_crs_engine(crs_engine_identity(), {
#'   # ...
#' })
#'
crs_engine <- function(env = parent.frame()) {
  if (!("engine" %in% names(crs_engine_env))) {
    engine <- crs_engine_if_unset()
    crs_engine_env$engine <- engine
  }

  crs_engine_env$engine
}

#' @rdname crs_engine
#' @export
crs_set_engine <- function(engine, env = parent.frame()) {
  stopifnot(is_crs_engine(engine, env))
  current <- crs_engine(env = env)
  crs_engine_env$engine <- engine
  invisible(current)
}

#' @rdname crs_engine
#' @export
with_crs_engine <- function(engine, expr, env = parent.frame()) {
  prev <- crs_set_engine(engine, env = env)
  on.exit(crs_set_engine(prev))
  force(expr)
}

#' @rdname crs_engine
#' @export
is_crs_engine <- function(engine, env = parent.frame()) {
  has_s3_method("crs_engine_transform", engine, env) ||
    has_s3_method("crs_engine_get_wk_trans", engine, env)
}

crs_engine_if_unset <- function(env = parent.frame()) {
  opt <- getOption(
    "crs2crs.engine",
    Sys.getenv("R_CRS2CRS_ENGINE", unset = "")
  )

  engine <- if (identical(opt, "")) {
    crs_engine_null()
  } else if (is.character(opt)) {
    opt_split <- strsplit(opt, "::", fixed = TRUE)[[1]]
    if (length(opt_split) == 2) {
      env <- asNamespace(opt_split[[1]])
      opt <- opt_split[[2]]
      inherit <- FALSE
    } else {
      inherit <- TRUE
    }

    if (exists(opt, env, mode = "function", inherits = inherit)) {
      try(get(opt, envir = env, mode = "function", inherits = inherit)())
    } else {
      warning("Can't find default crs2crs engine; using crs_engine_null()", call. = FALSE)
      crs_engine_null()
    }
  } else if (is.function(opt)) {
    try(opt())
  } else {
    opt
  }

  if (!is_crs_engine(engine, env = env)) {
    warning(
      paste0(
        "is_crs_engine() was FALSE for engine computed from\n",
        "`getOption('crs2crs.engine')` or environment variable\n",
        "R_CRS2CRS_ENGINE. Falling back to `crs_engine_null()`"
      ),
      call. = FALSE
    )
    crs_engine_null()
  } else {
    engine
  }
}

has_s3_method <- function(fun, obj, .env) {
  for (cls in class(obj)) {
    if (!is.null(utils::getS3method(fun, cls, optional = TRUE, envir = .env))) {
      return(TRUE)
    }
  }

  FALSE
}
