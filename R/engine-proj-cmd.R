
crs_engine_proj_cmd <- function(projinfo = "projinfo", proj = "proj",
                                env = list(), quiet = TRUE) {
  structure(
    list(
      projinfo = projinfo,
      proj = proj,
      env = env,
      quiet = quiet
    ),
    class = "crs2crs_engine_proj_cmd"
  )
}

crs_engine_proj_cmd_pipeline <- function(engine, handleable, crs_to,
                                         crs_from = wk::wk_crs(handleable)) {

}

crs_engine_proj_cmd_trans <- function(engine, pipeline, coords) {

}

crs_engine_get_wk_trans.crs2crs_engine_proj_cmd <- function(engine, handleable, crs_to, crs_from) {
  pipeline <- crs_engine_proj_cmd_pipeline(
    engine,
    handleable,
    crs_proj_definition(crs_to), crs_proj_definition(proj_from)
  )

  coords_original <- wk::wk_coords(handleable)
  coords <- crs_engine_proj_cmd_trans(engine, coords, pipeline[1])

  crs_trans_explicit(
    wk::as_xy(coords),
    use_z = "z" %in% names(coords),
    use_m = "m" %in% names(coords)
  )
}
