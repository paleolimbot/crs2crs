% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/engine-proj-cmd.R
\name{crs_engine_proj_cmd}
\alias{crs_engine_proj_cmd}
\alias{crs_has_default_proj_cmd}
\alias{crs_engine_proj_pipeline.crs2crs_engine_proj_cmd}
\alias{crs_engine_proj_pipeline_apply.crs2crs_engine_proj_cmd}
\alias{crs_engine_transform.crs2crs_engine_proj_cmd}
\alias{crs_engine_get_wk_trans.crs2crs_engine_proj_cmd}
\title{PROJ command-line transformation engine}
\usage{
crs_engine_proj_cmd(
  projinfo = getOption("crs2crs.projinfo", "projinfo"),
  cct = getOption("crs2crs.cct", "cct"),
  spatial_test = "intersects",
  env = NULL,
  quiet = FALSE
)

crs_has_default_proj_cmd()

\method{crs_engine_proj_pipeline}{crs2crs_engine_proj_cmd}(
  engine,
  handleable,
  crs_to,
  crs_from = wk::wk_crs(handleable),
  bbox = wk::wk_bbox(handleable),
  extra_args = character(),
  ...
)

\method{crs_engine_proj_pipeline_apply}{crs2crs_engine_proj_cmd}(engine, handleable, pipeline, ...)

\method{crs_engine_transform}{crs2crs_engine_proj_cmd}(
  engine,
  handleable,
  crs_to,
  crs_from = wk::wk_crs(handleable),
  ...
)

\method{crs_engine_get_wk_trans}{crs2crs_engine_proj_cmd}(engine, handleable, crs_to, crs_from, ...)
}
\arguments{
\item{projinfo}{Path to the projinfo executable}

\item{cct}{Path to the cct executable}

\item{spatial_test}{Use "none" to skip querying coordinate operations based
on \code{bbox}. Use "contains" to include only operations that completely
contain \code{handleable}, or "intersects" for.}

\item{env}{A list of environment variables to be applied during calls
to projinfo and proj}

\item{quiet}{Use \code{TRUE} to suppress output.}

\item{engine}{A transform engine such as \code{\link[=crs_engine_null]{crs_engine_null()}}}

\item{handleable}{A geometry vector (e.g., \code{\link[wk:wkb]{wkb()}}, \code{\link[wk:wkt]{wkt()}}, \code{\link[wk:xy]{xy()}},
\code{\link[wk:rct]{rct()}}, or \code{\link[sf:sfc]{sf::st_sfc()}}) for which \code{\link[wk:wk_handle]{wk_handle()}} is defined.}

\item{crs_to}{Source and destination coordinate reference systems}

\item{crs_from}{Source and destination coordinate reference systems}

\item{bbox}{The optional bounding box of the object.
Defaults to \code{\link[wk:wk_bbox]{wk::wk_bbox()}} of \code{handleable} forced to \code{crs_from}.
Use \code{NULL} to skip bounding box selectio for a single transformation.}

\item{extra_args}{Extra args to pass to \code{projinfo} (e.g., to use specific
areas or grid options)}

\item{...}{engine-specific transformation options}

\item{pipeline}{A PROJ coordinate transformation pipeline definition}
}
\value{
\itemize{
\item \code{crs_engine_proj_cmd()} returns an engine that uses command-line
PROJ utilities to transform coordinates
\item \code{crs_has_default_proj_cmd()}: \code{TRUE} if the default arguments
for this engine can transform coordinates
\item \code{crs_engine_proj_cmd_trans()}: Returns a modified version of \code{coords}
after running cct
}
}
\description{
PROJ command-line transformation engine
}
\examples{
if (crs_has_default_proj_cmd()) {
  engine <- crs_engine_proj_cmd()
  crs_transform(
    wk::xy(-64, 45, crs = "OGC:CRS84"), "EPSG:3857",
    engine = engine
  )
}

}
