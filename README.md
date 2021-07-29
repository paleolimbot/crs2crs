
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crs2crs

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/paleolimbot/crs2crs/branch/master/graph/badge.svg)](https://codecov.io/gh/paleolimbot/crs2crs?branch=master)
[![R-CMD-check](https://github.com/paleolimbot/crs2crs/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/crs2crs/actions)
<!-- badges: end -->

The goal of crs2crs is to provide a flexible framework for reproducible
coordinate transforms. With the last few releases of PROJ it can be
difficult to predict which transform will be applied when converting
between spatial reference systems. The crs2crs package provides tools to
make these more explicit or to define explicit transforms between
reference systems for future reproducibility. It is capable of using
PROJ in several forms but doesn’t require it.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/crs2crs")
```

If you can load the package, you’re good to go!

``` r
library(crs2crs)
```

## Example

Use the PROJ command-line engine to transform some coordinates:

``` r
crs_set_default_engine(crs_engine_proj_cmd())
#> Running cct --version
#> Running projinfo -q -s 'EPSG:4326' -t 'OGC:CRS84' -o PROJ
pos <- wk::xy(-64, 45, crs = "OGC:CRS84")
crs_transform(pos, "EPSG:3857")
#> Running projinfo -q --single-line -o PROJ -s 'OGC:CRS84' -t 'EPSG:3857' \
#>   --bbox '-64,45,-64,45' --spatial-test intersects
#> Running cct -d 16 '+proj=pipeline' '+step' '+proj=unitconvert' '+xy_in=deg' \
#>   '+xy_out=rad' '+step' '+proj=webmerc' '+lat_0=0' '+lon_0=0' '+x_0=0' \
#>   '+y_0=0' '+ellps=WGS84'
#> <wk_xy[1] with CRS=EPSG:3857>
#> [1] (-7124447 5621521)
```

You can also define an engine that uses R functions if you only need a
few transforms:

``` r
engine <- crs_engine_fun()
engine <- crs_engine_fun_define(engine, "EPSG:3857", "OGC:CRS84", function(coords) {
  r <- 6378137
  coords$x <- coords$x * pi / 180 * r
  coords$y <- log(tan(pi / 4 + coords$y * pi / 180 / 2)) * r
  coords
})
crs_set_default_engine(engine)
crs_transform(pos, "EPSG:3857")
#> <wk_xy[1] with CRS=EPSG:3857>
#> [1] (-7124447 5621521)
```

The `crs_transform()` function is built on `wk::wk_transform()`, so you
can pass it any object that defines a `wk::wk_handle()` method (like sf
objects!).

``` r
library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
nc <- read_sf(system.file("shape/nc.shp", package = "sf")) %>%
  st_transform("OGC:CRS84")

nc %>% 
  crs_transform("EPSG:3857", crs_from = "OGC:CRS84")
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -9386880 ymin: 4012991 xmax: -8399788 ymax: 4382079
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 100 × 15
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>  * <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1      10
#>  2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0      10
#>  3 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5     208
#>  4 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1     123
#>  5 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9    1066
#>  6 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7     954
#>  7 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0     115
#>  8 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0     254
#>  9 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4     748
#> 10 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1     160
#> # … with 90 more rows, and 4 more variables: BIR79 <dbl>, SID79 <dbl>,
#> #   NWBIR79 <dbl>, geometry <MULTIPOLYGON [m]>
```

You can also combine engines to, for example, force a custom pipeline to
be used for a given conversion:

``` r
proj_engine <- crs_engine_proj_cmd()
#> Running cct --version
#> Running projinfo -q -s 'EPSG:4326' -t 'OGC:CRS84' -o PROJ
engine <- crs_engine_fun_define(engine, "EPSG:3857", "OGC:CRS84", function(coords) {
  pipe <- paste(
    "+proj=pipeline",
    "+step +proj=unitconvert +xy_in=deg +xy_out=rad",
    "+step +proj=webmerc +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84"
  )
  xy_out <- crs_cct_proj_cmd(wk::as_xy(coords), pipe, engine = proj_engine)
  coords[c("x", "y")] <- as.data.frame(xy_out)[c("x", "y")]
  coords
})

crs_set_default_engine(engine)
crs_transform(nc, "EPSG:3857", "OGC:CRS84")
#> Running cct -d 16 '+proj=pipeline' '+step' '+proj=unitconvert' '+xy_in=deg' \
#>   '+xy_out=rad' '+step' '+proj=webmerc' '+lat_0=0' '+lon_0=0' '+x_0=0' \
#>   '+y_0=0' '+ellps=WGS84'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -9386880 ymin: 4012991 xmax: -8399788 ymax: 4382079
#> Projected CRS: WGS 84 / Pseudo-Mercator
#> # A tibble: 100 × 15
#>     AREA PERIMETER CNTY_ CNTY_ID NAME  FIPS  FIPSNO CRESS_ID BIR74 SID74 NWBIR74
#>  * <dbl>     <dbl> <dbl>   <dbl> <chr> <chr>  <dbl>    <int> <dbl> <dbl>   <dbl>
#>  1 0.114      1.44  1825    1825 Ashe  37009  37009        5  1091     1      10
#>  2 0.061      1.23  1827    1827 Alle… 37005  37005        3   487     0      10
#>  3 0.143      1.63  1828    1828 Surry 37171  37171       86  3188     5     208
#>  4 0.07       2.97  1831    1831 Curr… 37053  37053       27   508     1     123
#>  5 0.153      2.21  1832    1832 Nort… 37131  37131       66  1421     9    1066
#>  6 0.097      1.67  1833    1833 Hert… 37091  37091       46  1452     7     954
#>  7 0.062      1.55  1834    1834 Camd… 37029  37029       15   286     0     115
#>  8 0.091      1.28  1835    1835 Gates 37073  37073       37   420     0     254
#>  9 0.118      1.42  1836    1836 Warr… 37185  37185       93   968     4     748
#> 10 0.124      1.43  1837    1837 Stok… 37169  37169       85  1612     1     160
#> # … with 90 more rows, and 4 more variables: BIR79 <dbl>, SID79 <dbl>,
#> #   NWBIR79 <dbl>, geometry <MULTIPOLYGON [m]>
```
