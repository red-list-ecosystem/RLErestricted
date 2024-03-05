
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RLErestricted

<!-- badges: start -->
<!-- badges: end -->

`RLErestricted` is an R package that contains a set of tools suitable
for calculating the metrics required for making assessments of
ecosystems against the **IUCN Red List of Ecosystems** categories and
criteria.

## Overview

The `RLErestricted` package was developed to assist users conduct
assessments for the IUCN Red List of Ecosystems in `R`. Assessments of
ecosystems under the IUCN Red List of Ecosystems criteria require
calculation of standardised metrics that were developed to objectively
assess risk to ecosystem ([Keith et
al. 2013](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0062111)).

This package was designed to assist in the calculation of two standard
measures of the size of an ecosystems’ geographic distribution specified
in the IUCN Red List of Ecosystems guidelines ([Bland et
al. 2017](https://doi.org/10.2305/IUCN.CH.2016.RLE.3.en)). These are the
Extent of Occurrence (EOO) and Area of Occupancy (AOO).

In conducting an assessment with this package, we assume that you are
familiar with IUCN red listing protocols. In particular, you should
consult the IUCN guidelines and follow the recommended steps to ensure
consistent application of IUCN criteria ([Bland et
al. 2017](https://doi.org/10.2305/IUCN.CH.2016.RLE.3.en)).

We also assume that you are reasonably familiar with the `R` programming
language, and have some experience in conducting analyses of vector data
within the `R` environment using the package `sf` (simple features).

This is a work in progress and we aim to continually add new functions
to newer versions of package. Suggestions are welcomed, as are offers
for collaborative development.

## Installation

You can install the development version of RLErestricted from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("red-list-ecosystem/RLErestricted")
```

## Example

The goal of `RLErestricted` is to assist users calculate one spatial
metric (area of occupancy or AOO) to a group of polygons describing the
distribution of an ecosystem. This information can be used to apply one
of the criteria of the IUCN Red List of Ecosystems.

The first step is to create a AOO grid over the extent of the ecosystem:

``` r
library(RLErestricted)

AOO_grid <- create_AOO_grid(glaciers_on_volcanos)
#> Warning: attribute variables are assumed to be spatially constant throughout
#> all geometries
```

We can see the results is a grid of cells with information about the
area of the ecosystem:

``` r
AOO_grid
#> Simple feature collection with 69 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 619907 ymin: 7941362 xmax: 1149907 ymax: 8431362
#> Projected CRS: WGS 84 / UTM zone 18S
#> First 10 features:
#>    layer              area                          geoms        prop_area
#> 1    303    237.2767 [m^2] POLYGON ((1119907 7941362, ... 0.0002372767 [%]
#> 2   3167    816.8343 [m^2] POLYGON ((619907 8411362, 6... 0.0008168343 [%]
#> 3   3243   1018.7518 [m^2] POLYGON ((759907 8421362, 7... 0.0010187518 [%]
#> 4   2517  18084.8573 [m^2] POLYGON ((939907 8301362, 9... 0.0180848573 [%]
#> 5   2810  48106.1281 [m^2] POLYGON ((769907 8351362, 7... 0.0481061281 [%]
#> 6   2750  55725.2338 [m^2] POLYGON ((789907 8341362, 7... 0.0557252338 [%]
#> 7   2458  70935.6707 [m^2] POLYGON ((969907 8291362, 9... 0.0709356707 [%]
#> 8   2859  97390.8989 [m^2] POLYGON ((639907 8361362, 6... 0.0973908989 [%]
#> 9   2872 102244.0385 [m^2] POLYGON ((769907 8361362, 7... 0.1022440385 [%]
#> 10  1791 108072.6978 [m^2] POLYGON ((1119907 8181362, ... 0.1080726978 [%]
#>           cumm_area
#> 1  0.0001442651 [%]
#> 2  0.0006409034 [%]
#> 3  0.0012603082 [%]
#> 4  0.0122559678 [%]
#> 5  0.0415046704 [%]
#> 6  0.0753858171 [%]
#> 7  0.1185149654 [%]
#> 8  0.1777289896 [%]
#> 9  0.2398937404 [%]
#> 10 0.3056023375 [%]
```

And we can plot this grid:

<img src="man/figures/README-plot-1.png" width="100%" />
