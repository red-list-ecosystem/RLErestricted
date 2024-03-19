#' Create an Extent of Occurrence convex hull
#'
#' @param pols A simple feature with Polygons (or Multipolygons).
#'
#' @return A simple feature of a convex hull over the input polygons.
#' @import sf
#' @export
#'
create_EOO_chull <- function(pols) {
  out.chull <- st_convex_hull(pols)
  class(out.chull) <- c("EOO_convex_hull", class(pols))
  return(out.chull)
}

#' Print method for EOO convex hull
#'
#' @param x the EOO convex hull
#' @param output_units set units for area output.
#' @param ... further arguments passed to or from other methods.
#' @import crayon
#' @export
#'
print.EOO_convex_hull <- function(x, output_units = 'km2', ...) {
  cat(sprintf("%s with an extent of:\n",
              crayon::bold("EOO convex hull")))
  print(units::set_units(sf::st_area(x), output_units, mode = "standard"))
  NextMethod("print", x)
}
