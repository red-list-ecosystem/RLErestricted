#' Create an Area of Occurrence grid
#'
#' @param pols A simple feature with Polygons (or Multipolygons).
#' @param buffsize Size of the buffer in meters.
#' @param cellsize Cell size in meter.
#' @param jitter Logical, should we add random displacement?
#'
#' @return A simple feature of a grid over the input polygons.
#' @importFrom units set_units
#' @importFrom stats runif
#' @import sf
#' @import dplyr
#' @export
#'
#' @examples
#' AOO_grid <- create_AOO_grid(glaciers_on_volcanos)
create_AOO_grid <- function(pols, buffsize = 50000, cellsize = 10000, jitter = FALSE) {
  bf.pols <- pols |> sf::st_buffer(buffsize) |> sf::st_union()
  raw.grid <- sf::st_make_grid(bf.pols, cellsize = cellsize)

  grid <- sf::st_sf(layer = 1:length(raw.grid),
                      geoms = raw.grid, stringsAsFactors = FALSE)
  if (jitter) {
    xdist <- stats::runif(1) * cellsize/2
    ydist <- stats::runif(1) * cellsize/2
    qry <- pols
    sf::st_geometry(qry) <- sf::st_geometry(pols) + c(xdist, ydist)
    sf::st_crs(qry) <- sf::st_crs(pols)
    pols <- qry
  }
  data.intersect <-
    sf::st_intersection(pols, grid) |>
    dplyr::mutate(area = sf::st_area(.data$geom)) |> # add in areas in m2
    dplyr::as_tibble() |>
    dplyr::group_by(.data$layer) |> # for each cell, get area
    dplyr::summarise(area = sum(.data$area), .groups="drop")

  out.grid <-
    grid |>
    dplyr::inner_join(data.intersect, by = 'layer') |>
    dplyr::arrange(.data$area) |>
    dplyr::mutate(prop_area = (.data$area/sf::st_area(.data$geoms)) |> units::set_units("%"),
           cumm_area = units::set_units(cumsum(.data$area)/sum(.data$area),'%'))

  return(out.grid)
}
