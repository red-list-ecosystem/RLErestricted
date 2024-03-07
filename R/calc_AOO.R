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
#' print(AOO_grid)
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
  class(out.grid) <- c("AOO_grid", class(pols))
  return(out.grid)
}
#' Print method for AOO grid
#'
#' @param x the AOO grid object
#' @param ... further arguments passed to or from other methods.
#' @importFrom units set_units
#' @import sf
#' @import dplyr
#' @import crayon
#' @export
#'
print.AOO_grid <- function(x, ...) {
  all_cells <- sprintf("%s with a total of %s cells and total extent of:\n",
                       crayon::bold("AOO grid"),
                       nrow(x))
  rule_1p <- sprintf("There are %s cells with small occurrences (<1 %% of cell size)\n",
                     sum(x$prop_area < units::set_units(1, '%')))
  rule_1c <- sprintf("There are %s cells with marginal occurrences (<1 %% of total extent)\n",
                     sum(x$cumm_area < units::set_units(1, '%')))
  cat(all_cells)
  print(sum(x$area))
  cat(crayon::magenta(rule_1p))
  cat(crayon::magenta(rule_1c))
  NextMethod("print", x)

}
#' Summarise Area of Occurrence metrics
#'
#' @param AOO_grid The AOO grid created by function `create_AOO_grid`
#'
#' @return A table with a summary of results.
#' @importFrom units set_units
#' @import sf
#' @import dplyr
#' @export
#'
#' @examples
#' AOO_grid <- create_AOO_grid(glaciers_on_volcanos)
#' AOO_summary(AOO_grid)
AOO_summary <- function(AOO_grid) {
    sf::st_drop_geometry(AOO_grid) |>
    dplyr::mutate(
      old_1p_rule = .data$prop_area >= units::set_units(1, '%') ,
      new_1p_rule = .data$cumm_area >= units::set_units(1, '%')
    ) |>
    dplyr::summarise(
      AOO = dplyr::n(),
      AOO_1p = sum(.data$old_1p_rule),
      area_1p = (sum(dplyr::if_else(.data$old_1p_rule,
                                    .data$area,
                                    units::set_units(0, 'm2'))) / sum(.data$area)) %>% units::set_units('%'),
      AOO_1c = sum(.data$new_1p_rule),
      area_1c = (sum(dplyr::if_else(.data$new_1p_rule,
                                    .data$area,
                                    units::set_units(0, 'm2'))) / sum(.data$area)) %>% units::set_units('%')
    )
}

