#' Create ecomap object from an sf object
#'
#' @param x `sf` object representing the distribution of one or more ecosystems
#' @param names_from Which column includes the ecosystem names? if missing, will use a generic name
#' @param area_from Which column includes the ecosystem extent (area)? if missing, will try to calculate it
#' @param output_units Set units for area output.
#'
#' @return ecosystem vector map
#' @import sf
#' @import dplyr
#' @import units
#' @export
#'
#' @examples
#' require(sf)
#' glaciers_on_volcanos <- tropical_glaciers |>
#'   dplyr::filter(ecosystem_name %in% "Volcanos de Peru y Chile") |>
#'   sf::st_transform(crs = 32719)
#' class(glaciers_on_volcanos)
#' glaciers_on_volcanos <- sf_as_ecomap(glaciers_on_volcanos)
#' class(glaciers_on_volcanos)
#' glaciers_on_volcanos
sf_as_ecomap <- function(x, names_from = NA, area_from = NA, output_units = 'km2') {
  names_from <- coalesce(names_from, "ecosystem_name")
  if (any(colnames(x) %in% names_from)) {
    ecotype_names <- dplyr::pull(x, !!names_from) |> unique()
    attr(x, "number of ecosystem types") <- length(ecotype_names)
    attr(x, "ecosystem name column") <- names_from
    if (length(ecotype_names) == 1) {
      attr(x, "econame") <- ecotype_names
    }
  } else {
    x <- x |> dplyr::mutate(ecosystem_name = "unnamed ecosystem type")
    attr(x, "number of ecosystem types") <- 1
    attr(x, "ecosystem name column") <- "ecosystem_name"
  }
  area_from <- coalesce(area_from, "ecosystem_extent")
  if (any(colnames(x) %in% area_from)) {
    attr(x, "has area") <- TRUE
    attr(x, "ecosystem area column") <- area_from
  } else if (any(st_geometry_type(x) %in% c("POINT", "LINESTRING"))) {
    message("POINT and/or LINESTRING geometries detected. And no column with area estimates provided.")
    attr(x, "has area") <- FALSE
  } else {
    x <- x |> dplyr::mutate(ecosystem_extent =
                              units::set_units(sf::st_area(x),
                                               output_units,
                                               mode = "standard"))
    attr(x, "has area") <- TRUE
    attr(x, "ecosystem area column") <- "ecosystem_extent"
    agr_attributes <- st_agr(x)
    agr_attributes["ecosystem_extent"] <- "aggregate"
    st_agr(x) <- agr_attributes
  }
  class(x) <- c("ecomap", class(x))
  return(x)
}
#' Print method for ecomap object
#'
#' @param x the ecomap object
#' @param ... further arguments passed to or from other methods.
#' @import units
#' @import sf
#' @import dplyr
#' @import crayon
#' @export
#'
print.ecomap <- function(x, ...) {
  number_ecosystems <- attr(x, "number of ecosystem types")
  if (number_ecosystems>1) {
    msg1 <- sprintf("%s object with %s distinct ecosystems.\n",
                      crayon::bold("Ecosystem map (ecomap)"),
                      crayon::bold(length(number_ecosystems)))

  } else {
    msg1 <- sprintf("%s for %s\n",
                         crayon::bold("Ecosystem map (ecomap)"),
                         ifelse (is.null(attr(x, "econame")),
                                 "an unnamed ecosystem type",
                                 crayon::bgCyan(attr(x, "econame"))))
  }
  cat(msg1)

  if (attr(x, "has area")) {
    area_from <- attr(x, "ecosystem area column")
    msg2 <- sprintf("%s features representing a total ecosystem extent of: ", nrow(x))
    total_area <- dplyr::pull(x, !!area_from) |> sum()
    cat(crayon::cyan(msg2))
    print(total_area)
  } else {
    msg2 <- sprintf("%s features with no information on ecosystem extent.", nrow(x))
    cat(crayon::cyan(msg2))
  }
  NextMethod("print", x)
  invisible(x)
}
