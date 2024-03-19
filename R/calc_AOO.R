#' Create an Area of Occurrence grid
#'
#' @param pols A simple feature with Polygons (or Multipolygons).
#' @param buffsize Size of the buffer in meters.
#' @param cellsize Cell size in meter.
#' @param jitter Logical, should we add random displacement?
#' @param names_from Which column includes the ecosystem names? if missing, will use a generic name
#'
#' @return A simple feature of a grid over the input polygons.
#' @importFrom units set_units
#' @importFrom stats runif
#' @import sf
#' @import dplyr
#' @export
#'
create_AOO_grid <- function(pols, buffsize = 50000, cellsize = 10000, jitter = FALSE, names_from = NA) {

  names_from <- coalesce(names_from, "ecosystem_name")
  if (any(colnames(pols) %in% names_from)) {
    ecosystem_names <- pols %>% pull(!!names_from)
  } else {
    pols <- pols |> dplyr::mutate(ecosystem_name = "unnamed ecosystem type")
  }

  if (n_distinct(ecosystem_names)>1) {
    print("multiple ecosystem names given, do you want to combine all of them?")
  }

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
    sf::st_intersection(pols, grid)
  sf_column_name <- attr(data.intersect, "sf_column")
  data.intersect <-  data.intersect |>
    dplyr::mutate(area = sf::st_area(.data[[sf_column_name]])) |> # add in areas in m2
    dplyr::as_tibble() |>
    dplyr::group_by(.data[[!!names_from]],.data$layer) |> # for each cell, get area
    dplyr::summarise(area = sum(.data$area), .groups="drop")

  out.grid <-
    grid |>
    dplyr::inner_join(data.intersect, by = 'layer')
  sf_column_name <- attr(out.grid, "sf_column")
  out.grid <- out.grid |>
    dplyr::arrange(.data$area) |>
    dplyr::mutate(
      prop_area = (.data$area / sf::st_area(.data[[sf_column_name]])) |>
        units::set_units("%"),
      cumm_area = units::set_units(cumsum(.data$area)/sum(.data$area),'%'))
  class(out.grid) <- c("AOO_grid", class(pols))
  attr(out.grid, "ecosystem name column") <- names_from
  return(out.grid)
}
#' Print method for AOO grid
#'
#' @param x the AOO grid object
#' @param output_units set units for area output.
#' @param ... further arguments passed to or from other methods.
#' @import units
#' @import sf
#' @import dplyr
#' @import crayon
#' @export
#'
print.AOO_grid <- function(x, output_units = 'km2', ...) {
  names_from <- attr(x,"ecosystem name column")
  ecosystem_names <- unique(dplyr::pull(x, !!names_from))
  if (length(ecosystem_names)>1) {
    all_cells <- sprintf("%s for %s distinct ecosystems with a total of %s cells and total extent of:\n",
                         crayon::bold("AOO grid"),
                         length(ecosystem_names),
                         nrow(x))

  } else {
    all_cells <- sprintf("%s for %s with a total of %s cells and total extent of:\n",
                         crayon::bold("AOO grid"),
                         crayon::bold(ecosystem_names),
                         nrow(x))
  }
  total_area <- sum(units::set_units(x$area, output_units, mode = "standard"))
  rule_1p <- sprintf("There are %s cells with small occurrences (<1 %% of cell size)\n",
                     sum(x$prop_area < units::set_units(1, '%')))
  rule_1c <- sprintf("There are %s cells with marginal occurrences (<1 %% of total extent)\n",
                     sum(x$cumm_area < units::set_units(1, '%')))
  cat(all_cells)
  print(total_area)
  cat(crayon::magenta(rule_1p))
  cat(crayon::magenta(rule_1c))
  NextMethod("print", x)
  invisible(x)
}
#' Summarise Area of Occurrence metrics
#'
#' @param object The AOO grid created by function `create_AOO_grid`
#' @param output_units set units for area output.
#' @param conditions List with information
#' @param ... further arguments passed to or from other methods, currently ignored.
#'
#' @return A table with a summary of results.
#' @importFrom units set_units
#' @import sf
#' @import dplyr
#' @export
#'
#' @examples
#' require(sf)
#' glaciers_on_volcanos <- tropical_glaciers |>
#'   dplyr::filter(ecosystem_name %in% "Volcanos de Peru y Chile") |>
#'   sf::st_transform(crs = 32719)
#' AOO_grid <- create_AOO_grid(glaciers_on_volcanos)
#' summary(AOO_grid)
summary.AOO_grid <- function(object, output_units = 'km2', conditions = list(), ...) {
  names_from <- attr(object,"ecosystem name column")
    zero_units <- units::set_units(0, output_units, mode = "standard")
    ans <- sf::st_drop_geometry(object) |>
      dplyr::mutate(
        old_1p_rule = .data$prop_area >= units::set_units(1, '%') ,
        new_1p_rule = .data$cumm_area >= units::set_units(1, '%')
      ) |>
      dplyr::group_by(dplyr::pick(names_from)) |>
      dplyr::summarise(
        AOO = dplyr::n(),
        total_area = units::set_units(sum(.data$area), output_units, mode = "standard"),
      AOO_1p = sum(.data$old_1p_rule),
      area_1p = (sum(dplyr::if_else(.data$old_1p_rule,
                                    .data$area,
                                    zero_units)) / .data$total_area) |>
                                      units::set_units('%'),
      AOO_1c = sum(.data$new_1p_rule),
      area_1c = (sum(dplyr::if_else(.data$new_1p_rule,
                                    .data$area,
                                    zero_units)) / .data$total_area) |>
                                      units::set_units('%'),
      .groups = "drop"
    )
    class(ans) <- c("summary.AOO_grid", "tbl_df", "tbl", "data.frame")
    return(ans)
}

#' Threshold for IUCN RLE criterion B2
#'
#' @param x The AOO grid created by function `create_AOO_grid`
#' @param ecosystem_name Which ecosystem to use? (if multiple ecosystems are present in the grid)
#' @param rule Which rule to apply. One of `all` occurrences, exclude `marginal` occurrences or exclude `small` occurrences
#' @param conditions list of conditions considered when applying the criterion
#' @param ... further arguments passed to B_conditions() if conditions is not provided
#' @param useNT logical, should we apply rules for the Near Threatened category? TRUE by default, if FALSE the category Least Concern will be used, but a note will be added to the output. See details.
#'
#' @return B2 categories
#' @export
#'
thresholds.AOO_grid <- function(x, ecosystem_name = NA, rule = c("all", "marginal", "small"),
                                conditions = NULL, useNT = TRUE, ...) {
  names_from <- attr(x,"ecosystem name column")
  if (is.na(ecosystem_name)) {
    ans <- summary(x)
    if (nrow(ans)>1) {
      message("Multiple ecosystem types present in grid, but no name has been selected. Will apply the threshold to the first ecosystem type and ignore the rest")
      ans <- dplyr::slice(ans,1)
    } else {
      ecosystem_name <- pull(ans, !!names_from)
      message(sprintf("No ecosystem name has been selected. Will apply the threshold to %s.",
                      crayon::bgBlue(crayon::white(ecosystem_name))))
    }
  } else {
    ans <- summary(x) |> dplyr::filter(if_any(names_from), ~ . %in% ecosystem_name)
  }
  if (is.null(conditions)) {
    conditions <- B_conditions(...)
  }
  AOO_rule <-
    switch(first(rule),
         marginal = {"AOO_1c"},
         small = {"AOO_1p"},
         all = {"AOO"})
  note <-
    switch(first(rule),
           marginal = {"AOO excludes marginal occurrences (<1% of total extent)"},
           small = {"AOO excludes small occurrences (<1% of cell area)"},
           all = {"AOO includes all occurrences"})
  rationale <- c()

  AOO_val <- pull(ans, !!AOO_rule)
  thr_AOO <- c(-Inf, 2, 20, 50, Inf)
  thr_locations <- c(-Inf, 1, 5, 10, Inf)
  cats <- c("CR", "EN", "VU", "LC")
  condition_litterals <- c("a","b","c")
  declines <- with(conditions,c(spatial, environment, interactions))
  if (all(declines == FALSE)) {
    message("No observed or inferred continuing decline in any measure of spatial extent or environmental quality, or increase in disruption of biotic interactions")
    a <- FALSE
  } else {
    condition_litterals[1] <- paste("a",paste(c("i", "ii", "iii")[declines], collapse = "+"), sep = "")
    a <- TRUE
    if (declines[1]) {
      rationale <- c(rationale,"Observed or inferred continuing decline in measure of extent.")
      }
    if (declines[2]) {
      rationale <- c(rationale,"Observed or inferred continuing decline in measure of environmental quality.")
                     }
    if (declines[3]) {
      rationale <- c(rationale,"Observed or inferred continuing increase in measure of disruption of biotic interactions.")
      }
  }
  threats <- with(conditions,c(threats))
  if (!threats) {
    message("No observed threatening process")
    b <- FALSE
  } else {
    b <- TRUE
    rationale <- c(rationale,"Threatening processes.")
  }
  locations <- with(conditions, locations)

  AOO_category <- cut(AOO_val,breaks=thr_AOO, labels=cats, ordered_result = TRUE)
  if (is.na(locations)) {
    message("No threat defined locations are given")
    c <- FALSE
  } else {
    location_category <- cut(locations,breaks=thr_locations, labels=cats, ordered_result = TRUE)
      c <- (location_category <= AOO_category)
      rationale <- c(rationale,sprintf("Ecosystem type present at %s threat defined locations.", locations))
  }
  if (AOO_category < "LC") {
    if (any(c(a,b,c))) {
      condition_litterals <- paste(condition_litterals[c(a,b,c)], collapse = "; ")
      rationale <- c( sprintf("AOO metric below thresholds for the %s category, and following conditions met.",
                              AOO_category ),rationale)
    } else {
      if (useNT) {
        AOO_category <- "NT"
      } else {
        AOO_category <- "LC"
        note <- c(note, "Could be considered Near Threatened.")
      }
      rationale <- c(rationale, "AOO metric below thresholds for threatened categories, but no evidence of continuing declines, threatening processes or number of threat defined locations has been reported.")
      condition_litterals <- "(not met)"
    }
  } else {
    if (AOO_val < thr_AOO[4]*1.1) {
     if (any(c(a,b,c))) {
       if (useNT) {
         AOO_category = "NT"
         condition_litterals <- paste(condition_litterals[c(a,b,c)], collapse = "; ")
         rationale <- c(rationale, "AOO metric near to the thresholds for endangered categories.")
       } else {
         rationale <- c("AOO metric above the thresholds for threatened categories, but some conditions are met.", rationale)
         note <- c(note, "Could be considered Near Threatened.")
         condition_litterals <- NA
       }

     } else {
       condition_litterals <- NA
       rationale <- c("AOO metric above the thresholds for threatened categories, and no evidence of continuing declines, threatening processes or number of threat defined locations where given.", rationale)
     }

    } else {
      condition_litterals <- NULL
      rationale <- c("AOO metric well above the thresholds for threatened categories.")
    }
  }
  if (length(rationale) == 0) {
    rationale <- "Thresholds and conditions not met."
  }
  res <- tibble(
    ecosystem_name = ecosystem_name,
    metric = "AOO",
                value = AOO_val,
                criterion = "B2",
                category = AOO_category,
                conditions = condition_litterals,
                rationale = paste(rationale, collapse = " "),
                note = paste(note, collapse = " "))
  return(res)
}

