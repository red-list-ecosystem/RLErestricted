#' Create an Extent of Occurrence convex hull
#'
#' @param pols A simple feature with Polygons (or Multipolygons).
#' @param output_units Set units for area output.
#' @param names_from Which column includes the ecosystem names? if missing, will use a generic name
#'
#' @return A simple feature object with a convex hull and a regular grid over the input polygons.
#' @import sf
#' @import dplyr
#' @export
#'
create_EOO_chull <- function(pols, output_units = 'km2', names_from = NA) {
  names_from <- coalesce(names_from, "ecosystem_name")
  if (!any(colnames(pols) %in% names_from)) {
    pols <- pols |> dplyr::mutate(ecosystem_name = "unnamed ecosystem type")
  }
  out.pols <- pols |>
    group_by_at(names_from) |>
    summarise()
  out.pols$eco_area <- units::set_units(sf::st_area(out.pols), output_units, mode = "standard")
  out.chull <- sf::st_convex_hull(out.pols)
  out.chull$EOO <- units::set_units(sf::st_area(out.chull), output_units, mode = "standard")
  attr(out.chull, "ecosystem name column") <- names_from
  class(out.chull) <- c("EOO_convex_hull", class(pols))
  return(out.chull)
}

#' Print method for EOO convex hull
#'
#' @param x the EOO convex hull
#' @param output_units set units for area output.
#' @param ... further arguments passed to or from other methods.
#' @import cli
#' @import dplyr
#' @export
#'
print.EOO_convex_hull <- function(x, output_units = 'km2', ...) {
  names_from <- attr(x,"ecosystem name column")
  ecosystem_names <- unique(dplyr::pull(x, !!names_from))
  if (length(ecosystem_names) == 1) {
    cat(sprintf("The ecosystem %s has an extent of:\n",
                cli::col_cyan(ecosystem_names)
    ))
    print(units::set_units(x$eco_area, output_units, mode = "standard"))
    cat(sprintf("And a %s with an area of:\n",
                cli::col_magenta("EOO convex hull")
    ))
    print(units::set_units(x$EOO, output_units, mode = "standard"))
  } else {
    msg <- sprintf("This %s object has information for %s distinct ecosystem types.\nUse `base::subset`, `dplyr::filter` or `dplyr::slice` to get details for specific types.",
                   cli::style_bold("EOO convex hull"),
                   nrow(x))
    message(cli::col_cyan(msg))
  }
  cat("Details of spatial object below.\n")
  NextMethod("print", x)
}

#' Threshold for IUCN RLE criterion B1
#'
#' @param x The EOO convex hull created by function `create_EOO_chull`
#' @param ecosystem_name Which ecosystem to use? (if multiple ecosystems are present in the grid)
#' @param conditions list of conditions considered when applying the criterion
#' @param ... further arguments passed to B_conditions() if conditions is not provided
#' @param useNT logical, should we apply rules for the Near Threatened category? TRUE by default, if FALSE the category Least Concern will be used, but a note will be added to the output. See details.
#'
#' @return Category of risk of collapse for subcriterion B1
#' @import dplyr
#' @import cli
#' @export
#'
thresholds.EOO_convex_hull <- function(x, ecosystem_name = NA,
                                conditions = NULL, useNT = TRUE, ...) {
  names_from <- attr(x,"ecosystem name column")
  if (is.na(ecosystem_name)) {
    ans <- x
    if (nrow(ans)>1) {
      selected_type <- dplyr::slice(ans, 1) |> dplyr::pull(!!names_from)
      message(sprintf("Multiple ecosystem types present in the object, but no name has been selected. Will apply the threshold to the first ecosystem type (%s) and ignore the rest", selected_type))
      ans <- dplyr::slice(ans, 1)
    } else {
      selected_type <- pull(ans, !!names_from)
      cli::cli_alert_warning("No ecosystem name has been selected. Will apply the threshold to first type found.")
      message(cli::bg_blue(cli::col_white(ecosystem_name)))
    }
  } else {
    selected_type <- ecosystem_name
    ans <- dplyr::filter(x, .data[[names_from]] %in% selected_type)
    stopifnot(nrow(ans) == 1)
  }
  if (is.null(conditions)) {
    conditions <- B_conditions(...)
  }
  note <- c()
  rationale <- c()

  EOO_val <- pull(ans, .data[["EOO"]])
  thr_EOO <- units::set_units(c(-Inf, 2000, 20000, 50000, Inf), 'km2')
  thr_locations <- c(-Inf, 1, 5, 10, Inf)
  cats <- c("CR", "EN", "VU", "LC")
  condition_litterals <- c("a","b","c")
  declines <- with(conditions,c(spatial, environment, interactions))
  if (all(declines == FALSE)) {
    cli::cli_alert_danger("No observed or inferred continuing decline in any measure of spatial extent or environmental quality, or increase in disruption of biotic interactions")
    a <- FALSE
  } else {
    condition_litterals[1] <- paste("a",paste(c("i", "ii", "iii")[declines], collapse = "+"), sep = "")
    a <- TRUE
    if (declines[1]) {
      cli::cli_alert_success("Decline in measure of extent")
      rationale <- c(rationale,"Observed or inferred continuing decline in measure of extent.")
    }
    if (declines[2]) {
      cli::cli_alert_success("Decline in measure of environmental quality")
      rationale <- c(rationale,"Observed or inferred continuing decline in measure of environmental quality.")
    }
    if (declines[3]) {
      cli::cli_alert_success("Decline in measure of biotic disruption")
      rationale <- c(rationale,"Observed or inferred continuing increase in measure of disruption of biotic interactions.")
    }
  }
  threats <- with(conditions,c(threats))
  if (!threats) {
    cli::cli_alert_danger("No observed threatening process")
    b <- FALSE
  } else {
    cli::cli_alert_success("Observed threatening process")
    b <- TRUE
    rationale <- c(rationale,"Threatening processes.")
  }
  locations <- with(conditions, locations)

  EOO_category <- cut(EOO_val,breaks=thr_EOO, labels=cats, ordered_result = TRUE)
  if (is.na(locations)) {
    cli::cli_alert_danger("No threat defined locations are given")
    message("No threat defined locations are given")
    c <- FALSE
  } else {
    cli::cli_alert_success("Threat defined locations reported")
    location_category <- cut(locations,breaks=thr_locations, labels=cats, ordered_result = TRUE)
    c <- (location_category <= EOO_category)
    rationale <- c(rationale,sprintf("Ecosystem type present at %s threat defined locations.", locations))
  }
  if (EOO_category < "LC") {
    if (any(c(a,b,c))) {
      condition_litterals <- paste(condition_litterals[c(a,b,c)], collapse = "; ")
      rationale <- c( sprintf("EOO metric below thresholds for the %s category, and following conditions met.",
                              EOO_category ),rationale)
    } else {
      if (useNT) {
        EOO_category <- "NT"
      } else {
        EOO_category <- "LC"
        note <- c(note, "Could be considered Near Threatened.")
      }
      rationale <- c(rationale, "EOO metric below thresholds for threatened categories, but no evidence of continuing declines, threatening processes or number of threat defined locations has been reported.")
      condition_litterals <- "(not met)"
    }
  } else {
    if (EOO_val < thr_EOO[4]*1.1) {
      if (any(c(a,b,c))) {
        if (useNT) {
          EOO_category = "NT"
          condition_litterals <- paste(condition_litterals[c(a,b,c)], collapse = "; ")
          rationale <- c(rationale, "EOO metric near to the thresholds for endangered categories.")
        } else {
          rationale <- c("EOO metric above the thresholds for threatened categories, but some conditions are met.", rationale)
          note <- c(note, "Could be considered Near Threatened.")
          condition_litterals <- NA
        }

      } else {
        condition_litterals <- NA
        rationale <- c("EOO metric above the thresholds for threatened categories, and no evidence of continuing declines, threatening processes or number of threat defined locations where given.", rationale)
      }

    } else {
      condition_litterals <- NULL
      rationale <- c("EOO metric well above the thresholds for threatened categories.")
    }
  }
  if (length(rationale) == 0) {
    rationale <- "Thresholds and conditions not met."
  }
  res <- tibble(
    ecosystem_name = selected_type,
    metric = "EOO",
    value = EOO_val,
    criterion = "B2",
    category = EOO_category,
    conditions = condition_litterals,
    rationale = paste(rationale, collapse = " "),
    note = paste(note, collapse = " "))

  if (EOO_category %in% c("EN", "CR")) {
    rle_style <- cli::combine_ansi_styles(cli::style_bold, cli::col_white, cli::bg_red)
  } else if (EOO_category %in% c("VU")) {
    rle_style <- cli::combine_ansi_styles(cli::col_black, cli::bg_br_yellow)
  } else {
    rle_style <- cli::combine_ansi_styles(cli::col_grey, cli::bg_white)
  }
  message(
    rle_style(
      sprintf("Category according to criterion %s: %s.",
              cli::style_bold("B2"),
              cli::style_bold(EOO_category))))

  return(res)
}
