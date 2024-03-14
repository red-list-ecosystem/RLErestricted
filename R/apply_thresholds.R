
#' Apply IUCN Red List of Ecosystem thresholds
#'
#' @param x Object representing a spatial or functional metric
#' @param ... further arguments passed to or from other methods
#'
#' @return A category
#' @export
#'
thresholds <- function(x, ...) {
  UseMethod("thresholds")
}


#' Auxiliary for calculation of threshold for criterion B
#'
#' @param spatial observed or inferred decline
#' @param environment observed or inferred decline
#' @param interactions observed or inferred decline
#' @param threats threatening process
#' @param locations number of locations
#' @param ... additional parameters (ignored)
#'
#' @return A list with components named as the arguments.
#' @export
#'
B_conditions <- function(spatial = FALSE, environment = FALSE,
                         interactions = FALSE, threats = FALSE,
                         locations = NA, ...) {
  lst <- list(spatial = spatial,
              environment = environment,
              interactions = interactions,
              threats = threats,
              locations = locations)
  return(lst)
}
