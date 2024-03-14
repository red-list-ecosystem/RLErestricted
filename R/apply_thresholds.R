
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
