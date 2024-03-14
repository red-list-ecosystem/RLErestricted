#' Tropical glaciers of the world
#'
#' Outlines of tropical glacier complexes that are located in tropical regions
#' of the world. The original outlines of the glacier complexes come from the
#' Randolph Glacier Inventory (version 7.0), but have been simplified to reduce
#' file size while preserving their topology. The subset of glacier complexes
#' has been selected based on glacier groups suggested by Sagredo and Lowell
#' (2012) and adopted by Ferrer-Paris (2004).
#'
#' @format ## `tropical_glaciers`
#' A simple feature collection with 1742 feature, 2 fields and a geometry:
#' \describe{
#'   \item{rgi_id}{Original ID in the RGI 7.0 product}
#'   \item{ecosystem_name}{Name of the ecosystem}
#'   \item{utm_zone}{UTM zone suggested for projection}
#'   \item{geom}{Multipoligon geometry projected to CRS: WGS 84 / UTM zone 18S}
#' }
#' @source Ferrer-Paris, J.R. (2024). Data for the global RLE assessment of Tropical Glacier Ecosystems. Retrieved from osf.io/432sb
#' @source RGI Consortium, . (2023). Randolph Glacier Inventory - A Dataset of Global Glacier Outlines, Version 7. Glacier complex product, Region 16 Low Latitudes. Boulder, Colorado USA. National Snow and Ice Data Center. https://doi.org/10.5067/F6JMOVY5NAVZ. Date Accessed 03-14-2024.
#' @source Sagredo, E.A. & Lowell, T.V. (2012) Climatology of andean glaciers: A framework to understand glacier response to climate change. Global and Planetary Change, 86-87, 101–109.
"tropical_glaciers"
