# Function for producing a bounding box of NEON tile coordinates.
#
#' Given one or more NEON UTM tile coordinates, this function produces
#' a continuous set of tiles that cover the bounding box of the input
#' coordinates, optionally expanded by a buffer (in kilometers).
#' - NEON tiles are assumed to be 1 km x 1 km, aligned on 1000 m increments.
#' - If 'kmbuffer = 0', the function returns all tiles needed to fill in
#'   any missing coordinates within the continuous bounding box of the input.
#' - If 'kmbuffer > 0', the bounding box is expanded outward by that many
#'   kilometers in all directions, and the complete set of tiles is returned.
#'
#' @param coords A data.frame with columns 'easting' and 'northing'
#' giving NEON tile coordinates in meters (UTM).
#' @param kmbuffer Integer. The number of kilometers to extend outward
#' from the min/max extent of the input coordinates.
#' @return A data.frame with all tile coordinates ('easting', 'northing')
#'covering the continuous extent plus buffer, ordered by northing and easting.
#' @examples
#' # Single tile with 1 km buffer (3x3 grid of tiles)
#' tiles <- data.frame(easting = 403000, northing = 3284000)
#' adjoin_NEON_tiles(tiles, kmbuffer = 1)
#'
#' # Multiple tiles with no buffer (fills in missing tiles)
#' tiles <- data.frame(easting = c(403000, 405000),
#'                     northing = c(3284000, 3284000))
#' adjoin_NEON_tiles(tiles, kmbuffer = 0)
#'  @export

adjoin_neon_tiles <- function(coords, kmbuffer = 1) {
  # coords: data.frame with columns "easting" and "northing"
  # kmbuffer: number of km outward to extend in all directions
  
  if(!all(c("easting", "northing") %in% names(coords))) {
    stop("coords must have columns 'easting' and 'northing'")
  }
  
  # Convert buffer to meters
  buf_m <- kmbuffer * 1000
  
  # Find bounding box with buffer
  east_min <- min(coords$easting) - buf_m
  east_max <- max(coords$easting) + buf_m
  north_min <- min(coords$northing) - buf_m
  north_max <- max(coords$northing) + buf_m
  
  # Generate full grid of tiles
  east_seq <- seq(east_min, east_max, by = 1000)
  north_seq <- seq(north_min, north_max, by = 1000)
  
  grid <- expand.grid(easting = east_seq,
                      northing = north_seq)
  
  # Return full set
  return(grid[order(grid$northing, grid$easting), ])
}