# Function for creating a data frame of NEON AOP UTM coordinates.
#
# Takes a matrix or data frame consisting of three columns (X, Y, and ID)
# and returns a new data frame of coordinates in UTM, the ESPG code, 
# and the UTMs associated with the NEON tile file structure.
#
#' @param coords Matrix or data frame with X and Y columns. Optionally, an ID column.
#' @param input_crs EPSG code or proj4 string for input coordinates. Defaults to EPSG:4326 (WGS84).
#' @return Data frame with UTM easting, northing, tile references, and EPSG codes.
#' @export

list_AOP_Tiles <- function(coords, input_crs = 4326) {
  # coords: matrix or data.frame with two columns (X, Y), or optionally three 
  # with an ID column
  # input_crs: EPSG code (numeric or character) or proj4 string
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' required but not installed.")
  }
  
  if (!is.matrix(coords) && !is.data.frame(coords)) {
    stop("coordinates must be a matrix or data.frame with two columns (X, Y) or
         optionally three with an ID")
  }
  if (ncol(coords) < 2) {
    stop("coordinates must have at least two columns")
  }
  
  # Check for ID column
  has_id <- is.data.frame(coords) && "ID" %in% colnames(coords)
  ids <- if (has_id) coords$ID else seq_len(nrow(coords))
  
  # Warn if default input_crs was assumed
  if (identical(input_crs, 4326) && !("input_crs" %in% names(match.call()))) {
    warning(
      "input_crs not specified; assuming EPSG:4326 (WGS84 lon/lat).\n",
      "If your coordinates are in a different CRS, please specify input_crs 
      explicitly.\n",
      "You can obtain the CRS of your spatial data in R using 
      sf::st_crs(your_data)."
    )
  }
  
  # Create sf object from X and Y only
  pts_sf <- sf::st_as_sf(
    data.frame(x = coords[,1], y = coords[,2]),
    coords = c("x", "y"),
    crs = input_crs
  )
  
  pts_wgs84 <- sf::st_transform(pts_sf, crs = 4326)
  lonlat <- sf::st_coordinates(pts_wgs84)
  
  results <- vector("list", nrow(coords))
  
  for (i in seq_len(nrow(coords))) {
    lon <- lonlat[i, 1]
    lat <- lonlat[i, 2]
    
    utm_zone <- floor((lon + 180) / 6) + 1
    crs_utm <- if (lat >= 0) 32600 + utm_zone else 32700 + utm_zone
    
    pt_utm <- sf::st_transform(pts_sf[i, ], crs = crs_utm)
    coords_utm <- sf::st_coordinates(pt_utm)
    
    easting  <- floor(coords_utm[1] / 1000) * 1000
    northing <- floor(coords_utm[2] / 1000) * 1000
    
    results[[i]] <- data.frame(
      ID = ids[i],
      easting = coords_utm[1],
      northing = coords_utm[2],
      tile_easting = easting,
      tile_northing = northing,
      EPSG = crs_utm
    )
  }
  
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL
  return(result_df)
}
