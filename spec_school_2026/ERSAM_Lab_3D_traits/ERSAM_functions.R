###############################################################################
# Title: ERSAM_functions
# Date: 06/25/2025
# Authors: ERSAM Lab Crew 
#
# This script contains helper functions for:
# 1. Converting XY coordinates into NEON AOP tile grids (list_aop_Tiles)
# 2. Downloading and organizing NEON remote sensing & observational data (neon_download)
# 3. Bulk downloading hemispherical canopy images (bulk_photo_download)
# 4. Interactive JPG photo processing with hemispheR (process_photos_JPG)
# 5. Selecting adjoining tiles from input NEON tile coordinates (adjoin_neon_tiles)
#
# Required Packages: sf, neonUtilities, fs, purrr, stringr, glue, hemispheR, terra, progress
###############################################################################

###############################################################################
#
# Function 1: list_aop_tiles (needs better name)
#
# Takes a matrix or data frame consisting of three columns (X, Y, and ID)
# and returns a new data frame of coordinates in UTM, the ESPG code, 
# and the UTMs associated with the NEON tile file structure
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

###############################################################################
#
# Function 2: neon_download (also might need a better name)
#
# download NEON data and put into a temporary directory, 
# then reroute files into target directory with specified path.
# 
#' @param dpID NEON Data Product ID.
#' @param site NEON site code.
#' @param startdate Start date (YYYY-MM-DD) for non-AOP data.
#' @param enddate End date (YYYY-MM-DD) for non-AOP data.
#' @param year Year (YYYY) for AOP data.
#' @param easting, northing Optional tile-based coordinates for AOP downloads.
#' @param file_pattern Regex pattern to filter downloaded files.
#' @param include.provisional Include provisional data.
#' @param buffer Tile buffer (meters).
#' @param check.size Check file size before download.
#' @param token NEON API token.
#' @param release Data release version.
#' @param out_path Template for target directory.
#' @param write_log Save file move log CSV.
#' @param log_name Log file name.
#' @return None. Files are saved to disk.
#' @export

neon_download <- function(dpID,
                          site,
                          startdate = NULL,
                          enddate = NULL,
                          year = NULL,
                          easting = NULL,
                          northing = NULL,
                          level = NULL,
                          file_pattern = NULL,
                          include.provisional = FALSE,
                          buffer = 0,
                          check.size = FALSE,
                          token = NA,
                          release = "current",
                          out_path,
                          write_log = TRUE,
                          log_name = "file_move_log.csv") {
  
# Load required packages
  for (pkg in c("neonUtilities", "fs", "purrr", "stringr", "glue")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop(paste0("Package '", 
                                                        pkg, "' is required."))
  }
  
# Setup download directory
  download_dir <- fs::path(tempdir(), "neon_downloads")
  fs::dir_create(download_dir)
  
# Detect if AOP (remote sensing) data
  is_aop <- stringr::str_detect(dpID, "^DP1\\.3|^DP2|^DP3")
  
# Download data
  if (is_aop) {
    if (is.null(year)) stop("AOP data requires 'year'.")
    if (!is.null(easting) && !is.null(northing)) {
      neonUtilities::byTileAOP(dpID = dpID, site = site, year = year,
                               easting = easting, northing = northing, 
                               include.provisional = include.provisional,
                               buffer = buffer, token = token, 
                               check.size = check.size, savepath = download_dir)
    } else {
      neonUtilities::byFileAOP(dpID = dpID, site = site, year = year, 
                               include.provisional = include.provisional,
                               token = token, check.size = check.size,
                               savepath = download_dir)
    }
    zip_files <- fs::dir_ls(download_dir, regexp = "\\.zip$", recurse = TRUE)
    purrr::walk(zip_files, ~ utils::unzip(.x, exdir = download_dir))
  } else {
    if (is.null(startdate) || is.null(enddate)) {
      stop("Non-AOP data requires 'startdate' and 'enddate'.")
    }
    neonUtilities::zipsByProduct(dpID = dpID, site = site,
                                 startdate = startdate, enddate = enddate,
                                 package = "basic", release = release, 
                                 include.provisional = include.provisional, 
                                 token = token, check.size = check.size, 
                                 savepath = download_dir)
    zip_files <- fs::dir_ls(download_dir, regexp = "\\.zip$", recurse = TRUE)
    purrr::walk(zip_files, ~ utils::unzip(.x, exdir = download_dir))
  }
  
# Find relevant files
  files_to_move <- if (is.null(file_pattern)) {
    fs::dir_ls(download_dir, recurse = TRUE, type = "file")
  } else {
    fs::dir_ls(download_dir, recurse = TRUE, regexp = file_pattern, 
               type = "file")
  }
  
  files_to_move <- files_to_move[fs::file_exists(files_to_move)]
  if (length(files_to_move) == 0) stop("No files matched the pattern: ", 
                                       file_pattern)
  
# Create target directory
  target_dir <- fs::path(out_path)
  fs::dir_create(target_dir, recurse = TRUE)
  
# Prepare new file paths
  file_names <- fs::path_file(files_to_move)
  new_paths <- fs::path(target_dir, file_names)
  
  if (length(files_to_move) != length(new_paths)) {
    stop("Mismatch between number of source files and target file paths.")
  }
  
# Initialize log dataframe
  log_df <- data.frame(
    original_path = character(),
    new_path = character(),
    moved_at = character(),
    stringsAsFactors = FALSE
  )
  
# Move files and optionally log
  purrr::walk2(files_to_move, new_paths, function(from, to) {
    fs::file_move(from, to)
    if (write_log) {
      log_df <<- rbind(log_df, data.frame(
        original_path = as.character(from),
        new_path = as.character(to),
        moved_at = as.character(Sys.time()),
        stringsAsFactors = FALSE
      ))
    }
  })
  
# Write log CSV if enabled
  if (write_log && nrow(log_df) > 0) {
    log_path <- fs::path(target_dir, log_name)
    utils::write.csv(log_df, file = log_path, row.names = FALSE)
    message("Log saved to: ", log_path)
  }
  
  message("Download and organization complete! Files saved to: ", target_dir)
}

###############################################################################
#
# Function 3: bulk_photo_download
#
# download multiple hemispherical photos from the NEON repo with 
# the option of specifying a more specific (yyyy-mm-dd) date range 
#
#' @param images A data frame containing image metadata with columns 'startDate', 'imageFileUrl', and 'imageFileName'.
#' @param out_dir Directory path to save downloaded photos. Created if it does not exist.
#' @param date_range Optional vector of two Dates (start and end) to filter images by date.
#' 
#' @return NULL; downloads photos to the specified directory.
#' @export


bulk_photo_download <- function(images, out_dir, date_range = NULL) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  if (!is.null(date_range)) {
    images$startDate <- as.Date(images$startDate)
    images <- images %>%
      filter(startDate >= date_range[1] & startDate <= date_range[2])
  }
  
  for (i in 1:nrow(images)) {
    file_url <- images$imageFileUrl[i]
    file_name <- images$imageFileName[i]
    dest <- file.path(out_dir, file_name)
    
    if (!file.exists(dest)) {
      download.file(url = file_url, destfile = dest, mode = "wb")
      message(glue("Downloaded {file_name}"))
    } else {
      message(glue("Skipped {file_name} (already exists)"))
    }
  }
}

###############################################################################
#
# Function 4: process_photos_jpg
#
#' Processes a list of hemispherical photo files interactively using the hemispheR package.
#' Allows user to continue previous sessions or start new, modify processing parameters,
#' analyze gap fractions and canopy metrics, and save progress automatically.
#'
#' @param photo.files.list Character vector of paths to hemispherical photo files.
#' @param out_path Output directory path where processed data CSV will be saved.
#' 
#' @return A data frame containing canopy metrics for processed photos.
#' @export

process_photos_jpg <- function(photo.files.list, channel = 3, method = "Otsu", 
                               zonal = FALSE, stretch = FALSE, gamma = 1.0,
                               yes_display = FALSE, lens = "equidistant", out_path) {
  
  # -------------------- Package Dependencies ----------------------------
  
  # Load required packages
  for (pkg in c("progress", "terra", "hemispheR")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop(paste0("Package '", 
                                                            pkg, "' is required."))
  }
  
  library(progress)
  library(terra)
  library(hemispheR)
  
  # Initialize empty out.data
  out.data <- data.frame()
  
  # Initialize progress bar
  pb <- progress_bar$new(
    total = length(photo.files.list),
    format = "  Processing [:bar] :percent  ",
    clear = TRUE,
    width = 60
  )
  # cancel instructions for headless operation
  if (!yes_display) {
    cat("Running in headless mode (display = FALSE).\n")
    cat("Press ESC at any time to cancel and save progress.\n")
  }
  
  # -------------------- Activation Prompt ----------------------------
  
  # Ask user if continuing processing or starting new
  session_type <- readline(prompt = "Do you want to continue a processing session or start new? (continue/new/cancel): ")
  
  if (tolower(session_type) == "cancel") {
    stop("Processing cancelled by user before start.") 
  }
  
  if (tolower(session_type) == "continue") {
    file_path <- readline(prompt = "Enter path to existing processed CSV file: ")
    
    if (file.exists(file_path)) {
      out.data <- read.csv(file_path) # read in specified csv 
      
      last_image <- tail(out.data$ImageName, n = 1) # define last image processed
      last_index <- which(basename(photo.files.list) == last_image) # index list for last processed image
      
      if (length(last_index) == 0) {
        cat("Last processed image not found in photo list. Starting from beginning.\n")
        remaining_photos <- photo.files.list
      } else {
        remaining_photos <- photo.files.list[(last_index + 1):length(photo.files.list)]
      }
      
    } else {
      cat("File not found. Starting new session.\n")
      remaining_photos <- photo.files.list
    }
    
  } else if (tolower(session_type) == "new") {
    cat("Starting new session.\n")
    remaining_photos <- photo.files.list
  } else {
    stop("Invalid input. Exiting process.")
  }
  
  if (length(remaining_photos) == 0) {
    cat("No images left to process.\n")
    return(NULL)
  }
  
  # ----------------- Main Processing Loop ----------------------
  
  tryCatch({
    
    i <- 1
    while (i <= length(remaining_photos)) {
      lab.img <- remaining_photos[i]
      
      # Update progress bar at start of each iteration
      pb$tick()
      
      # Define variables the user can change during processing
      satisfied <- FALSE # Default to start loop
      stretch_val <- stretch 
      method_val <- method 
      zonal_val <- zonal 
      gamma_val <- gamma
      channel_val <- channel
      cam_val <- lens
      
      # create list of available threshold methods
      methods_list <- c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li",
                        "MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu",
                        "Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen")
      
      while (!satisfied) {
        
        cat("Processed images:", nrow(out.data), "| Remaining:", 
            length(remaining_photos) - i + 1, "\n")
        
        # Import image and binarize
        img2 <- import_fisheye(filename = lab.img,
                               channel = channel_val,
                               circular = FALSE,
                               gamma = gamma_val,
                               stretch = stretch_val,
                               display = yes_display)
        
        img.bw <- binarize_fisheye(img2, display = yes_display, 
                                   method = method_val, zonal = zonal_val)
        
        # User Prompt for satisfaction with binarized image
        if (yes_display == TRUE) {
          response <- readline(prompt = paste0("Satisfied with image ", 
                                               basename(lab.img), 
                                               "? (yes/no/skip/cancel): "))
          
          if (tolower(response) == "yes") {
            gap.frac <- gapfrac_fisheye(img.bw,
                                        startVZA = 0,
                                        endVZA = 17,
                                        nrings = 7,
                                        nseg = 8,
                                        lens = cam_val,
                                        display = TRUE) # calculate gap fraction
            
            canopy.data <- canopy_fisheye(gap.frac) # canopy metric calculations 
            canopy.data$ImageName <- basename(lab.img) # add image name to ID column
            
            out.data <- rbind(out.data, canopy.data) # append to output data frame 
            satisfied <- TRUE # exit loop for this image 
            
            # Ask user if they want to apply linear stretch
          } else if (tolower(response) == "no") {
            
            # Ask if they want to adjust gamma first
            adjust_gamma <- readline(prompt = paste0("Would you like to adjust 
                                                     the gamma? (yes/no or Enter to keep current): "))
            if (tolower(adjust_gamma) == "yes") {
              new_gamma <- readline(prompt = paste0("Enter new gamma value 
                                                    (numeric, current = ", gamma_val, "): "))
              gamma_num <- suppressWarnings(as.numeric(new_gamma))
              if (!is.na(gamma_num) && gamma_num > 0) {
                gamma_val <- gamma_num
                cat(paste0("Gamma adjusted to ", gamma_val, ". Reprocessing image...\n"))
              } else {
                cat("Invalid gamma value entered. Keeping current gamma.\n")
              }
            }
            
            # Ask user if they want to apply linear stretch
            stretch_input <- readline(prompt = "Toggle stretch? (TRUE/FALSE or 
                                  Enter to keep current): ")
            if (toupper(stretch_input) %in% c("TRUE", "FALSE")) 
              stretch_val <- as.logical(stretch_input)
            
            # Ask user if they want to change threshold method
            cat("\nAvailable Threshold Methods:\n")
            cat(paste(methods_list, collapse = ", "), "\n")
            cat(paste("Current Method:", method_val, "\n"))
            method_input <- readline(prompt = paste0("Enter new thresholding method 
                                 (or press enter to keep current):"))
            if (method_input != "" && method_input %in% methods_list) {
              method_val <- method_input
            } else if (method_input != "") {
              cat("Invalid method entered. Keeping current method.\n")
            } 
            
            # Ask user if they want to apply zonal threshold 
            zonal_input <- readline(prompt = paste0("Toggle zonal thresholding? 
                                (TRUE/FALSE or Enter to keep current): "))
            if (toupper(zonal_input) %in% c("TRUE", "FALSE")) 
              zonal_val <- as.logical(zonal_input)
            
          } else if (tolower(response) == "skip") {
            satisfied <- TRUE  # Skip this image
            
          } else if (tolower(response) == "cancel") {
            # Save data and abort
            save_file <- paste0(tools::file_path_sans_ext(out_path),"/", 
                                "processed_hemiphotos",format(Sys.time(), "%Y%m%d"),
                                ".csv")
            write.csv(out.data, save_file, row.names = FALSE)
            cat(paste0("\nProcess aborted. Data saved to ", save_file, "\n"))
            return(out.data)  # Exit function and return data frame
          } else {
            cat("Invalid input. Please type 'yes', 'no', 'skip', or 'cancel'.\n")
          }
        } else {
          gap.frac <- gapfrac_fisheye(img.bw,
                                      startVZA = 0,
                                      endVZA = 17,
                                      nrings = 7,
                                      nseg = 8,
                                      display = yes_display) # calculate gap fraction
          
          canopy.data <- canopy_fisheye(gap.frac) # canopy metric calculations 
          canopy.data$ImageName <- basename(lab.img) # add image name to ID column
          
          out.data <- rbind(out.data, canopy.data) # append to output data frame 
          satisfied <- TRUE # exit loop for this image 
          
        }
        i <- i + 1  # Increment after inner loop finishes
      }}
    
  }, 
  # Save the files if interrupted 
  interrupt = function(e) {
    cat("\n Processing interrupted by user. Saving progress...\n")
    save_file <- paste0(tools::file_path_sans_ext(out_path), "/", 
                        "processed_hemiphotos_", format(Sys.time(), "%Y%m%d"), ".csv")
    write.csv(out.data, save_file, row.names = FALSE)
    cat(paste0("Progress saved to ", save_file, "\n"))
    return(out.data)
  })
  
  # After all images processed, auto-save and complete
  save_file <- paste0(tools::file_path_sans_ext(out_path),"/", 
                      "processed_hemiphotos_",format(Sys.time(), "%Y%m%d"),".csv")
  write.csv(out.data, save_file, row.names = FALSE)
  cat(paste0("\nProcessing complete. Data saved to ", save_file, "\n"))
  
  return(out.data)
}


###############################################################################
#
# Function 5: adjoin_neon_tiles
#
#' Given one or more NEON UTM tile coordinates, this function produces
#' a continuous set of tiles that cover the bounding box of the input
#' coordinates, optionally expanded by a buffer (in kilometers).
#'
#' - NEON tiles are assumed to be 1 km x 1 km, aligned on 1000 m increments.
#' - If `kmbuffer = 0`, the function returns all tiles needed to fill in
#'   any missing coordinates within the continuous bounding box of the input.
#' - If `kmbuffer > 0`, the bounding box is expanded outward by that many
#'   kilometers in all directions, and the complete set of tiles is returned.
#'
#' @param coords A data.frame with columns `easting` and `northing`
#'   giving NEON tile coordinates in meters (UTM).
#' @param kmbuffer Integer. The number of kilometers to extend outward
#'   from the min/max extent of the input coordinates.
#'
#' @return A data.frame with all tile coordinates (`easting`, `northing`)
#'   covering the continuous extent plus buffer, ordered by northing and easting.
#'
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


















