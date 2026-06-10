# Function for downloading NEON data to a specified directory. 
#
# Wrapper for the neonUtilities package functions byFileAOP, byTileAOP, and 
# ZipsByProduct; downloads NEON data and puts into a temporary directory following
# NEONS file structure (required by the package), then reroute files into 
# target directory with user specified path.
# 
#' @param dpID NEON Data Product ID.
#' @param site NEON site code.
#' @param startdate Start date (YYYY-MM-DD) for non-AOP data.
#' @param enddate End date (YYYY-MM-DD) for non-AOP data.
#' @param year Year (YYYY) for AOP data.
#' @param easting Optional tile-based coordinates for AOP downloads.
#' @param northing Optional tile-based coordinates for AOP downloads.
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
