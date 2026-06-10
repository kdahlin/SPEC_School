# Interactive workflow wrapper around hemispheR functions.
# Intended for manual QA/QC and batch processing of NEON DHP imagery.

#' Processes a list of hemispherical photo files downloaded from NEON in .NEF format
#' interactively using the hemispheR and bRAW packages.
#' Allows user to continue previous sessions or start new, modify processing parameters,
#' analyze gap fractions and canopy metrics, and save progress automatically.
#' See the hemispheR user documentation for more details on listed parameters. 
#'
#' Citation:
#' Chiannuci, F. and M. Macek. 2023. hemispheR: an R package for fisheye canopy 
#' image analysis. Agricultural and Forest Meteorology 336; 109470. 
#'
#' @param photo.files.list Character. List of paths to hemispherical photo files.
#' @param gamma Logical. If set to TRUE, applies gamma adjustment on image import
#' . Native function bRAW::raw_blue.
#' @param method Character. The automated threshold method. Default is 'Otus'. For other
#' methods, see: https://imagej.net/plugins/auto-threshold 
#' Native function hemispheR::binarize_fisheye.
#' @param zonal Logical. If TRUE, divides the image in for sectors and applies 
#' automated classification separately to each region; useful in cases of uneven 
#' light. Native function hemispheR::binarize_fisheye.
#' @param startVZA Integer. The lower zenith angle (degree) used for analysis. 
#' Default is 0. Native function hemispheR::gapfrac_fisheye.
#' @param endVZA Integer. The upper zenith angle (degree) used for analysis. 
#' Default is 17. Native function hemispheR::gapfrac_fisheye.
#' @param lens Character. The lens type used when correcting fish-eye distortion. 
#' Default is equidistant. See 'list.lenses' in hemispheR package for more options.
#' Native function hemispheR::gapfrac_fisheye.
#' @param yes_display Logical. If TRUE the user will get interactive prompts
#' and plots generate as they process the photos. 
#' @param out_path Output directory path where processed data CSV will be saved.
#' @return A data frame containing canopy metrics for processed photos.
#' @export

process_photos_nef <- function(photo.files.list, method = "Otsu", 
                               zonal = FALSE, gamma = FALSE,
                               startVZA = 0, endVZA = 17, yes_display = FALSE, 
                               lens = "equidistant", out_path) {
  
  # -------------------- Package Dependencies ----------------------------
  
  # Load required packages
  for (pkg in c("terra", "hemispheR", "bRaw")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop(paste0("Package '", 
                                                            pkg, "' is required."))
  }
  
  # Initialize empty out.data
  out.data <- data.frame()

  # cancel instructions for headless operation
  if (!yes_display) {
    cat("Running in headless mode (display = FALSE).\n")
    cat("Press ESC at any time to cancel and save progress.\n")
  }
  
  # -------------------- Activation Prompt ----------------------------
  
  # Ask user if continuing processing or starting new
  session_type <- readline(prompt = paste("Do you want to continue a processing", 
                                           "session or start new? (continue/new/cancel): "))
  
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
      
      # Define variables the user can change during processing
      satisfied <- FALSE # Default to start loop
      method_val <- method 
      zonal_val <- zonal 
      gamma_val <- gamma
      cam_val <- lens
      startVZA <- startVZA
      endVZA <- endVZA
      
      # create list of available threshold methods
      methods_list <- c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li",
                        "MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu",
                        "Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen")
      
      while (!satisfied) {
        
        cat("Processed images:", nrow(out.data), "| Remaining:", 
            length(remaining_photos) - i + 1, "\n")
        
        img1 <- suppressWarnings(
          bRaw::raw_blue(
            lab.img,
            gamma.adj = gamma_val,
            circ.mask = NULL,
            display = yes_display,
            message = FALSE
          )
        )
        # rescale the image to typical 8-bit scale (0-255)
        img.rescaled <- img1 * 255
        
        # Bianrize image
        img.bw <- suppressWarnings(
          binarize_fisheye(img.rescaled, display = yes_display, 
                            method = method_val, zonal = zonal_val)
        )
        
        # User Prompt for satisfaction with binarized image
        if (yes_display == TRUE) {
          response <- readline(prompt = paste("Satisfied with image", 
                                              basename(lab.img), 
                                              "? (yes/no/skip/cancel): "))
          
          if (tolower(response) == "yes") {
            gap.frac <- suppressWarnings(
              gapfrac_fisheye(img.bw,
                              startVZA = startVZA,
                              endVZA = endVZA,
                              nrings = 7,
                              nseg = 8,
                              lens = cam_val,
                              display = yes_display) # calculate gap fraction
            )
            
            canopy.data <- canopy_fisheye(gap.frac) # canopy metric calculations 
            canopy.data$ImageName <- basename(lab.img) # add image name to ID column
            canopy.data$gamma <- gamma_val
            canopy.data$zonal <- zonal_val
            canopy.data$method <- method_val
            canopy.data$ImageName <- basename(lab.img)
            
            out.data <- rbind(out.data, canopy.data) # append to output data frame 
            satisfied <- TRUE # exit loop for this image 
            
            # Ask user if they want to apply linear stretch
          } else if (tolower(response) == "no") {
            
            # Ask if they want to adjust gamma first
            adjust_gamma <- readline(prompt = paste("Toggle gamma adjustment?", 
                                                    "(TRUE/FALSE or Enter to keep current): "))
              
              if (toupper(adjust_gamma) %in% c("TRUE", "FALSE")) 
                gamma_val <- as.logical(adjust_gamma)
            
            # Ask user if they want to change threshold method
            cat("\nAvailable Threshold Methods:\n")
            cat(paste(methods_list, collapse = ", "), "\n")
            cat(paste("Current Method:", method_val, "\n"))
            method_input <- readline(prompt = paste("Enter new thresholding method",
                                                    "(or press enter to keep current):"))
            if (method_input != "" && method_input %in% methods_list) {
              method_val <- method_input
            } else if (method_input != "") {
              cat("Invalid method entered. Keeping current method.\n")
            } 
            
            # Ask user if they want to apply zonal threshold 
            zonal_input <- readline(prompt = paste("Toggle zonal thresholding?", 
                                                   "(TRUE/FALSE or Enter to keep current): "))
            
            if (toupper(zonal_input) %in% c("TRUE", "FALSE")) 
              zonal_val <- as.logical(zonal_input)
            
          } else if (tolower(response) == "skip") {
            satisfied <- TRUE  # Skip this image
            
          } else if (tolower(response) == "cancel") {
            # Save data and abort
            save_file <- paste0(tools::file_path_sans_ext(out_path),"/", 
                                "processed_hemiphotos_",format(Sys.time(), "%Y%m%d"),
                                ".csv")
            write.csv(out.data, save_file, row.names = FALSE)
            cat(paste0("\nProcess aborted. Data saved to ", save_file, "\n"))
            return(out.data)  # Exit function and return data frame
          } else {
            cat("Invalid input. Please type 'yes', 'no', 'skip', or 'cancel'.\n")
          }
        } else {
          gap.frac <- suppressWarnings(gapfrac_fisheye(img.bw,
                                      startVZA = startVZA,
                                      endVZA = endVZA,
                                      nrings = 7,
                                      nseg = 8,
                                      display = yes_display) # calculate gap fraction
          )
          
          
          canopy.data <- canopy_fisheye(gap.frac) # canopy metric calculations 
          canopy.data$ImageName <- basename(lab.img) # add image name to ID column
          canopy.data$gamma <- gamma_val
          canopy.data$zonal <- zonal_val
          canopy.data$method <- method_val
          canopy.data$ImageName <- basename(lab.img)
          
          out.data <- rbind(out.data, canopy.data) # append to output data frame 
          satisfied <- TRUE # exit loop for this image 
          
        }
        
      }
      i <- i + 1  # Increment after inner loop finishes
    }
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


