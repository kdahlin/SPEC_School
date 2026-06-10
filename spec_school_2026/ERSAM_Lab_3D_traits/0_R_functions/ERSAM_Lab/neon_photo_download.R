# Function for downloading photos from NEON repository to the disk in bulk.
#
# Download multiple hemispherical photos from the NEON repository directly to your 
# disk without loading them into the R environment individually. 
#
#' @param images A data frame containing image metadata with columns 'startDate', 
#' 'imageFileUrl', and 'imageFileName'.
#' @param out_dir Directory path to save downloaded photos. Created if it does not exist.
#' @return None. Files are saved to disk.
#' @export


neon_photo_download <- function(images, out_dir) {
  
  for (i in 1:nrow(images)) {
    file_url <- images$imageFileUrl[i]
    file_name <- images$imageFileName[i]
    dest <- file.path(out_dir, file_name)
    
    if (!file.exists(dest)) {
      download.file(url = file_url, destfile = dest, mode = "wb")
      message("Downloaded ", images$imageFileName[i])
    } else {
      message("Skipped ", images$imageFileName[i], " (already exists)")
    }
  }
}
