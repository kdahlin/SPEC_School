# read .sig files collected from SpectraVista Corp spectrometer
# Adapted from original code by Aaron Kamoske
# Updated by Kyla Dahlin (kdahlin@msu.edu)

setwd("C:/Desktop/Spec2025/SPEC_School/spec_school_2025/")
getwd()

# list all the .sig files in the directory
sig.files <- list.files("MLBS_2025_spectra", 
                        pattern = c("\\.sig$"),
                        full.names = TRUE)

# loop through all the files and put them into a matrix
for (i in 1:length(sig.files)) {
  
  print(i)
  
  # read in the .sig file
  spec <- read.delim(sig.files[i])
  
  # save just the wavelength data, which occurs at row 27
  wavelength.data <- data.frame(spec[27:nrow(spec),])
  
  # we need to pull the sample name so that we use this later
  # note that the locations of "/" and "_" are hard coded here so check if you
  # have a different file structure!
  file.name <- tools::file_path_sans_ext(strsplit(sig.files[i], 
                                                  "/")[[1]][2])
  id.name <- paste0(strsplit(file.name, "_")[[1]][1], "_", 
                    strsplit(file.name, "_")[[1]][2])
  samp.name <- strsplit(file.name, "_")[[1]][3]
  
  #the first time through we want to add the header data
  if (i == 1){
    
    #create an empty matrix to save all the data in
    spectra.matrix <- as.data.frame(matrix(ncol = nrow(wavelength.data) + 2, 
                                           nrow = length(sig.files) + 1))
    
    #first we need to populate the first row of the matrix with header data
    names(spectra.matrix)[1] <- "ID"
    names(spectra.matrix)[2] <- "SAMPLE"
    
    #lets add in the wavelength names too
    for(z in 3:ncol(spectra.matrix)) {
      
      #find the wavelength - we subtract 2 because of starting at the 3rd column
      wl <- as.numeric(strsplit(as.character(wavelength.data[z-2,]), " ")[[1]][1])
      
      #create the header name
      wl.name <- paste0("nm",wl)
      
      #add the name to the spot in the matrix
      names(spectra.matrix)[z] <- wl.name
      
    }
    
    #now that the header is done lets add the data to the first row
    spectra.matrix[i, 1] <- id.name
    spectra.matrix[i, 2] <- samp.name
    
    #lets add in all the wavelength information
    for (q in 3:ncol(spectra.matrix)) {
      
      #add the spectral data
      spectra.matrix[i, q] <- as.numeric(strsplit(as.character(wavelength.data[q-2,]), " ")[[1]][7])
      
    }
  } else {
    
    #lets add in all the wavelength information
    for (q in 3:ncol(spectra.matrix)) {
      
      #add the id and sample name to the matrix
      spectra.matrix[i, 1] <- id.name
      spectra.matrix[i, 2] <- samp.name
      
      #add the spectral data
      spectra.matrix[i, q] <- as.numeric(strsplit(as.character(wavelength.data[q-2,]), " ")[[1]][7])
      
    }
  }
}

#lets write this as a csv with no column or row names and using a "," as a seperator
write.csv(spectra.matrix, 
          file = "MLBS2023_SPEC_School_merged_spectra.csv",
          row.names = FALSE)

#read in csv file we just created so that is in the correct format
spectra.csv <- read.csv("MLBS2023_SPEC_School_merged_spectra.csv")

#lets take the average off the spectral bands for each sample
#spec.avg <- aggregate(.~ID, FUN = mean, data = spectra.csv)

#lets remove the sample number column, since it is not needed any longer
spectra.csv$SAMPLE <- NULL
spectra.csv$nmNA<-NULL

#lets write this as a csv in the appropriate directory
setwd("C:/Desktop/Spec2025/SPEC_School/spec_school_2025/MLBS_2025_spectra")
write.csv(spectra.csv, 
          file = "MLBS2025_SPEC_School_average_Spectra_20250618.csv",
          row.names = FALSE)


#then i went in excel and transposed it, so read it in again
setwd("C:/Desktop/Spec2025/SPEC_School/spec_school_2025/MLBS_2025_spectra")
spectra2.csv<-read.csv("MLBS2025_SPEC_School_average_Spectra_20250618.csv")

#let's clean this up a little more
head(spectra2.csv)
colnames(spectra2.csv) <- sub("_NA$", "", colnames(spectra2.csv))

spectra2.csv$ID <- sub("^nm", "", spectra2.csv$ID)
colnames(spectra2.csv)[colnames(spectra2.csv) == "ID"] <- "wavelength"

getwd()
setwd("C:/Desktop/Spec2025/SPEC_School/spec_school_2025/leaf_level_spectra")
wa<-read.csv("weight.area.csv")

