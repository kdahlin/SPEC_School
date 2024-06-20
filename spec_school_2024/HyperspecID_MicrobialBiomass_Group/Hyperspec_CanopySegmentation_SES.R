

# Set up R Wrapper for DeepForest python package --------------------------


# install.packages('reticulate') # Install R package for interacting with Python
# reticulate::install_miniconda() # Install Python
# reticulate::py_install(c('gdal', 'rasterio', 'fiona')) # Install spatial dependencies via conda
# reticulate::conda_remove('r-reticulate', packages = c('mkl')) # Remove package that causes conflicts on Windows (and maybe macOS)
# reticulate::py_install('DeepForest', pip=TRUE) # Install the Python retriever package
# devtools::install_github('weecology/deepforestr') # Install the R package for running the retriever
# install.packages('raster') # For visualizing output for rasters

# Install reticulate package
# install.packages('reticulate')

# # Load reticulate library
# library(reticulate)

# Create a new Conda environment
# reticulate::conda_create("deepforest_env")

# # Use the new Conda environment
# reticulate::use_condaenv("deepforest_env", required = TRUE)
# 
# 
# # Update Conda
# reticulate::conda_update()

# Install spatial dependencies via Conda
# reticulate::py_install(c('gdal', 'rasterio', 'fiona'), envname = "deepforest_env")

# Install the DeepForest Python package via pip
# reticulate::py_install('DeepForest', envname = "deepforest_env", pip = TRUE)

# Remove mkl package if necessary (optional)
# reticulate::conda_remove('deepforest_env', packages = c('mkl'))

# install.packages('devtools')

# # Install the deepforestr R package from GitHub
# devtools::install_github('weecology/deepforestr')
# remotes::install_github("weecology/deepforestr") # development version from GitHub
# install.packages('weecology/deepforestr')

# Optional: Install raster package for visualizing output for rasters
# install.packages('raster')
# library('terra')

# library('DeepForest')
# # library('weecology/deepforestr')
# use_condaenv("deepforest_env", required = TRUE)
# 
# # Check if DeepForest can be imported
# py_run_string("import deepforest")



# Load packages & set up enviroment -----------------------------------------------------------
# Update Conda
reticulate::conda_update() # TODO unsure if needed when running after everything is installed

# Load reticulate library
library(reticulate)
reticulate::use_condaenv("deepforest_env", required = TRUE)
library('terra')
library(deepforestr)

# Check if DeepForest can be imported
py_run_string("import deepforest")


model = deepforestr::df_model()
model$use_release()

project_path <- '/Volumes/rs-016/ersamlab/hyperspec_id_group'
# Define multiple directory levels and file name
test_mlbs_image <- "2018_MLBS_3_541000_4140000_image_crop.tif"

# Combine the base path, sub-directory, and file name
file_path <- file.path(project_path, test_mlbs_image)

# Print the full path
print(file_path)


image_path = get_data(file_path) # Gets a path to an example image
bounding_boxes = model$predict_image(path=image_path, return_plot=FALSE)
head(bounding_boxes)
