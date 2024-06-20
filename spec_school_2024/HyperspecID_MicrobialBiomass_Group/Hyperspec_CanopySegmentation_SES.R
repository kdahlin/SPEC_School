install.packages('reticulate') # Install R package for interacting with Python
reticulate::install_miniconda() # Install Python
reticulate::py_install(c('gdal', 'rasterio', 'fiona')) # Install spatial dependencies via conda
reticulate::conda_remove('r-reticulate', packages = c('mkl')) # Remove package that causes conflicts on Windows (and maybe macOS)
reticulate::py_install('DeepForest', pip=TRUE) # Install the Python retriever package
devtools::install_github('weecology/deepforestr') # Install the R package for running the retriever
# install.packages('raster') # For visualizing output for rasters





# Install reticulate package
# install.packages('reticulate')

# Load reticulate library
library(reticulate)

# Create a new Conda environment
reticulate::conda_create("deepforest_env")

# Use the new Conda environment
reticulate::use_condaenv("deepforest_env", required = TRUE)


# Update Conda
reticulate::conda_update()

# Install spatial dependencies via Conda
reticulate::py_install(c('gdal', 'rasterio', 'fiona'), envname = "deepforest_env")

# Install the DeepForest Python package via pip
reticulate::py_install('DeepForest', envname = "deepforest_env", pip = TRUE)

# Remove mkl package if necessary (optional)
# reticulate::conda_remove('deepforest_env', packages = c('mkl'))

# install.packages('devtools')

# Install the deepforestr R package from GitHub
devtools::install_github('weecology/deepforestr')
remotes::install_github("weecology/deepforestr") # development version from GitHub
install.packages('weecology/deepforestr')

# Optional: Install raster package for visualizing output for rasters
# install.packages('raster')
library('terra')

# library('DeepForest')
# library('weecology/deepforestr')
use_condaenv("deepforest_env", required = TRUE)


library(deepforestr)

# Check if DeepForest can be imported
py_run_string("import deepforest")


model = deepforestr::df_model()
model$use_release()
