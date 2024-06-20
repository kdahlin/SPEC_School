#install.packages('reticulate') # Install R package for interacting with Python
#reticulate::install_miniconda() # Install Python
#reticulate::py_install(c('gdal', 'rasterio', 'fiona')) # Install spatial dependencies via conda
#reticulate::conda_remove('r-reticulate', packages = c('mkl')) # Remove package that causes conflicts on Windows (and maybe macOS)
#reticulate::py_install('DeepForest', pip=TRUE) # Install the Python retriever package
#devtools::install_github('weecology/deepforestr') # Install the R package for running the retriever
#install.packages('raster') # For visualizing output for rasters




reticulate::conda_create("myenv")

reticulate::conda_install("myenv", "numpy")
reticulate::conda_install("myenv", "deepforest")

library(reticulate)
use_condaenv("myenv", required = TRUE)
py_config()

deepforest <- import("deepforest")
model <- deepforest$main$deepforest()


image_path = deepforestr::get_data("OSBS_029.png") # Gets a path to an example image
bounding_boxes = model$predict_image(path=image_path, return_plot=FALSE)
head(bounding_boxes)
