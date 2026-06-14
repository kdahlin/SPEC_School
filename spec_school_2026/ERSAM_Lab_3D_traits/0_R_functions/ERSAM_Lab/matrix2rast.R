

# from matrix (n_pixel * n_model) to average/std trait map
matrix2rast <- function(matrix, nrow, ncol){
  
  avg_vector <- rowMeans(matrix, na.rm = TRUE)
  std_vector <- apply(matrix, 1, sd, na.rm = TRUE)
  
  avg_2D <- matrix(avg_vector, nrow, ncol, byrow = TRUE)
  avg_2D_layer <- rast(avg_2D)
  
  std_2D <- matrix(std_vector, nrow, ncol, byrow = TRUE)
  std_2D_layer <- rast(std_2D)
  
  return(list(avg.layer = avg_2D_layer, std.layer = std_2D_layer, 
              avg.vector = avg_vector, std.vector = std_vector))
}
