start_time <- Sys.time()
print(start_time)
### load the library and settings
{
  library("sf")
  library("lidR")
  library("terra")
  library("raster")
  library("stringr")
  library("leafR")
  library(doSNOW)
  library(foreach)
  library(dplyr)
  # library("mapview")
  
  
  pathJ <- 'Your Path'
  setwd(pathJ) # HPC use
  
  pathout <- file.path(pathJ, 'out')
  if(!dir.exists(pathout)){
    dir.create(pathout)
  }
  
  Psiteyears <- read.csv(file.path(pathJ, "dir.csv") ) %>% 
    dplyr::pull(DIR)
  
  
  
  
  reso <- 30
  halfreso <- reso/2
  buff <- 10
  
  
  args = commandArgs(trailingOnly=TRUE)  
  
  # Sys.setlocale('LC_ALL','C')
  options(scipen = 100, digits = 15)
}





##################### Functions
{
  ### metrics based on chms
  ### z0 is considered as the cut-off height of shrub/grass
  chm_metrics <- function(grid_30m, z0){
    grid_30m@data$Z[grid_30m@data$Z < z0] <- 0
    #if then statment to filter out any bad plots (no points or no points > 0)
    if(sum(grid_30m@data$Z) > 0) {
      chm <- lidR::grid_canopy(grid_30m, res = 1, p2r()) # this uses all the points in the chm 
      #metrics from a chm (grid_canopy)
      rumple <- rumple_index(chm)
      mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
      max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
      top.rugosity <- sd(chm@data@values, na.rm = TRUE)
      
      #deep gap fraction (fraction of total rasterized area (1 m2 cells) w/points that are 
      #zero height (> 3 m))
      cells <- length(chm@data@values) 
      chm.0 <- chm
      chm.0[is.na(chm.0)] <- 0 
      deepgaps <- sum(chm.0@data@values == 0) 
      deepgap.fraction <- deepgaps/cells 
      out.chm <- c(rumple, top.rugosity, mean.max.canopy.ht, max.canopy.ht, deepgap.fraction)
      
    } else {
      #set rumple to 0, gap fraction to 1 if there were not points > .5 m height or no points
      # out.chm <- c(0, 0, 0, 0, 1)
      out.chm <- rep(NA, 5)
    }
    
    names(out.chm) <- c("rumple", "top.rugosity", "mean.max.canopy.ht",   "max.canopy.ht", "deepgap.fraction")
    return(out.chm)
  }
  
  

  
  structural_diversity_metrics <- function(grid_30m, z0) {
    ##### added by JW
    grid_30m@data$Z[grid_30m@data$Z < z0] <- NA
    Zs <- grid_30m@data$Z
    Zs <- Zs[!is.na(Zs)]
    if(length(Zs)  > 1 ) {
      #metrics from cloud_metrics
      vert.sd <- sd(Zs, na.rm = TRUE)  # <- corrected
      meanH <- mean(Zs, na.rm = TRUE)   # <- corrected
      vertCV <- vert.sd / meanH 
      
      
      #metrics from grid_metrics
      sd.9m2 <- lidR::grid_metrics(grid_30m, ~sd(Z, na.rm = T), 3) #9 m2 grid for sd.sd, na.rm =T and add ~ JW
      sd.sd <- sd(sd.9m2@data@values, na.rm = TRUE) 
      mean.sd <- mean(sd.9m2@data@values, na.rm = TRUE)
      # vertq <- quantile(las_file$Z, probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1), na.rm = TRUE)  # <- corrected
      ### Gini coefficient
      # Change GC function to load laz file from the envrionment, not from directory
      n <- length(Zs)
      x <- sort(Zs)
      G <- 2 * sum(x * 1L:n)/sum(x) - (n + 1L)
      GC <- G/(n - 1L)
      
      
      out.plot <- c(meanH, vert.sd, vertCV, mean.sd, sd.sd, GC) ##, vertq
      
    } else{
      out.plot <- rep(NA, 6)
    }
    
    names(out.plot) <- c("meanH", "vert.sd",  "vertCV", "mean.sd", "sd.sd", "GC")
    ## ,    "q0", "q1", "q5", "q25", "q50", "q75", "q95", 'q99', "q100"
    return(out.plot)
  }
  
  
  
  LAI_lidR <- function(grid_30m, k, z0){
    # grid_30m <- las_file
    #metrics from a Z vector
    Zs <- grid_30m@data$Z
    Zs <- Zs[!is.na(Zs)]
    if(length(Zs) > 1) {
      gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = z0) #ignore points < 3 m
      GFP <- mean(gap_frac$gf) ###the proportion of accumulated number of points in neighboring layers (z0 to x + dz) / (z0 to x)
      # LADen <- lidR::LAD(Zs, dz = 1, k=k, z0= z0) #ignore points < 3 m
      # ##### LADen <- LAD2(Zs, dz = 1, k=k, z0= z0) #ignore points < 3 m
      # VAI <- sum(LADen$lad, na.rm=TRUE) 
      
      Zs <- Zs[Zs>=z0]  #### add by Jianmin
      VCI <- lidR::VCI(Zs, by = 1, zmax=100) 
      out.plot <- c(GFP, VCI)  ###VAI, 
    } else{
      out.plot <- c(NA, NA) ##3NA, 
    }
    
    names(out.plot) <- c('GFP', 'VCI') ###'LAI.lidR', 
    return(out.plot)
  }
  
  

  # Change lad.voxels function to load laz file from the envrionment, not from directory
  lad.voxels2 <- function (normlas.file, grain.size = 3, k = 1) {
    # normlas.file <- las_file_38
    # normlas.file <- las_file; grain.size = 30; k = 0.5
    LAD_VOXELS <- list()
    Z <- NA
    .las <- normlas.file
    .las@data$Z[.las@data$Z < 0] <- 0
    maxZ <- floor(max(.las@data$Z))
    func <- formula(paste0("~leafR::pointsByZSlice(Z, ", maxZ, ")"))
    t.binneds <- lidR::grid_metrics(.las, func, res = grain.size,
                                    start = c(min(.las@data$X), max(.las@data$Y)))
    # t.binneds <- rasterToPoints(t.binneds, na.rm =F )
    ### If it only has 1 band, then raster::values will not record names.
    ### It it has multiple bands, then it will record the names in the matrix or array.
    ### So add the names to the 1 band
    nlayer <- raster::nlayers(t.binneds)
    if(nlayer <= 1 ) return(NULL)  
    # if(nlayer == 1)  colname <- names(t.binneds) 
    t.binneds <- data.frame(sp::coordinates(t.binneds), raster::values(t.binneds))
    names(t.binneds)[1:2] <- c("X", "Y")
    # if(nlayer == 1) names(t.binneds)[3]  <- colname
    ### Difference than rasterToPoints is that it includes NA
    
    
    pulses.profile.dz1 <- t.binneds[, length(t.binneds):3]
    
    total.pulses.matrix.dz1 = matrix(apply(pulses.profile.dz1, 
                                           1, sum), ncol = length(pulses.profile.dz1), nrow = nrow(pulses.profile.dz1))
    cumsum.matrix.dz1 = matrix(apply(pulses.profile.dz1, 1, cumsum), 
                               ncol = length(pulses.profile.dz1), nrow = nrow(pulses.profile.dz1), 
                               byrow = TRUE)
    rm(pulses.profile.dz1)
    pulse.out.dz1 = total.pulses.matrix.dz1 - cumsum.matrix.dz1
    if (nrow(t.binneds) != 1) {
      pulse.in.dz1 <- cbind(total.pulses.matrix.dz1[, 1], 
                            pulse.out.dz1[, -c(ncol(pulse.out.dz1))])
    } else {
      pulse.in.dz1 <- c(total.pulses.matrix.dz1[, 1], pulse.out.dz1[, 
                                                                    -c(ncol(pulse.out.dz1))])
    }
    rm(total.pulses.matrix.dz1, cumsum.matrix.dz1)
    dz = 1
    LAD.dz1 = log(pulse.in.dz1/pulse.out.dz1) * 1/k * 1/dz
    rm(pulse.in.dz1, pulse.out.dz1)
    LAD.dz1[is.infinite(LAD.dz1)] <- NA
    LAD.dz1[is.nan(LAD.dz1)] <- NA
    LAD.dz1 <- LAD.dz1[, -c(ncol(LAD.dz1))] ### This step will cause ncol-1 columns and will make 
    LAD_VOXELS[["LAD"]] <-  matrix(LAD.dz1, ncol = nlayer - 1)
    LAD_VOXELS[["coordenates"]] = t.binneds[, c("X", "Y")]
    rm(LAD.dz1, t.binneds)
    return(LAD_VOXELS)
  }
  
  
  LAI_leafR <- function(grid_30m, grain.size, z0, k){
    VOXELS_LAD <- lad.voxels2(grid_30m, grain.size = grain.size, k = k)  ### try i <- 38, 60
    if(!is.null(VOXELS_LAD)){
      lad_profile <- leafR::lad.profile(VOXELS_LAD, relative = T) #uses relative total LAI
      lad_profile <- lad_profile %>% filter(height >= z0)
      fhd <- leafR::FHD(lad_profile, evenness = FALSE, LAD.threshold = -1) #default
      
      lad_profile_lai <- leafR::lad.profile(VOXELS_LAD, relative = F) #doesn't use relative LAI
      LAI <- leafR::lai(lad_profile_lai, min = z0, max = 100)
      # LAI_subcanopy <- leafR::lai(lad_profile_lai, min = 1, max = 5)
      out.plot <- c(fhd, LAI)
    } else {
      out.plot <- c(NA, NA)
    }
    names(out.plot) <- c('FHD', 'LAI')
    return(out.plot)
  }
  
}








## Take 2018/FullSite/D01/2018_HARV_5 as an example
Psiteyear <- Psiteyears[94]
{
  siteyear <- str_split(Psiteyear, "/")[[1]][4]
  site <- str_split(siteyear, "_")[[1]][2]
  
  beer.lambert.constant <- 1
  grain.sizes <- c(1, 10, 30)  ### c(3, 30)
  cutoffs <- c(0.5, 2, 5)  ### c(10)
  
  
  # ####below sites for tests
  # decid <- c('2021_SERC_5', '2021_MLBS_4', '2019_SERC_4', '2018_MLBS_3')
  # everg <- c('2021_WREF_4', '2021_SOAP_5', '2019_WREF_3', '2019_SOAP_4')
  # mixed <- c('2019_HARV_6', '2019_BART_5', '2018_HARV_5', '2018_BART_4')
  # sites_select <- c(decid, everg, mixed)
  # if(siteyear %in% c(sites_select)) {
  #   grain.sizes <- c(1, 3, 5, 10, 15, 30)
  # }
}


{
  pathsiteyear <- file.path(pathJ, "AOP_normalization_las", Psiteyear, "L1/DiscreteLidar/ClassifiedPointCloud")
  
  
  # boundary <- st_read(paste0(pathD, "AOP_normalization/", tileyear, ".shp")) ### Not every siteyear has
  # sitebound <- st_read(file.path(pathD, 'AOP', 'SiteBoundary', paste0(site, ".shp" ) ))
  # fishnet <- st_read(file.path(pathJ, "UTMgrids", 'SiteBoundary', paste0('Landsat_cells_', site, '.shp')) )
  centershps <- st_read(file.path(pathJ, 'UTMgrids', 'SiteBoundary',  paste0('Landsat_centers_', site,  '.shp') ) )
  # centershps <- st_centroid(grid) 
  centers_all <- centershps %>% 
    dplyr::mutate(as.data.frame(st_coordinates(geometry)) ) %>% 
    st_drop_geometry()
  
  
  
  pathcsv <- file.path(pathout, siteyear)
  # pathcsv <- file.path(pathJ, 'test', siteyear)
  if(!dir.exists(pathcsv)){
    dir.create(pathcsv)
  } else {
    tmp <- list.files(pathcsv, pattern = ".csv$")
    if(length(tmp) >0 ){
      print(pathcsv)
      print(paste0("There are already csv files in ", pathcsv, " Please check!"))
      #stop("stopped")
    }
    
  }
  ctg <- lidR::readLAScatalog(pathsiteyear)  #, select = 'CLASSIFICAITON != 2'
  opt_progress(ctg) <- F
}

# ### Test
# {
# 
#   X <- c(723298.1958, 723627.3271, 724134.0268, 723984.6388)
#   Y <- c(4705707.6682, 4703543.8788, 4702110.2435, 4703760.3717)
#   points <- st_as_sf(data.frame(X, Y), coords = c('X', 'Y'), remove = FALSE, crs = st_crs(ctg))
#   print(chunks[apply(st_contains(chunks, points, sparse = F), 2, which),])
# 
#   points %>%
#     mutate(X = (X-halfreso) %/% reso *reso + reso, Y= (Y-halfreso) %/% reso *reso + reso) %>%
#     left_join(centers_all, c('X', 'Y'))
# }






{
  n.cores <- parallel::detectCores()
  print(paste0('there are ', parallel::detectCores(), ' cores'))
  n.cores <- n.cores- 1
  my.cluster <- parallel::makeCluster(
    n.cores,
    type = "PSOCK"
  )
  #register it to be used by %dopar%
  # doParallel::registerDoParallel(cl = my.cluster)
  #check if it is registered (optional)
  # system.time({
  doSNOW::registerDoSNOW(my.cluster)
  # })
  print(foreach::getDoParRegistered())
}

#----------

fileOUT <- file.path(pathcsv, paste0(siteyear, "_CSD.csv") )
if(file.exists(fileOUT)) next
### Prepare the data
{
  print(paste0('generating chunk ', k, ' out of ', length(chunks$chunkID)))
  # system.time({
  bbox <- st_bbox(ctg)
  las_sub <- lidR::clip_rectangle(ctg, bbox[1]-buff, bbox[2]-buff, bbox[3]+buff, bbox[4]+buff )
  # })
  # print('here')
  # opt_progress(ctg) <- F
  
  if (length(las_sub$Z) ==  0)  next
  # intersect_grid <- as.logical(lengths(sf::st_intersects(fishnet, chunks[k,])))
  # intersect_grid <- fishnet[intersect_grid, ]
  # intersect_grid_buffer <- st_buffer(intersect_grid, 10)
  # centers <- as.logical(lengths(sf::st_intersects(centershps, chunks[k,])))
  # centers <- centershps[centers,] %>% 
  #   dplyr::mutate(as.data.frame(st_coordinates(geometry)) ) %>% 
  #   st_drop_geometry()
  # bbox <- st_bbox(chunks[k,])
  centers <- centers_all %>% 
    dplyr::filter(X>=bbox[1], Y >=bbox[2], X <bbox[3], Y < bbox[4])
  
  
  
  #system.time( {
  ### grid it into cells
  XX <- las_sub$X
  YY <- las_sub$Y
  groupdata <- data.frame(X = c(XX-buff, XX-buff, XX+buff, XX+buff), Y = c(YY-buff, YY+buff, YY+buff, YY-buff)) %>%
    mutate(X = (X-halfreso) %/% reso *reso + reso, Y= (Y-halfreso) %/% reso *reso + reso) %>%
    left_join(centers, c('X', 'Y')) %>% 
    group_by(cellID) %>% 
    group_data() %>% 
    # mutate(.rows = purrr::map(.rows, ~.x[!duplicated(.x)])) %>%
    # mutate(.rows = lapply(.rows, function(x) x[!duplicated(x)])) %>%
    group_by(cellID) %>%
    dplyr::mutate(.rows = list(.rows[[1]] %% length(XX) ) ) %>%
    dplyr::mutate(.rows = list(.rows[[1]][!duplicated(.rows[[1]])])) %>%
    # mutate(.rows = list(unique(.rows[[1]] %% length(XX) )  ) ) %>%
    filter(!is.na(cellID))
  las_sub_list <- lapply(1:length(groupdata$cellID),  function(id) las_sub[groupdata$.rows[[id]]] )
  centers <- centers %>% 
    filter(cellID %in% groupdata$cellID)
  #} )
  
  
  # las_sub[which(XX>=xmin[1] & XX<xmax[1] & YY>=ymin[1] & YY<ymax[1])]
  
  runs <- seq(length(groupdata$cellID))
  # runs <- seq(5001, 7500)
  # runs <- seq(1, 10)
  print(range(runs))
      
  pb <- txtProgressBar(max = length(runs), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  
  ### 173230, 173231, 136479, 136480, 111949
  # i <- which(groupdata$cellID==136479)
  # i <- which(centers$cellID == 140075)
  # i <- 100
  # las_file <- las_sub_list[[i]]
  # xx <- centers$X[i]
  # yy <- centers$Y[i]
  # plot(las_file, color = 'Classification')
}
# which(centers$cellID == 43273)


#### Parallel computing
# system.time({
OUT <- foreach(las_file = las_sub_list,
               xx = centers$X, 
               yy = centers$Y,
               cellID = centers$cellID,
               # .export = c('las_sub', 'centers'),
               .packages = c('dplyr', "leafR", "lidR"), #,'foreach', 'plyr', 'parallel'
               .combine = rbind,
               # do.call(plyr::rbind.fill, Y),
               .options.snow = opts,
               .inorder=F
) %dopar% 
  tryCatch(
    {
      # ii <- 30
      # las_file <- las_sub_list[[ii]]
      # xx <- centers$X[ii]
      # yy <- centers$Y[ii]
      # cellID <- centers$cellID[ii]
      ## las_file <- lidR::clip_rectangle(las_sub, xx-25, yy-25, xx+25, yy+25)
      ## las_file <- lidR::clip_roi(ctg, intersect_grid_buffer[i, ]) ### clip_rectangle
      
      # system.time({
      las_file <- filter_poi(classify_noise(las_file, ivf(res=3,n=0)),
                             Classification %in% c(1, 2, 3, 4, 5)
                             # Classification != 7, Classification != 6,  Classification != LASNOISE
      ) %>%
        filter_duplicates() %>%
        # filter_poi(Z >= 0.5 & Z < mean(las_file@data$Z)+ 6*sd(las_file@data$Z)) %>%
        filter_poi(Z >= 0) %>%
        filter_poi(Z < mean(las_file@data$Z)+ 6*sd(las_file@data$Z)) %>%
        filter_poi(X >= xx-halfreso & X < xx + halfreso & Y >= yy-halfreso & Y < yy + halfreso)
      # })
      
      # return(OUT)
      
      if (length(las_file$Z) > 0) {
        plot.area <- area(las_file)  ## No need to record
        #total number of points divided by plot area
        den <- length(las_file@data$Z)/plot.area  ## No need to record
        #max height to catch if outliers are being removed or something is off with outlier filter
        # maxZ <- max(las_file@data$Z)   ## No need to record
        #chm area metrics function w/ground points kept 
        vertq <- quantile(las_file$Z, probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1), na.rm = TRUE)  # <- corrected
        names(vertq) <- c('q0', 'q1', 'q5', 'q25', 'q50', 'q75', 'q95', 'q99', 'q100')
        out.i <- c(cellID = cellID, plot.area = plot.area, density = den, vertq)
        
        for(z0 in cutoffs){
          # z0 <- cutoffs[1]
          las_file2 <- las_file %>% filter_poi(Z >= z0)
          vertq <- quantile(las_file2$Z, probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1), na.rm = TRUE)  # <- corrected
          names(vertq) <- c('q0', 'q1', 'q5', 'q25', 'q50', 'q75', 'q95', 'q99', 'q100')
          veg.area <- area(las_file2)
          den <- length(las_file2@data$Z)/veg.area
          
          chm.metrics <- chm_metrics(las_file, z0)
          #if there are no points b/c all veg is < 0.5 m keep loop running
          FSD.i <- structural_diversity_metrics(las_file, z0)
          LAI.lidR <- LAI_lidR(las_file, beer.lambert.constant, z0)
          
          out.z <- c(veg.area = veg.area, density = den, vertq, chm.metrics, FSD.i, LAI.lidR)
          for(grain.size in grain.sizes){  #### only tests for several sites. once determined, focus on one grain.size.
            LAI.leafR <- LAI_leafR(las_file, grain.size, z0, beer.lambert.constant)
            ### only if in tests
            tmpnames <- names(LAI.leafR)
            names(LAI.leafR) <- paste0(tmpnames, '.grain', sprintf("%02d", grain.size))
            assign(paste0('LAI.leafR.grain', sprintf("%02d", grain.size)), LAI.leafR)
            out.z <- c(out.z, LAI.leafR)
          }
          tmpnames <- names(out.z)
          names(out.z) <- paste0('cutoff', sprintf("%02d", floor(z0*10)), '.', tmpnames )
          out.i <- c(out.i, out.z)
        }
        
        out.i <- c(out.i, success = 'T')
        out.i <- as.data.frame(t(out.i))
        #-------------------------------------------------------------------
        # out.i <- cbind.data.frame(cellID = cellID, veg.plot.area, den, ##maxZ, 
        #                           chm_metrics_i, FSD.i, 
        #                           fhd, gini, LAI, LAI_subcanopy, success = 'T')
        return(out.i)
        # OUT <- rbind(OUT, out.i) 
        # write.csv(OUT, file.path(pathJ, 'canopy_structure', 'test', paste0(dir.k, '_', intersect_grid_buffer$cellID[i], "_CSC.csv")  ))
        # }
        
      } 
      # else {
      #   OUT <- OUT
      #   write.csv(OUT, paste0("./AOP_normalization/test2/", dir.k, "_", intersect_grid_buffer$cellID[i], "_CSC.csv"))
      #  }
      
      # return(OUT)
    }, error=function(e) {
      ncolumns <- 12 + (11 + 5 + 6 + 2 + 2 * length(grain.sizes)) * length(cutoffs) 
      out.i <- c(rep(NA, ncolumns), 'F')
      outnames <- c("cellID", "plot.area", "density", 'q0', 'q1', 'q5', 'q25', 'q50', 'q75', 'q95', 'q99', 'q100')
      #### FORGOT ABOUT THE QUANTILE
      for(z0 in cutoffs){
        tmpnames <- c('veg.area', 'density', 'q0', 'q1', 'q5', 'q25', 'q50', 'q75', 'q95', 'q99', 'q100')
        tmpnames <- c(tmpnames, "rumple", "top.rugosity", "mean.max.canopy.ht",   "max.canopy.ht", "deepgap.fraction")
        tmpnames <- c(tmpnames, "meanH", "vert.sd",  "vertCV", "mean.sd", "sd.sd", "GC")
        tmpnames <- c(tmpnames, 'GFP', 'VCI') ###'LAI.lidR', 
        for(grain.size in grain.sizes) {
          tmpnames <- c(tmpnames, paste0(c('FHD', 'LAI'), '.grain', sprintf("%02d", grain.size)) )
        }
        outnames <- c(outnames, paste0('cutoff', sprintf("%02d", floor(z0*10)), '.', tmpnames))
      }
      outnames <- c(outnames, 'success')
      names(out.i) <- outnames
      out.i <- as.data.frame(t(out.i))
      # out.i <- data.frame(cellID = cellID, veg_plot_area = NA, den = NA, #maxZ = NA, 
      #                     rumple = NA, deepgap.fraction = NA,
      #                     mean.max.canopy.ht = NA, max.canopy.ht = NA, top.rugosity = NA,
      #                     vert.sd = NA, sd.sd = NA, GFP = NA, VAI = NA, VCI = NA, vertCV = NA, q0 = NA,
      #                     q25 = NA, q50 = NA, q75 = NA, q100 = NA, fhd = NA, gini = NA, LAI = NA, LAI_subcanopy = NA, 
      #                     success = 'F' ) #success = paste0(i, " Error: ", e$message)
      return(out.i)
    })

# })
# print('here')

    
    
# print(paste0('Chunk ', k, ' out of ', length(chunks$chunkID), ' is done')  )
if(!is.null(OUT)){
  # print(nrow(OUT))
  # print(OUT$success)
  # print(which(OUT$success != 'T'))
  # write.csv(OUT, paste0("./AOP_normalization/", laz_list_name_split[[k]][6], "_CSC.csv"))
  write.csv(OUT, fileOUT, row.names =  F )
}


close(pb)
  
  
  # registerDoSEQ()
stopCluster(my.cluster) ###parallel::



print("")
print(args)
print("")
print(start_time)  
end_time <- Sys.time()
print(end_time)
elaspsed_time <- end_time - start_time 
print(elaspsed_time)
