#' @title Calculate average POC flux at any depth
#' @author Stevie Walker
#' @date 8/10/22
#' @description finds the historical and long-term average in POC flux at any singular depth CMIP6 earth system models
#' @note this function takes a while to run, you can run each model in a different R session to make things go faster
#' @inputs depth resolved POC flux file
#' @output 3 matrices (lat x lon) of interpolated POC flux values for a 50 year historical average (1850-1900), long-term average (2080-2100), and change

calc_expc <- function(model.name, depth, ez.metric) {
  
  nc.expc <- list.files(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"), pattern = "expc")
  
  #open nc file
  setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
  nc_his <- nc_open(nc.expc[1])
  if(model.name == "UKESM") {
    nc_fut <- nc_open(nc.expc[6])
  } else {
    nc_fut <- nc_open(nc.expc[2])
  }
  
  #read in arrays for depth variable depth horizons
  if(depth == "MLD_max") {
    
    #read in MLDmax arrays - MLDmax for each year (these objects come from calc_MLD_max.R)
    MLD_max_his <- readRDS(paste("~/spatial_analysis/raster_output/MLD_max/",model.name,"_array_MLD_max_his.Rds",sep = ""))
    MLD_max_lt <- readRDS(paste("~/spatial_analysis/raster_output/MLD_max/",model.name,"_array_MLD_max_lt.Rds",sep = ""))
    
  } else if(depth == "EZ_depth" && model.name == "EC-Earth") {
    
    if(ez.metric == 1) {
      EZ_depth_lt <- readRDS(paste("~/spatial_analysis/raster_output/EZ_depth/",model.name,"_array_ez_depth_lt.Rds",sep = ""))
    } else if (ez.metric == 5) {
      EZ_depth_lt <- readRDS(paste("~/spatial_analysis/raster_output/EZ_depth/",model.name,"_array_ez_depth_lt_5.Rds",sep = ""))
    } else {
      EZ_depth_lt <- readRDS(paste("~/spatial_analysis/raster_output/EZ_depth/",model.name,"_array_ez_depth_10_lt.Rds",sep = ""))
    }
  } else if(depth == "EZ_depth") {
    
    if(ez.metric == 1) {
      #read in ez depth arrays
      EZ_depth_his <- readRDS(paste("~/spatial_analysis/raster_output/EZ_depth/",model.name,"_array_ez_depth_his.Rds",sep = ""))
      EZ_depth_lt <- readRDS(paste("~/spatial_analysis/raster_output/EZ_depth/",model.name,"_array_ez_depth_lt.Rds",sep = ""))
    } else if (ez.metric == 5) {
      #read in ez depth arrays
      EZ_depth_his <- readRDS(paste("~/spatial_analysis/raster_output/EZ_depth/",model.name,"_array_ez_depth_5_his.Rds",sep = ""))
      EZ_depth_lt <- readRDS(paste("~/spatial_analysis/raster_output/EZ_depth/",model.name,"_array_ez_depth_5_lt.Rds",sep = ""))
    } else {
      EZ_depth_his <- readRDS(paste("~/spatial_analysis/raster_output/EZ_depth/",model.name,"_array_ez_depth_10_his.Rds",sep = ""))
      EZ_depth_lt <- readRDS(paste("~/spatial_analysis/raster_output/EZ_depth/",model.name,"_array_ez_depth_10_lt.Rds",sep = ""))
    }
  } else {
    print("no arrays to read in")
  }
  
  if(depth == "EZ_depth" && model.name == "EC-Earth") {
    print("skip historical calculation")
  } else {
    
    
    # CALCULATE HISTORICAL FLUX ---------
    
    #storage container for list of matrices
    list_expc <- list()
    #storage container for second for loop
    output <- matrix(nrow = 360, ncol = 180)
    
    if(model.name == "UKESM") {
      v = seq(from = 1, to = 577, by = 12)
    } else {
      v = 1:50
    }
    #creates list of 50 arrays with POC flux at every lat,lon,depth
    for(k in 1:length(v)) {
      #read in a year of data
      t <- v[k]
      print(paste0("historical ", k))
      
      if(model.name == "UKESM") {
        #pulls out monthly array and takes year average
        expc <- ncvar_get(nc_his, "expc", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
        expc <- apply(expc, c(1,2,3),mean,na.rm=FALSE)
        
      } else {
        #pulls out array for one year, 3D with lat,lon,depth
        expc <- ncvar_get(nc_his,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
      }
      
      #calculates POC flux every grid cell for one year
      for(i in 1:360) {
        for(j in 1:180) {
          
          #make list and add needed columns
          ret <- list()
          #adjust depth variable extraction based on model name
          if(model.name == "CESM") {
            ret$depth <-  ncvar_get(nc_his, "lev") /100
          } else if (model.name == "IPSL") {
            ret$depth <-  ncvar_get(nc_his, "olevel")
          } else {
            ret$depth <-  ncvar_get(nc_his, "lev")
          }
          #subset expc for select lat and lon
          ret$expc <- extract(expc, indices = c(i,j), dims = c(1,2))
          #find depth horizon depth for each lat lon
          if(depth == "MLD_max") {
            #subset MLD max for each lat and lon
            ret$dh <- extract(MLD_max_his[, , k], indices = c(i,j), dims = c(1,2))
            #NA test
            ret$test <- ret$dh
          } else if(depth == "EZ_depth") {
            #subset EZ depth for each lat and lon
            ret$dh <- extract(EZ_depth_his[, , k], indices = c(i,j), dims = c(1,2))
            #NA test
            ret$test <- ret$dh
          } else if(depth == "PCD") {
            #find the PCD
            ret$dh <- ret$depth[which.max(ret$expc)]
            #NA test
            ret$test <- extract(expc[, , 2], indices = c(i,j), dims = c(1,2))
          } else {
            #the depth to interpolate at
            ret$dh <- depth
            #NA test
            ret$test <- extract(expc[, , 2], indices = c(i,j), dims = c(1,2))
          }
          #ocean values - if a value exists, then interpolate and store new interpolated POC flux in output matrix
          if (is.na(ret$test) == FALSE) {
            
            #find interpolated expc
            interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$dh)
            #store interpolated POC flux into the output matrix
            output[i, j] <- interp$y[1]
            #land values - if a value doesn't exist, then don't interpolate, just put an NA value in output matrix  
          } else {
            output[i,j] <- NA
          }
        }
      }
      #store each year of output into a list
      list_expc[[k]] <- output
    }
    
    #converts from list to matrices
    expc_his <- do.call(cbind, list_expc)
    #combines matrices into a single array
    expc_his <- array(expc_his, dim=c(dim(list_expc[[1]]), length(list_expc)))
    
    #50 year mean for 1850-1900
    mean_expc_his <- apply(expc_his, c(1, 2), mean, na.rm = FALSE)
    
  }
  
  ## CALCULATE LONG-TERM FLUX ----------------
  
  
  #storage container for list of matrices
  list_expc <- list()
  #storage container for second for loop
  output <- matrix(nrow = 360, ncol = 180)
  
  if(model.name == "UKESM") {
    #monthly version
    v = seq(from = 373, to = 601, by = 12)
  } else {
    #last 20 time steps of expc file
    v = seq(from = 67, to = 86, by = 1)
  }
  #creates list of 20 arrays with POC flux at every lat,lon,depth
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    print(paste0("future ", k))
    if(model.name == "UKESM") {
      #pulls out monthly array and takes year average
      expc <- ncvar_get(nc_fut, "expc", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
      expc <- apply(expc, c(1,2,3),mean,na.rm=FALSE)
      
    } else {
      #pulls out array for one year, 3D with lat,lon,depth
      expc <- ncvar_get(nc_fut,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    }
    
    #calculates POC flux every grid cell for one year
    for(i in 1:360) {
      for(j in 1:180) {
        
        #make list and add needed columns
        ret <- list()
        #adjust depth variable extraction based on model name
        if(model.name == "CESM") {
          ret$depth <-  ncvar_get(nc_fut, "lev") /100
        } else if (model.name == "IPSL") {
          ret$depth <-  ncvar_get(nc_fut, "olevel")
        } else {
          ret$depth <-  ncvar_get(nc_fut, "lev")
        }
        #subset expc for select lat and lon
        ret$expc <- extract(expc, indices = c(i,j), dims = c(1,2))
        #find depth horizon depth for each lat lon
        if(depth == "MLD_max") {
          #subset MLD max for each lat and lon
          ret$dh <- extract(MLD_max_lt[, , k], indices = c(i,j), dims = c(1,2))
          #NA test
          ret$test <- ret$dh
        } else if(depth == "EZ_depth") {
          #subset EZ depth for each lat and lon
          ret$dh <- extract(EZ_depth_lt[, , k], indices = c(i,j), dims = c(1,2))
          #NA test
          ret$test <- ret$dh
        } else if(depth == "PCD") {
          #find the PCD
          ret$dh <- ret$depth[which.max(ret$expc)]
          #NA test
          ret$test <- extract(expc[, , 2], indices = c(i,j), dims = c(1,2))
        } else {
          #the depth to interpolate at
          ret$dh <- depth
          #NA test - selected second depth value because some of the first depths have a zero POC flux value
          ret$test <- extract(expc[, , 2], indices = c(i,j), dims = c(1,2))
        }
        
        #ocean values - if a value exists, then interpolate and store new interpolated POC flux in output matrix
        if (is.na(ret$test) == FALSE) {
          
          #find interpolated expc
          interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$dh)
          #store interpolated POC flux into the output matrix
          output[i, j] <- interp$y[1]
          #land values - if a value doesn't exist, then don't interpolate, just put an NA value in output matrix  
        } else {
          output[i,j] <- NA
        }
      }
    }
    #store each year of output into a list
    list_expc[[k]] <- output
  }
  
  
  #converts from list to matrices
  expc_lt <- do.call(cbind, list_expc)
  #combines matrices into a single array
  expc_lt <- array(expc_lt, dim=c(dim(list_expc[[1]]), length(list_expc)))
  
  #20 year mean 2080-2100
  mean_expc_lt <- apply(expc_lt, c(1, 2), mean, na.rm = FALSE)
  
  if(depth == "EZ_depth" && model.name == "EC-Earth") {
    
    dimnames(mean_expc_lt) = list(lon = 1:360,lat = 1:180)
    old.df = melt(mean_expc_lt)
    
    #correct lat and lon scales
    old.df$lon <- ifelse(old.df$lon>180,
                         old.df$lon-360,
                         old.df$lon)
    old.df$lat <- ifelse(old.df$lat<90,
                         old.df$lat-90,
                         old.df$lat-90)
    
    r = rasterFromXYZ(old.df, crs = "+proj=longlat +datum=WGS84 +no_defs")
    #example plot
    plot(r)
    
    if(ez.metric == 1){
      writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/POC_EZ_depth/EC-Earth_POC_EZ_depth_lt_rg.asc"), varname = "expc", overwrite = TRUE)
    } else if (ez.metric == 5) {
      writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/POC_EZ_depth/EC-Earth_POC_EZ_depth_lt_5_rg.asc"), varname = "expc", overwrite = TRUE)
    } else {
      writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/POC_EZ_depth/EC-Earth_POC_EZ_depth_lt_10_rg.asc"), varname = "expc", overwrite = TRUE)
      
    }
  } else {
    
    #change in POC flux at the MLD max
    expc_change = mean_expc_lt - mean_expc_his
    
    #save each matrix as an ascii file
    matrices = list(mean_expc_lt, mean_expc_his, expc_change)
    
    if (depth == "MLD_max") {
      names = c("POC_MLD_max_lt", "POC_MLD_max_his", "POC_MLD_max_change")
    } else if(depth == "EZ_depth") {
      names = c("POC_EZ_depth_lt", "POC_EZ_depth_his", "POC_EZ_depth_change")
    } else if(depth == "PCD") {
      names = c("POC_PCD_lt", "POC_PCD_his", "POC_PCD_change")
    } else if(depth == 1000) {
      names = c("POC_1000_lt", "POC_1000_his", "POC_1000_change")
    } else {
      names = c("POC_100_lt", "POC_100_his", "POC_100_change")
    }
    
    for(i in 1:length(matrices)) {
      
      #convert to rasters (formatted for plotting)
      matrix <- matrices[[i]]
      
      dimnames(matrix) = list(lon = 1:360,lat = 1:180)
      old.df = melt(matrix)
      
      #correct lat and lon scales
      old.df$lon <- ifelse(old.df$lon>180,
                           old.df$lon-360,
                           old.df$lon)
      old.df$lat <- ifelse(old.df$lat<90,
                           old.df$lat-90,
                           old.df$lat-90)
      
      r = rasterFromXYZ(old.df, crs = "+proj=longlat +datum=WGS84 +no_defs")
      #example plot
      plot(r)
      
      if(depth == "EZ_depth" && ez.metric == 5) {
        writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/POC_", depth, "/", model.name,"_",names[i],"_5_rg.asc"), varname = "expc", overwrite = TRUE)
      } else if (depth == "EZ_depth" && ez.metric == 10) {
        writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/POC_", depth, "/", model.name,"_",names[i],"_10_rg.asc"), varname = "expc", overwrite = TRUE)
      } else {
        writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/POC_", depth, "/", model.name,"_",names[i],"_rg.asc"), varname = "expc", overwrite = TRUE)
      }
    }
  }
}
