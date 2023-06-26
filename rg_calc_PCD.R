#' @title Calculate average particle compensation depth
#' @author Stevie Walker
#' @date 10/25/22
#' @description finds the historical and long-term average PCD 
#' @note this function takes a while to run, you can run each model in a different R session to make things go faster
#' @inputs depth resolved POC flux file
#' @outputs rasters for plotting PCD


calc_PCD <- function(model.name) {
  
  nc.expc <- list.files(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"), pattern = "expc")
  
  #open nc file
  setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
  nc_his <- nc_open(nc.expc[1])
  if(model.name == "UKESM") {
    nc_fut <- nc_open(nc.expc[7])
  } else {
    nc_fut <- nc_open(nc.expc[2])
  }
  
  #storage container for list of matrices
  list_expc <- list()
  #storage container for second for loop
  output <- matrix(nrow = 360, ncol = 180)
  
  if(model.name == "UKESM") {
    v = seq(from = 1, to = 577, by = 12)
  } else {
    v = 1:50
  }
  #creates list of 50 arrays at every lat,lon,depth
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
    
    #calculates PCD every grid cell for one year
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
        #na test
        ret$test <- extract(expc[, , 2], indices = c(i,j), dims = c(1,2))
        
        #ocean values - if a value exists, extract PCD
        if (is.na(ret$test) == FALSE) {
          
          #store interpolated POC flux into the output matrix
          output[i, j] <- ret$depth[which.max(ret$expc)]
          #land values - if a value doesn't exist just put an NA value in output matrix  
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
  mean_PCD_his <- apply(expc_his, c(1, 2), mean, na.rm = FALSE)
  
  
  ## CALCULATE LONG-TERM PCD ----------------
  
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
    
    #calculates PCD every grid cell for one year
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
        #na test
        ret$test <- extract(expc[, , 2], indices = c(i,j), dims = c(1,2))
        
        #ocean values - if a value exists, extract PCD
        if (is.na(ret$test) == FALSE) {
          
          #store interpolated POC flux into the output matrix
          output[i, j] <- ret$depth[which.max(ret$expc)]
          #land values - if a value doesn't exist just put an NA value in output matrix  
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
  mean_PCD_lt <- apply(expc_lt, c(1, 2), mean, na.rm = FALSE)
  
  #save each matrix as an ascii file
  matrices = list(mean_PCD_lt, mean_PCD_his)
  
  names = c("PCD_lt", "PCD_his")
  
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
    #save PCD average
    writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/PCD/", model.name,"_",names[i],"_rg.asc"), varname = "depth", overwrite = TRUE)
  }
  
}