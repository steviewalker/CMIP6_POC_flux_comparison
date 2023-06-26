#' @title Calculate euphotic zone depth
#' @description euphotic zone depth is defined as 1% 5% or 10% of NPP max
#' @description calculates Ez depth change, historical, and long-term average Ez depth
#' @input NPP nc files
#' @output three matrices of long-term (2080-2100), historical (1850-1900), and change in Ez depth
#' @author Stevie Walker
#' @date 3/28/22

calc_ez_depth <- function(model.name, ez.metric) {
  
  nc.pp <- list.files(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"), pattern = "pp")
  
  #open nc file
  setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
  if(model.name == "EC-Earth") {
    nc_his = NA
  } else {
    nc_his <- nc_open(nc.pp[1])
  }
  if(model.name == "UKESM") {
    nc_fut <- nc_open(nc.pp[5])
  } else if(model.name == "EC-Earth") {
    nc_fut <- nc_open(nc.pp)
  } else {
    nc_fut <- nc_open(nc.pp[2])
  }
  
  # Calculate future average (2080-2100) euphotic zone depth -----------
  
  if(model.name == "UKESM" || model.name == "CMCC") {
    #monthly version
    v = seq(from = 793, to = 1021, by = 12)
  } else {
    #yearly version
    v = seq(from = 67, to = 86, by = 1)
  }
  
  #create list to store for loop ez depth arrays in
  list_ez <- list()
  
  #storage container for one percent npp data
  output <- matrix(nrow = 360, ncol = 180)  
  
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    
    print(paste0("future ", k))
    
    if(model.name == "UKESM" || model.name == "CMCC") {
      #pulls out monthly array and takes year average
      pp <- ncvar_get(nc_fut, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
      pp <- apply(pp, c(1,2,3),mean,na.rm=FALSE)
    } else {
      #pulls out array for one year, 3D with lat,lon,depth
      pp <- ncvar_get(nc_fut,"pp",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    }            
    #calculates column integrated npp for one year
    for(i in 1:360) {
      for(j in 1:180) {
        
        ret <- list()
        #subset npp for select lat and lon
        ret$npp <- extract(pp, indices = c(i,j), dims = c(1,2))
        #adjust depth variable extraction based on model name
        if(model.name == "CESM") {
          ret$depth <-  ncvar_get(nc_fut, "lev") /100
        } else if (model.name == "IPSL") {
          ret$depth <-  ncvar_get(nc_fut, "olevel")
        } else {
          ret$depth <-  ncvar_get(nc_fut, "lev")
        } 
        
        if(model.name == "GFDL" || model.name == "CMCC" || model.name == "CM4") {
          
          #fix negative NPP values
          profile = cbind(ret$npp,ret$depth) %>% as_tibble()
          colnames(profile) = c("npp","depth")
          profile$npp <- ifelse(profile$npp < 0,
                                0,
                                profile$npp)
          profile = profile %>% 
            filter(!duplicated(npp))
          
          if(ez.metric == 1) {
            #finds one percent of max npp
            one.percent = max(profile$npp,na.rm = TRUE)*0.01
          } else if (ez.metric == 5) {
            one.percent = max(profile$npp,na.rm = TRUE)*0.05
          } else {
            one.percent = max(profile$npp,na.rm = TRUE)*0.1
          }
          
          #add back to list
          ret$npp = profile$npp
          ret$depth = profile$depth
          ret$one.percent = one.percent
          #true/false test (pulls out second depth value)
          ret$test <- extract(pp[, , 2], indices = c(i,j), dims = c(1,2))
          
        } else {
          
          if(ez.metric == 1) {
            #finds one percent of max npp
            ret$one.percent = max(ret$npp,na.rm = TRUE)*0.01
          } else if(ez.metric == 5) {
            ret$one.percent = max(ret$npp,na.rm = TRUE)*0.05
          } else {
            ret$one.percent = max(ret$npp,na.rm = TRUE)*0.1
          }
          
          #true/false test (pulls out second depth value)
          ret$test <- extract(pp[, , 2], indices = c(i,j), dims = c(1,2))
        }
        if (is.na(ret$test) == FALSE) {
          
          #find euphotic zone depth
          ez <- approx(x = ret$npp, y  = ret$depth, xout = ret$one.percent)
          
          if(is.na(ez$y) == FALSE) {
            #store interpolated ez depth into the output matrix
            output[i, j] <- ez$y
            
          } else {
            #true false vector of non-na and na values
            keep <- complete.cases(ret$npp)
            
            #keep rows with values
            new.npp <- ret$npp[keep]
            
            #pull out bottom depth 
            output[i,j] = ret$depth[length(new.npp)]
          }
        } else {
          # land values
          output[i,j] = NA
        }
      }
    }
    #store output into the list
    list_ez[[k]] <- output
  }
  
  #converts from list to matrices
  ez_fut <- do.call(cbind, list_ez)
  #combines matrices into a single array
  ez_fut <- array(ez_fut, dim=c(dim(list_ez[[1]]), length(list_ez)))
  
  #20 year future mean
  ez_depth_fut <- apply(ez_fut, c(1,2), mean, na.rm = FALSE)
  
  #convert to rasters (formatted for plotting)
  dimnames(ez_depth_fut) = list(lon = 1:360,lat = 1:180)
  old.df = melt(ez_depth_fut)
  
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
  
  if(ez.metric == 1) {
    writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/EZ_depth/", model.name,"_EZ_depth_lt_rg.asc"), varname = "pp", overwrite = TRUE)
  } else if(ez.metric == 5){
    writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/EZ_depth/", model.name,"_EZ_depth_lt_5_rg.asc"), varname = "pp", overwrite = TRUE)
  } else {
    writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/EZ_depth/", model.name,"_EZ_depth_lt_10_rg.asc"), varname = "pp", overwrite = TRUE)
  }
  #save EC-Earth matrix and array since it only has future npp data 
  if(model.name == "EC-Earth") {
    
    setwd("~/spatial_analysis/raster_output/EZ_depth/")
    #saving non-averaged arrays for calc_expc_ez
    if(ez.metric == 1) {
      saveRDS(ez_fut, file = paste0(model.name,"_array_ez_depth_lt.Rds"), ascii = TRUE)
    } else{
      saveRDS(ez_fut, file = paste0(model.name,"_array_ez_depth_5_lt.Rds"), ascii = TRUE)
    }
    
  } else {
    
    setwd("~/spatial_analysis/raster_output/EZ_depth/")
    #saving non-averaged array for calc_expc_ez
    if(ez.metric == 1) {
      saveRDS(ez_fut, file = paste0(model.name,"_array_ez_depth_lt.Rds"), ascii = TRUE)
    } else if(ez.metric == 5){
      saveRDS(ez_fut, file = paste0(model.name,"_array_ez_depth_5_lt.Rds"), ascii = TRUE)
    } else {
      saveRDS(ez_fut, file = paste0(model.name,"_array_ez_depth_10_lt.Rds"), ascii = TRUE)
    }
    ## Calculate historical average (1850-1900) euphotic zone depth ------------
    
    
    if(model.name == "UKESM" || model.name == "CMCC") {
      #monthly version
      v = seq(from = 1, to = 589, by = 12)
    }  else {
      #yearly version
      v = 1:50
    }
    
    #create list to store for loop ez depth arrays in
    list_ez <- list()
    
    #storage container for one percent npp data
    output <- matrix(nrow = 360, ncol = 180)  
    
    for(k in 1:length(v)) {
      #read in a year of data
      t <- v[k]
      
      print(paste0("historical ", k))
      
      if(model.name == "UKESM" || model.name == "CMCC") {
        #pulls out monthly array and takes year average
        pp <- ncvar_get(nc_his, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
        pp <- apply(pp, c(1,2,3),mean,na.rm=FALSE)
      } else {
        #pulls out array for one year, 3D with lat,lon,depth
        pp <- ncvar_get(nc_his,"pp",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
      }    
      #calculates column integrated npp for one year
      for(i in 1:360) {
        for(j in 1:180) {
          
          ret <- list()
          #subset npp for select lat and lon
          ret$npp <- extract(pp, indices = c(i,j), dims = c(1,2))
          #adjust depth variable extraction based on model name
          if(model.name == "CESM") {
            ret$depth <-  ncvar_get(nc_his, "lev") /100
          } else if (model.name == "IPSL") {
            ret$depth <-  ncvar_get(nc_his, "olevel")
          } else {
            ret$depth <-  ncvar_get(nc_his, "lev")
          } 
          
          if(model.name == "GFDL" || model.name == "CMCC" || model.name == "CM4") {
            
            #fix negative NPP values
            profile = cbind(ret$npp,ret$depth) %>% as_tibble()
            colnames(profile) = c("npp","depth")
            profile$npp <- ifelse(profile$npp < 0,
                                  0,
                                  profile$npp)
            profile = profile %>% 
              filter(!duplicated(npp))
            
            if(ez.metric == 1) {
              #finds one percent of max npp
              one.percent = max(profile$npp,na.rm = TRUE)*0.01
            } else if (ez.metric == 5) {
              one.percent = max(profile$npp,na.rm = TRUE)*0.05
            } else {
              one.percent = max(profile$npp,na.rm = TRUE)*0.1
            }
            
            #add back to list
            ret$npp = profile$npp
            ret$depth = profile$depth
            ret$one.percent = one.percent
            #true/false test (pulls out second depth value)
            ret$test <- extract(pp[, , 2], indices = c(i,j), dims = c(1,2))
            
          } else {
            
            if(ez.metric == 1) {
              #finds one percent of max npp
              ret$one.percent = max(ret$npp,na.rm = TRUE)*0.01
            } else if(ez.metric == 5) {
              ret$one.percent = max(ret$npp,na.rm = TRUE)*0.05
            } else {
              ret$one.percent = max(ret$npp,na.rm = TRUE)*0.1
            }
            #true/false test (pulls out second depth value)
            ret$test <- extract(pp[, , 2], indices = c(i,j), dims = c(1,2))
          }
          if (is.na(ret$test) == FALSE) {
            
            #find euphotic zone depth
            ez <- approx(x = ret$npp, y  = ret$depth, xout = ret$one.percent)
            
            if(is.na(ez$y) == FALSE) {
              #store interpolated ez depth into the output matrix
              output[i, j] <- ez$y
              
            } else {
              #true false vector of non-na and na values
              keep <- complete.cases(ret$npp)
              
              #keep rows with values
              new.npp <- ret$npp[keep]
              
              #pull out bottom depth 
              output[i,j] = ret$depth[length(new.npp)]
            }
          } else {
            # land values
            output[i,j] = NA
          }
        }
      }
      #store output into the list
      list_ez[[k]] <- output
    }
    
    #converts from list to matrices
    ez_his <- do.call(cbind, list_ez)
    #combines matrices into a single array
    ez_his <- array(ez_his, dim=c(dim(list_ez[[1]]), length(list_ez)))
    
    #50 year historical mean
    ez_depth_his <- apply(ez_his, c(1,2), mean, na.rm = FALSE)
    
    ez_change = ez_depth_fut - ez_depth_his
    
    setwd("~/spatial_analysis/raster_output/EZ_depth/")
    
    if(ez.metric == 1) {
      #saving non-averaged arrays for calc_expc_ez
      saveRDS(ez_his, file = paste(model.name,"_array_ez_depth_his.Rds",sep=""), ascii = TRUE)
    } else if(ez.metric == 5) {
      saveRDS(ez_his, file = paste(model.name,"_array_ez_depth_5_his.Rds",sep=""), ascii = TRUE)
    } else {
      saveRDS(ez_his, file = paste0(model.name,"_array_ez_depth_10_his.Rds"), ascii = TRUE)
    }
    
    #save each matrix as an ascii file
    matrices = list(ez_depth_his, ez_change)
    
    names = c("EZ_depth_his", "EZ_depth_change")
    
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
      
      if(ez.metric == 1) {
        writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/EZ_depth/", model.name,"_",names[i],"_rg.asc"), varname = "pp", overwrite = TRUE)
      } else if(ez.metric == 5) {
        writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/EZ_depth/", model.name,"_",names[i],"_5_rg.asc"), varname = "pp", overwrite = TRUE)
      } else {
        writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/EZ_depth/", model.name,"_",names[i],"_10_rg.asc"), varname = "pp", overwrite = TRUE)
      }
    }
    
    
  }
}
