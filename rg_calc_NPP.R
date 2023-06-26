#' @title Calculate long-term and historical average column integrated NPP (mol m-2 d-1)
#' @author Stevie Walker
#' @date 8/12/22
#' @description finds long-term (2080-2100) and historical(1850-1900) average NPP, summed up vertically for each grid cell
#' @input ssp585 and historical pp files
#' @output three rasters: long-term NPP, historical NPP, and change in NPP

## Calculate water column integrated NPP --------------

calc_npp_avg <- function(model.name) {
  
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
  
  # Calculate future average (2080-2100) depth-integrated NPP -----------
  
  if(model.name == "UKESM" || model.name == "CMCC") {
    #monthly version
    v = seq(from = 793, to = 1021, by = 12)
  } else {
    #yearly version
    v = seq(from = 67, to = 86, by = 1)
  } 
  #create list to store for loop ez depth arrays in
  list_npp <- list()
  
  #storage container for one percent npp data
  output <- matrix(nrow = 360, ncol = 180)  
  
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    
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
        #true/false test (pulls out npp at second depth value)
        ret$test <- extract(pp[, , 2], indices = c(i,j), dims = c(1,2))
        #z (number of depth grid cells)
        ret$z <- length(ret$depth)
        
        #ocean values - if a value exists for npp, then find column integrated npp (mol m-2 d-1) and store in output matrix
        if (is.na(ret$test) == FALSE) {
          
          #create data frame
          #NOTE: need to change [1:x] to match the number of depth cells
          profile <- data.frame(ret$depth, ret$npp[1:ret$z]) %>%
            as_tibble() 
          #rename depth column
          profile <- rename(profile, depth = ret.depth)
          #also change this to match number of depth cells
          profile <- rename(profile, npp = ret.npp.1.ret.z.)
          
          #add calculated column height
          profile <- profile %>% 
            mutate(lead = lead(depth))
          profile <- profile %>%
            mutate(bottom_depth = rowMeans(profile[c('lead','depth')]))
          profile <- profile %>%
            mutate(lag = lag(bottom_depth))
          profile <- profile %>%
            mutate(height = bottom_depth - lag)
          
          #replace NA value in height column
          profile[1,6] = profile$bottom_depth[1]
          
          #calculate npp in mol m-2 yr-1
          profile <- profile %>%
            mutate(npp_new = npp*height)
          
          #store interpolated POC flux into the output matrix
          output[i, j] <- sum(profile$npp_new, na.rm = TRUE)
          #land values - if a value doesn't exist for npp test, then don't interpolate, just put an NA value in output matrix  
        } else {
          output[i,j] <- NA
        }
      }
    }
    
    #store each year of output into a list
    list_npp[[k]] <- output
  }
  
  #converts from list to matrices
  npp_lt <- do.call(cbind, list_npp)
  #combines matrices into a single array
  npp_lt <- array(npp_lt, dim=c(dim(list_npp[[1]]), length(list_npp)))
  
  #20 year mean for the end of the 21st century
  mean_npp_lt <- apply(npp_lt, c(1, 2), mean, na.rm = FALSE)
  
  #convert to rasters (formatted for plotting)
  dimnames(mean_npp_lt) = list(lon = 1:360,lat = 1:180)
  old.df = melt(mean_npp_lt)
  
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
  
  writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/NPP/", model.name,"_NPP_lt_rg.asc"), varname = "pp", overwrite = TRUE)
  
  #save EC-Earth matrix and array since it only has future npp data 
  if(model.name == "EC-Earth") {
    
    print("all done!")
    
  } else {
    
    ## Calculate historical average (1850-1900) euphotic zone depth ------------
    
    if(model.name == "UKESM" || model.name == "CMCC") {
      #monthly version
      v = seq(from = 1, to = 589, by = 12)
    }  else {
      #yearly version
      v = 1:50
    }
    
    #create list to store for loop ez depth arrays in
    list_npp <- list()
    
    #storage container for one percent npp data
    output <- matrix(nrow = 360, ncol = 180)  
    
    for(k in 1:length(v)) {
      #read in a year of data
      t <- v[k]
      
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
          #true/false test (pulls out npp at second depth value)
          ret$test <- extract(pp[, , 2], indices = c(i,j), dims = c(1,2))
          #z (number of depth grid cells)
          ret$z <- length(ret$depth)
          
          #ocean values - if a value exists for npp, then find column integrated npp (mol m-2 d-1) and store in output matrix
          if (is.na(ret$test) == FALSE) {
            
            #create data frame
            #NOTE: need to change [1:x] to match the number of depth cells
            profile <- data.frame(ret$depth, ret$npp[1:ret$z]) %>%
              as_tibble() 
            #rename depth column
            profile <- rename(profile, depth = ret.depth)
            #also change this to match number of depth cells
            profile <- rename(profile, npp = ret.npp.1.ret.z.)
            
            #add calculated column height
            profile <- profile %>% 
              mutate(lead = lead(depth))
            profile <- profile %>%
              mutate(bottom_depth = rowMeans(profile[c('lead','depth')]))
            profile <- profile %>%
              mutate(lag = lag(bottom_depth))
            profile <- profile %>%
              mutate(height = bottom_depth - lag)
            
            #replace NA value in height column
            profile[1,6] = profile$bottom_depth[1]
            
            #calculate npp in mol m-2 yr-1
            profile <- profile %>%
              mutate(npp_new = npp*height)
            
            #store interpolated POC flux into the output matrix
            output[i, j] <- sum(profile$npp_new, na.rm = TRUE)
            #land values - if a value doesn't exist for npp test, then don't interpolate, just put an NA value in output matrix  
          } else {
            output[i,j] <- NA
          }
        }
      }
      
      #store each year of output into a list
      list_npp[[k]] <- output
    }
    
    #converts from list to matrices
    npp_his <- do.call(cbind, list_npp)
    #combines matrices into a single array
    npp_his <- array(npp_his, dim=c(dim(list_npp[[1]]), length(list_npp)))
    
    #20 year mean for the end of the 21st century
    mean_npp_his <- apply(npp_his, c(1, 2), mean, na.rm = FALSE)
    
    npp_change = mean_npp_lt - mean_npp_his
    
    #save each matrix as an ascii file
    matrices = list(mean_npp_his, npp_change)
    
    names = c("NPP_his", "NPP_change")
    
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
      
      writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/NPP/", model.name,"_",names[i],"_rg.asc"), varname = "pp", overwrite = TRUE)
      
    }
  }
}

