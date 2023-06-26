#' @title Calculate time series of regionally integrated NPP change (1850-2100)
#' @author Stevie Walker
#' @date 3/7/22
#' @inputs npp nc file, areacello nc file
#' @output csv file of year and global NPP for each year

regional_ts_npp <- function(model.name, region) {
  
  #read in files
  setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
  nc.pp <- list.files(pattern = "pp")
  
  if(model.name == "EC-Earth") {
    nc_his = NA
  } else {
    nc_his <- nc_open(nc.pp[1])
  }  
  
  if(model.name == "EC-Earth") {
    nc_fut <- nc_open(nc.pp)
  } else {
  nc_fut <- nc_open(nc.pp[2])
  }
  #read in ocean cell area data
  setwd("~/spatial_analysis/regridded_nc_files/GFDL_rg/")
  area.name <- list.files(pattern = "areacello")
  nc_data_area <- nc_open(area.name)
  area <- ncvar_get(nc_data_area, "areacello")
  
  #subset area to the range
  
  #Southern Ocean S of 50
  if(region == "SO_50") {
    area_subset = area[,1:40]
    #Southern Ocean S of 60
  } else if(region == "SO_60"){
    area_subset = area[,1:30]
    #low latitudes
  } else if(region == "30_low_lats"){
    area_subset = area[,61:120]
    #Equatorial Pacific
  } else if(region == "EQ_Pacific") {
    area_subset = area[160:285,76:106] #15S to 15N, 160E to 75W 
    #make values in the Caribbean Sea NA
    area_subset[118:126,25:31] = 0
    #low latitudes without Equatorial Pacific
  } else if (region == "low_lats_no_EQ_Pacific") {
    area_subset = area[,61:120] #30S to 30N
    area_subset[160:285,15:45] = 0
    #North Atlantic - NOTE: in calc_region_area this is called North_Atlantic_no_Arctic
  } else if (region == "North_Atlantic") {
    area_subset = area[290:360,141:166] #40N to 65N, 70W to 0W
  } else {
    #Low Latitudes
    area_subset = area[,76:105] #15S-15N
  }
  
  setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
  ## future calculation --------------------------------------------
  
  if(model.name == "CMCC") {
    #for monthly data
    v <- seq(from = 1, to = 1021, by = 12)
  } else {
    v <- 1:86
  }
  
  #make first column of the vector (to be combined later)
  year <- seq(from = 2015, to = 2100, by = 1)
  
  #final vector output 
  npp_fut = vector(mode = "numeric", length = length(v))
  npp_fut_per_area = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop
  output_fut = area_subset*0
  
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    
    print(paste0("future ", k))
    
    if(model.name == "CMCC") {
      #monthly
      npp <- ncvar_get(nc_fut, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
      npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
    } else {
      #yearly
      npp <- ncvar_get(nc_fut, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    }
    
    #subset the region
    #Southern Ocean South of 60S
    if(region == "SO_60") {
      npp = npp[,1:30,]
      #Southern Ocean South of 50S
    } else if (region == "SO_50") {
      npp = npp[,1:40,]
      #low latitudes (30S to 30N)
    } else if (region == "30_low_lats") {
      npp = npp[,61:120,]
      # Equatorial Pacific
    } else if (region == "EQ_Pacific") {
      npp = npp[160:285,76:106,] #15S to 15N, 160E to 75W
      #make values in the Caribbean Sea NA
      npp[118:126,25:31,] = 0
      # Low latitudes without Equatorial Pacific
    } else if (region == "low_lats_no_EQ_Pacific") {
      npp = npp[,61:120,] #30S to 30N
      npp[160:285,15:45,] = 0
      #North Atlantic 
    } else if (region == "North_Atlantic") {
      npp = npp[290:360,141:166,] #40N to 65N, 70W to 0W
    } else {
      #Low Latitudes
      npp = npp[,76:105,] #15S-15N
    }
    
    #calculate NPP
    for(i in 1:nrow(area_subset)) {
      for(j in 1:ncol(area_subset)) {
        
        #make list and add needed columns
        ret <- list()
        #depth
        if(model.name == "CESM") {
          ret$depth <-  ncvar_get(nc_fut, "lev") /100
        } else if (model.name == "IPSL") {
          ret$depth <-  ncvar_get(nc_fut, "olevel")
        } else {
          ret$depth <-  ncvar_get(nc_fut, "lev")
        }
        #subset npp for select lat and lon
        ret$npp <- extract(npp, indices = c(i,j), dims = c(1,2))
        #true/false test (pulls out first )
        ret$test <- extract(npp[, , 1], indices = c(i,j), dims = c(1,2))
        
        z = length(ret$depth)
        
        #ocean values - if a value exists for npp, then find column integrated npp (mol m-2 d-1) and store in output matrix
        if (is.na(ret$test) == FALSE) {
          
          #create data frame
          #NOTE: need to change [1:x] to match the number of depth cells
          profile <- data.frame(ret$depth, ret$npp[1:z]) %>%
            as_tibble() 
          #rename depth column
          profile <- dplyr::rename(profile, depth = ret.depth)
          #also change this to match number of depth cells
          profile <- dplyr::rename(profile, npp = ret.npp.1.z.)
          
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
          output_fut[i,j] <- sum(profile$npp_new, na.rm = TRUE)
          #land values - if a value doesn't exist for MLDmax, then don't interpolate, just put an NA value in output matrix  
        } else {
          output_fut[i,j] <- NA
        }
      }
    }
    #multiply by cell area
    global_flux <- output_fut*area_subset
    
    #sum of all model cells
    sum_flux <- sum(global_flux, na.rm = TRUE)
    
    #sum of area subset (m2)
    area_sum <- sum(area_subset, na.rm = TRUE)
    
    #Total regional NPP in Pt / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    #total regional NPP per area (Pt/m2/yr)
    flux_per_area <- sum_flux/area_sum
    
    #assign regionally integrated POC flux value from t year to the output vectors
    npp_fut[k] = sum_flux
    npp_fut_per_area[k] = flux_per_area
  }
  
  df.fut <- qpcR:::cbind.na(year,npp_fut) %>%
    as_tibble()
  colnames(df.fut) = c('Year',model.name)
  
  df.fut.2 <- qpcR:::cbind.na(year,npp_fut_per_area) %>%
    as_tibble()
  colnames(df.fut.2) = c('Year',model.name)
  
  if(model.name == "EC-Earth") {
    write_csv(df.fut, paste0("~/regional_time_series_analysis/files/NPP/total_npp/",region,"_EC-Earth_time_series_npp.csv"))
    write_csv(df.fut.2, paste0("~/regional_time_series_analysis/files/NPP/per_area_npp/",region,"_EC-Earth_time_series_npp_per_area.csv"))
  } else if(model.name == "CMCC") {
    write_csv(df.fut, paste0("~/regional_time_series_analysis/files/NPP/total_npp/",region,"_CMCC_fut_time_series_npp.csv"))
    write_csv(df.fut.2, paste0("~/regional_time_series_analysis/files/NPP/per_area_npp/",region,"_CMCC_fut_time_series_npp_per_area.csv"))
  } else {
    print("save later")
  }
  
  ## historical calculation -------------------------------------------
  
  if(model.name == "EC-Earth") {
    print("done")
  } else {
    
    #make first column of the vector (to be combined later)
    
    year <- 1850:2014
    
    if(model.name == "CMCC") {
      #for monthly data
      v <- seq(from = 1, to = 1969, by = 12)
    } else {
      v <- 1:165
    }
    
    #final vector output 
    npp_his = vector(mode = "numeric", length = length(v))
    npp_his_per_area = vector(mode = "numeric", length = length(v))
    
    #storage container for second for loop
    output_his = area_subset*0
    
    for(k in 1:length(v)) {
      #read in a year of data
      t <- v[k]
      
      print(paste0("historical ", k))
      
      #pulls out array for one year, 3D with lat,lon,depth
      if(model.name == "CMCC") {
        #monthly
        npp <- ncvar_get(nc_his, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
        npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
      } else {
        #yearly
        npp <- ncvar_get(nc_his, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
      }
      
      #subset the region
      #Southern Ocean South of 60S
      if(region == "SO_60") {
        npp = npp[,1:30,]
        #Southern Ocean South of 50S
      } else if (region == "SO_50") {
        npp = npp[,1:40,]
        #low latitudes (30S to 30N)
      } else if (region == "30_low_lats") {
        npp = npp[,61:120,]
        # Equatorial Pacific
      } else if (region == "EQ_Pacific") {
        npp = npp[160:285,76:106,] #15S to 15N, 160E to 75W
        #make values in the Caribbean Sea NA
        npp[118:126,25:31,] = 0
        # Low latitudes without Equatorial Pacific
      } else if (region == "low_lats_no_EQ_Pacific") {
        npp = npp[,61:120,] #30S to 30N
        npp[160:285,15:45,] = 0
        #North Atlantic 
      } else if (region == "North_Atlantic") {
        npp = npp[290:360,141:166,] #40N to 65N, 70W to 0W
      } else {
        #Low Latitudes
        npp = npp[,76:105,] #15S-15N
      }
      
      #calculate column integrated NPP
      for(i in 1:nrow(area_subset)) {
        for(j in 1:ncol(area_subset)) {
          
          #make list and add needed columns
          ret <- list()
          if(model.name == "CESM") {
            ret$depth <-  ncvar_get(nc_his, "lev") /100
          } else if (model.name == "IPSL") {
            ret$depth <-  ncvar_get(nc_his, "olevel")
          } else {
            ret$depth <-  ncvar_get(nc_his, "lev")
          }
          #subset npp for select lat and lon
          ret$npp <- extract(npp, indices = c(i,j), dims = c(1,2))
          #true/false test (pulls out first )
          ret$test <- extract(npp[, , 1], indices = c(i,j), dims = c(1,2))
          
          #ocean values - if a value exists for npp, then find column integrated npp (mol m-2 d-1) and store in output matrix
          if (is.na(ret$test) == FALSE) {
            
            z = length(ret$depth)
            
            #create data frame
            #NOTE: need to change [1:x] to match the number of depth cells
            profile <- data.frame(ret$depth, ret$npp[1:z]) %>%
              as_tibble() 
            #rename depth column
            profile <- dplyr::rename(profile, depth = ret.depth)
            #also change this to match number of depth cells
            profile <- dplyr::rename(profile, npp = ret.npp.1.z.)
            
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
            output_his[i, j] <- sum(profile$npp_new, na.rm = TRUE)
            #land values - if a value doesn't exist, then don't interpolate, just put an NA value in output matrix  
          } else {
            output_his[i,j] <- NA
          }
        }
      }
      
      #multiply by cell area
      global_flux <- output_his*area_subset
      
      #sum of all model cells
      sum_flux <- sum(global_flux, na.rm = TRUE)
      
      #sum of area subset (m2)
      area_sum <- sum(area_subset, na.rm = TRUE)
      
      #Total regional NPP in Pt / yr for one year
      sum_flux <- sum_flux*12.01/1000000000000000
      
      #total regional NPP per area (Pt/m2/yr)
      flux_per_area <- sum_flux/area_sum
      
      #assign regionally integrated POC flux value from t year to the output vectors
      npp_his[k] = sum_flux
      npp_his_per_area[k] = flux_per_area
    
     }
    
    #binds npp vector to year vector
    df.his <- qpcR:::cbind.na(year,npp_his) %>%
      as_tibble()
    colnames(df.his) = c('Year',model.name)
    
    #binds npp vector to year vector
    df.his.2 <- qpcR:::cbind.na(year,npp_his_per_area) %>%
      as_tibble()
    colnames(df.his.2) = c('Year',model.name)
    
    time.series <- rbind(df.his,df.fut)
    df = data.frame(time.series) # this name will get replaced to time.series
    write.csv(df,paste0("~/regional_time_series_analysis/files/NPP/total_npp/",region,"_",model.name,"_time_series_npp.csv"))
    
    time.series.2 <- rbind(df.his.2,df.fut.2)
    df2 = data.frame(time.series.2) # this name will get replaced to time.series
    write.csv(df2,paste0("~/regional_time_series_analysis/files/NPP/per_area_npp/",region,"_",model.name,"_time_series_npp_per_area.csv"))
    
  }
  }


