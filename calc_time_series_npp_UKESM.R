#' @title Calculate time series of globally integrated NPP change (1850-2100) for UKESM
#' @author Stevie Walker
#' @date 3/7/22
#' @inputs npp nc file, areacello nc file
#' @output csv file of year and global NPP for each year


time_series_npp_UKESM <- function(UKESM.file) {
  
  #read in files
  setwd(paste0("~/senior_thesis/combined_UKESM_files"))
  nc.pp <- list.files(pattern = "pp")
  
  if(UKESM.file == 5) {
    nc_his = NA
  } else {
    nc_his <- nc_open(nc.pp[UKESM.file])
  }
  
  if(UKESM.file == 5) {
    nc_fut <- nc_open(nc.pp[UKESM.file])
  } else {
    nc_fut <- NA
  }
  
  #read in ocean cell area data
  area.name <- list.files(paste0("~/senior_thesis/combined_UKESM_files/"), pattern = "areacello")
  nc_data_area <- nc_open(area.name)
  area <- ncvar_get(nc_data_area, "areacello")
  
  ## future calculation --------------------------------------------
  
  #don't need to rerun future for these 
  if(UKESM.file != 5) {
    print("skip future") 
  } else {
    
    #for monthly data
    v <- seq(from = 1, to = 1021, by = 12)
    
    #make first column of the vector (to be combined later)
    year <- seq(from = 2015, to = 2100, by = 1)
    
    #final vector output 
    npp_fut = vector(mode = "numeric", length = length(v))
    
    #storage container for second for loop
    output_fut <- matrix(nrow = 360, ncol = 330)
    
    for(k in 1:length(v)) {
      #read in a year of data
      t <- v[k]
      
      print(paste0("future ", k))
      
      npp <- ncvar_get(nc_fut, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
      npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
      
      #calculates column integrated npp for one year
      for(i in 1:360) {
        for(j in 1:330) {
          
          #make list and add needed columns
          ret <- list()
          #depth
          ret$depth <-  ncvar_get(nc_fut, "lev")
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
      global_flux <- output_fut*area
      
      #sum of all model cells
      sum_flux <- sum(global_flux, na.rm = TRUE)
      
      #Total global NPP in Pt C / yr for one year
      sum_flux <- sum_flux*12.01/1000000000000000
      
      #assign globally integrated POC flux value from t year to the output vector
      npp_fut[k] = sum_flux
    }
    
    df.fut <- qpcR:::cbind.na(year,npp_fut) %>%
      as_tibble()
    colnames(df.fut) = c('Year','UKESM')
    
    write_csv(df.fut, paste0("~/senior_thesis/plotting_dataframes/time_series/NPP/UKESM_uncombined/UKESM_",UKESM.file,"_time_series_npp.csv"))
    
  }
  
  ## historical calculation -------------------------------------------
  
  #make first column of the vector (to be combined later)
  
  if(UKESM.file == 1) {
    year <- 1850:1899
  } else if(UKESM.file == 2) {
    year <- 1900:1949
  } else if(UKESM.file == 3) {
    year <- 1950:1999
  } else {
    year <- 2000:2014
  }
  
  
  if(UKESM.file == 4) {
    v <- seq(from = 1, to = 169, by = 12)
  } else {
    v <- seq(from = 1, to = 589, by = 12)
  }
  
  #final vector output 
  npp_his = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop
  output_his <- matrix(nrow = 360, ncol = 330)
  
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    
    print(paste0("historical ", k))
    
    #pulls out array for one year, 3D with lat,lon,depth
    #monthly
    npp <- ncvar_get(nc_his, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
    npp <- apply(npp, c(1,2,3),mean,na.rm=FALSE)
    
    #calculates column integrated npp for one year
    for(i in 1:360) {
      for(j in 1:330) {
        
        #make list and add needed columns
        ret <- list()
        #depth
        ret$depth <-  ncvar_get(nc_his, "lev")
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
    global_flux <- output_his*area
    
    #sum of all model cells
    sum_flux <- sum(global_flux, na.rm = TRUE)
    
    #Total global NPP in Pt C / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    #assign globally integrated POC flux value from t year to the output vector
    npp_his[k] = sum_flux
  }
  
  #binds npp_flux vector to year vector
  df.his <- qpcR:::cbind.na(year,npp_his) %>%
    as_tibble()
  colnames(df.his) = c('Year','UKESM')
  
  write.csv(df.his,paste0("~/senior_thesis/plotting_dataframes/time_series/NPP/UKESM_uncombined/UKESM_",UKESM.file,"_time_series_npp.csv"))
  
}
