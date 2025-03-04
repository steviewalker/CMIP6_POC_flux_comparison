#' @title Calculate time series of globally integrated POC flux change at the euphotic zone depth (1850-2100)
#' @author Stevie Walker
#' @date 4/4/22
#' @inputs expc nc file, areacello nc file, ez depth time series arrays (historical and future for all inputs except EC-Earth)
#' @output csv file of year and global POC flux for each year
#' @note UKESM expc ez depth function is located in calc_time_series_expc.R

time_series_expc_ez <- function(model.name, lon.length, lat.length, ez.metric) {
  
  ## Calculate globally integrated POC flux at ez depth
  
  if(model.name == "EC-Earth") {
    
    nc.expc <- list.files("~/senior_thesis/combined_EC-Earth_files/", pattern = "expc")
    area <- list.files("~/senior_thesis/combined_EC-Earth_files/", pattern = "areacello")
    #open nc file
    setwd("~/senior_thesis/combined_EC-Earth_files/")
    
    #read in POC flux data, ez depth arrays calculated in calc_time_series_ez_depth.R
    nc_data_2015 <- nc_open(nc.expc[2])
    if (ez.metric == 1) {
      ez_depth_fut <- readRDS(paste0("~/time_series_analysis/files/EZ_depth/",model.name,"_ez_depth_fut_time_series.Rds"))
    } else {
      #read in 10% NPP max file
      ez_depth_fut <- readRDS(paste0("~/time_series_analysis/files/EZ_depth/ten_percent/",model.name,"_ez_depth_10_fut_time_series.Rds"))
    }
    #read in ocean cell area data
    nc_data_area <- nc_open(area[2])
    area <- ncvar_get(nc_data_area, "areacello")
    
    dims = dim(ez_depth_fut)
    
    v <- 1:dims[3]
    
    #make first column of the vector (to be combined later)
    year <- seq(from = 2015, to = 2100, by = 1)
    
    #final vector output 
    POC_flux_fut = vector(mode = "numeric", length = length(v))
    
    #storage container for second for loop
    output_fut <- matrix(nrow = length(lon.length), ncol = length(lat.length))
    
    #creates list of 20 arrays with POC flux at every lat,lon,depth
    for(k in 1:length(v)) {
      #read in a year of data
      t <- v[k]
      #pulls out array for one year, 3D with lat,lon,depth
      expc_fut <- ncvar_get(nc_data_2015,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
      
      #calculates POC flux at MLD max for every grid cell for one year
      for(i in 1:length(lon.length)) {
        for(j in 1:length(lat.length)) {
          
          #make list and add needed columns
          ret <- list()
          #NOTE - make sure you divide by 100 here if the depth units are in cm (CESM), otherwise the depth units are in m
          ret$depth <-  ncvar_get(nc_data_2015, "lev")
          #subset expc for select lat and lon
          ret$expc <- extract(expc_fut, indices = c(i,j), dims = c(1,2))
          #subset ez depth for each lat and lon
          ret$ez <- extract(ez_depth_fut[, , k], indices = c(i,j), dims = c(1,2))
          
          #ocean values - if a value exists for ez depth, then interpolate and store new interpolated POC flux in output matrix
          if (is.na(ret$ez) == FALSE) {
            
            #find interpolated expc at mld max
            interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$ez)
            #store interpolated POC flux into the output matrix
            output_fut[i, j] <- interp$y[1]
            #land values - if a value doesn't exist for ez depth, then don't interpolate, just put an NA value in output matrix  
          } else {
            output_fut[i,j] <- NA
          }
        }
      }
      
      #multiply by cell area
      global_flux <- output_fut*area
      
      #sum of all model cells
      sum_flux <- sum(global_flux, na.rm = TRUE)
      
      #Total global POC flux in Pt C / yr for one year
      sum_flux <- sum_flux*12.01/1000000000000000
      
      #assign globally integrated POC flux value from t year to the output vector
      POC_flux_fut[k] = sum_flux
      
    }
    
    df.fut <- qpcR:::cbind.na(year,POC_flux_fut)
    df = data.frame(df.fut) # this name will get replaced to time.series
    #change column names for merging csv files later
    colnames(df) = c('Year',model.name)
    if (ez.metric == 1) {
      write.csv(df,paste0("~/time_series_analysis/files/POC_EZ/",model.name,"_time_series_expc_ez.csv"))
    } else {
      write.csv(df,paste0("~/time_series_analysis/files/POC_EZ/",model.name,"_time_series_expc_ez_10.csv"))
    }
    
    #for the rest of the models
  } else {
    
    ## Calculate globally integrated POC flux at ez depth -----------
    
    
    #read in POC flux data
    setwd(paste0("~/senior_thesis/combined_",model.name,"_files/"))
    nc.expc <- list.files(pattern = "expc")
    nc_data_1850 <- nc_open(nc.expc[1])
    if(model.name == "UKESM") {
      nc_data_2015 <- nc_open(nc.expc[7])
    } else {
      nc_data_2015 <- nc_open(nc.expc[2])
    }
    
    #read in ocean cell area data
    area.name <- list.files(pattern = "areacello")
    nc_data_area <- nc_open(area.name)
    area <- ncvar_get(nc_data_area, "areacello")
    
    #read in ez depth arrays calculated in calc_time_series_ez_depth.R
    if(ez.metric == 1) {
      ez_depth_his <- readRDS(paste0("~/time_series_analysis/files/EZ_depth/",model.name,"_ez_depth_his_time_series.Rds"))
      ez_depth_fut <- readRDS(paste0("~/time_series_analysis/files/EZ_depth/",model.name,"_ez_depth_fut_time_series.Rds"))
    } else {
      ez_depth_his <- readRDS(paste0("~/time_series_analysis/files/EZ_depth/ten_percent/",model.name,"_ez_depth_10_his_time_series.Rds"))
      ez_depth_fut <- readRDS(paste0("~/time_series_analysis/files/EZ_depth/ten_percent/",model.name,"_ez_depth_10_fut_time_series.Rds"))
    }
    
    dims = dim(ez_depth_fut)
    
    v <- 1:dims[3]
    
    #make first column of the vector (to be combined later)
    year <- seq(from = 2015, to = 2100, by = 1)
    
    #final vector output 
    POC_flux_fut = vector(mode = "numeric", length = length(v))
    
    #storage container for second for loop
    output_fut <- matrix(nrow = length(lon.length), ncol = length(lat.length))
    
    #creates list of 20 arrays with POC flux at every lat,lon,depth
    for(k in 1:length(v)) {
      #read in a year of data
      t <- v[k]
      
      print(paste0("future ",k))
      
      if(model.name == "UKESM") {
        #pulls out monthly array and takes year average
        expc_fut <- ncvar_get(nc_data_2015, "expc", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
        expc_fut <- apply(expc, c(1,2,3),mean,na.rm=FALSE)
        
      } else {
        #pulls out array for one year, 3D with lat,lon,depth
        expc_fut <- ncvar_get(nc_data_2015,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
      }      
      #calculates POC flux at MLD max for every grid cell for one year
      for(i in 1:length(lon.length)) {
        for(j in 1:length(lat.length)) {
          
          #make list and add needed columns
          ret <- list()
          #depth
          if(model.name == "CESM") {
            ret$depth <-  ncvar_get(nc_data_2015, "lev") /100
          } else if (model.name == "IPSL") {
            ret$depth <-  ncvar_get(nc_data_2015, "olevel")
          } else {
            ret$depth <-  ncvar_get(nc_data_2015, "lev")
          }          
          #subset expc for select lat and lon
          ret$expc <- extract(expc_fut, indices = c(i,j), dims = c(1,2))
          #subset EZ depth for each lat and lon
          ret$ez <- extract(ez_depth_fut[, , k], indices = c(i,j), dims = c(1,2))
          
          #ocean values - if a value exists for ez depth, then interpolate and store new interpolated POC flux in output matrix
          if (is.na(ret$ez) == FALSE) {
            
            #find interpolated expc at mld max
            interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$ez)
            #store interpolated POC flux into the output matrix
            output_fut[i, j] <- interp$y[1]
            #land values - if a value doesn't exist for ez depth, then don't interpolate, just put an NA value in output matrix  
          } else {
            output_fut[i,j] <- NA
          }
        }
      }
      
      #multiply by cell area
      global_flux <- output_fut*area
      
      #sum of all model cells
      sum_flux <- sum(global_flux, na.rm = TRUE)
      
      #Total global POC flux in Pt C / yr for one year
      sum_flux <- sum_flux*12.01/1000000000000000
      
      #assign globally integrated POC flux value from t year to the output vector
      POC_flux_fut[k] = sum_flux
      
    }
    
    df.fut <- qpcR:::cbind.na(year,POC_flux_fut)
    
    #historical calculation ----------
    
    #make first column of the vector (to be combined later)
    year <- seq(from = 1850, to = 2014, by = 1)
    
    dims = dim(ez_depth_his)
    
    #length of expc years
    v <- 1:dims[3]
    
    #final vector output 
    POC_flux_his = vector(mode = "numeric", length = length(v))
    
    #storage container for second for loop
    output_his <- matrix(nrow = length(lon.length), ncol = length(lat.length))
    
    #creates list of 20 arrays with POC flux at every lat,lon,depth
    for(k in 1:length(v)) {
      #read in a year of data
      t <- v[k]
      
      print(paste0("historical ",k))
      
      if(model.name == "UKESM") {
        #pulls out monthly array and takes year average
        expc_his <- ncvar_get(nc_data_1850, "expc", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
        expc_his <- apply(nc_data_1850, c(1,2,3),mean,na.rm=FALSE)
        
      } else {
        #pulls out array for one year, 3D with lat,lon,depth
        expc_his <- ncvar_get(nc_data_1850,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
      }      
      #calculates POC flux at MLD max for every grid cell for one year
      for(i in 1:length(lon.length)) {
        for(j in 1:length(lat.length)) {
          
          #make list and add needed columns
          ret <- list()
          if(model.name == "CESM") {
            ret$depth <-  ncvar_get(nc_data_1850, "lev") /100
          } else if (model.name == "IPSL") {
            ret$depth <-  ncvar_get(nc_data_1850, "olevel")
          } else {
            ret$depth <-  ncvar_get(nc_data_1850, "lev")
          }  
          #subset expc for select lat and lon
          ret$expc <- extract(expc_his, indices = c(i,j), dims = c(1,2))
          #subset MLD max for each lat and lon
          ret$ez <- extract(ez_depth_his[, , k], indices = c(i,j), dims = c(1,2))
          
          #ocean values - if a value exists for ez depth, then interpolate and store new interpolated POC flux in output matrix
          if (is.na(ret$ez) == FALSE) {
            
            #find interpolated expc at mld max
            interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$ez)
            #store interpolated POC flux into the output matrix
            output_his[i, j] <- interp$y[1]
            #land values - if a value doesn't exist for ez depth, then don't interpolate, just put an NA value in output matrix  
          } else {
            output_his[i,j] <- NA
          }
        }
      }
      
      #multiply by cell area
      global_flux <- output_his*area
      
      #sum of all model cells
      sum_flux <- sum(global_flux, na.rm = TRUE)
      
      #Total global POC flux in Pt C / yr for one year
      sum_flux <- sum_flux*12.01/1000000000000000
      
      #assign globally integrated POC flux value from t year to the output vector
      POC_flux_his[k] = sum_flux
      
    }
    
    #combine year and POC flux vector
    df.his <- qpcR:::cbind.na(year,POC_flux_his)
    
    time.series <- rbind(df.his,df.fut)
    df = data.frame(time.series)
    #change column names for merging csv files later
    colnames(df) = c('Year',model.name)
    if (ez.metric == 1) {
      write.csv(df,paste0("~/time_series_analysis/files/POC_EZ/",model.name,"_time_series_expc_ez.csv"))
    } else {
      write.csv(df,paste0("~/time_series_analysis/files/POC_EZ/",model.name,"_time_series_expc_ez_10.csv"))
    }
  }
  
}