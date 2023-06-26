#' @title Calculate time series of regionally integrated POC flux change at the euphotic zone depth (1850-2100)
#' @author Stevie Walker
#' @date 4/12/23
#' @inputs expc nc file, areacello nc file, ez depth time series arrays (historical and future for all inputs except EC-Earth)
#' @output csv file of year and global POC flux for each year
#' @note UKESM expc ez depth function is located in calc_time_series_expc.R

regional_ts_expc_ez <- function(region,model.name) {
  
  ## Calculate globally integrated POC flux at ez depth
  
  if(model.name == "EC-Earth") {
    print("skip historical calculation")
    
    setwd("~/spatial_analysis/regridded_nc_files/EC-Earth_rg/")
    nc.expc <- list.files(pattern = "expc")
    
    #read in POC flux data, ez depth arrays calculated in calc_time_series_ez_depth.R
    setwd("~/spatial_analysis/regridded_nc_files/EC-Earth_rg/")
    nc_data_2015 <- nc_open(nc.expc[2])
    ez_depth_fut <- readRDS(paste0("~/regional_time_series_analysis/files/rg_EZ_depth/rg_",model.name,"_ez_depth_fut_time_series.Rds"))
    
  } else {
    
    setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
    nc.expc <- list.files(pattern = "expc")
    
    #read in POC flux data, ez depth arrays calculated in calc_time_series_ez_depth.R
    nc_data_1850 <- nc_open(nc.expc[1])
    if(model.name == "UKESM") {
      nc_data_2015 <- nc_open(nc.expc[6])
    } else {
      nc_data_2015 <- nc_open(nc.expc[2])
    }
    
    if(model.name == "UKESM") {
      ez_depth_his <- readRDS(paste0("~/regional_time_series_analysis/files/rg_EZ_depth/rg_",model.name,"_1_ez_depth_his_time_series.Rds"))
    } else {
      ez_depth_his <- readRDS(paste0("~/regional_time_series_analysis/files/rg_EZ_depth/rg_",model.name,"_ez_depth_his_time_series.Rds"))
    }
    ez_depth_fut <- readRDS(paste0("~/regional_time_series_analysis/files/rg_EZ_depth/rg_",model.name,"_ez_depth_fut_time_series.Rds"))
    
    #subset historical data frame
    #Southern Ocean S of 50
    if(region == "SO_50") {
      ez_depth_his_subset = ez_depth_his[,1:40,]
      #Southern Ocean S of 60
    } else if(region == "SO_60"){
      ez_depth_his_subset = ez_depth_his[,1:30,]
      #low latitudes
    } else if(region == "30_low_lats"){
      ez_depth_his_subset = ez_depth_his[,61:120,]
      #Equatorial Pacific
    } else if(region == "EQ_Pacific") {
      ez_depth_his_subset = ez_depth_his[160:285,76:106,] #15S to 15N, 160E to 75W 
      #low latitudes without Equatorial Pacific
    } else if (region == "low_lats_no_EQ_Pacific") {
      ez_depth_his_subset = ez_depth_his[,61:120,] #30S to 30N
      #North Atlantic - NOTE: in calc_region_area this is called North_Atlantic_no_Arctic
    } else if (region == "North_Atlantic") {
      ez_depth_his_subset = ez_depth_his[290:360,141:166,] #40N to 65N, 70W to 0W
    } else {
      #Low Latitudes
      ez_depth_his_subset = ez_depth_his[,76:105,] #15S-15N
    }
    
  }
  
  #subset future ez depths
  #Southern Ocean S of 50
  if(region == "SO_50") {
    ez_depth_fut_subset = ez_depth_fut[,1:40,]
    #Southern Ocean S of 60
  } else if(region == "SO_60"){
    ez_depth_fut_subset = ez_depth_fut[,1:30,]
    #low latitudes
  } else if(region == "30_low_lats"){
    ez_depth_fut_subset = ez_depth_fut[,61:120,]
    #Equatorial Pacific
  } else if(region == "EQ_Pacific") {
    ez_depth_fut_subset = ez_depth_fut[160:285,76:106,] #15S to 15N, 160E to 75W 
    #low latitudes without Equatorial Pacific
  } else if (region == "low_lats_no_EQ_Pacific") {
    ez_depth_fut_subset = ez_depth_fut[,61:120,] #30S to 30N
    #North Atlantic - NOTE: in calc_region_area this is called North_Atlantic_no_Arctic
  } else if (region == "North_Atlantic") {
    ez_depth_fut_subset = ez_depth_fut[290:360,141:166,] #40N to 65N, 70W to 0W
  } else {
    #Low Latitudes
    ez_depth_fut_subset = ez_depth_fut[,76:105,] #15S-15N
  }
  
  setwd(paste0("~/spatial_analysis/regridded_nc_files/GFDL_rg/"))
  area.name = list.files(pattern = "areacello")
  nc_data_area <- nc_open(area.name)
  area <- ncvar_get(nc_data_area, "areacello")
  
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
  
  dims = dim(ez_depth_fut_subset)
  
  v <- 1:dims[3]
  
  #make first column of the vector (to be combined later)
  year <- seq(from = 2015, to = 2100, by = 1)
  
  #final vector output 
  POC_flux_fut = vector(mode = "numeric", length = length(v))
  POC_flux_fut_area = vector(mode = "numeric", length = length(v))
  #storage container for second loop - empty matrix with dimensions of the subset
  output_fut = area_subset*0
  
  #creates list of 20 arrays with POC flux at every lat,lon,depth
  for(k in 1:length(v)) {
    #read in a year of data
    t <- v[k]
    
    print(paste0("future ",k))
    
    if(model.name == "UKESM") {
      #pulls out monthly array and takes year average
      variable_fut <- ncvar_get(nc_data_2015, "expc", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
      
    } else {
      #pulls out array for one year, 3D with lat,lon,depth
      variable_fut <- ncvar_get(nc_data_2015,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    }     
    #get rid of NA values to fix interpolation issue
    variable_fut[is.na(variable_fut)] <- 0
    
    #subset the region
    #Southern Ocean South of 60S
    if(region == "SO_60") {
      variable_fut = variable_fut[,1:30,]
      #Southern Ocean South of 50S
    } else if (region == "SO_50") {
      variable_fut = variable_fut[,1:40,]
      #low latitudes (30S to 30N)
    } else if (region == "30_low_lats") {
      variable_fut = variable_fut[,61:120,]
      # Equatorial Pacific
    } else if (region == "EQ_Pacific") {
      variable_fut = variable_fut[160:285,76:106,] #15S to 15N, 160E to 75W
      #make values in the Caribbean Sea NA
      variable_fut[118:126,25:31,] = 0
      # Low latitudes without Equatorial Pacific
    } else if (region == "low_lats_no_EQ_Pacific") {
      variable_fut = variable_fut[,61:120,] #30S to 30N
      variable_fut[160:285,15:45,] = 0
      #North Atlantic 
    } else if (region == "North_Atlantic") {
      variable_fut = variable_fut[290:360,141:166,] #40N to 65N, 70W to 0W
    } else {
      #Low Latitudes
      variable_fut = variable_fut[,76:105,] #15S-15N
    }
    
    if(model.name == "UKESM") {
      variable_fut <- apply(variable_fut, c(1,2,3),mean,na.rm=FALSE)
    } else {}
    
    #calculate POC flux at select DH
    for(i in 1:nrow(area_subset)) {
      for(j in 1:ncol(area_subset)) {
        
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
        ret$expc <- extract(variable_fut, indices = c(i,j), dims = c(1,2))
        #subset EZ depth for each lat and lon
        ret$ez <- extract(ez_depth_fut_subset[, , k], indices = c(i,j), dims = c(1,2))
        
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
    global_flux <- output_fut*area_subset
    
    #sum of all model cells
    sum_flux <- sum(global_flux, na.rm = TRUE)
    
    #sum of area subset (m2)
    area_sum <- sum(area_subset, na.rm = TRUE)
    
    #Total regional POC flux in Pt C / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    #total regional POC flux per area (Pt C/m2/yr)
    flux_per_area <- sum_flux/area_sum
    
    #assign regionally integrated POC flux value from t year to the output vectors
    POC_flux_fut[t] = sum_flux
    POC_flux_fut_area[t] = flux_per_area
    
  }
  
  df.fut <- qpcR:::cbind.na(year,POC_flux_fut)
  #binds POC_flux vector to year vector
  df.fut.2 <- qpcR:::cbind.na(year,POC_flux_fut_area)
  
  if(model.name == "EC-Earth") {
    
    #change column names for merging csv files later
    colnames(df.fut) = c('Year',model.name)
    write.csv(df.fut, paste0("~/regional_time_series_analysis/files/POC_EZ/total_flux/",region,"_",model.name,"_time_series_expc_ez.csv"))
    #change column names for merging csv files later
    colnames(df.fut.2) = c('Year',model.name)
    write.csv(df.fut.2,paste0("~/regional_time_series_analysis/files/POC_EZ/per_area_flux/",region,"_",model.name,"_time_series_expc_ez.csv"))
    
  } else {
    
    
    #historical calculation ----------
    
    #make first column of the vector (to be combined later)
    year <- seq(from = 1850, to = 2014, by = 1)
    
    dims = dim(ez_depth_his_subset)
    
    v <- 1:dims[3]
    
    #final vector output 
    POC_flux_his = vector(mode = "numeric", length = length(v))
    POC_flux_his_area = vector(mode = "numeric", length = length(v))
    #storage container for second loop - empty matrix with dimensions of the subset
    output_his = area_subset*0
    
    
    #creates list of 20 arrays with POC flux at every lat,lon,depth
    for(k in 1:length(v)) {
      #read in a year of data
      t <- v[k]
      
      print(paste0("historical ",k))
      
      if(model.name == "UKESM") {
        #pulls out monthly array and takes year average
        variable_his <- ncvar_get(nc_data_1850, "expc", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
        
      } else {
        #pulls out array for one year, 3D with lat,lon,depth
        variable_his <- ncvar_get(nc_data_1850,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
      } 
      
      #get rid of NA values to fix interpolation issue
      variable_his[is.na(variable_his)] <- 0
      
      
      #subset the region
      #Southern Ocean South of 60S
      if(region == "SO_60") {
        variable_his = variable_his[,1:30,]
        #Southern Ocean South of 50S
      } else if (region == "SO_50") {
        variable_his = variable_his[,1:40,]
        #low latitudes (30S to 30N)
      } else if (region == "30_low_lats") {
        variable_his = variable_his[,61:120,]
        # Equatorial Pacific
      } else if (region == "EQ_Pacific") {
        variable_his = variable_his[160:285,76:106,] #15S to 15N, 160E to 75W
        #make values in the Caribbean Sea NA
        variable_his[118:126,25:31,] = 0
        # Low latitudes without Equatorial Pacific
      } else if (region == "low_lats_no_EQ_Pacific") {
        variable_his = variable_his[,61:120,] #30S to 30N
        variable_his[160:285,15:45,] = 0
        #North Atlantic 
      } else if (region == "North_Atlantic") {
        variable_his = variable_his[290:360,141:166,] #40N to 65N, 70W to 0W
      } else {
        #Low Latitudes
        variable_his = variable_his[,76:105,] #15S-15N
      }
      
      #take yearly average
      if(model.name == "UKESM") {
        variable_his <- apply(variable_his, c(1,2,3),mean,na.rm=FALSE)
      } else {}
      
      
      #calculate POC flux at select DH
      for(i in 1:nrow(area_subset)) {
        for(j in 1:ncol(area_subset)) {
          
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
          ret$expc <- extract(variable_his, indices = c(i,j), dims = c(1,2))
          #subset MLD max for each lat and lon
          ret$ez <- extract(ez_depth_his_subset[, , k], indices = c(i,j), dims = c(1,2))
          
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
      global_flux <- output_his*area_subset
      
      #sum of all model cells
      sum_flux <- sum(global_flux, na.rm = TRUE)
      
      #sum of area subset (m2)
      area_sum <- sum(area_subset, na.rm = TRUE)
      
      #Total regional POC flux in Pt C / yr for one year
      sum_flux <- sum_flux*12.01/1000000000000000
      
      #total regional POC flux per area (Pt C/m2/yr)
      flux_per_area <- sum_flux/area_sum
      
      #assign regionally integrated POC flux value from t year to the output vectors
      POC_flux_his[t] = sum_flux
      POC_flux_his_area[t] = flux_per_area
      
    }
    
    #binds POC_flux vector to year vector
    df.his <- qpcR:::cbind.na(year,POC_flux_his)
    time.series <- rbind(df.his,df.fut)
    df = data.frame(time.series)
    #change column names for merging csv files later
    colnames(df) = c('Year',model.name)
    
    
    #binds POC_flux vector to year vector
    df.his.2 <- qpcR:::cbind.na(year,POC_flux_his_area)
    time.series.2 <- rbind(df.his.2,df.fut.2)
    df2 = data.frame(time.series.2)
    #change column names for merging csv files later
    colnames(df2) = c('Year',model.name)
    
    write.csv(df,paste0("~/regional_time_series_analysis/files/POC_EZ/total_flux/",region,"_",model.name,"_time_series_expc_ez.csv"))
    write.csv(df2,paste0("~/regional_time_series_analysis/files/POC_EZ/per_area_flux/",region,"_",model.name,"_time_series_expc_ez_per_area.csv"))
    
  }
  
  
}

#rename files with a pattern

#setwd("~/regional_time_series_analysis/files/POC_EZ/total_flux/")
#from_files <- list.files(pattern = "_per_area.csv")

#new.names <- from_files
#new.names <- gsub("_per_area.csv", ".csv", new.names)
#file.rename(from_files, new.names)
