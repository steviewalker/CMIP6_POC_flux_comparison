#' @title Calculate regionally integrated POC flux time series
#' @author Stevie Walker
#' @date 2/20/23
#' @description use for POC flux at MLDmax, PCD, and 1000m

regional_ts_expc <- function(model.name, region, DH) {
  
  ## Read in nc files --------------
  
  nc.expc <- list.files(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"), pattern = "expc")
  
  #read in POC flux data, ez depth arrays calculated in calc_time_series_ez_depth.R
  setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
  nc_data_1850 <- nc_open(nc.expc[1])
  nc_data_2015 <- nc_open(nc.expc[2])
  
  MLD.fut <- readRDS(paste0("~/regional_time_series_analysis/files/rg_MLDmax/rg_",model.name,"_MLD_fut_time_series.Rds"))
  MLD.his <- readRDS(paste0("~/regional_time_series_analysis/files/rg_MLDmax/rg_",model.name,"_MLD_his_time_series.Rds"))
  
  #read in regridded ocean cell area data, use either GFDL or CM4 since these models have data archived on the same regular grid the models were regridded to
  setwd(paste0("~/spatial_analysis/regridded_nc_files/GFDL_rg/"))
  area.name = list.files(pattern = "areacello")
  nc_data_area <- nc_open(area.name)
  area <- ncvar_get(nc_data_area, "areacello")

  #Subset MLD dfs --------------
    
  #Southern Ocean S of 50
  if(region == "SO_50") {
    MLD_fut_subset = MLD.fut[,1:40,]
    #Southern Ocean S of 60
  } else if(region == "SO_60"){
    MLD_fut_subset = MLD.fut[,1:30,]
    #low latitudes
  } else if(region == "30_low_lats"){
    MLD_fut_subset = MLD.fut[,61:120,]
    #Equatorial Pacific
  } else if(region == "EQ_Pacific") {
    MLD_fut_subset = MLD.fut[160:285,76:106,] #15S to 15N, 160E to 75W 
    #low latitudes without Equatorial Pacific
  } else if (region == "low_lats_no_EQ_Pacific") {
    MLD_fut_subset = MLD.fut[,61:120,] #30S to 30N
    #North Atlantic - NOTE: in calc_region_area this is called North_Atlantic_no_Arctic
  } else if (region == "North_Atlantic") {
    MLD_fut_subset = MLD.fut[290:360,141:166,] #40N to 65N, 70W to 0W
  } else {
    #Low Latitudes
    MLD_fut_subset = MLD.fut[,76:105,] #15S-15N
  }
  
  #Southern Ocean S of 50
  if(region == "SO_50") {
    MLD_his_subset = MLD.his[,1:40,]
    #Southern Ocean S of 60
  } else if(region == "SO_60"){
    MLD_his_subset = MLD.his[,1:30,]
    #low latitudes
  } else if(region == "30_low_lats"){
    MLD_his_subset = MLD.his[,61:120,]
    #Equatorial Pacific
  } else if(region == "EQ_Pacific") {
    MLD_his_subset = MLD.his[160:285,76:106,] #15S to 15N, 160E to 75W 
    #low latitudes without Equatorial Pacific
  } else if (region == "low_lats_no_EQ_Pacific") {
    MLD_his_subset = MLD.his[,61:120,] #30S to 30N
    #North Atlantic - NOTE: in calc_region_area this is called North_Atlantic_no_Arctic
  } else if (region == "North_Atlantic") {
    MLD_his_subset = MLD.his[290:360,141:166,] #40N to 65N, 70W to 0W
  } else {
    #Low Latitudes
    MLD_his_subset = MLD.his[,76:105,] #15S-15N
  }
  
  dims = dim(MLD_fut_subset)
  
  ## calculate grid range to subset to ----------
  
  #only need to do this for one file, since the rasters to subset will be the same
  
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

 
  ## future calculation --------------------------------------------
  
  #length should be 86 years
  v <- 1:dims[3]
  
  #make first column of the vector (to be combined later)
  year <- seq(from = 2015, to = 2100, by = 1)
  
  #final vector output 
  POC_flux_fut = vector(mode = "numeric", length = length(v))
  POC_flux_fut_area = vector(mode = "numeric", length = length(v))
  #storage container for second loop - empty matrix with dimensions of the subset
  output_fut = area_subset*0
  
  
  for(k in 1:length(v)) {
    
    #year to read in
    t <- v[k]
    #print what time step code is on
    print(paste0("future ", k))
    
    #get variable at specified time range (1 year)
    variable_fut <- ncvar_get(nc_data_2015,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
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
    
    #calculate POC flux at select DH
    #for(i in min(range$x):max(range$x)) {
    #  for(j in min(range$y):max(range$y)) {
        
    
    #calculate POC flux at select DH
    for(i in 1:nrow(area_subset)) {
      for(j in 1:ncol(area_subset)) {
        
        #make list and add needed columns
        ret <- list()
        #depth
        if(model.name == "CESM") {
          ret$depth <-  ncvar_get(nc_data_2015, "lev")/100
        } else if (model.name == "IPSL") {
          ret$depth <-  ncvar_get(nc_data_2015, "olevel")
        } else {
          ret$depth <-  ncvar_get(nc_data_2015, "lev")
        }          
        #subset expc for select lat and lon
        ret$expc <- extract(variable_fut, indices = c(i,j), dims = c(1,2))
        #does ocean value exist?
        ret$test <- extract(MLD_fut_subset[, , k], indices = c(i,j), dims = c(1,2))
        
        if(model.name == "IPSL" && DH == "PCD") {
          #subset to fix weird error for this model
          ret$depth <- ret$depth[ret$depth <= 301]
          ret$expc <- ret$expc[1:length(ret$depth)]
        } else {}
        
        if(DH == "MLDmax") {
          #subset MLD max for each lat and lon
          ret$DH <- extract(MLD_fut_subset[, , k], indices = c(i,j), dims = c(1,2))
          #extract depth of max POC flux value
        } else if(DH == "PCD") {
          ret$DH <- ret$depth[which.max(ret$expc)]
        } else {
          ret$DH <- DH
        }
        
        #ocean values - if a value exists for MLDmax, then interpolate and store new interpolated POC flux in output matrix
        if (is.na(ret$test) == FALSE) {
          
          #just assign the max POC flux for this depth horizon, skip interpolation
          if(DH == "PCD") {
            output_fut[i,j] = max(ret$expc, na.rm = TRUE)
          } else {
            #find interpolated expc at mld max
            interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$DH)
            #store interpolated POC flux into the output matrix
            output_fut[i, j] <- interp$y[1]
          }
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
  
  ## historical calculation -------------------------------------------
  
  #make first column of the vector (to be combined later)
  year <- seq(from = 1850, to = 2014, by = 1)
  
  dims = dim(MLD_his_subset)
  
  #length of expc years
  v <- 1:dims[3]
  
  #final vector outputs 
  POC_flux_his = vector(mode = "numeric", length = length(v))
  POC_flux_his_area = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop, an empty matrix
  output_his = area_subset*0
  
  for(k in 1:length(v)) {
    
    #year to read in
    t <- v[k]
    
    print(paste0("historical ", k))
    
    #get variable at specified time range (1 year)
    variable_his <- ncvar_get(nc_data_1850,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
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
      #Low Latitudes (15S to 15N)
      variable_his = variable_his[,76:105,] #15S-15N
    }
    
    for(i in 1:nrow(area_subset)) {
      for(j in 1:ncol(area_subset)) {
        
        #make list and add needed columns
        ret <- list()
        #depth (doesn't matter which nc file you extract this from)
        if(model.name == "CESM") {
          ret$depth <-  ncvar_get(nc_data_2015, "lev") /100
        } else if (model.name == "IPSL") {
          ret$depth <-  ncvar_get(nc_data_2015, "olevel")
        } else {
          ret$depth <-  ncvar_get(nc_data_2015, "lev")
        }   
        #subset expc for select lat and lon
        ret$expc <- extract(variable_his, indices = c(i,j), dims = c(1,2))
        #does ocean value exist?
        ret$test <- extract(MLD_his_subset[, , k], indices = c(i,j), dims = c(1,2))
        
        if(model.name == "IPSL" && DH == "PCD") {
          #subset to fix weird error for this model
          ret$depth <- ret$depth[ret$depth <= 301]
          ret$expc <- ret$expc[1:length(ret$depth)]
        } else {}
        
        if(DH == "MLDmax") {
          #subset MLD max for each lat and lon
          ret$DH <- extract(MLD_his_subset[, , k], indices = c(i,j), dims = c(1,2))
        } else if(DH == "PCD") {
          #extract PCD
          ret$DH <- ret$depth[which.max(ret$expc)]
        } else {
          ret$DH <- DH
        }
        
        #ocean values - if a value exists for MLDmax, then interpolate and store new interpolated POC flux in output matrix
        if (is.na(ret$test) == FALSE) {
          
          #just assign the max POC flux for this depth horizon, skip interpolation
          if(DH == "PCD") {
            output_his[i,j] = max(ret$expc, na.rm = TRUE)
          } else {
            #find interpolated expc at mld max
            interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$DH)
            #store interpolated POC flux into the output matrix
            output_his[i, j] <- interp$y[1]
          }
          #land values - if a value doesn't exist for MLDmax, then don't interpolate, just put an NA value in output matrix  
        } else {
          output_his[i,j] <- NA
        }
      }
    }
    
    #multiply by cell area (mol/yr)
    global_flux_his <- output_his*area_subset
    
    #sum of all model cells (mol/yr)
    sum_flux <- sum(global_flux_his, na.rm = TRUE)
    
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
  
  if(DH == "MLDmax") {
    write.csv(df,paste0("~/regional_time_series_analysis/files/POC_MLDmax/total_flux/",region,"_",model.name,"_time_series_expc_MLDmax.csv"))
  } else if(DH == "PCD") {
    write.csv(df,paste0("~/regional_time_series_analysis/files/POC_PCD/total_flux/",region,"_",model.name,"_time_series_expc_PCD.csv"))
  } else if(DH == 1000) {
    write.csv(df,paste0("~/regional_time_series_analysis/files/POC_1000/total_flux/",region,"_",model.name,"_time_series_expc_1000.csv"))
  } else {
    write.csv(df,paste0("~/regional_time_series_analysis/files/POC_100/total_flux/",region,"_",model.name,"_time_series_POC_100.csv"))
  }
  
  #binds POC_flux vector to year vector
  df.his.2 <- qpcR:::cbind.na(year,POC_flux_his_area)
  time.series.2 <- rbind(df.his.2,df.fut.2)
  df2 = data.frame(time.series.2)
  #change column names for merging csv files later
  colnames(df2) = c('Year',model.name)
  
  if(DH == "MLDmax") {
    write.csv(df2,paste0("~/regional_time_series_analysis/files/POC_MLDmax/per_area_flux/",region,"_",model.name,"_time_series_expc_MLDmax_per_area.csv"))
  } else if(DH == "PCD") {
    write.csv(df2,paste0("~/regional_time_series_analysis/files/POC_PCD/per_area_flux/",region,"_",model.name,"_time_series_expc_PCD_per_area.csv"))
  } else if(DH == 1000) {
    write.csv(df2,paste0("~/regional_time_series_analysis/files/POC_1000/per_area_flux/",region,"_",model.name,"_time_series_expc_1000_per_area.csv"))
  } else {
    write.csv(df2,paste0("~/regional_time_series_analysis/files/POC_100/per_area_flux/",region,"_",model.name,"_time_series_POC_100_per_area.csv"))
  }
  
  
}
