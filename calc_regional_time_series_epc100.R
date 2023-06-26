#' @title Calculate regionally integrated POC flux time-series (1850-2100) at 100 m
#' @author Stevie Walker
#' @date 3/3/23
#' @description calculates POC flux at 100m for every year and saves into a data frame for plotting the time-series figure
#' @input historical POC flux and predicted POC flux (both 100m)
#' @output data frame with two columns (x = year, y = integrated POC flux)

regional_ts_100 <- function(model.name, region) {
  
  #open POC flux files
  setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
  nc.epc100 <- list.files(pattern = "epc100")
  nc_data_1850 <- nc_open(nc.epc100[1])
  nc_data_2015 <- nc_open(nc.epc100[2])
  #open area file
  setwd("~/spatial_analysis/regridded_nc_files/GFDL_rg/")
  area <- list.files(pattern = "areacello")
  nc_data_area <- nc_open(area)
  area <- ncvar_get(nc_data_area, "areacello")
  
  #subset region
  
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
    area_subset[118:126,25:31] = NA
    #low latitudes without Equatorial Pacific
  } else if (region == "low_lats_no_EQ_Pacific") {
    area_subset = area[,61:120] #30S to 30N
    area_subset[160:285,15:45] = NA
    #North Atlantic - NOTE: in calc_region_area this is called North_Atlantic_no_Arctic
  } else if (region == "North_Atlantic") {
    area_subset = area[290:360,141:166] #40N to 65N, 70W to 0W
  } else {
    #Low Latitudes
    area_subset = area[,76:105] #15S-15N
  }
  
  #for every 12 time steps, calculate yearly average
  v <- seq(from = 1, to = 1969, by = 12)
  
  #make first column of the vector (to be combined later)
  year <- seq(from = 1850, to = 2014, by = 1)
  
  #vector output 
  POC_flux = vector(mode = "numeric", length = length(v))
  POC_flux_area = vector(mode = "numeric", length = length(v))
  
  
  for(i in 1:length(v)) {
    
    t <- v[i]
    
    #get variable at specified time range (12 months)
    variable_his <- ncvar_get(nc_data_1850,"epc100",start= c(1,1,t), count = c(-1,-1, 12))
    
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
      # Low latitudes without Equatorial Pacific
    } else if (region == "low_lats_no_EQ_Pacific") {
      variable_his = variable_his[,61:120,] #30S to 30N
      variable_his[160:285,15:45,] = NA
      #North Atlantic 
    } else if (region == "North_Atlantic") {
      variable_his = variable_his[290:360,141:166,] #40N to 65N, 70W to 0W
    } else {
      #Low Latitudes
      variable_his = variable_his[,76:105,] #15S-15N
    }
    
    #calculate average POC flux for each grid cell over one year
    var_average1 <- apply(variable_his, c(1,2),mean,na.rm=FALSE)*31536000
    
    #multiply by cell area
    global_flux <- var_average1*area_subset
    
    #sum of all model cells
    sum_flux <- sum(global_flux, na.rm = TRUE)
    
    #sum of area subset (m2)
    area_sum <- sum(area_subset, na.rm = TRUE)
    
    #Total global POC flux in Pt C / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    #total regional POC flux per area (Pt C/m2/yr)
    flux_per_area <- sum_flux/area_sum
    
    POC_flux[i] = sum_flux
    POC_flux_area[i] = flux_per_area
    
  }
  
  #binds POC_flux vector to year vector
  df.his <- qpcR:::cbind.na(year,POC_flux)
  df.his.2 <- qpcR:::cbind.na(year,POC_flux_area)
  
  ## Calculate Future Globally Integrated POC Flux -------------
  
  #for every 12 time steps, calculate yearly average
  v <- seq(from = 1, to = 1021, by = 12)
  
  year <- seq(from = 2015, to = 2100, by = 1)
  
  #vector output 
  POC_flux_fut = vector(mode = "numeric", length = length(v))
  POC_flux_fut_area = vector(mode = "numeric", length = length(v))
  
  for(i in 1:length(v)) {
    
    t <- v[i]
    
    # counter here
    #get variable at specified time range (12 months)
    variable_fut <- ncvar_get(nc_data_2015,"epc100",start= c(1,1,t), count = c(-1,-1, 12))
    
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
      # Low latitudes without Equatorial Pacific
    } else if (region == "low_lats_no_EQ_Pacific") {
      variable_fut = variable_fut[,61:120,] #30S to 30N
      variable_fut[160:285,15:45,] = NA
      #North Atlantic 
    } else if (region == "North_Atlantic") {
      variable_fut = variable_fut[290:360,141:166,] #40N to 65N, 70W to 0W
    } else {
      #North Atlantic with the Arctic Ocean
      variable_fut = variable_fut[,76:105,] #15S-15N
    }
    
    #calculate average POC flux for each grid cell over one year
    var_average1 <- apply(variable_fut, c(1,2),mean,na.rm=FALSE)*31536000
    
    #multiply by cell area
    global_flux <- var_average1*area_subset
    
    #sum of all model cells
    sum_flux <- sum(global_flux, na.rm = TRUE)
    
    #sum of area subset (m2)
    area_sum <- sum(area_subset, na.rm = TRUE)
    
    #Total global POC flux in Pt C / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    #total regional POC flux per area (Pt C/m2/yr)
    flux_per_area <- sum_flux/area_sum
    
    POC_flux_fut[i] = sum_flux
    POC_flux_fut_area[i] = flux_per_area
    
  }
  
  #total flux
  df.fut <- qpcR:::cbind.na(year,POC_flux_fut)
  time.series <- rbind(df.his,df.fut)
  df = data.frame(time.series)
  #change column names for merging csv files later
  colnames(df) = c('Year',model.name)
  write.csv(df,paste0("~/regional_time_series_analysis/files/POC_100/total_flux/",region,"_",model.name,"_time_series_POC_100.csv"))
  
  #flux per area
  df.fut.2 <- qpcR:::cbind.na(year,POC_flux_fut_area)
  time.series.2 <- rbind(df.his.2,df.fut.2)
  df2 = data.frame(time.series.2)
  #change column names for merging csv files later
  colnames(df2) = c('Year',model.name)
  write.csv(df2,paste0("~/regional_time_series_analysis/files/POC_100/per_area_flux/",region,"_",model.name,"_time_series_POC_100_per_area.csv"))
  
}