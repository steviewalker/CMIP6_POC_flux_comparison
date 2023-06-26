#' @title Calculate time series of regionally integrated POC flux change at the MLDmax, 1000m, and EZ depth for UKESM (1850-2100)
#' @author Stevie Walker
#' @date 4/4/23
#' @description UKESM has expc files split up and resolved monthly, so this is the adapted function for it
#' @inputs regridded expc nc file, areacello nc file, MLDmax time series arrays (historical and future for all inputs)
#' @output csv file of year and regional POC flux for each year
#' @note this function uses a lot of RAM

regional_ts_expc_UKESM <- function(region, DH, file) {
  
  ## Read in data ----------
  
  setwd("~/spatial_analysis/regridded_nc_files/UKESM_rg/")
  nc.expc <- list.files(pattern = "expc")
  nc_data <- nc_open(nc.expc[file])
  
  #read in regridded ocean cell area data, use either GFDL or CM4 since these models have data archived on the same regular grid the models were regridded to
  setwd(paste0("~/spatial_analysis/regridded_nc_files/GFDL_rg/"))
  area.name = list.files(pattern = "areacello")
  nc_data_area <- nc_open(area.name)
  area <- ncvar_get(nc_data_area, "areacello")
  
  setwd("~/spatial_analysis/regridded_nc_files/UKESM_rg/")  
  
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
  
  
  if(DH == "MLDmax") {
    #load correct section of MLDmax array
    if(file == 1) {
      interp.array <- readRDS("~/regional_time_series_analysis/files/rg_MLDmax/rg_UKESM_MLD_his_time_series.Rds")
      interp.array = interp.array[,,1:50]
    } else if(file == 2) {
      interp.array <- readRDS("~/regional_time_series_analysis/files/rg_MLDmax/rg_UKESM_MLD_his_time_series.Rds")
      interp.array = interp.array[,,51:100]
    } else if(file == 3) {
      interp.array <- readRDS("~/regional_time_series_analysis/files/rg_MLDmax/rg_UKESM_MLD_his_time_series.Rds")
      interp.array = interp.array[,,101:150]
    } else if(file == 4) {
      interp.array <- readRDS("~/regional_time_series_analysis/files/rg_MLDmax/rg_UKESM_MLD_his_time_series.Rds")
      interp.array = interp.array[,,151:165]
    } else if(file == 5) {
      interp.array <- readRDS("~/regional_time_series_analysis/files/rg_MLDmax/rg_UKESM_MLD_fut_time_series.Rds")
      interp.array = interp.array[,,1:35]
    } else {
      interp.array <- readRDS("~/regional_time_series_analysis/files/rg_MLDmax/rg_UKESM_MLD_fut_time_series.Rds")
      interp.array = interp.array[,,36:86]
    }
    
    
    #come back to this once running POC EZ
    
  } else if(DH == "EZ_depth") {
    
    if(file == 1 || file == 2 || file == 3 || file == 4) {
      interp.array <- readRDS(paste0("~/regional_time_series_analysis/files/rg_EZ_depth/rg_UKESM_",file,"_ez_depth_his_time_series.Rds"))
    } else if(file == 5) {
      interp.array <- readRDS("~/regional_time_series_analysis/files/rg_EZ_depth/rg_UKESM_ez_depth_fut_time_series.Rds")
      interp.array = interp.array[,,1:35]
    } else {
      interp.array <- readRDS("~/regional_time_series_analysis/files/rg_EZ_depth/rg_UKESM_ez_depth_fut_time_series.Rds")
      interp.array = interp.array[,,36:86]
    }
  } else {
    #read in one array for the NA test later on in the code
    interp.array <- readRDS("~/regional_time_series_analysis/files/rg_MLDmax/rg_UKESM_MLD_his_time_series.Rds")
  }
  
  ## subset interpolation array ------------
  
  #Southern Ocean S of 50
  if(region == "SO_50") {
    interp.array = interp.array[,1:40,]
    #Southern Ocean S of 60
  } else if(region == "SO_60"){
    interp.array = interp.array[,1:30,]
    #low latitudes
  } else if(region == "30_low_lats"){
    interp.array = interp.array[,61:120,]
    #Equatorial Pacific
  } else if(region == "EQ_Pacific") {
    interp.array = interp.array[160:285,76:106,] #15S to 15N, 160E to 75W 
    #low latitudes without Equatorial Pacific
  } else if (region == "low_lats_no_EQ_Pacific") {
    interp.array = interp.array[,61:120,] #30S to 30N
    #North Atlantic - NOTE: in calc_region_area this is called North_Atlantic_no_Arctic
  } else if (region == "North_Atlantic") {
    interp.array = interp.array[290:360,141:166,] #40N to 65N, 70W to 0W
  } else {
    #Low Latitudes
    interp.array = interp.array[,76:105,] #15S-15N
  }
  
  ## sequencing -----------
  
  if(file == 1 || file == 2 || file == 3) {
    time.step <- seq(from = 1, to = 589, by = 12)
  } else if(file == 4) {
    time.step <- seq(from = 1, to = 169, by = 12)
  } else if(file == 5) {
    time.step <- seq(from = 1, to = 409, by = 12)
  } else {
    time.step <- seq(from = 1, to = 601, by = 12)
  }
  
  if(file == 1) {
    year <- seq(from = 1850, to = 1899, by = 1)
  } else if(file == 2) {
    year <- seq(from = 1900, to = 1949, by = 1)
  } else if(file == 3) {
    year <- seq(from = 1950, to = 1999, by = 1)
  } else if(file == 4) {
    year <- seq(from = 2000, to = 2014, by = 1)
  } else if(file == 5) {
    year <- seq(from = 2015, to = 2049, by = 1)
  } else {
    year <- seq(from = 2050, to = 2100, by = 1)
  }
  
  ## Calculate globally integrated POC flux at MLDmax, EZ depth, PCD, or 1000m from 1850-2100 -------
  
  #final vector output 
  POC_flux = vector(mode = "numeric", length = length(time.step))
  POC_flux_area = vector(mode = "numeric", length = length(time.step))
  #storage container for second loop - empty matrix with dimensions of the subset
  output = area_subset*0
  
  for(k in 1:length(time.step)) {
    
    #year to read in
    t <- time.step[k]
    
    #print the time step the code is on
    print(k)
    
    #get variable at specified time range (1 year), average monthly
    expc <- ncvar_get(nc_data,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,12))*31536000
    
    #subset the region
    #Southern Ocean South of 60S
    if(region == "SO_60") {
      expc = expc[,1:30,,]
      #Southern Ocean South of 50S
    } else if (region == "SO_50") {
      expc = expc[,1:40,,]
      #low latitudes (30S to 30N)
    } else if (region == "30_low_lats") {
      expc = expc[,61:120,,]
      # Equatorial Pacific
    } else if (region == "EQ_Pacific") {
      expc = expc[160:285,76:106,,] #15S to 15N, 160E to 75W
      #make values in the Caribbean Sea NA
      expc[118:126,25:31,,] = 0
      # Low latitudes without Equatorial Pacific
    } else if (region == "low_lats_no_EQ_Pacific") {
      expc = expc[,61:120,,] #30S to 30N
      expc[160:285,15:45,,] = 0
      #North Atlantic 
    } else if (region == "North_Atlantic") {
      expc = expc[290:360,141:166,,] #40N to 65N, 70W to 0W
    } else {
      #Low Latitudes
      expc = expc[,76:105,,] #15S-15N
    }
    
    #take yearly average
    expc <- apply(expc, c(1,2,3),mean,na.rm=FALSE)
    
    #calculate POC flux at select DH
    for(i in 1:nrow(area_subset)) {
      for(j in 1:ncol(area_subset)) {
        
        #make list and add needed columns
        ret <- list()
        #depth
        ret$depth <-  ncvar_get(nc_data, "lev")
        #subset expc for select lat and lon
        ret$expc <- extract(expc, indices = c(i,j), dims = c(1,2))
        #does ocean value exist?
        ret$test <- extract(interp.array[, , k], indices = c(i,j), dims = c(1,2))
        if(DH == 1000) {
          ret$DH <- 1000
        } else if(DH == "PCD") {
          #depth of max POC flux
          ret$DH <- ret$depth[which.max(ret$expc)]
        } else {
          #subset array for each lat and lon
          ret$DH <- extract(interp.array[, , k], indices = c(i,j), dims = c(1,2))
        }
        #ocean values - if a value exists then interpolate and store new interpolated POC flux in output matrix
        if (is.na(ret$test) == FALSE) {
          
          #find interpolated expc at mld max
          interp <- approx(x = ret$depth, y  = ret$expc, xout = ret$DH)
          #store interpolated POC flux into the output matrix
          output[i, j] <- interp$y[1]
          #land values - if a value doesn't exist for MLDmax, then don't interpolate, just put an NA value in output matrix  
        } else {
          output[i,j] <- NA
        }
      }
    }
    
    #multiply by cell area
    global_flux <- output*area_subset
    
    #sum of all model cells
    sum_flux <- sum(global_flux, na.rm = TRUE)
    
    #sum of area subset (m2)
    area_sum <- sum(area_subset, na.rm = TRUE)
    
    #Total regional POC flux in Pt C / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    #total regional POC flux per area (Pt C/m2/yr)
    flux_per_area <- sum_flux/area_sum
    
    #assign regionally integrated POC flux value from t year to the output vectors
    POC_flux[k] = sum_flux
    POC_flux_area[k] = flux_per_area    
    
  }
  
  #binds POC_flux vector to year vector
  df <- qpcR:::cbind.na(year,POC_flux) %>%
    as_tibble()
  #change column names for merging csv files later
  colnames(df) = c('Year','UKESM')
  
  
  if(DH == "MLDmax") {
    write.csv(df,paste0("~/regional_time_series_analysis/files/POC_MLDmax/total_flux/UKESM_uncombined/",region,"_UKESM_",file,"_time_series_expc_MLDmax.csv"))
  } else if(DH == "EZ_depth") {
    write.csv(df,paste0("~/regional_time_series_analysis/files/POC_EZ/total_flux/UKESM_uncombined/",region,"_UKESM_",file,"_time_series_expc_ez.csv"))
  } else if(DH == "PCD") {
    write.csv(df,paste0("~/regional_time_series_analysis/files/POC_PCD/total_flux/UKESM_uncombined/",region,"_UKESM_",file,"_time_series_expc_PCD.csv"))
  } else {
    write.csv(df,paste0("~/regional_time_series_analysis/files/POC_1000/total_flux/UKESM_uncombined/",region,"_UKESM_",file,"_time_series_expc_1000.csv"))
  }
  
  #binds POC_flux vector to year vector
  df2 <- qpcR:::cbind.na(year,POC_flux_area) %>%
    as_tibble()
  #change column names for merging csv files later
  colnames(df2) = c('Year','UKESM')
  
  
  if(DH == "MLDmax") {
    write.csv(df2,paste0("~/regional_time_series_analysis/files/POC_MLDmax/per_area_flux/UKESM_uncombined/",region,"_UKESM_",file,"_time_series_expc_MLDmax_per_area.csv"))
  } else if(DH == "EZ_depth") {
    write.csv(df2,paste0("~/regional_time_series_analysis/files/POC_EZ/per_area_flux/UKESM_uncombined/",region,"_UKESM_",file,"_time_series_expc_ez_per_area.csv"))
  } else if(DH == "PCD") {
    write.csv(df2,paste0("~/regional_time_series_analysis/files/POC_PCD/per_area_flux/UKESM_uncombined/",region,"_UKESM_",file,"_time_series_expc_PCD_per_area.csv"))
  } else {
    write.csv(df2,paste0("~/regional_time_series_analysis/files/POC_1000/per_area_flux/UKESM_uncombined/",region,"_UKESM_",file,"_time_series_expc_1000_per_area.csv"))
  }
  
}

combine_regional_ts_UKESM <- function(DH, region) {
  
  if(DH == "NPP") {
    setwd("~/regional_time_series_analysis/files/NPP/total_npp/UKESM_uncombined/")
  } else {
    setwd(paste0("~/regional_time_series_analysis/files/POC_",DH,"/total_flux/UKESM_uncombined/"))
  }
  
  if(region == "EQ_Pacific") {
    df.sep <- list.files(pattern = region)
    #get rid of low_lats_no_EQ_Pacific values
    df.sep <- df.sep[!str_detect(df.sep,pattern="low_lats_no")]
  } else {
    df.sep <- list.files(pattern = region)
  }
  
  #create empty list for storing for loop output
  time.series <- list()
  
  for(i in df.sep) {
    
    #read in csv file
    df <- read_csv(i)
    #store into list
    time.series[[i]] <- df
  }
  
  combined <- time.series %>%
    bind_rows
  
  if(DH == "NPP") {
    write_csv(combined, paste0("~/regional_time_series_analysis/files/NPP/total_npp/",region,"_UKESM_time_series_npp.csv"))
  } else {
    write_csv(combined, paste0("~/regional_time_series_analysis/files/POC_",DH,"/total_flux/",region,"_UKESM_time_series_expc_",DH,".csv"))
  }
  #repeat for per area flux --------
  
  if(DH == "NPP") {
    setwd("~/regional_time_series_analysis/files/NPP/per_area_npp/UKESM_uncombined/")
  } else {
    setwd(paste0("~/regional_time_series_analysis/files/POC_",DH,"/per_area_flux/UKESM_uncombined/"))
  }
  
  if(region == "EQ_Pacific") {
    df.sep <- list.files(pattern = region)
    #get rid of low_lats_no_EQ_Pacific values
    df.sep <- df.sep[!str_detect(df.sep,pattern="low_lats_no")]
  } else {
    df.sep <- list.files(pattern = region)
  }
  
  #create empty list for storing for loop output
  time.series <- list()
  
  for(i in df.sep) {
    
    #read in csv file
    df <- read_csv(i)
    #store into list
    time.series[[i]] <- df
  }
  
  combined <- time.series %>%
    bind_rows
  
  if(DH == "NPP") {
    write_csv(combined, paste0("~/regional_time_series_analysis/files/NPP/per_area_npp/",region,"_UKESM_time_series_npp_per_area.csv"))
  } else {
    write_csv(combined, paste0("~/regional_time_series_analysis/files/POC_",DH,"/per_area_flux/",region,"_UKESM_time_series_expc_",DH,"_per_area.csv"))
  }
  
}
