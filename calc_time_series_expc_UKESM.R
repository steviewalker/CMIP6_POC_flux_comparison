#' @title Calculate time series of globally integrated POC flux change at the MLDmax, PCD, 1000m, and EZ depth for UKESM (1850-2100)
#' @author Stevie Walker
#' @date 11/1/21
#' @description UKESM has expc files split up and resolved monthly, so this is the adapted function for it
#' @inputs expc nc file, areacello nc file, MLDmax time series arrays (historical and future for all inputs)
#' @output csv files of year and global POC flux for each year, split into 6 chunks and stored in UKESM_uncombined

time_series_expc_UKESM <- function(DH, file) {
  
  ## Read in data ----------
  
  setwd("~/senior_thesis/combined_UKESM_files/")
  nc.expc <- list.files(pattern = "expc")
  nc_data <- nc_open(nc.expc[file])
  
  #read in ocean cell area data
  area.name <- list.files(pattern = "areacello")
  nc_data_area <- nc_open(area.name)
  area <- ncvar_get(nc_data_area, "areacello")
  
  #read in the correct array time period and subset if needed to match the number of years in nc_data
  if(DH == "MLDmax") {
    if(file == 1) {
      #1850-1899
      interp.array <- readRDS("~/time_series_analysis/files/MLDmax/UKESM_MLD_his_time_series.Rds")
      interp.array = interp.array[,,1:50]
    } else if(file == 2) {
      #1900-1949
      interp.array <- readRDS("~/time_series_analysis/files/MLDmax/UKESM_MLD_his_time_series.Rds")
      interp.array = interp.array[,,51:100]
    } else if(file == 3) {
      #1950-1999
      interp.array <- readRDS("~/time_series_analysis/files/MLDmax/UKESM_MLD_his_time_series.Rds")
      interp.array = interp.array[,,101:150]
    } else if(file == 4) {
      #2000-2014
      interp.array <- readRDS("~/time_series_analysis/files/MLDmax/UKESM_MLD_his_time_series.Rds")
      interp.array = interp.array[,,151:165]
    } else if(file == 5) {
      #2015-2049
      interp.array <- readRDS("~/time_series_analysis/files/MLDmax/UKESM_MLD_fut_time_series.Rds")
      interp.array = interp.array[,,1:35]
    } else {
      #2050-2100
      interp.array <- readRDS("~/time_series_analysis/files/MLDmax/UKESM_MLD_fut_time_series.Rds")
      interp.array = interp.array[,,36:86]
    }
  } else if(DH == "ez_depth") {
    if(file == 1 || file == 2 || file == 3 || file == 4) {
      interp.array <- readRDS(paste0("~/time_series_analysis/files/EZ_depth/UKESM_",file,"_ez_depth_his_time_series.Rds"))
    } else if(file == 5){
      interp.array <- readRDS("~/time_series_analysis/files/EZ_depth/UKESM_ez_depth_fut_time_series.Rds")
      interp.array = interp.array[,,1:35]
    } else {
      interp.array <- readRDS("~/time_series_analysis/files/EZ_depth/UKESM_ez_depth_fut_time_series.Rds")
      interp.array = interp.array[,,36:86]
    }
  } else if(DH == "ez_depth_10") {
    if(file == 1 || file == 2 || file == 3 || file == 4) {
      interp.array <- readRDS(paste0("~/time_series_analysis/files/EZ_depth/ten_percent/UKESM_",file,"_ez_depth_10_his_time_series.Rds"))
    } else if(file == 5){
      interp.array <- readRDS("~/time_series_analysis/files/EZ_depth/ten_percent/UKESM_ez_depth_10_fut_time_series.Rds")
      interp.array = interp.array[,,1:35]
    } else {
      interp.array <- readRDS("~/time_series_analysis/files/EZ_depth/ten_percent/UKESM_ez_depth_10_fut_time_series.Rds")
      interp.array = interp.array[,,36:86]
    }
  } else {
    #read in one array for the NA test later on in the code
    interp.array <- readRDS("~/time_series_analysis/files/MLDmax/UKESM_MLD_his_time_series.Rds")
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
  
  #storage container for second for loop
  output <- matrix(nrow = 360, ncol = 330)
  
  for(k in 1:length(time.step)) {
    
    #year to read in
    t <- time.step[k]
    
    #print the time step the code is on
    print(k)
    
    #get variable at specified time range (1 year), average monthly
    expc <- ncvar_get(nc_data,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,12))*31536000
    expc <- apply(expc, c(1,2,3),mean,na.rm=FALSE)
    
    for(i in 1:360) {
      for(j in 1:330) {
        
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
    global_flux <- output*area
    
    #sum of all model cells
    sum_flux <- sum(global_flux, na.rm = TRUE)
    
    #Total global POC flux in Pt C / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    #assign globally integrated POC flux value from t year to the output vector
    POC_flux[k] = sum_flux
    
  }
  
  #combine vectors
  df <- qpcR:::cbind.na(year,POC_flux) %>%
    as_tibble()
  colnames(df) = c('Year','UKESM')
  
  #save files
  if(DH == "MLDmax") {
    write.csv(df,paste0("~/time_series_analysis/files/POC_MLDmax/UKESM_uncombined/UKESM_",file,"_time_series_expc_MLDmax.csv"))
  } else if(DH == "ez_depth") {
    write.csv(df,paste0("~/time_series_analysis/files/POC_EZ/UKESM_uncombined/UKESM_",file,"_time_series_expc_ez.csv"))
  } else if(DH == "ez_depth_10") {
    write.csv(df,paste0("~/time_series_analysis/files/POC_EZ/UKESM_uncombined/UKESM_",file,"_time_series_expc_ez_10.csv"))
  } else if(DH == "PCD") {
    write.csv(df,paste0("~/time_series_analysis/files/POC_PCD/UKESM_uncombined/UKESM_",file,"_time_series_expc_PCD.csv"))
  } else {
    write.csv(df,paste0("~/time_series_analysis/files/POC_1000/UKESM_uncombined/UKESM_",file,"_time_series_expc_1000.csv"))
  }
  
}