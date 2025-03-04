#' @title Calculate time series of globally integrated POC flux change at the MLDmax and at 1000m (1850-2100)
#' @author Stevie Walker
#' @date 11/1/21
#' @inputs expc nc file, areacello nc file, MLDmax time series arrays (historical and future for all inputs)
#' @output csv file of year and global POC flux for each year

#model.name = "CM4"
#lon.length = 1:360
#lat.length = 1:180
#DH = 100

#time_series_expc(model.name = "CM4",
#                 lon.length = 1:360,
#                 lat.length = 1:180,
#                 DH = 100)

#add this line when calculating CM4 POC flux at different depth levels (you can also just change the DH function input to the depth you want, probably easier!)
#else if(DH == 100 && model.name == "CM4") {
#  output_fut[i,j] = ret$expc[6] # I changed these individually to test out the different depths, 8 = 112.5 m, 7 = 87.5 m, 6 = 62.5 m
#}

time_series_expc <- function(model.name, lon.length, lat.length, DH) {
  
  #read in expc nc files
  setwd(paste0("~/senior_thesis/combined_",model.name,"_files/"))
  nc.expc <- list.files(pattern = "expc")
  nc_data_1850 <- nc_open(nc.expc[1])
  nc_data_2015 <- nc_open(nc.expc[2])
  
  #read in ocean cell area data
  area.name <- list.files(pattern = "areacello")
  nc_data_area <- nc_open(area.name)
  area <- ncvar_get(nc_data_area, "areacello")
  area[area == 0] <- NA
  
  #read in MLDmax arrays
  MLD.fut <- readRDS(paste0("~/time_series_analysis/files/MLDmax/",model.name,"_MLD_fut_time_series.Rds"))
  MLD.his <- readRDS(paste0("~/time_series_analysis/files/MLDmax/",model.name,"_MLD_his_time_series.Rds"))
  
  dims = dim(MLD.fut)
  
  ## future calculation --------------------------------------------
  
  #length should be 86 years
  v <- 1:dims[3]
  
  #make first column of the vector (to be combined later)
  year <- seq(from = 2015, to = 2100, by = 1)
  
  #final vector output 
  POC_flux_fut = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop
  output_fut <- matrix(nrow = length(lon.length), ncol = length(lat.length))
  
  #calculate future POC flux 
  for(k in 1:length(v)) {
    
    #year to read in
    t <- v[k]
    #print what time step code is on
    print(paste0("future ", k))
    
    #get variable at specified time range (1 year), convert to mol/m2/yr
    variable_fut <- ncvar_get(nc_data_2015,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
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
        ret$expc <- extract(variable_fut, indices = c(i,j), dims = c(1,2))
        #does ocean value exist?
        ret$test <- extract(MLD.fut[, , k], indices = c(i,j), dims = c(1,2))
        
        if(model.name == "IPSL" && DH == "PCD") {
          #subset to fix weird error for this model
          ret$depth <- ret$depth[ret$depth <= 301]
          ret$expc <- ret$expc[1:length(ret$depth)]
        } else {}
        
        if(DH == "MLDmax") {
          #subset MLD max for each lat and lon
          ret$DH <- extract(MLD.fut[, , k], indices = c(i,j), dims = c(1,2))
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
          }  else {
            #find linearly interpolated expc
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
    global_flux <- output_fut*area
    
    #sum of all model cells
    sum_flux <- sum(global_flux, na.rm = TRUE)
    
    #Total global POC flux in Pg C / yr for one year
    sum_flux <- sum_flux*12.01/1000000000000000
    
    #assign globally integrated POC flux value from t year to the output vector
    POC_flux_fut[t] = sum_flux
    
  }
  
  #combine year and POC flux
  df.fut <- qpcR:::cbind.na(year,POC_flux_fut)
  
  ## historical calculation -------------------------------------------
  
  #make first column of the vector (to be combined later)
  year <- seq(from = 1850, to = 2014, by = 1)
  
  dims = dim(MLD.his)
  
  #length of expc years, should be 165 years
  v <- 1:dims[3]
  
  #final vector output 
  POC_flux_his = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop
  output_his <- matrix(nrow = length(lon.length), ncol = length(lat.length))
  
  for(k in 1:length(v)) {
    
    #year to read in
    t <- v[k]
    
    print(paste0("historical ", k))
    
    #get variable at specified time range (1 year), convert to mol/m2/yr
    variable_his <- ncvar_get(nc_data_1850,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    
    #interpolate POC flux at MLDmax for every grid cell and year
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
        ret$expc <- extract(variable_his, indices = c(i,j), dims = c(1,2))
        #does ocean value exist?
        ret$test <- extract(MLD.his[, , k], indices = c(i,j), dims = c(1,2))
        
        if(model.name == "IPSL" && DH == "PCD") {
          #subset to fix weird error for this model
          ret$depth <- ret$depth[ret$depth <= 301]
          ret$expc <- ret$expc[1:length(ret$depth)]
        } else {}
        
        if(DH == "MLDmax") {
          #subset MLD max for each lat and lon
          ret$DH <- extract(MLD.his[, , k], indices = c(i,j), dims = c(1,2))
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
            #find linearly interpolated expc
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
    
    #multiply by cell area
    global_flux_his <- output_his*area
    
    #sum of all model cells
    sum_flux_his <- sum(global_flux_his, na.rm = TRUE)
    
    #Total global POC flux in Pt C / yr for one year
    sum_flux_his <- sum_flux_his*12.01/1000000000000000
    
    #assign globally integrated POC flux value from t year to the output vector
    POC_flux_his[t] = sum_flux_his
    
  }
  
  
  #binds POC_flux vector to year vector
  df.his <- qpcR:::cbind.na(year,POC_flux_his)
  
  #bind historical and future df
  time.series <- rbind(df.his,df.fut)
  df = data.frame(time.series) # this name will get replaced to time.series
  #change column names for merging csv files later
  colnames(df) = c('Year',model.name)
  
  #save output
  if(DH == "MLDmax") {
    write.csv(df,paste0("~/time_series_analysis/files/POC_MLDmax/",model.name,"_time_series_expc_MLDmax.csv"))
  } else if(DH == "PCD") {
    write.csv(df,paste0("~/time_series_analysis/files/POC_PCD/",model.name,"_time_series_expc_PCD.csv"))
  } else if(DH == 1000) {
    write.csv(df,paste0("~/time_series_analysis/files/POC_1000/",model.name,"_time_series_expc_1000.csv"))
  } else {
    write.csv(df,paste0("~/time_series_analysis/files/POC_100/",model.name,"_time_series.csv"))
  }
}

