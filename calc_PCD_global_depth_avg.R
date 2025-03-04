#' @title Calculate historical average and long term average globally integrated PCD
#' @date 3/3/25
#' @description For Table S1

calc_PCD_avg <- function(model.name,DH,lon.length,lat.length) {
  
  #read in expc nc files
  setwd(paste0("~/senior_thesis/combined_",model.name,"_files/"))
  nc.expc <- list.files(pattern = "expc")
  if(model.name == "UKESM") {
    nc_data_1850 <- nc_open(nc.expc[1])
    nc_data_2015 <- nc_open(nc.expc[6])
  } else{
    nc_data_1850 <- nc_open(nc.expc[1])
    nc_data_2015 <- nc_open(nc.expc[2])
  }
  ## future calculation --------------------------------------------
  
  #length should be 20 years
  if(model.name == "UKESM") {
    time.step = seq(from = 1, to = 601, by = 12)
    v = time.step[32:51]
  } else {
    v <- 67:86
  }
  #final vector output 
  PCD_avg_fut = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop
  output_fut <- matrix(nrow = length(lon.length), ncol = length(lat.length))
  
  #calculate future POC flux 
  for(k in 1:length(v)) {
    
    #year to read in
    t <- v[k]
    #print what time step code is on
    print(paste0("future ", k))
    
    if (model.name == "UKESM") {
      #get variable at specified time range (monthly), convert to mol/m2/yr
      variable_fut <-  ncvar_get(nc_data_2015,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,12))*31536000
      variable_fut <- apply(variable_fut, c(1,2,3),mean,na.rm=FALSE)    
    }else {
      variable_fut <-  ncvar_get(nc_data_2015,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    }
    
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
        
        if(model.name == "IPSL" && DH == "PCD") {
          #subset to fix weird error for this model
          ret$depth <- ret$depth[ret$depth <= 301]
          ret$expc <- ret$expc[1:length(ret$depth)]
        } else {}
        
        #calc PCD
        ret$DH <- ret$depth[which.max(ret$expc)]
        
        #ocean values - if a value exists
        if (is.na(ret$expc[1]) == FALSE) {
          
          #store PCD into the output matrix
          output_fut[i, j] <- ret$DH
          #land values - if a value doesn't exist
        } else {
          output_fut[i,j] <- NA
        }
      }
    }
    
    PCD_avg_fut[k] = mean(output_fut,na.rm = TRUE)
    
  }
  
  PCD_fut = mean(PCD_avg_fut,na.rm = TRUE)
  
  ## historical calculation --------------------------------------------
  
  #length should be 50 years
  if(model.name == "UKESM") {
    v = seq(from = 1, to = 589, by = 12)
  } else {
    v <- 1:50
  }
  
  #final vector output 
  PCD_avg_his = vector(mode = "numeric", length = length(v))
  
  #storage container for second for loop
  output_his <- matrix(nrow = length(lon.length), ncol = length(lat.length))
  
  #calculate historical average PCD  
  for(k in 1:length(v)) {
    
    #year to read in
    t <- v[k]
    #print what time step code is on
    print(paste0("historical ", k))
    
    #get variable at specified time range (1 year), convert to mol/m2/yr
    #get variable at specified time range (monthly), convert to mol/m2/yr
    if(model.name == "UKESM") {
      variable_his <-  ncvar_get(nc_data_1850,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,12))*31536000
      variable_his <- apply(variable_his, c(1,2,3),mean,na.rm=FALSE)    
    } else {
      variable_his <-  ncvar_get(nc_data_1850,"expc",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
    }
    
    for(i in 1:length(lon.length)) {
      for(j in 1:length(lat.length)) {
        
        #make list and add needed columns
        ret <- list()
        #depth
        if(model.name == "CESM") {
          ret$depth <-  ncvar_get(nc_data_1850, "lev") /100
        } else if (model.name == "IPSL") {
          ret$depth <-  ncvar_get(nc_data_1850, "olevel")
        } else {
          ret$depth <-  ncvar_get(nc_data_1850, "lev")
        }          
        #subset expc for select lat and lon
        ret$expc <- extract(variable_his, indices = c(i,j), dims = c(1,2))
        
        if(model.name == "IPSL" && DH == "PCD") {
          #subset to fix weird error for this model
          ret$depth <- ret$depth[ret$depth <= 301]
          ret$expc <- ret$expc[1:length(ret$depth)]
        } else {}
        
        #calc PCD
        ret$DH <- ret$depth[which.max(ret$expc)]
        
        #ocean values - if a value exists for ez depth, then interpolate and store new interpolated POC flux in output matrix
        if (is.na(ret$expc[1]) == FALSE) {
          
          #store interpolated POC flux into the output matrix
          output_his[i, j] <- ret$DH
          #land values - if a value doesn't exist for ez depth, then don't interpolate, just put an NA value in output matrix  
        } else {
          output_his[i,j] <- NA
        }
      }
    }
    
    PCD_avg_his[k] = mean(output_his,na.rm = TRUE)
    
  }
  
  PCD_his = mean(PCD_avg_his,na.rm = TRUE)
  
  PCD_table = cbind(model.name,PCD_his,PCD_fut) %>%
    as_tibble()
  write_csv(PCD_table,paste0("~/time_series_analysis/files/average_change/PCD_depths/",model.name,"_PCD_depth.csv"))
  
  print(paste0(model.name," PCD historical average: ",PCD_his))
  print(paste0(model.name," PCD future average: ",PCD_fut))
  
}