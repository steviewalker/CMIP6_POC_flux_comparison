#' @title Calculate integrated global NPP time-series (1850-2100)
#' @author Stevie Walker
#' @date 5/27/24
#' @description calculates global NPP for every year using intpp (2D NPP) output and saves into a data frame for plotting the time-series figure
#' @input historical NPP and ssp585 NPP
#' @output data frame with two columns (x = year, y = globally integrated NPP)

time_series_intpp <- function(model.name) {
  
  #get files
  setwd(paste0("~/senior_thesis/combined_",model.name,"_files"))
  nc.intpp <- list.files(pattern = "intpp")
  area <- list.files(pattern = "areacello")
  
  #open nc files
  nc_data_1850 <- nc_open(nc.intpp[1])
  nc_data_2015 <- nc_open(nc.intpp[2])
  
  nc_data_area <- nc_open(area[1])
  
  area <- ncvar_get(nc_data_area, "areacello")
  
  #for every 12 time steps, calculate yearly average
  v <- seq(from = 1, to = 1969, by = 12)
  
  #make first column of the vector (to be combined later)
  year <- seq(from = 1850, to = 2014, by = 1)
  
  #vector output 
  NPP_his = vector(mode = "numeric", length = length(v))
  
  #calculate historical npp
  for(i in 1:length(v)) {
    
    t <- v[i]
    print(paste0("historical ",i))
    
    #get variable at specified time range (12 months)
    variable_his <- ncvar_get(nc_data_1850,"intpp",start= c(1,1,t), count = c(-1,-1, 12)) #units = mol/m2/s
    
    #calculate average NPP for each grid cell over one year
    var_average1 <- apply(variable_his, c(1,2),mean,na.rm=FALSE)*31536000 #convert to mol/m2/yr
    
    #multiply by cell area
    global_npp <- var_average1*area
    
    #sum of all model cells, total global POC flux in Pg C / yr for one year
    sum_npp <- sum(global_npp, na.rm = TRUE)*12.01/1000000000000000
    
    NPP_his[i] = sum_npp
    
  }
  
  #historical vector
  df.his <- qpcR:::cbind.na(year,NPP_his)
  
  ## Calculate Future Globally Integrated POC Flux -------------
  
  #for every 12 time steps, calculate yearly average
  v <- seq(from = 1, to = 1021, by = 12)
  
  year <- seq(from = 2015, to = 2100, by = 1)
  
  #vector output 
  NPP_fut = vector(mode = "numeric", length = length(v))
  
  #calculate future npp
  for(i in 1:length(v)) {
    
    t <- v[i]
    print(paste0("future ",i))
    
    #get variable at specified time range (12 months)
    variable_fut <- ncvar_get(nc_data_2015,"intpp",start= c(1,1,t), count = c(-1,-1, 12)) #units = mol/m2/s
    
    #calculate average NPP for each grid cell over one year
    var_average2 <- apply(variable_fut, c(1,2),mean,na.rm=FALSE)*31536000 #convert to mol/m2/yr
    
    #multiply by cell area
    global_npp <- var_average2*area
    
    #sum of all model cells, total global POC flux in Pg C / yr for one year
    sum_npp <- sum(global_npp, na.rm = TRUE)*12.01/1000000000000000
    
    NPP_fut[i] = sum_npp
    
  }
  
  #future vector
  df.fut <- qpcR:::cbind.na(year,NPP_fut)
  
  time.series <- rbind(df.his,df.fut)
  df = data.frame(time.series)
  #change column names for merging csv files later
  colnames(df) = c('Year',model.name)
  write.csv(df,paste0("~/time_series_analysis/files/NPP/",model.name,"_time_series_intpp.csv"))
  
}