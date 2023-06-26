#' @title Calculate yearly maximum annual mixed layer depth for historical and future regridded data
#' @author Stevie Walker
#' @date 10/26/21
#' @description finds MLDmax for every grid cell in every year from 1850-2100, to be used in regional time series calculation
#' @input regridded mlotst model file
#' @output 2 large arrays (historical and future) of yearly mixed layer depth, each matrix is one year

rg_time_series_MLDmax <- function(model.name) {
  
  setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
  nc.mld <- list.files(pattern = "mlotst")
  nc_data_1850 <- nc_open(nc.mld[1])
  nc_data_2015 <- nc_open(nc.mld[2])
  
  ## Historical ------------
  
  #for every 12 time steps, calculate yearly average
  v <- seq(from = 1, to = 1969, by = 12)
  
  #create list to store for loop output in
  list_max <- list()
  
  for(i in 1:length(v)) {
    #read in a year of MLD data
    t <- v[i]
    print(t)
    var <- ncvar_get(nc_data_1850,"mlotst",start= c(1,1,t), count = c(-1,-1,12))
    #calculate the MLD max for each year at each lat and lon
    max <- apply(var, c(1,2),max,na.rm=FALSE)
    #store output into the list
    list_max[[i]] <- max
  }
  
  Y2 <- do.call(cbind, list_max)
  his_array <- array(Y2, dim=c(dim(list_max[[1]]), length(list_max)))
  
  setwd('~/regional_time_series_analysis/files/rg_MLDmax/')
  #saving non-averaged historical array for calc_time_series_expc
  saveRDS(his_array, file = paste("rg_",model.name,"_MLD_his_time_series.Rds",sep=""), ascii = TRUE)
  
  
  ## Future ------------
  
  #for every 12 time steps, calculate yearly average
  v <- seq(from = 1, to = 1021, by = 12)
  n = length(v)
  
  #create list to store for loop output in
  list_max <- list()
  
  for(i in 1:n) {
    #read in a year of MLD data
    t <- v[i]
    print(t)
    var <- ncvar_get(nc_data_2015,"mlotst",start= c(1,1,t), count = c(-1,-1,12))
    #calculate the MLD max for each year at each lat and lon
    max <- apply(var, c(1,2),max,na.rm=FALSE)
    #store output into the list
    list_max[[i]] <- max
  }
  
  Y1 <- do.call(cbind, list_max)
  fut_array <- array(Y1, dim=c(dim(list_max[[1]]), length(list_max)))
  
  setwd('~/regional_time_series_analysis/files/rg_MLDmax/')
  #saving non-averaged future array for calc_time_series_expc
  saveRDS(fut_array, file = paste("rg_",model.name,"_MLD_fut_time_series.Rds",sep=""), ascii = TRUE)
  
}
