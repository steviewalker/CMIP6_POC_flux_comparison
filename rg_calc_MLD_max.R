#' @title Calculate maximum annual mixed layer depth
#' @author Stevie Walker
#' @date 7/20/21
#' @description finds the 20 year short-term average MLDmax, 20 year long-term average MLDmax, and the change between the 2 for ESM data
#' @description second function finds historical average MLDmax

calc_MLD_max <- function(model.name) {
  
  nc.mld <- list.files(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"), pattern = "mlotst")
  
  setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
  nc_his <- nc_open(nc.mld[1])
  nc_fut <- nc_open(nc.mld[2])
  
  ## Calculate historical MLDmax (1850-1900) --------------
  
  #historical 50 yr sequence
  v <- seq(from = 1, to = 589, by = 12)
  
  #create list to store for loop output in
  list_max <- list()
  
  for(i in 1:length(v)) {
    #read in a year of MLD data
    t <- v[i]
    var <- ncvar_get(nc_his,"mlotst",start= c(1,1,t), count = c(-1,-1,12))
    #calculate the MLD max for each year at each lat and lon
    max <- apply(var, c(1,2),max,na.rm=FALSE)
    #store output into the list
    list_max[[i]] <- max
  }
  
  ls_his <- do.call(cbind, list_max)
  ls_his <- array(ls_his, dim=c(dim(list_max[[1]]), length(list_max)))
  
  #historical 50 year MLD max average
  MLD_max_his <- apply(ls_his, c(1, 2), mean, na.rm = FALSE)
  
  ## Calculate average MLDmax for 2080-2100 ----------
  
  #long term sequence
  v <- seq(from = 781, to = 1009, by = 12)
  
  #create list to store for loop output in
  list_max <- list()
  
  for(i in 1:length(v)) {
    #read in a year of MLD data
    t <- v[i]
    var <- ncvar_get(nc_fut,"mlotst",start= c(1,1,t), count = c(-1,-1,12))
    #calculate the MLD max for each year at each lat and lon
    max <- apply(var, c(1,2),max,na.rm=FALSE)
    #store output into the list
    list_max[[i]] <- max
  }
  
  ls_lt <- do.call(cbind, list_max)
  ls_lt <- array(ls_lt, dim=c(dim(list_max[[1]]), length(list_max)))
  
  #20 year avg long-term MLDmax
  MLD_max_lt <- apply(ls_lt, c(1, 2), mean, na.rm = FALSE)
  
  #difference
  MLD_change = MLD_max_lt - MLD_max_his
  
  #saving non-averaged arrays for calc_expc_avg
  saveRDS(ls_his, file = paste0("~/spatial_analysis/raster_output/MLD_max/",model.name,"_array_MLD_max_his.Rds"), ascii = TRUE)
  saveRDS(ls_lt, file = paste0("~/spatial_analysis/raster_output/MLD_max/",model.name,"_array_MLD_max_lt.Rds"), ascii = TRUE)
  
  #save each matrix as an ascii file
  matrices = list(MLD_max_lt, MLD_max_his, MLD_change)
  names = c("MLD_max_lt", "MLD_max_his", "MLD_max_change")
  
  for(i in 1:length(matrices)) {
    
    #convert to rasters (formatted for plotting)
    matrix <- matrices[[i]]
    
    dimnames(matrix) = list(lon = 1:360,lat = 1:180)
    old.df = melt(matrix)
    
    #correct lat and lon scales
    old.df$lon <- ifelse(old.df$lon>180,
                         old.df$lon-360,
                         old.df$lon)
    old.df$lat <- ifelse(old.df$lat<90,
                         old.df$lat-90,
                         old.df$lat-90)
    
    r = rasterFromXYZ(old.df, crs = "+proj=longlat +datum=WGS84 +no_defs")
    #example plot
    plot(r)
    
    writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/MLD_max/", model.name,"_",names[i],"_rg.asc"), varname = "mlotst", overwrite = TRUE)
    
  }
} 