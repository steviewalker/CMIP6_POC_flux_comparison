#' @title Calculate Regridded average NPP (2D intpp file)
#' @author Stevie Walker
#' @date 5/27/24
#' @description calculates historical average (1850-1900), long-term average (2080-2100), and change in NPP
#' @input intpp nc file
#' @output matrices of average NPP

calc_intpp_avg <- function(model.name) {
  
  ## Calculating long-term, historical, and change ---------------
  
  nc.intpp <- list.files(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"), pattern = "intpp")
  
  #open nc file
  setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
  nc_his <- nc_open(nc.intpp[1])
  nc_fut <- nc_open(nc.intpp[2])
  
  ## Calculating long-term average (2080-2100) and change in epc100 -----------
  
  #get variable at specified time range (12 months x 20 years = 240 months, 1023 months - 240 months)
  #starting at Jan 2080
  intpp_lt <- ncvar_get(nc_fut,"intpp",start= c(1,1,781), count = c(-1,-1,240))
  #calculate average POC flux for each grid cell over the years 20800-2100
  intpp_lt <- apply(intpp_lt, c(1,2),mean,na.rm=FALSE)*31536000
  
  #get variable at specified time range (12 months x 50 years = 600 months, aka 600 time count)
  intpp_his <- ncvar_get(nc_his,"intpp",start= c(1,1,1), count = c(-1,-1,600))*31536000
  #calculate average POC flux for each grid cell over the years 1850-1900
  intpp_his <- apply(intpp_his, c(1,2),mean,na.rm=FALSE)
  
  #change in POC flux at 100m
  intpp_change = intpp_lt - intpp_his
  
  #save each matrix as an ascii file
  matrices = list(intpp_lt, intpp_his, intpp_change)
  names = c("intpp_lt", "intpp_his", "intpp_change")
  
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
    
    writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/NPP/", model.name,"_",names[i],"_rg.asc"), varname = "intpp", overwrite = TRUE)
    
  }
  
}

