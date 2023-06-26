#' @title Calculate Regridded average POC flux at 100m
#' @author Stevie Walker
#' @date 7/14/21
#' @description calculates historical average (1850-1900), long-term average (2080-2100), and change in POC flux at 100m
#' @input epc100 nc file
#' @output matrices of average POC flux at 100m
#' @source used code from code from https://www.researchgate.net/post/How-can-I-extract-a-time-series-of-variable-for-a-specific-location-specific-longitude-and-latitude-from-a-CMIP5-experiment-netCDF-data-set

calc_epc100_avg <- function(model.name) {

## Calculating long-term, historical, and change ---------------

nc.epc100 <- list.files(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"), pattern = "epc100")
  
#open nc file
setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
nc_his <- nc_open(nc.epc100[1])
nc_fut <- nc_open(nc.epc100[2])

## Calculating long-term average (2080-2100) and change in epc100 -----------

#get variable at specified time range (12 months x 20 years = 240 months, 1023 months - 240 months)
#starting at Jan 2080
epc100_lt <- ncvar_get(nc_fut,"epc100",start= c(1,1,781), count = c(-1,-1,240))
#calculate average POC flux for each grid cell over the years 20800-2100
epc100_lt <- apply(epc100_lt, c(1,2),mean,na.rm=FALSE)*31536000

#get variable at specified time range (12 months x 50 years = 600 months, aka 600 time count)
epc100_his <- ncvar_get(nc_his,"epc100",start= c(1,1,1), count = c(-1,-1,600))*31536000
#calculate average POC flux for each grid cell over the years 1850-1900
epc100_his <- apply(epc100_his, c(1,2),mean,na.rm=FALSE)

#change in POC flux at 100m
epc100_change = epc100_lt - epc100_his

#save each matrix as an ascii file
matrices = list(epc100_lt, epc100_his, epc100_change)
names = c("POC_100_lt", "POC_100_his", "POC_100_change")

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

writeRaster(r, filename = paste0("~/spatial_analysis/raster_output/POC_100/", model.name,"_",names[i],"_rg.asc"), varname = "epc100", overwrite = TRUE)

}

}

