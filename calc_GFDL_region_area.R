#' @title Calculate regridded region area 
#' @author Stevie Walker
#' @date 5/2/23
#' @description GFDL region areas


## Calculate area just for GFDL ------------- 

#read in expc file to get where cells are NA
setwd(paste0("~/spatial_analysis/regridded_nc_files/GFDL_rg/"))
#nc.expc <- list.files(pattern = "expc")
#nc_his <- nc_open(nc.expc[1])
#expc <- ncvar_get(nc_his,"expc",start= c(1,1,1,1), count = c(-1,-1,-1,1))*31536000
#pull out surface layer
#expc_surface <- expc[,,1]
#switch all values to one for multiplying by area matrix in next step
#expc_surface_na <- ifelse(is.na(expc_surface) == FALSE,
#                          1,
#                         NA)
area_file <- list.files(pattern = "areacello")
nc_data_area <- nc_open(area_file)
area <- ncvar_get(nc_data_area, "areacello")

regions = c("15_low_lats", "30_low_lats", "EQ_Pacific", "global" ,"low_lats_no_EQ_Pacific", "North_Atlantic", "SO_50", "SO_60")

areas = vector(mode = "numeric", length = 8)

for(k in 1:length(regions)) {
  
  #Southern Ocean S of 50
  if(k == 7) {
    area_subset = area[,1:40]
    #na_map = expc_surface_na[,1:40]
    #Southern Ocean S of 60
  } else if(k == 8){
    area_subset = area[,1:30]
    #na_map = expc_surface_na[,1:30]
    #low latitudes
  } else if(k == 2){
    area_subset = area[,61:120]
    #na_map = expc_surface_na[,61:120]
    #Equatorial Pacific
  } else if(k == 3) {
    area_subset = area[160:285,76:106] #15S to 15N, 160E to 75W 
    #make values in the Caribbean Sea NA
    area_subset[118:126,25:31] = NA
    #na_map = expc_surface_na[160:285,76:106]
    #na_map[118:126,25:31] = NA
    #low latitudes without Equatorial Pacific
  } else if (k == 5) {
    area_subset = area[,61:120] #30S to 30N
    area_subset[160:285,15:45] = NA
    #na_map = expc_surface_na[,61:120]
    #na_map[160:285,15:45] = NA
    #North Atlantic
  } else if (k == 6) {
    area_subset = area[290:360,141:180] #40N and higher, 70W to 0W
    #na_map = expc_surface_na[290:360,141:180]
  } else if(k == 1){
    #Low Latitudes
    area_subset = area[,76:105] #15S-15N
    #na_map = expc_surface_na[,76:105] #15S-15N
  } else {
    area_subset = area
    #na_map = expc_surface_na
  }
  
  #multiply matrices to include land areas (NA values) in region area
  area_final = area_subset
  
  #double check range (won't work when you run for loop, i don't know why)
  # a <- raster(area_final)
  # plot(a)
  
  #sum of area subset (m2)
  area_sum <- sum(area_final, na.rm = TRUE)
  
  areas[k] = area_sum 
  
}

areas = areas %>% 
  as_tibble() 
colnames(areas) = "Area"
areas$Region = regions
areas <- areas %>% 
  relocate(Region, .before = Area)
write_csv(areas, file = "~/regional_time_series_analysis/files/region_area/GFDL_region_areas.csv")

