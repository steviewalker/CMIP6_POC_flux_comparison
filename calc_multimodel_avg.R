#' @title Calculate multimodel average for any variable
#' @author Stevie Walker
#' @date 8/10/22
#' @notes make sure the variable matches the format in the folder and asc file name (ex. MLD_max not MLDmax), varname is the variable name in the original nc file
#' @notes uncomment/comment lines for 5% metric variables

# calculate multimodel average ---------

calc_multimodel_avg <- function(variable, varname) {
  
  change <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0("_",variable, "_change_rg.asc"))
  #exception for CM4, which uses 10% NPP max definition
  if(variable == "POC_EZ_depth") {
    replace(change,2,"/home/atlantis/spatial_analysis/raster_output/POC_EZ_depth/CM4_POC_EZ_depth_change_10_rg.asc")
  } else {}
  all_files <- stack(change)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_",variable,"_change_rg.asc"), varname = varname, overwrite = TRUE)
  
  his <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0("_",variable, "_his_rg.asc"))
  if(variable == "POC_EZ_depth") {
    replace(his,2,"/home/atlantis/spatial_analysis/raster_output/POC_EZ_depth/CM4_POC_EZ_depth_his_10_rg.asc")
  } else {}
  all_files <- stack(his)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_",variable,"_his_rg.asc"), varname = varname, overwrite = TRUE)
  
  lt <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0("_",variable, "_lt_rg.asc"))
  if(variable == "POC_EZ_depth") {
    replace(lt,2,"/home/atlantis/spatial_analysis/raster_output/POC_EZ_depth/CM4_POC_EZ_depth_lt_10_rg.asc")
  } else {}
  all_files <- stack(lt)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_",variable,"_lt_rg.asc"), varname = varname, overwrite = TRUE)
  
}

#for TE and e-ratio
calc_multimodel_avg_2 <- function(DH,variable) {
  
  change <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0(DH,"_",variable, "_change_rg.asc"))
  all_files <- stack(change)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_POC_",DH,"_",variable,"_change_rg.asc"), varname = variable, overwrite = TRUE)
  
  change <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0(DH,"_",variable, "_his_rg.asc"))
  all_files <- stack(change)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_POC_",DH,"_",variable,"_his_rg.asc"), varname = variable, overwrite = TRUE)
  
  change <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0(DH,"_",variable, "_lt_rg.asc"))
  all_files <- stack(change)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_POC_",DH,"_",variable,"_lt_rg.asc"), varname = variable, overwrite = TRUE)
  
}

#for POC at ez depth (5% NPP max)
calc_multimodel_avg_5 <- function(variable, varname) {
  
  change <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0("_",variable, "_change_5_rg.asc"))
  all_files <- stack(change)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_",variable,"_change_5_rg.asc"), varname = varname, overwrite = TRUE)
  
  his <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0("_",variable, "_his_5_rg.asc"))
  all_files <- stack(his)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_",variable,"_his_5_rg.asc"), varname = varname, overwrite = TRUE)
  
  lt <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0("_",variable, "_lt_5_rg.asc"))
  all_files <- stack(lt)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_",variable,"_lt_5_rg.asc"), varname = varname, overwrite = TRUE)
  
}

#for absolute TE and e-ratio (not used in final analysis)
calc_multimodel_avg_absolute <- function(DH,variable) {
  
  change <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0(DH,"_",variable, "_change_absolute_rg.asc"))
  all_files <- stack(change)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_POC_",DH,"_",variable,"_change_absolute_rg.asc"), varname = variable, overwrite = TRUE)
  
  change <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0(DH,"_",variable, "_his_absolute_rg.asc"))
  all_files <- stack(change)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_POC_",DH,"_",variable,"_his_absolute_rg.asc"), varname = variable, overwrite = TRUE)
  
  change <- list.files(paste0("~/spatial_analysis/raster_output/",variable), full.names = TRUE, pattern = paste0(DH,"_",variable, "_lt_absolute_rg.asc"))
  all_files <- stack(change)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_POC_",DH,"_",variable,"_lt_absolute_rg.asc"), varname = variable, overwrite = TRUE)
  
}


calc_multimodel_avg_NPP <- function(variable) {
  
  change <- list.files(paste0("~/spatial_analysis/raster_output/NPP"), full.names = TRUE, pattern = paste0("_",variable, "_change_rg.asc"))
  #exception for CM4, which uses 10% NPP max definition
  all_files <- stack(change)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/NPP/multimodel_",variable,"_change_rg.asc"), varname = varname, overwrite = TRUE)
  
  his <- list.files(paste0("~/spatial_analysis/raster_output/NPP"), full.names = TRUE, pattern = paste0("_",variable, "_his_rg.asc"))
  all_files <- stack(his)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/NPP/multimodel_",variable,"_his_rg.asc"), varname = varname, overwrite = TRUE)
  
  lt <- list.files(paste0("~/spatial_analysis/raster_output/NPP"), full.names = TRUE, pattern = paste0("_",variable, "_lt_rg.asc"))
  all_files <- stack(lt)
  multimodel_mean <- calc(all_files, mean, na.rm = TRUE)
  writeRaster(multimodel_mean, filename = paste0("~/spatial_analysis/raster_output/NPP/multimodel_",variable,"_lt_rg.asc"), varname = varname, overwrite = TRUE)
}