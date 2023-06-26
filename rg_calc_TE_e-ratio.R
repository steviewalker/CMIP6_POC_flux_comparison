#' @title Calculate the historical, long-term, and change in TE and e-ratio for every model and depth horizon
#' @author Stevie Walker
#' @date 9/10/22

calc_TE <- function(model.name, DH) {
  
  #only calculate long-term TE for EC-Earth
  if(DH == "EZ_depth" && model.name == "EC-Earth") {
    setwd("~/spatial_analysis/raster_output/POC_1000/")
    POC_1000_lt <- raster(paste0(model.name, "_POC_1000_lt_rg.asc"))
    
    setwd(paste0("~/spatial_analysis/raster_output/POC_",DH,"/"))
    POC_lt <- raster(paste0(model.name, "_POC_",DH,"_lt_rg.asc"))
    
    TE_lt = (POC_1000_lt/POC_lt)*100
    #baseline for calculating absolute percent transferred
    TE_lt_baseline = mean(as.matrix(TE_lt),na.rm = TRUE)
    #absolute percent transferred
    TE_lt_absolute = TE_lt-TE_lt_baseline
    
    writeRaster(TE_lt, paste0("~/spatial_analysis/raster_output/TE/", model.name, "_POC_", DH,"_TE_lt_rg.asc"), overwrite = TRUE)
    writeRaster(TE_lt_absolute, paste0("~/spatial_analysis/raster_output/TE/", model.name, "_POC_", DH,"_TE_lt_absolute_rg.asc"), overwrite = TRUE)
    
  } else {
  
  setwd("~/spatial_analysis/raster_output/POC_1000/")
  POC_1000_his <- raster(paste0(model.name, "_POC_1000_his_rg.asc"))
  POC_1000_lt <- raster(paste0(model.name, "_POC_1000_lt_rg.asc"))
  
  setwd(paste0("~/spatial_analysis/raster_output/POC_",DH,"/"))
  #exception for CM4, which used 10% NPP max definition
  if(model.name == "CM4" && DH == "EZ_depth") {
    POC_lt <- raster(paste0(model.name, "_POC_",DH,"_lt_10_rg.asc"))
    POC_his <- raster(paste0(model.name, "_POC_",DH,"_his_10_rg.asc"))
  } else {
    POC_his <- raster(paste0(model.name, "_POC_",DH,"_his_rg.asc"))
    POC_lt <- raster(paste0(model.name, "_POC_",DH,"_lt_rg.asc"))
  }
  
  TE_lt = (POC_1000_lt/POC_lt)*100
  TE_his = (POC_1000_his/POC_his)*100
  
  #percent change in percent transferred
  TE_change = TE_lt - TE_his
  
  #baseline for calculating absolute percent transferred
  TE_lt_baseline = mean(as.matrix(TE_lt),na.rm = TRUE)
  #absolute percent transferred
  TE_lt_absolute = TE_lt-TE_lt_baseline
  #repeat for his
  TE_his_baseline = mean(as.matrix(TE_his),na.rm = TRUE)
  TE_his_absolute = TE_his-TE_his_baseline
  
  TE_absolute_change = TE_lt_absolute - TE_his_absolute
  
  #save percent change in percent transferred rasters
  writeRaster(TE_lt, paste0("~/spatial_analysis/raster_output/TE/", model.name, "_POC_", DH,"_TE_lt_rg.asc"), overwrite = TRUE)
  writeRaster(TE_his, paste0("~/spatial_analysis/raster_output/TE/", model.name, "_POC_", DH,"_TE_his_rg.asc"), overwrite = TRUE)
  writeRaster(TE_change, paste0("~/spatial_analysis/raster_output/TE/", model.name, "_POC_", DH,"_TE_change_rg.asc"), overwrite = TRUE)
  #save absolute percent transferred rasters
  writeRaster(TE_lt_absolute, paste0("~/spatial_analysis/raster_output/TE/", model.name, "_POC_", DH,"_TE_lt_absolute_rg.asc"), overwrite = TRUE)
  writeRaster(TE_his_absolute, paste0("~/spatial_analysis/raster_output/TE/", model.name, "_POC_", DH,"_TE_his_absolute_rg.asc"), overwrite = TRUE)
  writeRaster(TE_absolute_change, paste0("~/spatial_analysis/raster_output/TE/", model.name, "_POC_", DH,"_TE_change_absolute_rg.asc"), overwrite = TRUE)
  }
}


calc_e_ratio <- function(model.name, DH) {
  
  if(model.name == "EC-Earth") {
    
    setwd(paste0("~/spatial_analysis/raster_output/POC_",DH,"/"))
    POC_lt <- raster(paste0(model.name, "_POC_",DH,"_lt_rg.asc"))
    
    setwd("~/spatial_analysis/raster_output/NPP")
    NPP_lt <- raster(paste0(model.name,"_NPP_lt_rg.asc"))
    
    e_ratio_lt = POC_lt/NPP_lt
    
    writeRaster(e_ratio_lt, paste0("~/spatial_analysis/raster_output/e_ratio/", model.name, "_POC_", DH,"_e_ratio_lt_rg.asc"), overwrite = TRUE)
    
  } else {
    
    setwd(paste0("~/spatial_analysis/raster_output/POC_",DH,"/"))
    
    if(model.name == "CM4" && DH == "EZ_depth") {
      POC_his <- raster(paste0(model.name, "_POC_",DH,"_his_10_rg.asc"))
      POC_lt <- raster(paste0(model.name, "_POC_",DH,"_lt_10_rg.asc"))
    } else {
      POC_his <- raster(paste0(model.name, "_POC_",DH,"_his_rg.asc"))
      POC_lt <- raster(paste0(model.name, "_POC_",DH,"_lt_rg.asc"))
    }
   
    setwd("~/spatial_analysis/raster_output/NPP")
    NPP_his <- raster(paste0(model.name,"_NPP_his_rg.asc"))
    NPP_lt <- raster(paste0(model.name,"_NPP_lt_rg.asc"))
    
    e_ratio_lt = POC_lt/NPP_lt
    e_ratio_his = POC_his/NPP_his
    e_ratio_change = e_ratio_lt - e_ratio_his
    
    writeRaster(e_ratio_lt, paste0("~/spatial_analysis/raster_output/e_ratio/", model.name, "_POC_", DH,"_e_ratio_lt_rg.asc"), overwrite = TRUE)
    writeRaster(e_ratio_his, paste0("~/spatial_analysis/raster_output/e_ratio/", model.name, "_POC_", DH,"_e_ratio_his_rg.asc"), overwrite = TRUE)
    writeRaster(e_ratio_change, paste0("~/spatial_analysis/raster_output/e_ratio/", model.name, "_POC_", DH,"_e_ratio_change_rg.asc"), overwrite = TRUE)
  }
}