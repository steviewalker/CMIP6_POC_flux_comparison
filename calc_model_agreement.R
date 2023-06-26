#' @title Calculate model agreement on the sign of POC flux change
#' @author Stevie Walker
#' @date 10/18/22
#' @description calculates whether all models agree on the sign of variable change, creates raster of ones and zeros for where it does and does not agree. Also plots this agreement and calculates the percent of disagreement.

calc_model_agreement <- function(variable) {
  
  if(variable == "all_DH") {
    
    setwd("~/spatial_analysis/raster_output/model_agreement/")
    POC <- list.files(pattern = "POC")
    
    #list to store up/down rasters
    list_rasters <- list()
    for(i in 1:length(POC)) {
      
      r <- raster(POC[i])
      list_rasters[i] = r
    }
    
    all <- stack(list_rasters)
    d <- calc(all, mean, na.rm = TRUE)
    plot(d)
    
    #save raster where 100% of models agree on the sign of variable change
    match_7 = (d>=1)
    match_7[match_7==TRUE] = 1
    match_7[match_7==FALSE] = 0
    plot(match_7)
    
    #calculate percent of raster where models agree
    p <- prop.table(table(as.vector(match_7)))["1"]*100
    print(paste0("For all 5 depth horizons, models agree on the sign of change in ",p,"% of the ocean"))
    
    writeRaster(match_7, paste0("~/spatial_analysis/raster_output/model_agreement/",variable,"_7_model_sign_match.asc"), overwrite = TRUE)
    
    #comparison of each POC flux DH with 100m DH ------------
    } else if(variable == "POC_100_EZ_depth" || variable == "POC_100_MLD_max" || variable == "POC_100_1000" || variable == "POC_100_PCD") {
    
      name = str_split(variable, pattern = "100_")
      
      setwd("~/spatial_analysis/raster_output/model_agreement/")
      POC_var <- raster(list.files(pattern = paste0("POC_",name[[1:2]])))
      POC_100 <- raster("POC_100_7_model_sign_match.asc")
      
      all <- stack(POC_var,POC_100)
      d <- calc(all, mean, na.rm = TRUE)
      plot(d)
      
      #save raster where 100% of models agree on the sign of variable change
      match_7 = (d>=1)
      match_7[match_7==TRUE] = 1
      match_7[match_7==FALSE] = 0
      plot(match_7)
      
      #calculate where 100m and the other DH agree on the sign of change
      p <- prop.table(table(as.vector(match_7)))["1"]*100
      print(paste0("For 100m and ",name[[1:2]],", models agree on the sign of change in ",p,"% of the ocean"))
      
      writeRaster(match_7, paste0("~/spatial_analysis/raster_output/model_agreement/",variable,"_7_model_sign_match.asc"), overwrite = TRUE)
      
  } else if(variable == "POC_shallow_DHs") {
    
    setwd("~/spatial_analysis/raster_output/model_agreement/")
    POC_100 <- raster("POC_100_7_model_sign_match.asc")
    POC_EZ <- raster("POC_EZ_depth_7_model_sign_match.asc")
    POC_PCD <- raster("POC_PCD_7_model_sign_match.asc")
    
      all <- stack(POC_EZ,POC_100, POC_PCD)
    
      d <- calc(all, mean, na.rm = TRUE)
      plot(d)
      
      #save raster where 100% of models agree on the sign of variable change
      match_7 = (d>=1)
      match_7[match_7==TRUE] = 1
      match_7[match_7==FALSE] = 0
      plot(match_7)
      
      #calculate where 100m and the other DH agree on the sign of change
      p <- prop.table(table(as.vector(match_7)))["1"]*100
      print(paste0("For shallow DH's models agree on the sign of change in ",p,"% of the ocean"))
      
      writeRaster(match_7, paste0("~/spatial_analysis/raster_output/model_agreement/",variable,"_7_model_sign_match.asc"), overwrite = TRUE)
      
   } else if(variable == "e_ratio") {
     
     setwd(paste0("~/spatial_analysis/raster_output/",variable))
    #list relevant rasters and take out multimodel raster
    files <- list.files(pattern = "POC_100_e_ratio_change", full.names = TRUE)
    files <- Filter(function(x) !any(grepl("multimodel", x)), files)
    
    #list to store up/down rasters
    list_up_down <- list()
    
    #if value is positive, assign 1, if negative, assign -1 to the new raster
    for(i in 1:length(files)) {
      
      model <- raster(files[i])
      
      b = (model>0)
      b[b==TRUE] = 1
      b[b==FALSE] = -1
      
      list_up_down[i] = b
    }
    
    #average the 7 models
    matrices <- stack(list_up_down)
    d <- calc(matrices, mean, na.rm = TRUE)
    plot(d)
    
    percent_agreement = abs(d)
    plot(percent_agreement)
    
    #save raster where 100% of models agree on the sign of variable change
    match_7 = (percent_agreement>=1)
    match_7[match_7==TRUE] = 1
    match_7[match_7==FALSE] = 0
    plot(match_7)
    
    #calculate percent of raster where models don't agree
    p <- prop.table(table(as.vector(match_7)))["1"]*100
    print(paste0("For variable ",variable," models agree on the sign of change in ",p,"% of the ocean"))
    
    writeRaster(match_7, paste0("~/spatial_analysis/raster_output/model_agreement/",variable,"_7_model_sign_match.asc"), overwrite = TRUE)

   } else if (variable == "TE") {
     
     setwd(paste0("~/spatial_analysis/raster_output/",variable))
     #list relevant rasters and take out multimodel raster
     files <- list.files(pattern = "POC_100_TE_change_rg", full.names = TRUE)
     files <- Filter(function(x) !any(grepl("multimodel", x)), files)
     
     #list to store up/down rasters
     list_up_down <- list()
     
     #if value is positive, assign 1, if negative, assign -1 to the new raster
     for(i in 1:length(files)) {
       
       model <- raster(files[i])
       
       b = (model>0)
       b[b==TRUE] = 1
       b[b==FALSE] = -1
       
       list_up_down[i] = b
     }
     
     #average the 7 models
     matrices <- stack(list_up_down)
     d <- calc(matrices, mean, na.rm = TRUE)
     plot(d)
     
     percent_agreement = abs(d)
     plot(percent_agreement)
     
     #save raster where 100% of models agree on the sign of variable change
     match_7 = (percent_agreement>=1)
     match_7[match_7==TRUE] = 1
     match_7[match_7==FALSE] = 0
     plot(match_7)
     
     #calculate percent of raster where models don't agree
     p <- prop.table(table(as.vector(match_7)))["1"]*100
     print(paste0("For variable ",variable," models agree on the sign of change in ",p,"% of the ocean"))
     
     writeRaster(match_7, paste0("~/spatial_analysis/raster_output/model_agreement/",variable,"_7_model_sign_match.asc"), overwrite = TRUE)
     
   } else {
    
    setwd(paste0("~/spatial_analysis/raster_output/",variable))
    #list relevant rasters and take out multimodel raster
    files <- list.files(pattern = "change", full.names = TRUE)
    files <- Filter(function(x) !any(grepl("multimodel", x)), files)
    
    if(variable == "POC_EZ_depth" || variable == "EZ_depth") {
      files <- Filter(function(x) !any(grepl("change_5", x)), files)
      files <- Filter(function(x) !any(grepl("/CM4_POC_EZ_depth_change_rg", x)), files)
    } else {}
    
    #list to store up/down rasters
    list_up_down <- list()
    
    #if value is positive, assign 1, if negative, assign -1 to the new raster
    for(i in 1:length(files)) {
      
      model <- raster(files[i])
      
      b = (model>0)
      b[b==TRUE] = 1
      b[b==FALSE] = -1
      
      list_up_down[i] = b
    }
    
    #average the 7 models
    matrices <- stack(list_up_down)
    d <- calc(matrices, mean, na.rm = TRUE)
    plot(d)
    
    percent_agreement = abs(d)
    plot(percent_agreement)
    
    #save raster where 100% of models agree on the sign of variable change
    match_7 = (percent_agreement>=1)
    match_7[match_7==TRUE] = 1
    match_7[match_7==FALSE] = 0
    plot(match_7)
    
    #calculate percent of raster where models don't agree
    p <- prop.table(table(as.vector(match_7)))["1"]*100
    print(paste0("For variable ",variable," models agree on the sign of change in ",p,"% of the ocean"))
    
    writeRaster(match_7, paste0("~/spatial_analysis/raster_output/model_agreement/",variable,"_7_model_sign_match.asc"), overwrite = TRUE)
    
  }
  # plotting model agreement ------------
  
  #convert raster to a 'conventional' dataframe
  points <- rasterToPoints(match_7, spatial = TRUE)
  df_sign  <- data.frame(points)
  df_sign = subset(df_sign, select = -c(4))
  colnames(df_sign) = c("value", "x", "y")
  
  #plot where all 7 models agree on the sign of POC flux change
  fig_sign <- ggplot() +
    geom_raster(data = df_sign , aes(x = x, y = y, fill = value)) +
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = paste0("Model agreement on the sign of ", variable, " change"),
         subtitle = "Black regions = agreement") +
    scale_fill_cmocean(limits = c(0,1), oob = squish, name = "deep", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "none")
  
  fig_sign
  
  ggsave(paste(variable,"_sign_of_change_agreement.png",sep=""), plot = fig_sign, path = "~/spatial_analysis/figures/model_agreement/", width = 26, height = 15, units = "cm", dpi = 400)
  
}



