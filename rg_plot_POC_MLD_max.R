#plotting ------------

plot_POC_MLD_max <- function(model.name) {
  

  change = raster(paste0("~/spatial_analysis/raster_output/POC_MLD_max/",model.name,"_POC_MLD_max_change_rg.asc"))
  his = raster(paste0("~/spatial_analysis/raster_output/POC_MLD_max/",model.name,"_POC_MLD_max_his_rg.asc"))
  lt = raster(paste0("~/spatial_analysis/raster_output/POC_MLD_max/",model.name,"_POC_MLD_max_lt_rg.asc"))
  

  #convert raster to a 'conventional' dataframe
  geo <- rasterToPoints(his, spatial = TRUE)
  df  <- data.frame(geo)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  #edit multimodel name for nice plotting label
  if(model.name == "multimodel") {
    model.name = "Multi-model"
  } else {
    model.name = model.name
  }
  
  fig_his <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = paste0(model.name, " Historical (1850-1900) POC flux at MLDmax"),
         fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
    scale_fill_cmocean(limits = c(0,4), oob = squish, name = "deep", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  fig_his
  
  #convert raster to a 'conventional' dataframe
  geo <- rasterToPoints(lt, spatial = TRUE)
  df  <- data.frame(geo)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  fig_lt <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = paste0(model.name, " Long-term (2080-2100) POC flux at MLDmax"),
         fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
    #POC flux
    scale_fill_cmocean(limits = c(0,4), oob = squish, name = "deep", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  fig_lt
  
  #convert raster to a 'conventional' dataframe
  geo <- rasterToPoints(change, spatial = TRUE)
  df  <- data.frame(geo)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  fig_change <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = paste0(model.name, " Change in POC flux at MLDmax"),
         fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
    scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  fig_change
  
  figure <- grid.arrange(fig_his, fig_lt, fig_change,ncol = 1, nrow = 3)
  
  #change multimodel name back for saving and save individual multi-model panels
  if(model.name == "Multi-model") {
    model.name = "multimodel"
    
    ggsave(paste(model.name,"_POC_MLD_max_his.png",sep=""), plot = fig_his, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
    ggsave(paste(model.name,"_POC_MLD_max_lt.png",sep=""), plot = fig_lt, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
    ggsave(paste(model.name,"_POC_MLD_max_change.png",sep=""), plot = fig_change, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
    
  } else {
    model.name = model.name
  }
  
  ggsave(paste(model.name,"_expc_MLD_max_avg_map.png",sep=""), plot = figure, path = "~/spatial_analysis/figures/POC_MLD_max/", width = 22, height = 30, units = "cm", dpi = 400)
  
}


#with hatching -------
plot_POC_MLD_max_hatch <- function(model.name) {
  
  #download/load land shapefile
  setwd("/tmp/")
  test.file <- file.exists("RtmpyMdpTN")  
  if(test.file == FALSE) {
    land <- ne_download(scale = 50, type = 'land', category = 'physical')
    world <- ne_load(scale = 50, type = 'land', category = 'physical', returnclass = "sf")
  } else {
    world <- ne_load(scale = 50, type = 'land', category = 'physical', returnclass = "sf") }
  
  change = raster("~/spatial_analysis/raster_output/POC_MLD_max/multimodel_POC_MLD_max_change_rg.asc")
  sign = raster("~/spatial_analysis/raster_output/model_agreement/POC_MLD_max_7_model_sign_match.asc")
  
  #convert raster to a 'conventional' dataframe
  points <- rasterToPoints(sign, spatial = TRUE)
  df_sign  <- data.frame(points)
  df_sign = subset(df_sign, select = -c(4))
  colnames(df_sign) = c("value", "x", "y")

  
  #plot where all 7 models agree on the sign of POC flux change
  fig_sign <- ggplot() +
    geom_raster(data = df_sign , aes(x = x, y = y, fill = value)) +
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = "Model agreement on the sign of POC flux change at MLDmax") +
    scale_fill_cmocean(limits = c(0,1), oob = squish, name = "deep", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  fig_sign
  
  #convert raster to a 'conventional' dataframe
  geo <- rasterToPoints(change, spatial = TRUE)
  df  <- data.frame(geo)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  #plot change in POC flux
  fig_change <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = paste0("Multimodel Average Change in POC flux at MLDmax"),
         fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
    scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  fig_change
  
  ggsave(paste(model.name,"_POC_MLD_max_change.png",sep=""), plot = fig_change, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
  
}

