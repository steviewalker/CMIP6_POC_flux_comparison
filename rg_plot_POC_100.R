#plotting ------------

plot_POC_100 <- function(model.name) {
  
  change = raster(paste0("~/spatial_analysis/raster_output/POC_100/",model.name,"_POC_100_change_rg.asc"))
  his = raster(paste0("~/spatial_analysis/raster_output/POC_100/",model.name,"_POC_100_his_rg.asc"))
  lt = raster(paste0("~/spatial_analysis/raster_output/POC_100/",model.name,"_POC_100_lt_rg.asc"))
  
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
    labs(title = paste0(model.name, " Historical (1850-1900) POC flux at 100m"),
         fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
    scale_fill_cmocean(limits = c(0,5), oob = squish, name = "deep", direction = 1) +
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
    labs(title = paste0(model.name, " Long-term (2080-2100) POC flux at 100m"),
         fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
    #POC flux
    scale_fill_cmocean(limits = c(0,5), oob = squish, name = "deep", direction = 1) +
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
    labs(title = paste0(model.name, " Change in POC flux at 100m"),
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
    
    ggsave(paste(model.name,"_POC_100_his.png",sep=""), plot = fig_his, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
    ggsave(paste(model.name,"_POC_100_lt.png",sep=""), plot = fig_lt, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
    ggsave(paste(model.name,"_POC_100_change.png",sep=""), plot = fig_change, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
    
  } else {
    model.name = model.name
  }
  
  ggsave(paste(model.name,"_epc100_avg_map.png",sep=""), plot = figure, path = "~/spatial_analysis/figures/POC_100/", width = 22, height = 30, units = "cm", dpi = 400)
  
}


