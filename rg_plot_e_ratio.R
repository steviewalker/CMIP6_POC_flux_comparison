plot_e_ratio <- function(model.name, DH) {
  
  if(model.name == "EC-Earth") {
    
    lt = raster(paste0("~/spatial_analysis/raster_output/e_ratio/EC-Earth_POC_", DH,"_e_ratio_lt_rg.asc"))
    
    #convert raster to a 'conventional' dataframe
    geo <- rasterToPoints(lt, spatial = TRUE)
    df  <- data.frame(geo)
    df = subset(df, select = -c(4))
    colnames(df) = c("value", "x", "y")
    
    DH_title <- 
      if(DH == "100") {
        DH_title = "e-ratio at 100m"
      } else if(DH == "MLD_max") {
        DH_title = "e-ratio at MLDmax"
      } else if(DH == "EZ_depth") {
        DH_title = "e-ratio at EZ depth"
      } else if( DH == "PCD") {
        DH_title = "e-ratio at PCD"
      } else {
        DH_title = "e-ratio at 1000m"
      }
    
    fig_lt <- ggplot() +
      geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
      geom_sf(data = world, fill = "snow") +
      coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
      labs(title = paste0("EC-Earth future (2080-2100) ",DH_title),
           fill = "e-ratio") +
      scale_fill_cmocean(limits = c(0,0.6), oob = squish, name = "deep", direction = 1) +
      scale_y_continuous(breaks = seq(-80,80, by = 20)) +
      scale_x_continuous(breaks = seq(-180,180, by = 30)) +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_bw() +
      theme(panel.grid = element_blank())
    
    if(DH == 1000) {
      fig_lt = fig_lt + scale_fill_cmocean(limits = c(0,0.1), oob = squish, name = "deep", direction = 1)
    } else {}
    
    fig_lt
    
    ggsave(paste(model.name,"_", DH, "_e_ratio_lt.png",sep=""), plot = fig_lt, path = "~/spatial_analysis/figures/e_ratio/", width = 28, height = 15, units = "cm", dpi = 400)
    
  } else {
    
  change = raster(paste0("~/spatial_analysis/raster_output/e_ratio/",model.name,"_POC_", DH,"_e_ratio_change_rg.asc"))
  his = raster(paste0("~/spatial_analysis/raster_output/e_ratio/",model.name,"_POC_", DH,"_e_ratio_his_rg.asc"))
  lt = raster(paste0("~/spatial_analysis/raster_output/e_ratio/",model.name,"_POC_", DH,"_e_ratio_his_rg.asc"))
  
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
  
  DH_title <- 
    if(DH == "100") {
      DH_title = "e-ratio at 100m"
    } else if(DH == "MLD_max") {
      DH_title = "e-ratio at MLDmax"
    } else if(DH == "EZ_depth") {
      DH_title = "e-ratio at EZ depth"
    } else if( DH == "PCD") {
      DH_title = "e-ratio at PCD"
    } else {
      DH_title = "e-ratio at 1000m"
    }
  
  fig_his <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = paste0(model.name, " historical (1850-1900) ", DH_title),
         fill = "e-ratio") +
    scale_fill_cmocean(limits = c(0,0.6), oob = squish, name = "deep", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  if(DH == 1000) {
  fig_his = fig_his + scale_fill_cmocean(limits = c(0,0.1), oob = squish, name = "deep", direction = 1)
  } else {}
  
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
    labs(title = paste0(model.name, " future (2080-2100) ", DH_title),
         fill = "e-ratio") +
    scale_fill_cmocean(limits = c(0,0.6), oob = squish, name = "deep", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  if(DH == 1000) {
    fig_lt = fig_lt + scale_fill_cmocean(limits = c(0,0.1), oob = squish, name = "deep", direction = 1)
  } else {}

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
    labs(title = paste0(model.name, " change in ", DH_title),
         fill = "e-ratio") +
    scale_fill_cmocean(limits = c(-0.2,0.2), oob = squish, name = "balance", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
 
  if(DH == 1000) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(-0.02,0.02), oob = squish, name = "balance", direction = 1)
  } else {}
   
  fig_change
  
  figure <- grid.arrange(fig_his, fig_lt, fig_change,ncol = 1, nrow = 3)
  
  #change multimodel name back for saving and save individual multi-model panels
  if(model.name == "Multi-model") {
    model.name = "multimodel"
    
    ggsave(paste(model.name,"_", DH, "_e_ratio_his.png",sep=""), plot = fig_his, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
    ggsave(paste(model.name,"_", DH, "_e_ratio_lt.png",sep=""), plot = fig_lt, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
    ggsave(paste(model.name,"_", DH, "_e_ratio_change.png",sep=""), plot = fig_change, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
    
  } else {
    model.name = model.name
  }
  
  ggsave(paste(model.name,"_", DH, "_e_ratio_avg_map.png",sep=""), plot = figure, path = "~/spatial_analysis/figures/e_ratio/", width = 22, height = 30, units = "cm", dpi = 400)
  
  }
}

