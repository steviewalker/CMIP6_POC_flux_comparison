#' @title Plot average POC flux at EZ depth
#' @author Stevie Walker
#' @date 8/10/22
#' @description plots the historical and long-term average in POC flux at different EZ depth metrics

#plotting ------------

plot_POC_EZ_depth <- function(model.name, ez.metric) {
  
  if(ez.metric == 1) {
    subtitle = "EZ Depth = 1% of Max NPP"
  } else if (ez.metric == 5) {
    subtitle = "EZ Depth = 5% of Max NPP"
  } else {
    subtitle = "EZ Depth = 10% of Max NPP"
  }
  
  if(model.name == "EC-Earth") {
    
    if(ez.metric == 1) {
      lt = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_lt_rg.asc"))
    } else if (ez.metric == 5) {
      lt = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_lt_5_rg.asc"))
    } else {
      lt = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_lt_10_rg.asc"))
    }
    
    #convert raster to a 'conventional' dataframe
    geo <- rasterToPoints(lt, spatial = TRUE)
    df  <- data.frame(geo)
    df = subset(df, select = -c(4))
    colnames(df) = c("value", "x", "y")
    
    fig_lt <- ggplot() +
      geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
      geom_sf(data = world, fill = "snow") +
      coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
      labs(title = paste0(model.name, " Long-term (2080-2100) POC flux at EZ Depth"),
           subtitle = subtitle,
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
    
    if(ez.metric == 1) {
      ggsave(paste(model.name,"_POC_EZ_depth_lt.png",sep=""), plot = fig_lt, path = "~/spatial_analysis/figures/POC_EZ_depth/", width = 28, height = 15, units = "cm", dpi = 400)
    } else if(ez.metric == 5) {
      ggsave(paste(model.name,"_POC_EZ_depth_5_lt.png",sep=""), plot = fig_lt, path = "~/spatial_analysis/figures/POC_EZ_depth/", width = 28, height = 15, units = "cm", dpi = 400)
    } else {
      ggsave(paste(model.name,"_POC_EZ_depth_10_lt.png",sep=""), plot = fig_lt, path = "~/spatial_analysis/figures/POC_EZ_depth/", width = 28, height = 15, units = "cm", dpi = 400)
    }
    
  } else {
    
    if(ez.metric == 1) {
      change = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_change_rg.asc"))
      his = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_his_rg.asc"))
      lt = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_lt_rg.asc"))
    } else if(ez.metric == 5) {
      change = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_change_5_rg.asc"))
      his = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_his_5_rg.asc"))
      lt = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_lt_5_rg.asc"))
    } else {
      change = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_change_10_rg.asc"))
      his = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_his_10_rg.asc"))
      lt = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_lt_10_rg.asc"))
    }
    
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
      labs(title = paste0(model.name, " Historical (1850-1900) POC flux at EZ Depth"),
           subtitle = subtitle,
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
      labs(title = paste0(model.name, " Long-term (2080-2100) POC flux at EZ Depth"),
           subtitle = subtitle,
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
      labs(title = paste0(model.name, " Change in POC flux at EZ Depth"),
           subtitle = subtitle,
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
      if(ez.metric == 1) {
        ggsave(paste(model.name,"_POC_EZ_depth_his.png",sep=""), plot = fig_his, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
        ggsave(paste(model.name,"_POC_EZ_depth_lt.png",sep=""), plot = fig_lt, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
        ggsave(paste(model.name,"_POC_EZ_depth_change.png",sep=""), plot = fig_change, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
        ggsave(paste(model.name,"_expc_EZ_depth_avg_map.png",sep=""), plot = figure, path = "~/spatial_analysis/figures/POC_EZ_depth/", width = 22, height = 30, units = "cm", dpi = 400)
        } else if (ez.metric == 5) {
        ggsave(paste(model.name,"_POC_EZ_depth_5_his.png",sep=""), plot = fig_his, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
        ggsave(paste(model.name,"_POC_EZ_depth_5_lt.png",sep=""), plot = fig_lt, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
        ggsave(paste(model.name,"_POC_EZ_depth_5_change.png",sep=""), plot = fig_change, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
        ggsave(paste(model.name,"_expc_EZ_depth_5_avg_map.png",sep=""), plot = figure, path = "~/spatial_analysis/figures/POC_EZ_depth/", width = 22, height = 30, units = "cm", dpi = 400)
        } else {
          ggsave(paste(model.name,"_POC_EZ_depth_10_his.png",sep=""), plot = fig_his, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
          ggsave(paste(model.name,"_POC_EZ_depth_10_lt.png",sep=""), plot = fig_lt, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
          ggsave(paste(model.name,"_POC_EZ_depth_10_change.png",sep=""), plot = fig_change, path = "~/spatial_analysis/figures/multimodel/", width = 28, height = 15, units = "cm", dpi = 400)
          ggsave(paste(model.name,"_expc_EZ_depth_10_avg_map.png",sep=""), plot = figure, path = "~/spatial_analysis/figures/POC_EZ_depth/", width = 22, height = 30, units = "cm", dpi = 400)
        }
    } else { 
      if(ez.metric == 1) {
        ggsave(paste(model.name,"_expc_EZ_depth_avg_map.png",sep=""), plot = figure, path = "~/spatial_analysis/figures/POC_EZ_depth/", width = 22, height = 30, units = "cm", dpi = 400)
      } else if (ez.metric == 5) {
        ggsave(paste(model.name,"_expc_EZ_depth_5_avg_map.png",sep=""), plot = figure, path = "~/spatial_analysis/figures/POC_EZ_depth/", width = 22, height = 30, units = "cm", dpi = 400)
      } else {
        ggsave(paste(model.name,"_expc_EZ_depth_10_avg_map.png",sep=""), plot = figure, path = "~/spatial_analysis/figures/POC_EZ_depth/", width = 22, height = 30, units = "cm", dpi = 400)
      }
    }
  }
}

plot_metric_diff_POC_EZ <- function(model.name) {
  
  if(model.name == "EC-Earth") {
    
    lt_diff = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth_sensitivity/",model.name,"_POC_EZ_depth_sensitivity_lt_rg.asc"))
    
    #convert raster to a 'conventional' dataframe
    geo <- rasterToPoints(lt_diff, spatial = TRUE)
    df  <- data.frame(geo)
    df = subset(df, select = -c(4))
    colnames(df) = c("value", "x", "y")
    
    fig_lt <- ggplot() +
      geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
      geom_sf(data = world, fill = "snow") +
      coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
      labs(title = paste0(model.name, " difference in long-term (2080-2100) POC EZ depth metric"),
           subtitle = "1% NPP max - 5% NPP max",
           fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
      scale_fill_cmocean(limits = c(-0.5,0.5), oob = squish, name = "balance", direction = 1) +
      scale_y_continuous(breaks = seq(-80,80, by = 20)) +
      scale_x_continuous(breaks = seq(-180,180, by = 30)) +
      xlab("Longitude") +
      ylab("Latitude") +
      theme_bw() +
      theme(panel.grid = element_blank())
    
    fig_lt
    
    ggsave(paste(model.name,"_POC_EZ_depth_lt_metric_sensitivity.png",sep=""), plot = fig_lt, path = "~/spatial_analysis/figures/EZ_depth_metric_sensitivity/", width = 28, height = 15, units = "cm", dpi = 400)
    
  } else {
  
  change_diff = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth_sensitivity/",model.name,"_POC_EZ_depth_sensitivity_change_rg.asc"))
  his_diff = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth_sensitivity/",model.name,"_POC_EZ_depth_sensitivity_his_rg.asc"))
  lt_diff = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth_sensitivity/",model.name,"_POC_EZ_depth_sensitivity_lt_rg.asc"))
  
  #edit multimodel name for nice plotting label
  if(model.name == "multimodel") {
    model.name = "Multi-model"
  } else {
    model.name = model.name
  }
  
  #convert raster to a 'conventional' dataframe
  geo <- rasterToPoints(his_diff, spatial = TRUE)
  df  <- data.frame(geo)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  fig_his <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = paste0(model.name, " difference in historical (1850-1900) POC at EZ depth metric"),
         subtitle = "1% NPP max - 5% NPP max",
         fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
    scale_fill_cmocean(limits = c(-0.5,0.5), oob = squish, name = "balance", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  fig_his
  
  #convert raster to a 'conventional' dataframe
  geo <- rasterToPoints(lt_diff, spatial = TRUE)
  df  <- data.frame(geo)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  fig_lt <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = paste0(model.name, " difference in long-term (2080-2100) POC at EZ depth metric"),
         subtitle = "1% NPP max - 5% NPP max",
         fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
    scale_fill_cmocean(limits = c(-0.5,0.5), oob = squish, name = "balance", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  fig_lt
  
  #convert raster to a 'conventional' dataframe
  geo <- rasterToPoints(change_diff, spatial = TRUE)
  df  <- data.frame(geo)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  fig_change <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = paste0(model.name, " Difference in the change in POC at EZ depth metrics"),
         subtitle = "1% NPP max - 5% NPP max",
         fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
    scale_fill_cmocean(limits = c(-0.2,0.2), oob = squish, name = "balance", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  fig_change
  
  figure <- grid.arrange(fig_his, fig_lt, fig_change,ncol = 1, nrow = 3)
  
  ggsave(paste(model.name,"_POC_EZ_depth_metric_sensitivity.png",sep=""), plot = figure, path = "~/spatial_analysis/figures/EZ_depth_metric_sensitivity/", width = 22, height = 30, units = "cm", dpi = 400)
  
}
}