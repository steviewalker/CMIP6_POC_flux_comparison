#' @title Plots the historical, long-term, and change in TE for every model and depth horizon
#' @author Stevie Walker
#' @date 9/10/22
#' @description plots either absolute or normalized percent change in TE by model

plot_TE_by_model <- function(variable, model.name, type) {
  
  if(type == "percent") {
    
    change = raster(paste0("~/spatial_analysis/raster_output/TE/",model.name,"_", variable, "_TE_change_rg.asc"))
    his = raster(paste0("~/spatial_analysis/raster_output/TE/",model.name,"_", variable, "_TE_his_rg.asc"))
    lt = raster(paste0("~/spatial_analysis/raster_output/TE/",model.name,"_", variable, "_TE_lt_rg.asc"))
    
  } else {
    
    change = raster(paste0("~/spatial_analysis/raster_output/TE/",model.name,"_", variable, "_TE_change_absolute_rg.asc"))
    his = raster(paste0("~/spatial_analysis/raster_output/TE/",model.name,"_", variable, "_TE_his_absolute_rg.asc"))
    lt = raster(paste0("~/spatial_analysis/raster_output/TE/",model.name,"_", variable, "_TE_lt_absolute_rg.asc"))
    
  }
  
  if(variable == "POC_MLD_max") {
    title = "MLDmax"
  } else if(variable == "POC_PCD") {
    title = "PCD"
  } else if(variable == "POC_EZ_depth") {
    title = "EZ depth"
  } else if(variable == "POC_100") {
    title = "100m"
  } else {
    title = "1000m"
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
    labs(title = paste0(model.name, " Historical (1850-1900) TE Between ",title," and 1000m"),
         fill = "% Transferred",
         tag = "% Change in % Transferred") +
    scale_fill_cmocean(limits = c(0,30), oob = squish, name = "deep", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.tag = element_text(size = 16, face = "bold", vjust = 1.5),
          plot.tag.position = "top")
  
  fig_his
  
  #future ----------
  
  #convert raster to a 'conventional' dataframe
  geo <- rasterToPoints(lt, spatial = TRUE)
  df  <- data.frame(geo)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  #edit multimodel name for nice plotting label
  if(model.name == "multimodel") {
    model.name = "Multi-model"
  } else {
    model.name = model.name
  }
  
  fig_lt <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = paste0(model.name, " Long-term (2080-2100) TE Between ",title," and 1000m"),
         fill = "% Transferred",
         tag = " ") +
    scale_fill_cmocean(limits = c(0,30), oob = squish, name = "deep", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  fig_lt
  
  #change --------
  
  #convert raster to a 'conventional' dataframe
  geo <- rasterToPoints(change, spatial = TRUE)
  df  <- data.frame(geo)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  #edit multimodel name for nice plotting label
  if(model.name == "multimodel") {
    model.name = "Multi-model"
  } else {
    model.name = model.name
  }
  
  fig_change <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "snow") +
    coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
    labs(title = paste0(model.name, " Change in TE Between ",title," and 1000m"),
         fill = "% Transferred",
         tag = " ") +
    scale_fill_cmocean(limits = c(-20,20), oob = squish, name = "balance", direction = 1) +
    scale_y_continuous(breaks = seq(-80,80, by = 20)) +
    scale_x_continuous(breaks = seq(-180,180, by = 30)) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  fig_change
  
  #change scales for absolute change in TE
  if(type == "absolute") {
    fig_his = fig_his + scale_fill_cmocean(limits = c(-10,10), oob = squish, name = "deep", direction = 1) +
      labs(title = paste0(model.name, " Historical (1850-1900) TE Between ",title," and 1000m"),
           fill = "% Transferred",
           tag = "Absolute Change in % Transferred")
    fig_lt = fig_lt + scale_fill_cmocean(limits = c(-10,10), oob = squish, name = "deep", direction = 1)
  } else {}
  
  figure <- grid.arrange(fig_his, fig_lt, fig_change,ncol = 1, nrow = 3)
  
  if(type == "percent") {
    ggsave(paste(model.name,"_TE_",variable,"_avg_map.png",sep=""), plot = figure, path = paste0("~/spatial_analysis/figures/TE/by_model/"), width = 22, height = 30, units = "cm", dpi = 400)
  } else {
    ggsave(paste(model.name,"_TE_",variable,"_absolute_avg_map.png",sep=""), plot = figure, path = paste0("~/spatial_analysis/figures/TE/by_model/"), width = 22, height = 30, units = "cm", dpi = 400)
  }
}



