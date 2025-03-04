## PAPER FIGURES

## Fig 2. Depth Horizon depths relative to 100 m ---------

land <- ne_download(scale = 50, type = 'land', category = 'physical')
world <- ne_load(scale = 50, type = 'land', category = 'physical', returnclass = "sf")

variable = c("EZ_depth", "PCD", "MLD_max","EZ_depth", "PCD", "MLD_max")

map_list <- list()

for(i in 1:length(variable)) {
  
  if (i == 1 | i == 2 | i == 3) {
    
    change = raster(paste0("~/spatial_analysis/raster_output/",variable[i],"/multimodel_",variable[i],"_his_rg.asc"))
    #set projection for original raster (Mercator)
    crs(change) = "+proj=longlat +datum=WGS84 +no_defs"
    #change to Robinson projection
    newchange <- projectRaster(change,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    names(newchange) = "depth"
    #prepare for plotting
    geo3 = rasterToPoints(newchange, spatial = TRUE)
    df  <- data.frame(geo3)
    df = subset(df, select = -c(4))
    colnames(df) = c("value", "x", "y")
    
  } else {
    
    change = raster(paste0("~/spatial_analysis/raster_output/",variable[i],"/multimodel_",variable[i],"_lt_rg.asc"))
    #set projection for original raster (Mercator)
    crs(change) = "+proj=longlat +datum=WGS84 +no_defs"
    #change to Robinson projection
    newchange <- projectRaster(change,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    names(newchange) = "depth"
    #prepare for plotting
    geo3 = rasterToPoints(newchange, spatial = TRUE)
    df  <- data.frame(geo3)
    df = subset(df, select = -c(4))
    colnames(df) = c("value", "x", "y")
    
  }
  
  #labels
  if(i == 1) {
    title = "a) Historical Average (1850-1900) EZ Depth"
  } else if(i == 2) {
    title = "c) Historical Average (1850-1900) PCD"
  } else if(i == 3) {
    title = expression(paste("e) ","Historical Average (1850-1900) MLD"[max],sep=""))
  } else if(i == 4) {
    title = "b) Future Average (2080-2100) EZ Depth"
  } else if(i == 5) {
    title = "d) Future Average (2080-2100) PCD"
  } else {
    title = expression(paste("f) ","Future Average (2080-2100) MLD"[max],sep=""))
  }
  
  fig_change <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  title,
         fill = "(m)") +
    scale_fill_cmocean(limits = c(0,200), oob = squish, name = "balance", direction = -1) +
    scale_x_continuous(breaks = seq(-179.6, 179.9, 359.5)) +
    scale_y_continuous(breaks = seq(-89.8, 88.4,178.2)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal_grid()  +
    theme(panel.grid.major = element_line(color = "black"),
          panel.ontop = TRUE,
          plot.title = element_text(family = "calibri", size = 22, face = "plain"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 22),
          plot.margin = unit(c(0.3,0.8,0.1,0.2), "cm"))
  
  fig_change
  
  if(i == 2 | i == 5) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(0,150), oob = squish, name = "balance", direction = -1, start = 0.25)
  } else if(i == 3 | i == 6) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(0,300), oob = squish, name = "balance", direction = -1, end = 0.75)
  } else {}
  
  map_list[[i]] = assign(variable[i], fig_change)
  
}

maps_grid <- plot_grid(
  plot_grid(map_list[[1]], map_list[[4]], map_list[[2]],
            map_list[[5]], map_list[[3]],map_list[[6]],nrow = 3, ncol = 2))

ggsave("fig2_DH_depth_relative_to_100m_maps.jpg", plot = maps_grid, path = "~/master_notebooks/paper_figs/", width = 46, height = 36, units = "cm", dpi = 500)


## Fig 3. time series at 5 DHs --------------

#my custom scale
color5 = c("#E78AC3", "#FFD92F", "#A6D854", "thistle2","#66C2A5" ,"#FC8D62", "navajowhite3", "#8DA0CB")

setwd("~/time_series_analysis/files/all_models/")
dfs <- list.files(pattern = "normalized_time_series_expc*")
DH = c("100m", "1000m", "EZ", "MLDmax", "PCD")
plot.title = c("a) 100 m", "e) 1000 m", "b) EZ Depth", "d) MLDmax", "c) PCD")

ts_list <- list()

for(i in 1:length(dfs)) {
  
  normalized.epc100 = read_csv(paste0("~/time_series_analysis/files/all_models/", dfs[i]))
  colnames(normalized.epc100) <- c("Year", "CESM2","GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  normalized.epc100 <- normalized.epc100 %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  plot.normalized.epc100 <- data.table::melt(normalized.epc100,  id.vars = 'Year', value.name = 'POC_flux_100', variable.name = "Model")
  
  #plot time series
  plot <- ggplot() +
    geom_line(data = plot.normalized.epc100, aes(x = Year, y = POC_flux_100, color = Model), size = 1.5) +
    geom_smooth(data = plot.normalized.epc100, aes(x = Year, y = POC_flux_100, color = Model),size = 1.5, se = FALSE) +
    theme_bw() +
    labs(title = plot.title[i]) +
    xlab(NULL) +
    ylab("Change Relative to 1850-1900 (%)") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(limits = c(75,108)) +
    scale_color_manual(values = color5) +
    theme(plot.title = element_text(family = "calibri", size = 28),
          axis.text = element_text(family = "calibri", size = 18),
          axis.title.y = element_text(family = "calibri", size = 18, margin = margin(r = 14)),
          legend.key.size = unit(1.5, 'cm'), 
          legend.key.height = unit(1.3, 'cm'), 
          legend.key.width = unit(2, 'cm'), 
          legend.text = element_text(family = "calibri", size = 20),
          legend.title = element_text(family = "calibri", size = 26),
          legend.box.spacing = unit(1, 'in'),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm")) +
    guides(color = guide_legend(override.aes = list(linewidth = 6)))
  
  plot
  
  if(i == 2) {
    plot <- plot + scale_y_continuous()
  } else if (i == 4) {
    plot <- plot + labs(title = expression(paste("d)"," MLD"[max],sep=""))) + ylab("")
  } else if (i==3) {
    plot <- plot + ylab("")
    }else {}
  ts_list[[i]] = assign(DH[i], plot)
  
}

ts_DH <- plot_grid(
  plot_grid(ts_list[[1]] + theme(legend.position="none"), ts_list[[3]] + theme(legend.position="none"),
            ts_list[[5]] + theme(legend.position="none"), ts_list[[4]] + theme(legend.position="none"), nrow = 2, ncol = 2),
  plot_grid(NULL, ts_list[[2]], NULL, nrow = 1, rel_widths = c(0.32, 1, 0.08)),
  nrow = 2, rel_heights = c(0.7,0.36))

ts_DH

ggsave(filename = "fig3_time_series_faceted.jpg", plot = ts_DH, path = "~/master_notebooks/paper_figs/", width = 45, height = 45, units = "cm", dpi = 500)


## Fig 4. Spatial Maps w/ Stippling ---------

land <- ne_download(scale = 50, type = 'land', category = 'physical')
world <- ne_load(scale = 50, type = 'land', category = 'physical', returnclass = "sf")

variable = c("POC_100", "POC_1000", "POC_EZ_depth", "POC_MLD_max", "POC_PCD")

map_list <- list()

for(i in 1:length(variable)) {
  
  sign_agreement <- raster(paste0("~/spatial_analysis/raster_output/model_agreement/",variable[i],"_7_model_sign_match.asc"))
  
  #set projection for original raster (Mercator)
  crs(sign_agreement) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  rob_sign <- projectRaster(sign_agreement,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(rob_sign) = "POC"
  #prepare for plotting
  points = rasterToPoints(rob_sign, spatial = TRUE)
  df_sign  <- data.frame(points)
  df_sign = subset(df_sign, select = -c(4))
  colnames(df_sign) = c("value", "x", "y")
  df_sign <- filter(df_sign, value == 1)
  
  change = raster(paste0("~/spatial_analysis/raster_output/",variable[i],"/multimodel_",variable[i],"_change_rg.asc"))
  #set projection for original raster (Mercator)
  crs(change) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  newchange <- projectRaster(change,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(newchange) = "POC"
  #prepare for plotting
  geo3 = rasterToPoints(newchange, spatial = TRUE)
  df  <- data.frame(geo3)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  if(i == 1) {
    
    ocean = raster("~/regional_time_series_analysis/files/region_area/region_boundaries_map.asc")
    
    #set projection for original raster (Mercator)
    crs(ocean) = "+proj=longlat +datum=WGS84 +no_defs"
    #change to Robinson projection
    newchange <- projectRaster(ocean,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", method = "ngb", res = 45000)
    names(newchange) = "POC"
    #prepare for plotting
    geo3 = rasterToPoints(newchange, spatial = TRUE)
    regions  <- data.frame(geo3)
    regions = subset(regions, select = -c(4))
    colnames(regions) = c("value", "x", "y")
  } else {}
  
  #labels
  if(i == 1) {
    title = "a) 100 m"
  } else if(i == 2) {
    title = "e) 1000 m"
  } else if(i == 4) {
    title = expression(paste("d)"," MLD"[max],sep=""))
  } else if(i == 3) {
    title = "b) EZ Depth"
  } else {
    title = "c) PCD"
  }

  
  fig_change <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_point(data = df_sign, aes(x = x, y = y, fill = value), na.rm = FALSE, color = "black", size = 0.3, shape = 4, stroke = 0.5, alpha = 0.7) +
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  title,
         fill = expression(paste("mol/", m^2, "/yr",sep = " "))) +
    scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1) +
    scale_x_continuous(breaks = seq(-179.6, 179.9, 359.5)) +
    scale_y_continuous(breaks = seq(-89.8, 88.4,178.2)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal_grid()  +
    theme(panel.grid.major = element_line(color = "black"),
          panel.ontop = TRUE,
          plot.title = element_text(family = "calibri", size = 32, face = "plain"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 26),
          plot.margin = unit(c(0.1,0.8,0.1,0.1), "cm"))
  
  fig_change
  
  if(i == 2) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(-0.4,0.4), oob = squish, name = "balance", direction = 1)
  } else if(i == 1) {
    fig_change = fig_change + geom_point(data = regions , aes(x = x, y = y, fill = value), size = 0.08, shape = 3) #19
  } else {}
  
  map_list[[i]] = assign(variable[i], fig_change)
  
}

maps_grid <- plot_grid(
  plot_grid(map_list[[1]], map_list[[3]],
            map_list[[5]], map_list[[4]],nrow = 2, ncol = 2),
  plot_grid(NULL, map_list[[2]], NULL, nrow = 1, rel_widths = c(0.2, 1, 0.2)),
  nrow = 2, rel_heights = c(0.65,0.3))

ggsave("fig4_flux_change_maps.jpg", plot = maps_grid, path = "~/master_notebooks/paper_figs/", width = 57, height = 43, units = "cm", dpi = 500)

## Fig 5. Global time series interrelations --------

#my custom scale
color5 = c("#E78AC3", "#FFD92F", "#A6D854", "thistle2","#66C2A5" ,"#FC8D62", "navajowhite3", "#8DA0CB")

interrelations_list <- list()

panels <- c("NPP","e-ratio", "POC_100", "TE", "POC_1000")

for(i in 1:length(panels)) {

if (i == 1) {
  df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_time_series_intpp.csv"))
  plot.title = "a) NPP"
  ylabel = "% Change"
} else if (i == 2){
  df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_time_series_e_ratio_100.csv"))
  plot.title = "b) E-ratio"
  ylabel = "% Change"
} else if (i == 3){
  df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_time_series_expc_100.csv"))
  plot.title = "c) POC Flux at 100 m"
  ylabel = "% Change"
} else if (i == 4){
  df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_TE_time_series_100_1000.csv"))
  plot.title = "d) Transfer Efficiency"
  ylabel = "% Change in % Transferred"
} else {
  df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_time_series_expc_1000.csv"))
  plot.title = "e) POC Flux at 1000 m"
  ylabel = "% Change"
}
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'variable', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = variable, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    labs(title = plot.title) +
    xlab(NULL) +
    ylab("") +
    scale_y_continuous(n.breaks = 6) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = color5) +
    theme(plot.title = element_text(family = "calibri", size = 20),
          axis.text = element_text(family = "calibri", size = 15),
          axis.title = element_text(family = "calibri", size = 18),
          axis.title.y = element_text(family = "calibri", size = 14, margin = margin(r = 9)),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(0.8, 'cm'), 
          legend.key.width = unit(1.4, 'cm'), 
          legend.text = element_text(family = "calibri", size = 17),
          legend.title = element_text(family = "calibri", size = 20),
          legend.box.spacing = unit(1, 'in'),
          plot.margin = unit(c(0.3,1,0.5,1), "cm")) +
    guides(color = guide_legend(override.aes = list(linewidth = 6)))
  
  plot
  
  if(i == 1) {
    plot <- plot + ylab("Change Relative to 1850-1900 (%)") +
      theme(plot.title = element_text(family = "calibri", size = 20),
            axis.text = element_text(family = "calibri", size = 15),
            axis.title = element_text(family = "calibri", size = 18),
            axis.title.y = element_text(family = "calibri", size = 15, margin = margin(r = 9)),
            legend.key.size = unit(1, 'cm'), 
            legend.key.height = unit(0.8, 'cm'), 
            legend.key.width = unit(1.4, 'cm'), 
            legend.text = element_text(family = "calibri", size = 17),
            legend.title = element_text(family = "calibri", size = 20),
            legend.box.spacing = unit(1, 'in'),
            plot.margin = unit(c(0.3,1,0.5,1), "cm"))

  } else {}
    
  
  interrelations_list[[i]] = assign(panels[i], plot)
  
}

fig3 <- grid_arrange_shared_legend(interrelations_list[[1]],interrelations_list[[2]], interrelations_list[[3]], interrelations_list[[4]], interrelations_list[[5]],
                                  nrow = 1, ncol = 5, position = "right")

ggsave(filename = "fig5_global_interrelations.jpg", plot = fig3, path = "~/master_notebooks/paper_figs/", width = 70, height = 10, units = "cm", dpi = 600)


## Fig 7. Regional time series plot interrelations ---------

#my custom scale
color5 = c("#E78AC3", "#FFD92F", "#A6D854", "thistle2","#66C2A5" ,"#FC8D62", "navajowhite3", "#8DA0CB")

POC_100_list <- list()
NPP_list <- list()
e_ratio_list <- list()
TE_list <- list()
POC_1000_list <- list()

regions <- c("global","low_lats_no_EQ_Pacific", "EQ_Pacific", "SO_60", "North_Atlantic")

#POC 100 ----------

for(i in 1:length(regions)) {
  
  if(i == 4) {
    plot.subtitle = "Southern Ocean"
  } else if(i == 3) {
    plot.subtitle = "Equatorial Pacific"
    #low latitudes without Equatorial Pacific
  } else if (i == 2) {
    plot.subtitle = "Low Latitudes"
  } else if (i == 5) {
    plot.subtitle = "North Atlantic"
  }  else {
    plot.subtitle = expression(paste("c)"," POC flux"[100],sep=""))
  }
  
  if(i == 1) {
    plot.subtitle2 = "k)"
  } else if(i == 2) {
    plot.subtitle2 = "c)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "h)"
  } else if (i == 4) {
    plot.subtitle2 = "m)"
  }  else {
    plot.subtitle2 = "r)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_time_series_expc_100.csv"))
  } else {
    df <- read_csv(paste0("normalized_", regions[i], "_time_series_POC_100_all.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = POC_flux, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - POC flux at 100m")) +
    labs(title = plot.subtitle2) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(n.breaks = 6) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = color5) +
    theme(plot.title = element_text(family = "calibri", size = 24),
          axis.text = element_text(family = "calibri", size = 16),
          axis.title = element_text(family = "calibri", size = 18),
          legend.key.size = unit(1.5, 'cm'), 
          legend.key.height = unit(1.3, 'cm'), 
          legend.key.width = unit(2, 'cm'), 
          legend.text = element_text(family = "calibri", size = 22),
          legend.title = element_text(family = "calibri", size = 26),
          legend.box.spacing = unit(1, 'in'),
          plot.margin = unit(c(0.3,0.7,0.5,0.3), "cm")) +
    guides(color = guide_legend(override.aes = list(linewidth = 6)))
  
  plot
  
  if(i == 1) {
    plot = plot + ylab("Change Relative to 1850-1900")
  } else {}
  
  POC_100_list[[i]] = assign(paste0("POC_100_", regions[i]), plot)
  
}

# POC 1000 -----------

for(i in 1:length(regions)) {
  
  if(i == 4) {
    plot.subtitle = "Southern Ocean"
  } else if(i == 3) {
    plot.subtitle = "Equatorial Pacific"
    #low latitudes without Equatorial Pacific
  } else if (i == 2) {
    plot.subtitle = "Low Latitudes"
  } else if (i == 5) {
    plot.subtitle = "North Atlantic"
  }  else {
    plot.subtitle = "Global"
  }
  
  if(i == 1) {
    plot.subtitle2 = "e)"
  } else if(i == 2) {
    plot.subtitle2 = "e)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "j)"
  } else if (i == 4) {
    plot.subtitle2 = "o)"
  }  else {
    plot.subtitle2 = "t)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_time_series_expc_1000.csv"))
  } else {
    df <- read_csv(paste0("normalized_", regions[i], "_time_series_POC_1000_all.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = POC_flux, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - POC flux at 1000m")) +
    labs(title = plot.subtitle2) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(n.breaks = 6) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = color5) +
    theme(plot.title = element_text(family = "calibri", size = 24),
          axis.text = element_text(family = "calibri", size = 16),
          axis.title = element_text(family = "calibri", size = 18),
          legend.key.size = unit(1.5, 'cm'), 
          legend.key.height = unit(1.3, 'cm'), 
          legend.key.width = unit(2, 'cm'), 
          legend.text = element_text(family = "calibri", size = 22),
          legend.title = element_text(family = "calibri", size = 26),
          legend.box.spacing = unit(1, 'in'),
          plot.margin = unit(c(0.3,0.7,0.5,0.3), "cm")) +
    guides(color = guide_legend(override.aes = list(linewidth = 6)))
  
  plot
  
  if(i == 1) {
    plot = plot + ylab("Change Relative to 1850-1900")
  } else {}
  
  POC_1000_list[[i]] = assign(paste0("POC_100_", regions[i]), plot)
  
}

# NPP --------------

for(i in 1:length(regions)) {
  
  if(i == 4) {
    plot.subtitle = "Southern Ocean"
  } else if(i == 3) {
    plot.subtitle = "Equatorial Pacific"
    #low latitudes without Equatorial Pacific
  } else if (i == 2) {
    plot.subtitle = "Low Latitudes"
  } else if (i == 5) {
    plot.subtitle = "North Atlantic"
  }  else {
    plot.subtitle = "Global"
  }
  
  if(i == 1) {
    plot.subtitle2 = "a)"
  } else if(i == 2) {
    plot.subtitle2 = "a)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "f)"
  } else if (i == 4) {
    plot.subtitle2 = "k)"
  }  else {
    plot.subtitle2 = "p)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_time_series_intpp.csv"))
  } else {
    df <- read_csv(paste0("normalized_", regions[i], "_time_series_NPP_all.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'NPP', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = NPP, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - NPP")) +
    labs(title = plot.subtitle2) +
    xlab(NULL) +
    ylab("Change Relative to 1850-1900 (%)") +
    scale_y_continuous(n.breaks = 6) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = color5) +
    theme(plot.title = element_text(family = "calibri", size = 24),
          axis.text = element_text(family = "calibri", size = 16),
          axis.title = element_text(family = "calibri", size = 18),
          axis.title.y = element_text(family = "calibri", size = 14, margin = margin(r = 5)),
          legend.key.size = unit(1.5, 'cm'), 
          legend.key.height = unit(1.3, 'cm'), 
          legend.key.width = unit(2, 'cm'), 
          legend.text = element_text(family = "calibri", size = 22),
          legend.title = element_text(family = "calibri", size = 26),
          legend.box.spacing = unit(1, 'in'),
          plot.margin = unit(c(0.3,0.7,0.5,0.3), "cm")) +
    guides(color = guide_legend(override.aes = list(linewidth = 6)))
  
  if(i == 1) {
    plot = plot + ylab("Change Relative to 1850-1900")
  } else {}
  
  plot
  
  NPP_list[[i]] = assign(paste0("NPP_", regions[i]), plot)
  
}

# e-ratio --------------

for(i in 1:length(regions)) {
  
  if(i == 4) {
    plot.subtitle = "Southern Ocean"
  } else if(i == 3) {
    plot.subtitle = "Equatorial Pacific"
    #low latitudes without Equatorial Pacific
  } else if (i == 2) {
    plot.subtitle = "Low Latitudes"
  } else if (i == 5) {
    plot.subtitle = "North Atlantic"
  }  else {
    plot.subtitle = "Global"
  }
  
  if(i == 1) {
    plot.subtitle2 = "f)"
  } else if(i == 2) {
    plot.subtitle2 = "b)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "g)"
  } else if (i == 4) {
    plot.subtitle2 = "l)"
  }  else {
    plot.subtitle2 = "q)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_time_series_e_ratio_100.csv"))
  } else {
    df <- read_csv(paste0("normalized_", regions[i], "_e_ratio_time_series_POC_100.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = e_ratio, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - E-ratio")) +
    labs(title = plot.subtitle2) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(n.breaks = 6) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = color5) +
    theme(plot.title = element_text(family = "calibri", size = 24),
          axis.text = element_text(family = "calibri", size = 16),
          axis.title = element_text(family = "calibri", size = 18),
          legend.key.size = unit(1.5, 'cm'), 
          legend.key.height = unit(1.3, 'cm'), 
          legend.key.width = unit(2, 'cm'), 
          legend.text = element_text(family = "calibri", size = 22),
          legend.title = element_text(family = "calibri", size = 26),
          legend.box.spacing = unit(1, 'in'),
          plot.margin = unit(c(0.3,0.7,0.5,0.3), "cm")) +
    guides(color = guide_legend(override.aes = list(linewidth = 6)))
  
  if(i == 1) {
    plot = plot + ylab("Change Relative to 1850-1900")
  } else {}
  
  plot
  
  e_ratio_list[[i]] = assign(paste0("e_ratio_", regions[i]), plot)
  
}

# TE ----------

for(i in 1:length(regions)) {
  
  if(i == 4) {
    plot.subtitle = "Southern Ocean"
  } else if(i == 3) {
    plot.subtitle = "Equatorial Pacific"
    #low latitudes without Equatorial Pacific
  } else if (i == 2) {
    plot.subtitle = "Low Latitudes"
  } else if (i == 5) {
    plot.subtitle = "North Atlantic"
  }  else {
    plot.subtitle = "Global"
  }
  
  if(i == 1) {
    plot.subtitle2 = "p)"
  } else if(i == 2) {
    plot.subtitle2 = "d)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "i)"
  } else if (i == 4) {
    plot.subtitle2 = "n)"
  }  else {
    plot.subtitle2 = "s)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_TE_time_series_100_1000.csv"))
  } else {
    df <- read_csv(paste0("normalized_", regions[i], "_TE_time_series_POC_100_POC_1000.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = TE, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - Transfer Efficiency")) +
    labs(title = plot.subtitle2) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_continuous(n.breaks = 6) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = color5) +
    theme(plot.title = element_text(family = "calibri", size = 24),
          axis.text = element_text(family = "calibri", size = 16),
          axis.title = element_text(family = "calibri", size = 18),
          legend.key.size = unit(1.5, 'cm'), 
          legend.key.height = unit(1.3, 'cm'), 
          legend.key.width = unit(2, 'cm'), 
          legend.text = element_text(family = "calibri", size = 22),
          legend.title = element_text(family = "calibri", size = 26),
          legend.box.spacing = unit(1, 'in'),
          plot.margin = unit(c(0.3,0.7,0.5,0.3), "cm")) +
    guides(color = guide_legend(override.aes = list(linewidth = 6)))
  plot
  
  if(i == 1) {
    plot = plot + ylab("Change Relative to 1850-1900")
  } else {}
  
  plot
  
  TE_list[[i]] = assign(paste0("TE_", regions[i]), plot)
  
}


#fig3 <- grid_arrange_shared_legend(NPP_list[[1]],NPP_list[[2]], NPP_list[[3]], NPP_list[[4]], NPP_list[[5]],
#                                   e_ratio_list[[1]],e_ratio_list[[2]], e_ratio_list[[3]], e_ratio_list[[4]], e_ratio_list[[5]],
#                                   POC_100_list[[1]],POC_100_list[[2]], POC_100_list[[3]], POC_100_list[[4]], POC_100_list[[5]],
#                                   TE_list[[1]],TE_list[[2]], TE_list[[3]], TE_list[[4]], TE_list[[5]],
#                                   POC_1000_list[[1]],POC_1000_list[[2]], POC_1000_list[[3]], POC_1000_list[[4]], POC_1000_list[[5]],
#                                   nrow = 5, ncol = 5, position = "right")

fig3 <- grid_arrange_shared_legend(NPP_list[[2]],e_ratio_list[[2]], POC_100_list[[2]], TE_list[[2]], POC_1000_list[[2]],
                                   NPP_list[[3]],e_ratio_list[[3]], POC_100_list[[3]], TE_list[[3]], POC_1000_list[[3]],
                                   NPP_list[[4]],e_ratio_list[[4]], POC_100_list[[4]], TE_list[[4]], POC_1000_list[[4]],
                                   NPP_list[[5]],e_ratio_list[[5]], POC_100_list[[5]], TE_list[[5]], POC_1000_list[[5]],
                                   nrow = 4, ncol = 5, position = "right")

ggsave(filename = "fig7_regional_interrelations.jpg", plot = fig3, path = "~/master_notebooks/paper_figs/", width = 70, height = 43, units = "cm", dpi = 600)
#height = 57


#Fig 6. spatial interrelations --------

change_list = list()

land <- ne_download(scale = 50, type = 'land', category = 'physical')
world <- ne_load(scale = 50, type = 'land', category = 'physical', returnclass = "sf")

variable = c("intpp", "e_ratio", "POC_100", "TE", "POC_1000")

for(i in 1:length(variable)) {
  
  sign_agreement <- raster(paste0("~/spatial_analysis/raster_output/model_agreement/",variable[i],"_7_model_sign_match.asc"))
  #set projection for original raster (Mercator)
  crs(sign_agreement) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  rob_sign <- projectRaster(sign_agreement,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(rob_sign) = "POC"
  #prepare for plotting
  points = rasterToPoints(rob_sign, spatial = TRUE)
  df_sign  <- data.frame(points)
  df_sign = subset(df_sign, select = -c(4))
  colnames(df_sign) = c("value", "x", "y")
  df_sign <- filter(df_sign, value == 1)
  
  if (i == 4 || i == 2) {
    change = raster(paste0("~/spatial_analysis/raster_output/",variable[i],"/multimodel_POC_100_",variable[i],"_change_rg.asc"))
  }  else if (i == 1) {
    change = raster(paste0("~/spatial_analysis/raster_output/NPP/multimodel_",variable[i],"_change_rg.asc"))
  } else {
    change = raster(paste0("~/spatial_analysis/raster_output/",variable[i],"/multimodel_",variable[i],"_change_rg.asc"))
  }
  #set projection for original raster (Mercator)
  crs(change) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  newchange <- projectRaster(change,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(newchange) = "POC"
  #prepare for plotting
  geo3 = rasterToPoints(newchange, spatial = TRUE)
  df  <- data.frame(geo3)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  #labels
  if(i == 1) {
    title = "b) Change in NPP"
    legend.label = expression(paste("mol/", m^2, "/yr",sep = " "))
  } else if(i == 2) {
    title = "d) Change in E-ratio"
    legend.label = "             "
  } else if(i == 3) {
    title = expression(paste("f) Change in POC Flux"["100 m"],sep=""))
    legend.label = expression(paste("mol/", m^2, "/yr",sep = " "))
  } else if(i == 4) {
    title = "h) Change in Transfer Efficiency"
    legend.label = "%           "
  } else {
    title = expression(paste("j) Change in POC Flux"["1000 m"],sep=""))
    legend.label = expression(paste("mol/", m^2, "/yr",sep = " "))
  }
  
  fig_change <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_point(data = df_sign, aes(x = x, y = y, fill = value), na.rm = FALSE, color = "black", size = 0.3, shape = 4, stroke = 0.5, alpha = 0.7) +
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  title,
         fill = legend.label) +
    scale_fill_cmocean(limits = c(-6,6), oob = squish, name = "balance", direction = 1) +
    scale_x_continuous(breaks = seq(-179.6, 179.9, 359.5)) +
    scale_y_continuous(breaks = seq(-89.8, 88.4,178.2)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal_grid()  +
    theme(panel.grid.major = element_line(color = "black"),
          panel.ontop = TRUE,
          plot.title = element_text(family = "calibri", size = 28, face = "plain"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 24),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm"))
  
  if(i == 5) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(-0.2,0.2), oob = squish, name = "balance", direction = 1)
  } else if(i == 2) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(-0.1,0.1), oob = squish, name = "balance", direction = 1)
  } else if(i == 3) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1)
  } else if(i == 4) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(-10,10), oob = squish, name = "balance", direction = 1)
  } else {
  }
  
  fig_change
  
  change_list[[i]] = assign(paste0("change_", variable[i]), fig_change)
  
}

historical_list = list()

for(i in 1:length(variable)) {
  
  if (i == 4 || i == 2) {
    his = raster(paste0("~/spatial_analysis/raster_output/",variable[i],"/multimodel_POC_100_",variable[i],"_his_rg.asc"))
  }  else if (i == 1) {
    his = raster(paste0("~/spatial_analysis/raster_output/NPP/multimodel_",variable[i],"_his_rg.asc"))
  } else {
    his = raster(paste0("~/spatial_analysis/raster_output/",variable[i],"/multimodel_",variable[i],"_his_rg.asc"))
  }
  
  #set projection for original raster (Mercator)
  crs(his) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  newhis <- projectRaster(his,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(newhis) = "POC"
  #prepare for plotting
  geo3 = rasterToPoints(newhis, spatial = TRUE)
  df  <- data.frame(geo3)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  if(i == 1) {
    
    ocean = raster("~/regional_time_series_analysis/files/region_area/region_boundaries_map.asc")
    
    #set projection for original raster (Mercator)
    crs(ocean) = "+proj=longlat +datum=WGS84 +no_defs"
    #change to Robinson projection
    newchange <- projectRaster(ocean,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", method = "ngb", res = 45000)
    names(newchange) = "POC"
    #prepare for plotting
    geo3 = rasterToPoints(newchange, spatial = TRUE)
    regions  <- data.frame(geo3)
    regions = subset(regions, select = -c(4))
    colnames(regions) = c("value", "x", "y")
  } else {}
  
  #labels
  if(i == 1) {
    title = "a) Historical (1850-1900) NPP"
    legend.label = expression(paste("mol/", m^2, "/yr",sep = " "))
  } else if(i == 2) {
    title = "c) Historical (1850-1900) E-ratio"
    legend.label = "              "
  } else if(i == 3) {
    title = expression(paste("e) Historical (1850-1900) POC Flux"["100 m"],sep=""))
    legend.label = expression(paste("mol/", m^2, "/yr",sep = " "))
  } else if(i == 4) {
    title = "g) Historical (1850-1900) Transfer Efficiency"
    legend.label = "%            "
  } else {
    title = expression(paste("i) Historical (1850-1900) POC Flux"["1000 m"],sep=""))
    legend.label = expression(paste("mol/", m^2, "/yr",sep = " "))
  }
  
  fig_his <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  title,
         fill = legend.label) +
    scale_fill_cmocean(limits = c(0,20), oob = squish, name = "deep", direction = -1, end = 0.9) +
    scale_x_continuous(breaks = seq(-179.6, 179.9, 359.5)) +
    scale_y_continuous(breaks = seq(-89.8, 88.4,178.2)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal_grid()  +
    theme(panel.grid.major = element_line(color = "black"),
          panel.ontop = TRUE,
          plot.title = element_text(family = "calibri", size = 28, face = "plain"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 24),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm"))
  
  if(i == 5) {
    fig_his = fig_his + scale_fill_cmocean(limits = c(0,1), oob = squish, name = "deep", end = 0.9, direction = -1)
  } else if(i == 2) {
    fig_his = fig_his + scale_fill_cmocean(limits = c(0,0.4), oob = squish, name = "deep", end = 0.9, direction = -1)
  } else if(i == 3) {
    fig_his = fig_his + scale_fill_cmocean(limits = c(0,4), oob = squish, name = "deep",end = 0.9,  direction = -1)
  } else if(i == 4) {
    fig_his = fig_his + scale_fill_cmocean(limits = c(0,30), oob = squish, name = "deep",end = 0.9, direction = -1)
  } else {
    fig_his = fig_his + geom_point(data = regions , aes(x = x, y = y, fill = value), size = 0.2, shape = 19)
  }
  
  fig_his
  
  historical_list[[i]] = assign(paste0("his_", variable[i]), fig_his)
  
}

maps_grid_10 <- plot_grid(historical_list[[1]], change_list[[1]], historical_list[[2]], change_list[[2]],
                          historical_list[[3]], change_list[[3]],historical_list[[4]], change_list[[4]],
                          historical_list[[5]], change_list[[5]], nrow = 5, ncol = 2)

ggsave("fig6_spatial_interrelations.jpg", plot = maps_grid_10, path = "~/master_notebooks/paper_figs/", width = 59, height = 68, units = "cm", dpi = 500)
