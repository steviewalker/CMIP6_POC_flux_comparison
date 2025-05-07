## Fig S1. EZ depth metric comparison at HOTS ---------

location = "HOTS"
#location = "BATS"
#location = "NW_Pacific"

#save NPP time-series data frame for all models
df.fut = read_csv(file = paste0("~/depth_profile_analysis/files/NPP_profiles/",location,"_fut_NPP_profile_all.csv"))
colnames(df.fut) <- c("depth", "CESM2","GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
df.his = read_csv(file = paste0("~/depth_profile_analysis/files/NPP_profiles/",location,"_his_NPP_profile_all.csv"))
colnames(df.his) <- c("depth", "CESM2","GFDL-CM4" ,"CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")

#add column for model key (reformatting data specific to the below plot)
df_his <- data.table::melt(df.his,  id.vars = 'depth', value.name = 'NPP', variable.name = "Model")
#add column for model key (reformatting data specific to the below plot)
df_fut <- data.table::melt(df.fut,  id.vars = 'depth', value.name = 'NPP', variable.name = "Model")
profile_combined <- bind_rows("Future" = df_fut, "Historical" = df_his,  .id = "time_period")

#combine ez depth info csv files

#replacing model names
conditions = c("CESM", "CM4","CMCC","EC-Earth","GFDL","IPSL","MPI","UKESM")
replacements = c("CESM2","GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
conditions_his = c("CESM", "CM4","CMCC","GFDL","IPSL","MPI","UKESM")
replacements_his = c("CESM2","GFDL-CM4" ,"CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")


setwd("~/depth_profile_analysis/files/NPP_profiles/ez_depths/")
df.depths.fut <- list.files(pattern = paste0("10_percent_fut_",location)) %>%  
  lapply(read_csv) %>% 
  bind_rows
df.depths.his <- list.files(pattern = paste0("10_percent_his_",location)) %>%  
  lapply(read_csv) %>% 
  bind_rows

df.depths.fut$model = replace(df.depths.fut$model, df.depths.fut$model %in% conditions,replacements)
df.depths.his$model = replace(df.depths.his$model, df.depths.his$model %in% conditions_his,replacements_his)

ez_depths_10 <- bind_rows("Future" = df.depths.fut, "Historical" = df.depths.his,  .id = "time_period")
ez_depths_10$percent <- rep("10%",times = 15)
colnames(ez_depths_10) <- c("time_period", "location", "Model", "depth", "NPP", "percent")

df.depths.fut <- list.files(pattern = paste0("5_percent_fut_",location)) %>%  
  lapply(read_csv) %>% 
  bind_rows
df.depths.his <- list.files(pattern = paste0("5_percent_his_",location)) %>%  
  lapply(read_csv) %>% 
  bind_rows

df.depths.fut$model = replace(df.depths.fut$model, df.depths.fut$model %in% conditions,replacements)
df.depths.his$model = replace(df.depths.his$model, df.depths.his$model %in% conditions_his,replacements_his)

ez_depths_5 <- bind_rows("Future" = df.depths.fut, "Historical" = df.depths.his,  .id = "time_period")
ez_depths_5$percent <- rep("5%",times = 15)
colnames(ez_depths_5) <- c("time_period", "location", "Model", "depth", "NPP", "percent")

df.depths.fut <- list.files(pattern = paste0("1_percent_fut_",location)) %>%  
  lapply(read_csv) %>% 
  bind_rows
df.depths.his <- list.files(pattern = paste0("1_percent_his_",location)) %>%  
  lapply(read_csv) %>% 
  bind_rows

df.depths.fut$model = replace(df.depths.fut$model, df.depths.fut$model %in% conditions,replacements)
df.depths.his$model = replace(df.depths.his$model, df.depths.his$model %in% conditions_his,replacements_his)

ez_depths_1 <- bind_rows("Future" = df.depths.fut, "Historical" = df.depths.his,  .id = "time_period")
ez_depths_1$percent <- rep("1%",times = 15)
colnames(ez_depths_1) <- c("time_period", "location", "Model", "depth", "NPP", "percent")

ez_depths = rbind(ez_depths_1,ez_depths_5,ez_depths_10)

#write_csv(ez_depths, paste0("~/depth_profile_analysis/files/NPP_profiles/",location,"_ez_depths_info_all.csv"))

depth_profile <- ggplot() +
  geom_path(data = profile_combined, aes(x = NPP, y = depth, linetype = time_period), size = 1) +
  geom_point(data = ez_depths, aes(x = NPP, y = depth, shape = time_period, color = percent), size = 3) +
  scale_y_continuous(limits = c(400,0),expand = c(0,0),trans = "reverse") +
  xlab("NPP (mg/m3/d)") + 
  ylab("Depth (m)") +
  labs(title = paste0(location, " NPP Profiles"),
       #labs(title = "Northwest Pacific NPP Profiles",
       linetype = "Time Period",
       color = "EZ Depth Metric",
       shape = "EZ Depth") +
  theme_bw() +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

faceted2 = depth_profile + facet_wrap(facets = ~Model, ncol = 3, nrow = 3)

faceted2

ggsave(paste("figS1_",location,"_EZ_depth_percents_faceted.png",sep = ""), plot = faceted2, path = "~/master_notebooks/paper_figs/supplemental/", width = 19, height = 20, units = "cm", dpi = 400)

## Fig SX. EZ depth comparison -------------

change_list = list()

land <- ne_download(scale = 50, type = 'land', category = 'physical')
world <- ne_load(scale = 50, type = 'land', category = 'physical', returnclass = "sf")

model_fullnames = c("CESM2","GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
model_list = c("CESM","CM4" ,"CMCC", "EC-Earth", "GFDL", "IPSL", "MPI", "UKESM")

## EZ depth long-term 1% NPP max -------

for(i in 1:length(model_list)) {
  
  esm_change <- raster(paste0("~/spatial_analysis/raster_output/EZ_depth/",model_list[i],"_EZ_depth_lt_rg.asc"))
  #set projection for original raster (Mercator)
  crs(esm_change) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  rob_sign <- projectRaster(esm_change,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(rob_sign) = "POC"
  #prepare for plotting
  points = rasterToPoints(rob_sign, spatial = TRUE)
  geo3  <- data.frame(points)
  df  <- data.frame(geo3)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  
  fig_change <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  model_fullnames[i],
         fill = "meters") +
    scale_fill_cmocean(limits = c(0,300), oob = squish, name = "deep", direction = 1) +
    scale_x_continuous(breaks = seq(-179.6, 179.9, 359.5)) +
    scale_y_continuous(breaks = seq(-89.8, 88.4,178.2)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal_grid()  +
    theme(panel.grid.major = element_line(color = "black"),
          panel.ontop = TRUE,
          plot.title = element_text(family = "calibri", size = 28, face = "plain", hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(0.7, 'cm'), 
          legend.key.height = unit(0.7, 'cm'), 
          legend.key.width = unit(0.7, 'cm'), 
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 20),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm"))
  fig_change
  
  change_list[[i]] = assign(paste0("change_", model_list[i]), fig_change)
  
}

maps_EZ_lt_1 <- plot_grid(change_list[[1]], change_list[[3]], change_list[[4]],change_list[[2]],
                          change_list[[5]], change_list[[6]],change_list[[7]], change_list[[8]],
                          nrow = 8, ncol = 1)

# EZ depth long-term NPP max 10% ---------------

for(i in 1:length(model_list)) {
  
  esm_change <- raster(paste0("~/spatial_analysis/raster_output/EZ_depth/",model_list[i],"_EZ_depth_lt_10_rg.asc"))
  #set projection for original raster (Mercator)
  crs(esm_change) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  rob_sign <- projectRaster(esm_change,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(rob_sign) = "POC"
  #prepare for plotting
  points = rasterToPoints(rob_sign, spatial = TRUE)
  geo3  <- data.frame(points)
  df  <- data.frame(geo3)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  
  fig_change <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  model_fullnames[i],
         fill = "meters") +
    scale_fill_cmocean(limits = c(0,300), oob = squish, name = "deep", direction = 1) +
    scale_x_continuous(breaks = seq(-179.6, 179.9, 359.5)) +
    scale_y_continuous(breaks = seq(-89.8, 88.4,178.2)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal_grid()  +
    theme(panel.grid.major = element_line(color = "black"),
          panel.ontop = TRUE,
          plot.title = element_text(family = "calibri", size = 28, face = "plain", hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(0.7, 'cm'), 
          legend.key.height = unit(0.7, 'cm'), 
          legend.key.width = unit(0.7, 'cm'), 
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 20),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm"))
  fig_change
  
  change_list[[i]] = assign(paste0("change_", model_list[i]), fig_change)
  
}

maps_EZ_lt_10 <- plot_grid(change_list[[1]], change_list[[3]], change_list[[4]],change_list[[2]],
                           change_list[[5]], change_list[[6]],change_list[[7]], change_list[[8]],
                           nrow = 8, ncol = 1)


maps_EZ <- plot_grid(maps_EZ_lt_1, maps_EZ_lt_10,
                     nrow = 1, ncol = 2)


ggsave("figS2_individual_model_EZ_depth_metric_maps_300m_scale.jpg", plot = maps_EZ, path = "~/master_notebooks/paper_figs/supplemental/", width = 30, height = 60, units = "cm", dpi = 600)


## Fig S3. Depth Horizon depths ---------

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
    
    change = raster(paste0("~/spatial_analysis/raster_output/",variable[i],"/multimodel_",variable[i],"_change_rg.asc"))
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
    title = "b) Change in EZ Depth"
  } else if(i == 5) {
    title = "d) Change in PCD"
  } else {
    title = expression(paste("f) ","Change in MLD"[max],sep=""))
  }
  
  fig_change <- ggplot() +
    geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  title,
         fill = "(m)") +
    scale_fill_cmocean(limits = c(0,200), oob = squish, name = "deep", direction = 1) +
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
  
  if(i == 2) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(0,150), oob = squish, name = "deep", direction = 1)
  } else if(i == 3) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(0,300), oob = squish, name = "deep", direction = 1)
  } else if(i == 4 | i == 5) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(-50,50), oob = squish, name = "balance", direction = 1)
  } else if(i == 6) {
    fig_change = fig_change + scale_fill_cmocean(limits = c(-150,150), oob = squish, name = "balance", direction = 1)
  } else {}
  
  map_list[[i]] = assign(variable[i], fig_change)
  
}

maps_grid <- plot_grid(
  plot_grid(map_list[[1]], map_list[[4]], map_list[[2]],
            map_list[[5]], map_list[[3]],map_list[[6]],nrow = 3, ncol = 2))

ggsave("figS2_DH_depth_maps.jpg", plot = maps_grid, path = "~/master_notebooks/paper_figs/supplemental/", width = 46, height = 36, units = "cm", dpi = 500)


## Fig S4. absolute time series at 5 DHs --------------

#my custom scale
color5 = c("#E78AC3", "#FFD92F", "#A6D854", "thistle2","#66C2A5" ,"#FC8D62", "navajowhite3", "#8DA0CB")

setwd("~/time_series_analysis/files/all_models/")
dfs <- list.files(pattern = "^time_series_expc*")
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
    ylab("POC flux (Pg C/yr)") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(limits = c(4,13.5)) +
    scale_color_manual(values = color5) +
    theme(plot.title = element_text(family = "calibri", size = 28),
          axis.text = element_text(family = "calibri", size = 18),
          axis.title.y = element_text(family = "calibri", size = 18, margin = margin(r = 10)),
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
    plot <- plot + labs(title = expression(paste("d)"," MLD"[max],sep="")))
  } else {}
  ts_list[[i]] = assign(DH[i], plot)
  
}

ts_DH <- plot_grid(
  plot_grid(ts_list[[1]] + theme(legend.position="none"), ts_list[[3]] + theme(legend.position="none"),
            ts_list[[5]] + theme(legend.position="none"), ts_list[[4]] + theme(legend.position="none"), nrow = 2, ncol = 2),
  plot_grid(NULL, ts_list[[2]], NULL, nrow = 1, rel_widths = c(0.32, 1, 0.08)),
  nrow = 2, rel_heights = c(0.7,0.36))

#ts_DH

ggsave(filename = "figS3_absolute_time_series_faceted.jpg", plot = ts_DH, path = "~/master_notebooks/paper_figs/supplemental/", width = 45, height = 45, units = "cm", dpi = 500)

## Fig SX. Intermodel POC flux DH differences ------------

#my custom scale
color_DH = c("#E78AC3","#66C2A5", "#FFD92F","#FC8D62", "#8DA0CB")

setwd("~/time_series_analysis/files/all_models/")
dfs <- list.files(pattern = "^normalized_time_series_expc*")
dfs <- dfs[-3] #remove EZ 10
DH_labs = c("100m", "1000m", "EZ", "MLDmax", "PCD")
plot.title = c("CESM2","GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  
#create empty list for storing for loop output
time.series <- list()

for(i in 1:length(dfs)) {
  
  #read in csv file
  df <- read_csv(dfs[i])
  #rename each model with DH info
  colnames(df) <- c("Year", paste0("CESM_",DH_labs[i]),paste0("CM4_",DH_labs[i]) ,paste0("CMCC_",DH_labs[i]), paste0("EC-Earth_",DH_labs[i]), paste0("GFDL_",DH_labs[i]), paste0("IPSL_",DH_labs[i]), paste0("MPI_",DH_labs[i]), paste0("UKESM_",DH_labs[i]))
  #store into list
  time.series[[i]] <- df
}

#join by year 
df <- time.series %>% 
  reduce(left_join, by = "Year")

model = c("CESM","CM4" ,"CMCC", "EC-Earth", "GFDL", "IPSL", "MPI", "UKESM")

DH_list <- list()

for(i in 1:length(model)) {
  
  DH_df = df %>% 
    dplyr::select(Year, paste0(model[i],"_100m"), paste0(model[i],"_EZ"),paste0(model[i],"_PCD"),paste0(model[i],"_MLDmax"),paste0(model[i],"_1000m"))
  DH_df <- data.table::melt(DH_df,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "DH")
  
  #plot time series
  plot <- ggplot() +
    geom_line(data = DH_df, aes(x = Year, y = POC_flux, color = DH), size = 1) +
    geom_smooth(data = DH_df, aes(x = Year, y = POC_flux, color = DH),size = 1.2, se = FALSE)  +
    theme_bw() +
    labs(title = plot.title[i], color = "Depth Horizon") +
    xlab(NULL) +
    ylab("Change Relative to 1850-1900 (%)") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous() +
    scale_color_manual(labels = c("100 m", "EZ Depth", "PCD", "MLDmax","1000 m"),values = color_DH) +
    theme(plot.title = element_text(family = "calibri", size = 28),
          axis.text = element_text(family = "calibri", size = 18),
          axis.title.y = element_text(family = "calibri", size = 18, margin = margin(r = 10)),
          legend.key.size = unit(1.5, 'cm'), 
          legend.key.height = unit(1.3, 'cm'), 
          legend.key.width = unit(2, 'cm'), 
          legend.text = element_text(family = "calibri", size = 20),
          legend.title = element_text(family = "calibri", size = 26),
          legend.box.spacing = unit(1, 'in'),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm")) +
    guides(color = guide_legend(override.aes = list(linewidth = 6)))
  
  plot
  
  DH_list[[i]] = assign(model[i], plot)
  
}

DH_combined <- plot_grid(DH_list[[1]]+theme(legend.position="none"), DH_list[[3]]+theme(legend.position="none"),DH_list[[4]]+theme(legend.position="none"),DH_list[[2]]+theme(legend.position="none"),
            DH_list[[5]]+theme(legend.position="none"),DH_list[[6]]+theme(legend.position="none"),DH_list[[7]]+theme(legend.position="none"),DH_list[[8]]+theme(legend.position="none"),nrow = 4, ncol = 2)
          

ggsave(filename = "figS5_intermodel_DH_time_series.jpg", plot = DH_combined, path = "~/master_notebooks/paper_figs/supplemental/", width = 40, height = 50, units = "cm", dpi = 500)

#calculate intermodel range between 100 m, EZ, and PCD change ----------
DH_ranges <- c()

for(i in 1:length(model)) {
  
DH_df = df %>% 
  dplyr::select(Year, paste0(model[i],"_100m"), paste0(model[i],"_EZ"),paste0(model[i],"_PCD"))

#calculate range of POC flux change
mean_lt <- dplyr::filter(DH_df, between(Year,2080,2100)) %>% 
  summarise_if(is.numeric, mean)
mean_lt <- subset(mean_lt, select = -c(1))
DH_ranges[i] = max(mean_lt) - min(mean_lt)

}

DH_ranges_MLDmax <- c()

for(i in 1:length(model)) {
  
  DH_df = df %>% 
    dplyr::select(Year, paste0(model[i],"_100m"), paste0(model[i],"_EZ"),paste0(model[i],"_PCD"),paste0(model[i],"_MLDmax"))
  
  #calculate range of POC flux change
  mean_lt <- dplyr::filter(DH_df, between(Year,2080,2100)) %>% 
    summarise_if(is.numeric, mean)
  mean_lt <- subset(mean_lt, select = -c(1))
  DH_ranges_MLDmax[i] = max(mean_lt) - min(mean_lt)
  
}

DH_ranges = as_tibble(DH_ranges) %>%
  cbind(wMLDmax = DH_ranges_MLDmax) %>%
  cbind(Model = plot.title)


write_csv(DH_ranges,"~/time_series_analysis/files/average_change/intermodel_POC_flux_DH_ranges.csv")

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
    ylab("POC flux (Pg C/yr)") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(limits = c(4,13.5)) +
    scale_color_manual(values = color5) +
    theme(plot.title = element_text(family = "calibri", size = 28),
          axis.text = element_text(family = "calibri", size = 18),
          axis.title.y = element_text(family = "calibri", size = 18, margin = margin(r = 10)),
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
    plot <- plot + labs(title = expression(paste("d)"," MLD"[max],sep="")))
  } else {}
  ts_list[[i]] = assign(DH[i], plot)
  
}

ts_DH <- plot_grid(
  plot_grid(ts_list[[1]] + theme(legend.position="none"), ts_list[[3]] + theme(legend.position="none"),
            ts_list[[5]] + theme(legend.position="none"), ts_list[[4]] + theme(legend.position="none"), nrow = 2, ncol = 2),
  plot_grid(NULL, ts_list[[2]], NULL, nrow = 1, rel_widths = c(0.32, 1, 0.08)),
  nrow = 2, rel_heights = c(0.7,0.36))

#ts_DH

ggsave(filename = "figS3_absolute_time_series_faceted.jpg", plot = ts_DH, path = "~/master_notebooks/paper_figs/supplemental/", width = 45, height = 45, units = "cm", dpi = 500)




## Fig S5. Absolute POC flux Spatial Maps ---------

land <- ne_download(scale = 50, type = 'land', category = 'physical')
world <- ne_load(scale = 50, type = 'land', category = 'physical', returnclass = "sf")

variable = c("POC_100", "POC_1000", "POC_EZ_depth", "POC_MLD_max", "POC_PCD")

map_list <- list()

for(i in 1:length(variable)) {
  
  change = raster(paste0("~/spatial_analysis/raster_output/",variable[i],"/multimodel_",variable[i],"_his_rg.asc"))
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
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  title,
         fill = expression(paste("mol/", m^2, "/yr",sep = " "))) +
    scale_fill_cmocean(limits = c(0,4), oob = squish, name = "deep", direction = -1, end = 0.9) +
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
    fig_change = fig_change + scale_fill_cmocean(limits = c(0,1), oob = squish, name = "deep", direction = -1, end = 0.9)
  } else if(i == 1) {
    fig_change = fig_change + geom_point(data = regions , aes(x = x, y = y, fill = value), size = 0.2, shape = 19)
  } else {}
  
  map_list[[i]] = assign(variable[i], fig_change)
  
}

maps_grid <- plot_grid(
  plot_grid(map_list[[1]], map_list[[3]],
            map_list[[5]], map_list[[4]],nrow = 2, ncol = 2),
  plot_grid(NULL, map_list[[2]], NULL, nrow = 1, rel_widths = c(0.2, 1, 0.2)),
  nrow = 2, rel_heights = c(0.65,0.3))

ggsave("figS4_historical_POC_flux_maps.jpg", plot = maps_grid, path = "~/master_notebooks/paper_figs/supplemental/", width = 57, height = 43, units = "cm", dpi = 500)

# Fig S7. Absolute POC flux, TE, e-ratio, NPP at each region -----------

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
    plot.subtitle = "Global"
  }
  
  if(i == 1) {
    plot.subtitle2 = "c)"
  } else if(i == 2) {
    plot.subtitle2 = "h)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "m)"
  } else if (i == 4) {
    plot.subtitle2 = "r)"
  }  else {
    plot.subtitle2 = "w)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/time_series_epc100_all.csv"))
  } else {
    df <- read_csv(paste0(regions[i], "_time_series_POC_100_all.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = POC_flux, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - POC flux 100m")) +
    labs(title = plot.subtitle2) +
    xlab(NULL) +
    ylab("Pg C/yr") +
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
    plot.subtitle2 = "j)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "o)"
  } else if (i == 4) {
    plot.subtitle2 = "t)"
  }  else {
    plot.subtitle2 = "y)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/time_series_expc_1000_all.csv"))
  } else {
    df <- read_csv(paste0(regions[i], "_time_series_POC_1000_all.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = POC_flux, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - POC flux 1000m")) +
    labs(title = plot.subtitle2) +
    xlab(NULL) +
    ylab("Pg C/yr") +
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
    plot.subtitle2 = "f)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "k)"
  } else if (i == 4) {
    plot.subtitle2 = "p)"
  }  else {
    plot.subtitle2 = "u)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/time_series_intpp_all.csv"))
  } else {
    df <- read_csv(paste0(regions[i], "_time_series_NPP_all.csv"))
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
    ylab("Pg C/yr") +
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
    plot.subtitle2 = "b)"
  } else if(i == 2) {
    plot.subtitle2 = "g)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "l)"
  } else if (i == 4) {
    plot.subtitle2 = "q)"
  }  else {
    plot.subtitle2 = "v)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/time_series_e_ratio_100.csv"))
  } else {
    df <- read_csv(paste0(regions[i], "_e_ratio_time_series_POC_100.csv"))
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
    plot = plot + ylab(NULL)
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
    plot.subtitle2 = "d)"
  } else if(i == 2) {
    plot.subtitle2 = "i)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "n)"
  } else if (i == 4) {
    plot.subtitle2 = "s)"
  }  else {
    plot.subtitle2 = "x)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/TE_time_series_100_1000.csv"))
  } else {
    df <- read_csv(paste0(regions[i], "_TE_time_series_POC_100_POC_1000.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = TE, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - TE")) +
    labs(title = plot.subtitle2) +
    xlab(NULL) +
    ylab("% Transferred") +
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
  
  
  TE_list[[i]] = assign(paste0("TE_", regions[i]), plot)
  
}


fig3 <- grid_arrange_shared_legend(NPP_list[[1]],e_ratio_list[[1]], POC_100_list[[1]], TE_list[[1]], POC_1000_list[[1]],
                                   NPP_list[[2]],e_ratio_list[[2]], POC_100_list[[2]], TE_list[[2]], POC_1000_list[[2]],
                                   NPP_list[[3]],e_ratio_list[[3]], POC_100_list[[3]], TE_list[[3]], POC_1000_list[[3]],
                                   NPP_list[[4]],e_ratio_list[[4]], POC_100_list[[4]], TE_list[[4]], POC_1000_list[[4]],
                                   NPP_list[[5]],e_ratio_list[[5]], POC_100_list[[5]], TE_list[[5]], POC_1000_list[[5]],
                                   nrow = 5, ncol = 5, position = "right")

ggsave(filename = "figS5_regional_interrelations_absolute.jpg", plot = fig3, path = "~/master_notebooks/paper_figs/supplemental", width = 74, height = 57, units = "cm", dpi = 600)

## Fig S8. Regional time series at each depth horizon ---------

#my custom scale
color5 = c("#E78AC3", "#FFD92F", "#A6D854", "thistle2","#66C2A5" ,"#FC8D62", "navajowhite3", "#8DA0CB")

POC_100_list <- list()
POC_EZ_list <- list()
POC_PCD_list <- list()
POC_MLD_max_list <- list()
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
    plot.subtitle = "Global"
  }
  
  if(i == 1) {
    plot.subtitle2 = "a)"
  } else if(i == 2) {
    plot.subtitle2 = "f)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "k)"
  } else if (i == 4) {
    plot.subtitle2 = "p)"
  }  else {
    plot.subtitle2 = "u)"
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
    #labs(title = paste0(plot.subtitle, " - 100 m")) +
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
    plot.subtitle2 = "j)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "o)"
  } else if (i == 4) {
    plot.subtitle2 = "t)"
  }  else {
    plot.subtitle2 = "y)"
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
    #labs(title = paste0(plot.subtitle, " - 1000 m")) +
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
  
  
  POC_1000_list[[i]] = assign(paste0("POC_1000_", regions[i]), plot)
  
}

# POC PCD --------------

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
    plot.subtitle2 = "c)"
  } else if(i == 2) {
    plot.subtitle2 = "h)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "m)"
  } else if (i == 4) {
    plot.subtitle2 = "r)"
  }  else {
    plot.subtitle2 = "w)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_time_series_expc_PCD.csv"))
  } else {
    df <- read_csv(paste0("normalized_", regions[i], "_time_series_POC_PCD_all.csv"))  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'NPP', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = NPP, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - PCD")) +
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
  
  
  POC_PCD_list[[i]] = assign(paste0("POC_PCD_", regions[i]), plot)
  
}

# POC EZ --------------

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
    plot.subtitle2 = "b)"
  } else if(i == 2) {
    plot.subtitle2 = "g)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "l)"
  } else if (i == 4) {
    plot.subtitle2 = "q)"
  }  else {
    plot.subtitle2 = "v)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_time_series_expc_ez.csv"))
  } else {
    df <- read_csv(paste0("normalized_", regions[i], "_time_series_POC_EZ_all.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = e_ratio, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - EZ Depth")) +
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
  
  
  POC_EZ_list[[i]] = assign(paste0("POC_EZ_", regions[i]), plot)
  
}

# POC MLDmax ----------

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
    plot.subtitle2 = "d)"
  } else if(i == 2) {
    plot.subtitle2 = "i)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "n)"
  } else if (i == 4) {
    plot.subtitle2 = "s)"
  }  else {
    plot.subtitle2 = "x)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/normalized_time_series_expc_MLDmax.csv"))
  } else {
    df <- read_csv(paste0("normalized_", regions[i], "_time_series_POC_MLDmax_all.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = TE, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - MLDmax")) +
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
  
  POC_MLD_max_list[[i]] = assign(paste0("POC_MLD_max_", regions[i]), plot)
  
}


fig3 <- grid_arrange_shared_legend(POC_100_list[[1]],POC_EZ_list[[1]], POC_PCD_list[[1]], POC_MLD_max_list[[1]], POC_1000_list[[1]],
                                   POC_100_list[[2]],POC_EZ_list[[2]], POC_PCD_list[[2]], POC_MLD_max_list[[2]], POC_1000_list[[2]],
                                   POC_100_list[[3]],POC_EZ_list[[3]], POC_PCD_list[[3]], POC_MLD_max_list[[3]], POC_1000_list[[3]],
                                   POC_100_list[[4]],POC_EZ_list[[4]], POC_PCD_list[[4]], POC_MLD_max_list[[4]], POC_1000_list[[4]],
                                   POC_100_list[[5]],POC_EZ_list[[5]], POC_PCD_list[[5]], POC_MLD_max_list[[5]], POC_1000_list[[5]],
                                   nrow = 5, ncol = 5, position = "right")

ggsave(filename = "figS6_regional_POC_flux.jpg", plot = fig3, path = "~/master_notebooks/paper_figs/supplemental/", width = 74, height = 57, units = "cm", dpi = 600)

# Fig S5. Absolute POC flux, TE, e-ratio, NPP at each region -----------

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
    plot.subtitle = "Global"
  }
  
  if(i == 1) {
    plot.subtitle2 = "c)"
  } else if(i == 2) {
    plot.subtitle2 = "h)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "m)"
  } else if (i == 4) {
    plot.subtitle2 = "r)"
  }  else {
    plot.subtitle2 = "w)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/time_series_epc100_all.csv"))
  } else {
    df <- read_csv(paste0(regions[i], "_time_series_POC_100_all.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = POC_flux, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - POC flux 100m")) +
    labs(title = plot.subtitle2) +
    xlab(NULL) +
    ylab("Pg C/yr") +
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
    plot.subtitle2 = "j)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "o)"
  } else if (i == 4) {
    plot.subtitle2 = "t)"
  }  else {
    plot.subtitle2 = "y)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/time_series_expc_1000_all.csv"))
  } else {
    df <- read_csv(paste0(regions[i], "_time_series_POC_1000_all.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = POC_flux, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - POC flux 1000m")) +
    labs(title = plot.subtitle2) +
    xlab(NULL) +
    ylab("Pg C/yr") +
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
    plot.subtitle2 = "f)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "k)"
  } else if (i == 4) {
    plot.subtitle2 = "p)"
  }  else {
    plot.subtitle2 = "u)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/time_series_intpp_all.csv"))
  } else {
    df <- read_csv(paste0(regions[i], "_time_series_NPP_all.csv"))
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
    ylab("Pg C/yr") +
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
    plot.subtitle2 = "b)"
  } else if(i == 2) {
    plot.subtitle2 = "g)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "l)"
  } else if (i == 4) {
    plot.subtitle2 = "q)"
  }  else {
    plot.subtitle2 = "v)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/time_series_e_ratio_100.csv"))
  } else {
    df <- read_csv(paste0(regions[i], "_e_ratio_time_series_POC_100.csv"))
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
    plot = plot + ylab(NULL)
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
    plot.subtitle2 = "d)"
  } else if(i == 2) {
    plot.subtitle2 = "i)"
    #low latitudes without Equatorial Pacific
  } else if (i == 3) {
    plot.subtitle2 = "n)"
  } else if (i == 4) {
    plot.subtitle2 = "s)"
  }  else {
    plot.subtitle2 = "x)"
  }
  
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  if(i == 1) {
    df <- read_csv(paste0("~/time_series_analysis/files/all_models/TE_time_series_100_1000.csv"))
  } else {
    df <- read_csv(paste0(regions[i], "_TE_time_series_POC_100_POC_1000.csv"))
  }
  
  colnames(df) <- c("Year", "CESM2", "GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  df.melt <- data.table::melt(df,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")
  
  plot <- ggplot(data = df.melt, aes(x = Year, y = TE, color = Model)) +
    geom_line(size = 1) +
    geom_smooth(size = 1, se = FALSE) +
    theme_bw() +
    #labs(title = paste0(plot.subtitle, " - TE")) +
    labs(title = plot.subtitle2) +
    xlab(NULL) +
    ylab("% Transferred") +
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
  
  
  TE_list[[i]] = assign(paste0("TE_", regions[i]), plot)
  
}


fig3 <- grid_arrange_shared_legend(NPP_list[[1]],e_ratio_list[[1]], POC_100_list[[1]], TE_list[[1]], POC_1000_list[[1]],
                                   NPP_list[[2]],e_ratio_list[[2]], POC_100_list[[2]], TE_list[[2]], POC_1000_list[[2]],
                                   NPP_list[[3]],e_ratio_list[[3]], POC_100_list[[3]], TE_list[[3]], POC_1000_list[[3]],
                                   NPP_list[[4]],e_ratio_list[[4]], POC_100_list[[4]], TE_list[[4]], POC_1000_list[[4]],
                                   NPP_list[[5]],e_ratio_list[[5]], POC_100_list[[5]], TE_list[[5]], POC_1000_list[[5]],
                                   nrow = 5, ncol = 5, position = "right")

ggsave(filename = "figS5_regional_interrelations_absolute.jpg", plot = fig3, path = "~/master_notebooks/paper_figs/supplemental", width = 74, height = 57, units = "cm", dpi = 600)


#Fig S6. individual model POC flux  --------

change_list = list()

land <- ne_download(scale = 50, type = 'land', category = 'physical')
world <- ne_load(scale = 50, type = 'land', category = 'physical', returnclass = "sf")

model_fullnames = c("CESM2","GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
model_list = c("CESM","CM4" ,"CMCC", "EC-Earth", "GFDL", "IPSL", "MPI", "UKESM")
DHs = c("POC_100", "POC_EZ", "POC_PCD", "POC_MLD_max", "POC_1000")


## 100 m -------

for(i in 1:length(model_list)) {
  
  esm_change <- raster(paste0("~/spatial_analysis/raster_output/POC_100/",model_list[i],"_POC_100_change_rg.asc"))
  #set projection for original raster (Mercator)
  crs(esm_change) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  rob_sign <- projectRaster(esm_change,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(rob_sign) = "POC"
  #prepare for plotting
  points = rasterToPoints(rob_sign, spatial = TRUE)
  geo3  <- data.frame(points)
  df  <- data.frame(geo3)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  
  fig_change <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  model_fullnames[i],
         fill = expression(paste("mol/", m^2, "/yr",sep = " "))) +
    scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1) +
    scale_x_continuous(breaks = seq(-179.6, 179.9, 359.5)) +
    scale_y_continuous(breaks = seq(-89.8, 88.4,178.2)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal_grid()  +
    theme(panel.grid.major = element_line(color = "black"),
          panel.ontop = TRUE,
          plot.title = element_text(family = "calibri", size = 28, face = "plain", hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(0.7, 'cm'), 
          legend.key.height = unit(0.7, 'cm'), 
          legend.key.width = unit(0.7, 'cm'), 
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 20),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm"))
  fig_change
  
  #if(i == 5) {
  #  fig_change = fig_change + scale_fill_cmocean(limits = c(-0.2,0.2), oob = squish, name = "balance", direction = 1)
  
  fig_change
  
  change_list[[i]] = assign(paste0("change_", model_list[i]), fig_change)
  
}

maps_POC_100 <- plot_grid(change_list[[1]], change_list[[3]], change_list[[4]],change_list[[2]],
                          change_list[[5]], change_list[[6]],change_list[[7]], change_list[[8]],
                          nrow = 8, ncol = 1)
# PCD ----------

for(i in 1:length(model_list)) {
  
  esm_change <- raster(paste0("~/spatial_analysis/raster_output/POC_PCD/",model_list[i],"_POC_PCD_change_rg.asc"))
  #set projection for original raster (Mercator)
  crs(esm_change) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  rob_sign <- projectRaster(esm_change,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(rob_sign) = "POC"
  #prepare for plotting
  points = rasterToPoints(rob_sign, spatial = TRUE)
  geo3  <- data.frame(points)
  df  <- data.frame(geo3)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  
  fig_change <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  model_fullnames[i],
         fill = expression(paste("mol/", m^2, "/yr",sep = " "))) +
    scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1) +
    scale_x_continuous(breaks = seq(-179.6, 179.9, 359.5)) +
    scale_y_continuous(breaks = seq(-89.8, 88.4,178.2)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal_grid()  +
    theme(panel.grid.major = element_line(color = "black"),
          panel.ontop = TRUE,
          plot.title = element_text(family = "calibri", size = 28, face = "plain", hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(0.7, 'cm'), 
          legend.key.height = unit(0.7, 'cm'), 
          legend.key.width = unit(0.7, 'cm'), 
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 20),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm"))
  fig_change
  
  #if(i == 5) {
  #  fig_change = fig_change + scale_fill_cmocean(limits = c(-0.2,0.2), oob = squish, name = "balance", direction = 1)
  
  fig_change
  
  change_list[[i]] = assign(paste0("change_", model_list[i]), fig_change)
  
}

maps_POC_PCD <- plot_grid(change_list[[1]], change_list[[3]], change_list[[4]],change_list[[2]],
                          change_list[[5]], change_list[[6]],change_list[[7]], change_list[[8]],
                          nrow = 8, ncol = 1)

## MLD max ---------

for(i in 1:length(model_list)) {
  
  esm_change <- raster(paste0("~/spatial_analysis/raster_output/POC_MLD_max/",model_list[i],"_POC_MLD_max_change_rg.asc"))
  #set projection for original raster (Mercator)
  crs(esm_change) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  rob_sign <- projectRaster(esm_change,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(rob_sign) = "POC"
  #prepare for plotting
  points = rasterToPoints(rob_sign, spatial = TRUE)
  geo3  <- data.frame(points)
  df  <- data.frame(geo3)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  
  fig_change <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  model_fullnames[i],
         fill = expression(paste("mol/", m^2, "/yr",sep = " "))) +
    scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1) +
    scale_x_continuous(breaks = seq(-179.6, 179.9, 359.5)) +
    scale_y_continuous(breaks = seq(-89.8, 88.4,178.2)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal_grid()  +
    theme(panel.grid.major = element_line(color = "black"),
          panel.ontop = TRUE,
          plot.title = element_text(family = "calibri", size = 28, face = "plain", hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(0.7, 'cm'), 
          legend.key.height = unit(0.7, 'cm'), 
          legend.key.width = unit(0.7, 'cm'), 
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 20),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm"))
  fig_change
  
  #if(i == 5) {
  #  fig_change = fig_change + scale_fill_cmocean(limits = c(-0.2,0.2), oob = squish, name = "balance", direction = 1)
  
  fig_change
  
  change_list[[i]] = assign(paste0("change_", model_list[i]), fig_change)
  
}

maps_POC_MLDmax <- plot_grid(change_list[[1]], change_list[[3]], change_list[[4]],change_list[[2]],
                             change_list[[5]], change_list[[6]],change_list[[7]], change_list[[8]],
                             nrow = 8, ncol = 1)

## EZ depth ----------

for(i in 1:length(model_list)) {
  
  if (i == 4) {
    esm_change <- raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model_list[i],"_POC_EZ_depth_lt_rg.asc"))
  } else {
    esm_change <- raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model_list[i],"_POC_EZ_depth_change_rg.asc"))
  }
  #set projection for original raster (Mercator)
  crs(esm_change) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  rob_sign <- projectRaster(esm_change,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(rob_sign) = "POC"
  #prepare for plotting
  points = rasterToPoints(rob_sign, spatial = TRUE)
  geo3  <- data.frame(points)
  df  <- data.frame(geo3)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  fig_change <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  model_fullnames[i],
         fill = expression(paste("mol/", m^2, "/yr",sep = " "))) +
    scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1) +
    scale_x_continuous(breaks = seq(-179.6, 179.9, 359.5)) +
    scale_y_continuous(breaks = seq(-89.8, 88.4,178.2)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal_grid()  +
    theme(panel.grid.major = element_line(color = "black"),
          panel.ontop = TRUE,
          plot.title = element_text(family = "calibri", size = 28, face = "plain", hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(0.7, 'cm'), 
          legend.key.height = unit(0.7, 'cm'), 
          legend.key.width = unit(0.7, 'cm'), 
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 20),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm"))
  fig_change
  
  #if(i == 5) {
  #  fig_change = fig_change + scale_fill_cmocean(limits = c(-0.2,0.2), oob = squish, name = "balance", direction = 1)
  
  fig_change
  
  change_list[[i]] = assign(paste0("change_", model_list[i]), fig_change)
  
}

maps_POC_EZ <- plot_grid(change_list[[1]], change_list[[3]], change_list[[4]],change_list[[2]],
                         change_list[[5]], change_list[[6]],change_list[[7]], change_list[[8]],
                         nrow = 8, ncol = 1)

## 1000 m ---------

for(i in 1:length(model_list)) {
  
  esm_change <- raster(paste0("~/spatial_analysis/raster_output/POC_1000/",model_list[i],"_POC_1000_change_rg.asc"))
  #set projection for original raster (Mercator)
  crs(esm_change) = "+proj=longlat +datum=WGS84 +no_defs"
  #change to Robinson projection
  rob_sign <- projectRaster(esm_change,crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  names(rob_sign) = "POC"
  #prepare for plotting
  points = rasterToPoints(rob_sign, spatial = TRUE)
  geo3  <- data.frame(points)
  df  <- data.frame(geo3)
  df = subset(df, select = -c(4))
  colnames(df) = c("value", "x", "y")
  
  
  fig_change <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
    geom_sf(data = world, fill = "grey80") +
    coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
    labs(title =  model_fullnames[i],
         fill = expression(paste("mol/", m^2, "/yr",sep = " "))) +
    scale_fill_cmocean(limits = c(-0.2,0.2), oob = squish, name = "balance", direction = 1) +
    scale_x_continuous(breaks = seq(-179.6, 179.9, 359.5)) +
    scale_y_continuous(breaks = seq(-89.8, 88.4,178.2)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_minimal_grid()  +
    theme(panel.grid.major = element_line(color = "black"),
          panel.ontop = TRUE,
          plot.title = element_text(family = "calibri", size = 28, face = "plain", hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(0.7, 'cm'), 
          legend.key.height = unit(0.7, 'cm'), 
          legend.key.width = unit(0.7, 'cm'), 
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 20),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm"))
  fig_change
  
  change_list[[i]] = assign(paste0("change_", model_list[i]), fig_change)
  
}

maps_POC_1000 <- plot_grid(change_list[[1]], change_list[[3]], change_list[[4]],change_list[[2]],
                           change_list[[5]], change_list[[6]],change_list[[7]], change_list[[8]],
                           nrow = 8, ncol = 1)


maps_DHs <- plot_grid(maps_POC_100, maps_POC_EZ, maps_POC_PCD, maps_POC_MLDmax, maps_POC_1000,
                      nrow = 1, ncol = 5)


#ggsave("figS1_individual_model_POC_flux_DH_change.jpg", plot = maps_grid_10, path = "~/master_notebooks/paper_figs/supplemental/", width = 20, height = 68, units = "cm", dpi = 600)

ggsave("figS1_individual_model_POC_flux_DH_change.jpg", plot = maps_DHs, path = "~/master_notebooks/paper_figs/supplemental/", width = 80, height = 65, units = "cm", dpi = 600)


# Fig S2. EZ metric time series comparison ----------------

#my custom scale
color5 = c("#E78AC3", "#FFD92F", "#A6D854", "thistle2","#66C2A5" ,"#FC8D62", "navajowhite3", "#8DA0CB")

ez_1 = read_csv("~/time_series_analysis/files/all_models/time_series_expc_ez_all.csv")
ez_10 = read_csv("~/time_series_analysis/files/all_models/time_series_expc_ez_10_all.csv")                
norm_ez_1 = read_csv("~/time_series_analysis/files/all_models/normalized_time_series_expc_ez.csv")
norm_ez_10 = read_csv("~/time_series_analysis/files/all_models/normalized_time_series_expc_ez_10.csv")

temp <- ez_1$CM4
ez_1 <- ez_1 %>% mutate(CM4 = ez_10$CM4)
ez_10 <- ez_10 %>% mutate(CM4 = temp)
temp2 <- norm_ez_1$CM4
norm_ez_1 <- norm_ez_1 %>% mutate(CM4 = norm_ez_10$CM4)
norm_ez_10 <- norm_ez_10 %>% mutate(CM4 = temp2)

plotsEZ <- list(ez_1,ez_10,norm_ez_1,norm_ez_10)

ts_list = list()
titlea = expression(paste("a) ","POC flux at EZ depth: 1% NPP"[max],sep=""))
titleb = expression(paste("b) ","POC flux at EZ depth: 10% NPP"[max],sep=""))
titlec = expression(paste("c) ","POC flux change at EZ depth: 1% NPP"[max],sep=""))
titled = expression(paste("d) ","POC flux change at EZ depth: 10% NPP"[max],sep=""))

plot.title = list(titlea,titleb,titlec,titled)

for(i in 1:length(plotsEZ)) {
  
  df = plotsEZ[[i]]
  colnames(df) <- c("Year", "CESM2","GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
  df <- df %>%
    relocate("GFDL-CM4", .before = "GFDL-ESM4")
  melt_df <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "Model")
  
  if (i == 1 | i == 2) {
    yaxis = "POC flux (Pg C/yr)"
  } else {
    yaxis = "Change Relative to 1850-1900 (%)"
  }
  #plot time series
  plot <- ggplot() +
    geom_line(data = melt_df, aes(x = Year, y = POC_flux, color = Model), size = 1.5) +
    geom_smooth(data = melt_df, aes(x = Year, y = POC_flux, color = Model),size = 1.5, se = FALSE) +
    theme_bw() +
    labs(title = plot.title[[i]]) +
    xlab(NULL) +
    ylab(yaxis) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(limits = c(4,13.5)) +
    scale_color_manual(values = color5) +
    theme(plot.title = element_text(family = "calibri", size = 25),
          axis.text = element_text(family = "calibri", size = 18),
          axis.title.y = element_text(family = "calibri", size = 18, margin = margin(r = 10)),
          legend.key.size = unit(1.5, 'cm'), 
          legend.key.height = unit(1.3, 'cm'), 
          legend.key.width = unit(2, 'cm'), 
          legend.text = element_text(family = "calibri", size = 20),
          legend.title = element_text(family = "calibri", size = 26),
          legend.box.spacing = unit(1, 'in'),
          plot.margin = unit(c(0.3,0.8,0.3,0.3), "cm")) +
    guides(color = guide_legend(override.aes = list(linewidth = 6)))
  
  plot
  
  if(i == 3 | i == 4) {
    plot <- plot + scale_y_continuous(limits = c(75,108))
  } else {}
  
  ts_list[[i]] = assign(DH[i], plot)
  
}

ts_EZ <- plot_grid(
  ts_list[[1]] + theme(legend.position = "none"),
  ts_list[[2]] + theme(legend.position = "none"),
  ts_list[[3]] + theme(legend.position = "none"),
  ts_list[[4]] + theme(legend.position = "none"),
  nrow = 2, ncol = 2)

ggsave(filename = "figS3_EZ_metric_time_series.jpg", plot = ts_EZ, path = "~/master_notebooks/paper_figs/supplemental/", width = 45, height = 35, units = "cm", dpi = 500)
