#'@title Assessment of DH depths and POC fluxes relative to 100m
#' @author Stevie Walker
#' @date 5/2/23
#' @description calculates and plots differences in EZ depth, MLDmax, and PCD relative to 100m
#' @note plots of actual depth differences are found in plotting functions for the depths (rg_plot_PCD.R, etc.)


plot_POC_DH_diff <- function(model.name,DH) {

change_100 = raster(paste0("~/spatial_analysis/raster_output/POC_100/",model.name,"_POC_100_change_rg.asc"))
his_100 = raster(paste0("~/spatial_analysis/raster_output/POC_100/",model.name,"_POC_100_his_rg.asc"))

if(model.name == "multimodel") {
model.title = "Multimodel"
} else {
  model.title = model.name
}

if (DH == "EZ_depth") {
  
  change = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_change_rg.asc"))
  his = raster(paste0("~/spatial_analysis/raster_output/POC_EZ_depth/",model.name,"_POC_EZ_depth_his_rg.asc"))
  
  plot.title = paste0(model.title, " POC flux difference, 100m - EZ depth")
  plot.title2 = paste0(model.title, " difference in POC flux change, 100m - EZ depth")

  } else if (DH == "MLDmax") {
  
    change = raster(paste0("~/spatial_analysis/raster_output/POC_MLD_max/",model.name,"_POC_MLD_max_change_rg.asc"))
    his = raster(paste0("~/spatial_analysis/raster_output/POC_MLD_max/",model.name,"_POC_MLD_max_his_rg.asc"))
    
    plot.title = paste0(model.title, " POC flux difference, 100m - MLDmax")
    plot.title2 = paste0(model.title, " difference in POC flux change, 100m - MLDmax")
    
  } else {
  
    change = raster(paste0("~/spatial_analysis/raster_output/POC_PCD/",model.name,"_POC_PCD_change_rg.asc"))
    his = raster(paste0("~/spatial_analysis/raster_output/POC_PCD/",model.name,"_POC_PCD_his_rg.asc"))
    
    plot.title = paste0(model.title, " POC flux difference, 100m - PCD")
    plot.title2 = paste0(model.title, " difference in POC flux change, 100m - PCD")
    
}

change_diff = change_100 - change
his_diff = his_100 - his

#convert raster to a 'conventional' dataframe
geo <- rasterToPoints(his_diff, spatial = TRUE)
df  <- data.frame(geo)
df = subset(df, select = -c(4))
colnames(df) = c("value", "x", "y")

fig_his <- ggplot() +
  geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
  geom_sf(data = world, fill = "snow") +
  coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
  labs(title = plot.title,
       fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
  scale_fill_cmocean(limits = c(-2,2), oob = squish, name = "balance", direction = 1) +
  scale_y_continuous(breaks = seq(-80,80, by = 20)) +
  scale_x_continuous(breaks = seq(-180,180, by = 30)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(panel.grid = element_blank())

fig_his

#convert raster to a 'conventional' dataframe
geo <- rasterToPoints(change_diff, spatial = TRUE)
df  <- data.frame(geo)
df = subset(df, select = -c(4))
colnames(df) = c("value", "x", "y")

fig_change <- ggplot() +
  geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
  geom_sf(data = world, fill = "snow") +
  coord_sf(xlim = c(-179.5, 179.5), ylim = c(-89.5, 90.5), expand = FALSE) +
  labs(title = plot.title2,
       fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
  scale_fill_cmocean(limits = c(-1,1), oob = squish, name = "balance", direction = 1) +
  scale_y_continuous(breaks = seq(-80,80, by = 20)) +
  scale_x_continuous(breaks = seq(-180,180, by = 30)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(panel.grid = element_blank())

fig_change

figure <- grid.arrange(fig_his, fig_change, ncol = 2, nrow = 1)

ggsave(paste(model.name,"_POC_", DH, "_POC_100_differences.png",sep=""), plot = figure, path = "~/spatial_analysis/figures/POC_DH_differences/", width = 35, height = 10, units = "cm", dpi = 400)

}