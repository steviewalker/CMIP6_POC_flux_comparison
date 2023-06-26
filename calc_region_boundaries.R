#' @title Plot region boundaries
#' @author Stevie Walker
#' @date 6/6/23
#' @description creates a raster of region areas, for use in adding regional boundaries over global maps (ie Figure 2 and 4)

#load example raster to manipulate
map = raster(paste0("~/spatial_analysis/raster_output/POC_100/multimodel_POC_100_his_rg.asc"))

ocean = (map >=0)
ocean[ocean==TRUE] = NA

#southern ocean
ocean[150,] = 0
#low latitudes
ocean[120,] = 0
ocean[60,] = 0
#north atlantic
ocean[25:50,180] = 0
ocean[25:50,110] = 0
ocean[25,110:180] = 0
ocean[50,110:180] = 0
#equatorial pacific
ocean[75:105,105] = 0
ocean[75:105,0] = 0
ocean[75:105,340] = 0
ocean[75,340:361] = 0
ocean[105,340:361] = 0
ocean[75,0:105] = 0
ocean[105,0:105] = 0

plot(ocean)

writeRaster(ocean, "~/regional_time_series_analysis/files/region_area/region_boundaries_map.asc", overwrite = TRUE)

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

sign_agreement <- raster(paste0("~/spatial_analysis/raster_output/model_agreement/",variable,"_7_model_sign_match.asc"))

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

change = raster(paste0("~/spatial_analysis/raster_output/",variable,"/multimodel_",variable,"_change_rg.asc"))
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
if(variable == "POC_100") {
  title = "a) 100m"
} else if(variable == "POC_1000") {
  title = "e) 1000m"
} else if(variable == "POC_MLD_max") {
  title = expression(paste("d)"," MLD"[max],sep=""))
} else if(variable == "POC_EZ_depth") {
  title = "c) EZ Depth"
} else {
  title = "b) PCD"
}

fig_change <- ggplot() +
  geom_raster(data = df , aes(x = x, y = y, fill = value)) + 
  geom_point(data = df_sign, aes(x = x, y = y, fill = value), na.rm = FALSE, size = 0.1, shape = 20, alpha = 0.4) +
  geom_sf(data = world, fill = "grey80") +
  geom_point(data = regions , aes(x = x, y = y, fill = value), size = 0.2, shape = 19) + 
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs", expand = FALSE) +
  labs(title =  title,
       fill = expression(paste("mol ", m^-2, " ", yr^-1,sep = " "))) +
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
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

fig_change

ggsave("region_area_map.png", plot = fig_change, path = "~/regional_time_series_analysis/figures/region_area/", width = 26, height = 15, units = "cm", dpi = 400)
