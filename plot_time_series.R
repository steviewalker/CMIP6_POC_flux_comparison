#' @title Plotting Globally Integrated Time Series Data
#' @author Stevie Walker
#' @date 1/25/22
#' @description plots 4 figures of globally integrated time series at 100m, MLDmax, normalized 100m, and normalized MLDmax
#' @description also saves tidied and combined time series data frames

#colors for time series
color = c("violet", "brown2" ,"goldenrod2", "aquamarine3", "darkorchid3", "darkorange2", "indianred4", "royalblue2")

#Rcolorbrewer Set1 - colorblind friendly
color2 = c("#F781BF", "#E41A1C", "#FFD92F", "#4DAF4A", "#984EA3" ,"#FF7F00", "#A65628", "#377EB8")
#Rcolorbrewer Set2 - colorblind friendly
color3 = c("#E78AC3", "#B3B3B3", "#FFD92F", "#A6D854", "#66C2A5" ,"#FC8D62", "#E5C494", "#8DA0CB")
#Rcolorbrewer mix of 1 and 2
color4 = c("#F781BF", "#E41A1C", "#FFD92F", "#66C2A5", "#984EA3" ,"#FF7F00", "#A65628", "#377EB8")

#my custom scale
color5 = c("#E78AC3", "gainsboro", "#FFD92F", "#A6D854", "#66C2A5" ,"#FC8D62", "#E5C494", "#8DA0CB")


#customPalette <- brewer.pal(8, "Set1") #create your color palette

## 1. TIME SERIES AT 100m ---------------

setwd("~/time_series_analysis/files/POC_100/")
df.sep <- list.files(pattern = "*_time_series.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year (2015 has a lot of repeats, but that doesn't effect later plotting)
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/time_series_analysis/files/all_models/time_series_epc100_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux_100', variable.name = "Model")

#plot time series at 100m
figure <- ggplot(data = df2, aes(x = Year, y = POC_flux_100, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at 100 m (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +  
  scale_color_manual(values = color3) +
  scale_x_continuous(expand = c(0,0)) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure

#save figure
ggsave(filename = "time_series_POC_100.png", plot = figure, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


## 2. TIME SERIES AT MLDMAX -------------------


setwd("~/time_series_analysis/files/POC_MLDmax/")
df.expc <- list.files(pattern = "*_time_series_expc_MLDmax.csv$")

#create empty list for storing for loop output
time.series2 <- list()

for(i in df.expc) {
  
  #read in csv file
  df.expc <- read_csv(i)
  #get rid of random ...1 column
  df.expc <- subset(df.expc, select = -c(...1))
  #store into list
  time.series2[[i]] <- df.expc
}

#join by year (2015 has a lot of repeats, but that doesn't effect later plotting)
df.expc <- time.series2 %>% 
  reduce(left_join, by = "Year")

#add column for model key
df2.expc <- data.table::melt(df.expc,  id.vars = 'Year', value.name = 'POC_flux_expc', variable.name = "Model")

#save POC flux at MLDmax time-series data frame for all models
write_csv(df.expc, file = "~/time_series_analysis/files/all_models/time_series_expc_MLDmax_all.csv")


#plot time series at MLDmax
figure2 <- ggplot(data = df2.expc, aes(x = Year, y = POC_flux_expc, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at MLDmax (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure2

#save figure
ggsave(filename = "time_series_POC_MLDmax.png", plot = figure2, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


## 3. NORMALIZED POC FLUX AT 100M RELATIVE TO 1850-1900 MEAN ----------------


#read in df created for first figure
epc100.all <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_100_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(epc100.all, Year <= 1900) %>% 
  summarise_if(is.numeric, mean)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#need to fix year column
normalized.epc100 <- 
  subset(epc100.all, select = -c(1))

#calculate normalized POC flux
normalized.epc100 <- mapply('/', normalized.epc100, mean1850_1900)*100

normalized.epc100 <- normalized.epc100 %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM)

#save df
write_csv(normalized.epc100, "~/time_series_analysis/files/all_models/normalized_time_series_epc100.csv")

#add column for model key (reformatting data specific to the below plot)
plot.normalized.epc100 <- data.table::melt(normalized.epc100,  id.vars = 'Year', value.name = 'POC_flux_100', variable.name = "Model")

#my custom scale
color5 = c("#E78AC3", "thistle2", "#FFD92F", "#A6D854", "#66C2A5" ,"#FC8D62", "navajowhite3", "#8DA0CB")

#plot time series at 100m
figure3 <- ggplot(data = plot.normalized.epc100, aes(x = Year, y = POC_flux_100, color = Model)) +
  geom_line(size = 1) +
  geom_smooth(size = 1, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Global POC Flux at 100m (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Percent Change") +
  #scale_y_continuous(limits = c(75, 110), n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(75,108)) +
  scale_color_manual(values = color5) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12)) +
  guides(color = guide_legend(override.aes = list(linewidth = 6)))
figure3

#save figure
ggsave(filename = "normalized_time_series_POC_100.png", plot = figure3, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


## 4. NORMALIZED POC FLUX AT MLD MAX RELATIVE TO 1850-1900 AVG --------------


#read in df created for first figure
expc.all <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_MLDmax_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(expc.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#need to fix year column
normalized.expc <- 
  subset(expc.all, select = -c(1))

#calculate normalized POC flux
normalized.expc <- mapply('/', normalized.expc, mean1850_1900)*100

normalized.expc <- normalized.expc %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.expc, "~/time_series_analysis/files/all_models/normalized_time_series_expc_MLDmax.csv")


#add column for model key (reformatting data specific to the below plot)
plot.normalized.expc <- data.table::melt(normalized.expc,  id.vars = 'Year', value.name = 'POC_flux_MLDmax', variable.name = "Model")

#plot time series at 100m
figure4 <- ggplot(data = plot.normalized.expc, aes(x = Year, y = POC_flux_MLDmax, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Global POC Flux at MLDmax (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Percent Change") +
  scale_y_continuous(limits = c(75, 108), n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure4

#save figure
ggsave(filename = "normalized_time_series_POC_MLDmax.png", plot = figure4, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 5. TIME SERIES AT 1000M -------------

setwd("~/time_series_analysis/files/POC_1000/")
df.sep <- list.files(pattern = "*_time_series_expc_1000.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/time_series_analysis/files/all_models/time_series_expc_1000_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux_1000', variable.name = "Model")


#plot time series at 100m
figure5 <- ggplot(data = df2, aes(x = Year, y = POC_flux_1000, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at 1000m (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure5

#save figure
ggsave(filename = "time_series_POC_1000.png", plot = figure5, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


## 6. NORMALIZED POC FLUX AT 1000M RELATIVE TO 1850-1900 AVG --------------


#read in df created for first figure
ts.1000.all <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_1000_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(ts.1000.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#need to fix year column
normalized.1000 <- 
  subset(ts.1000.all, select = -c(1))

#calculate normalized POC flux
normalized.1000 <- mapply('/', normalized.1000, mean1850_1900)*100

normalized.1000 <- normalized.1000 %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.1000, "~/time_series_analysis/files/all_models/normalized_time_series_expc_1000.csv")


#add column for model key (reformatting data specific to the below plot)
plot.normalized.1000 <- data.table::melt(normalized.1000,  id.vars = 'Year', value.name = 'POC_flux_1000', variable.name = "Model")

#plot time series at 1000m
figure6 <- ggplot(data = plot.normalized.1000, aes(x = Year, y = POC_flux_1000, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Global POC Flux at 1000m (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Percent Change") +
  scale_color_manual(values = color) +
  scale_x_continuous(expand = c(0,0)) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))#+
#scale_y_continuous(limits = c(60, 112), n.breaks = 6)

figure6

#save figure
ggsave(filename = "normalized_time_series_POC_1000.png", plot = figure6, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


## 7. FACETED NORMALIZED TIME SERIES FIGURE -----------

combined <- grid.arrange(figure3, figure4,figure6, ncol = 1)

ggsave(filename = "normalized_time_series_faceted.png", plot = combined, path = "~/time_series_analysis/figures/faceted/", width = 20, height = 36, units = "cm", dpi = 400)

combined2 <- grid.arrange(figure, figure2, figure5, ncol = 1)

ggsave(filename = "time_series_faceted.png", plot = combined2, path = "~/time_series_analysis/figures/faceted", width = 20, height = 36, units = "cm", dpi = 400)


## 8. TIME SERIES NPP -------------------

setwd("~/time_series_analysis/files/NPP/")
df.npp <- list.files(pattern = "*_time_series_npp.csv$")

#create empty list for storing for loop output
time.series2 <- list()

for(i in df.npp) {
  
  #read in csv file
  df.npp <- read_csv(i)
  #get rid of random ...1 column
  df.npp <- subset(df.npp, select = -c(...1))
  #store into list
  time.series2[[i]] <- df.npp
}

#join by year (2015 has a lot of repeats, but that doesn't effect later plotting)
df.npp <- time.series2 %>% 
  reduce(left_join, by = "Year")

#add column for model key
df2.npp <- data.table::melt(df.npp,  id.vars = 'Year', value.name = 'NPP', variable.name = "Model")

#save NPP time-series data frame for all models
write_csv(df.npp, file = "~/time_series_analysis/files/all_models/time_series_npp_all.csv")

#plot time series at MLDmax
figure7 <- ggplot(data = df2.npp, aes(x = Year, y = NPP, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global NPP (1850-2100)") +
  xlab(NULL) +
  ylab("NPP (Pg C/yr)") +
  scale_y_continuous(limits = c(28,61),n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure7

#save figure
ggsave(filename = "time_series_npp.png", plot = figure7, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

## 8b. TIME SERIES INTPP -------------------

setwd("~/time_series_analysis/files/NPP/")
df.npp <- list.files(pattern = "*_time_series_intpp.csv$")

#create empty list for storing for loop output
time.series2 <- list()

for(i in df.npp) {
  
  #read in csv file
  df.npp <- read_csv(i)
  #get rid of random ...1 column
  df.npp <- subset(df.npp, select = -c(...1))
  #store into list
  time.series2[[i]] <- df.npp
}

#join by year (2015 has a lot of repeats, but that doesn't effect later plotting)
df.npp <- time.series2 %>% 
  reduce(left_join, by = "Year")

#add column for model key
df2.npp <- data.table::melt(df.npp,  id.vars = 'Year', value.name = 'NPP', variable.name = "Model")

#save NPP time-series data frame for all models
write_csv(df.npp, file = "~/time_series_analysis/files/all_models/time_series_intpp_all.csv")

#plot time series at MLDmax
figure7b <- ggplot(data = df2.npp, aes(x = Year, y = NPP, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global NPP (1850-2100)") +
  xlab(NULL) +
  ylab("NPP (Pg C/yr)") +
  scale_y_continuous(limits = c(28,61),n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure7b

#save figure
ggsave(filename = "time_series_intpp.png", plot = figure7b, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 9. NORMALIZED NPP TIME SERIES ------------

#read in df created for first figure
ts.npp.all <- read_csv("~/time_series_analysis/files/all_models/time_series_intpp_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(ts.npp.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)
mean1850_1900 <- subset(mean1850_1900, select = -c(1))


#need to fix year column
normalized.npp <- 
  subset(ts.npp.all, select = -c(1))

#calculate normalized POC flux
normalized.npp <- mapply('/', normalized.npp, mean1850_1900)*100

normalized.npp <- normalized.npp %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#end of 21st century average
mean2090_2100 = dplyr::filter(normalized.npp, between(Year, 2090, 2100)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)
mean2090_2100

#add column for model key
normalized.npp2 <- data.table::melt(normalized.npp,  id.vars = 'Year', value.name = 'NPP', variable.name = "Model")


#save df
write_csv(normalized.npp, "~/time_series_analysis/files/all_models/normalized_time_series_npp.csv")


#plot NPP time series
figure8 <- ggplot(data = normalized.npp2, aes(x = Year, y = NPP, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Normalized Time Series Change in Global NPP (1850-2100)") +
  xlab(NULL) +
  ylab("NPP (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure8

#save figure
ggsave(filename = "normalized_time_series_npp.png", plot = figure8, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


#read in df created for first figure
ts.npp.all <- read_csv("~/time_series_analysis/files/all_models/time_series_intpp_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(ts.npp.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)
mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#need to fix year column
normalized.npp <- 
  subset(ts.npp.all, select = -c(1))

#calculate normalized POC flux
normalized.npp <- mapply('/', normalized.npp, mean1850_1900)*100

normalized.npp <- normalized.npp %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#add column for model key
normalized.npp2 <- data.table::melt(normalized.npp,  id.vars = 'Year', value.name = 'NPP', variable.name = "Model")


#save df
write_csv(normalized.npp, "~/time_series_analysis/files/all_models/normalized_time_series_intpp.csv")


#plot NPP time series
figure8 <- ggplot(data = normalized.npp2, aes(x = Year, y = NPP, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Normalized Time Series Change in Global NPP (1850-2100)") +
  xlab(NULL) +
  ylab("NPP (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure8

#save figure
ggsave(filename = "normalized_time_series_intpp.png", plot = figure8, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 10a. TRANSFER EFFICIENCY TIME SERIES 100m to 1000m -----------

#read in data frames
setwd("~/time_series_analysis/files/all_models/")
ts_100 <- read_csv("time_series_epc100_all.csv")
ts_1000 <- read_csv("time_series_expc_1000_all.csv")

no_year_100 <- ts_100[2:9]
no_year_1000 <- ts_1000[2:9]

TE <- (no_year_1000/no_year_100)*100
TE$Year = 1850:2100

TE <- TE %>%
  dplyr::relocate(Year, .before = CESM) 

#save df
write_csv(TE, "~/time_series_analysis/files/all_models/TE_time_series_100_1000.csv")


#add column for model key
TE2 <- data.table::melt(TE,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure10a <- ggplot(data = TE2, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between 100m and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure10a

#save figure
ggsave(filename = "time_series_TE_100_1000.png", plot = figure10a, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

#10b. TRANSFER EFFICIENCY TIME SERIES MLDMAX TO 1000M ---------------

#read in data frames
setwd("~/time_series_analysis/files/all_models/")
ts_expc <- read_csv("time_series_expc_MLDmax_all.csv")
ts_1000 <- read_csv("time_series_expc_1000_all.csv")

no_year_expc <- ts_expc[2:9]
no_year_1000 <- ts_1000[2:9]

TE <- (no_year_1000/no_year_expc)*100
TE$Year = 1850:2100

TE <- TE %>%
  dplyr::relocate(Year, .before = CESM) 

#save df
write_csv(TE, "~/time_series_analysis/files/all_models/TE_time_series_MLDmax_1000.csv")


#add column for model key
TE2 <- data.table::melt(TE,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure10b <- ggplot(data = TE2, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between MLDmax and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure10b

#save figure
ggsave(filename = "time_series_TE_MLDmax_1000.png", plot = figure10b, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 10c. TRANSFER EFFICIENCY TIME SERIES EZ Depth to 1000m -----------

#read in data frames
setwd("~/time_series_analysis/files/all_models/")
ts_ez <- read_csv("time_series_expc_ez_all.csv")
ts_1000 <- read_csv("time_series_expc_1000_all.csv")

no_year_ez <- ts_ez[2:9]

no_year_1000 <- ts_1000[2:9]

TE <- (no_year_1000/no_year_ez)*100
TE$Year = 1850:2100

TE <- TE %>%
  dplyr::relocate(Year, .before = CESM) 

#save df
write_csv(TE, "~/time_series_analysis/files/all_models/TE_time_series_ez_1000.csv")


#add column for model key
TE2 <- data.table::melt(TE,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure10c <- ggplot(data = TE2, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between EZ Depth and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))


figure10c

#save figure
ggsave(filename = "time_series_TE_ez_1000.png", plot = figure10c, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 10d. TRANSFER EFFICIENCY TIME SERIES PCD to 1000m -----------

#read in data frames
setwd("~/time_series_analysis/files/all_models/")
ts_ez <- read_csv("time_series_expc_PCD_all.csv")
ts_1000 <- read_csv("time_series_expc_1000_all.csv")

no_year_ez <- ts_ez[2:9]

no_year_1000 <- ts_1000[2:9]

TE <- (no_year_1000/no_year_ez)*100
TE$Year = 1850:2100

TE <- TE %>%
  dplyr::relocate(Year, .before = CESM) 

#save df
write_csv(TE, "~/time_series_analysis/files/all_models/TE_time_series_PCD_1000.csv")


#add column for model key
TE2 <- data.table::melt(TE,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure10d <- ggplot(data = TE2, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between PCD and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))


figure10d

#save figure
ggsave(filename = "time_series_TE_PCD_1000.png", plot = figure10d, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 11a. NORMALIZED TRANSFER EFFICIENCY TIME SERIES 100m to 1000m -----------

#read in df created for first figure
TE.all <- read_csv("~/time_series_analysis/files/all_models/TE_time_series_100_1000.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(TE.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
normalized.te <- 
  subset(TE.all, select = -c(1))

#calculate normalized TE
normalized.te <- mapply('/', normalized.te, mean1850_1900)*100

#add back year again
normalized.te <- normalized.te %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#end of 21st century average
mean2090_2100 = dplyr::filter(normalized.te, between(Year, 2090, 2100)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)
100 - mean2090_2100

#save df
write_csv(normalized.te, "~/time_series_analysis/files/all_models/normalized_TE_time_series_100_1000.csv")

#melt and add column for model key
normalized.te <- data.table::melt(normalized.te,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")


figure11a <- ggplot(data = normalized.te, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Normalized Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between 100m and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))


figure11a

#save figure
ggsave(filename = "normalized_time_series_100_1000_TE.png", plot = figure11a, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 11b. NORMALIZED TRANSFER EFFICIENCY TIME SERIES MLDmax to 1000m -----------

#read in df created for first figure
TE.all <- read_csv("~/time_series_analysis/files/all_models/TE_time_series_MLDmax_1000.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(TE.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
normalized.te <- 
  subset(TE.all, select = -c(1))

#calculate normalized TE
normalized.te <- mapply('/', normalized.te, mean1850_1900)*100

#add back year again
normalized.te <- normalized.te %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.te, "~/time_series_analysis/files/all_models/normalized_TE_time_series_MLDmax_1000.csv")

#melt and add column for model key
normalized.te <- data.table::melt(normalized.te,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure11b <- ggplot(data = normalized.te, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Normalized Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between MLDmax and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure11b

#save figure
ggsave(filename = "normalized_time_series_MLDmax_1000_TE.png", plot = figure11b, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 11c. NORMALIZED TRANSFER EFFICIENCY TIME SERIES EZ Depth to 1000m -----------

#read in df created for first figure
TE.all <- read_csv("~/time_series_analysis/files/all_models/TE_time_series_ez_1000.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(TE.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(TE.all,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(5))

#replace NA EC-Earth value
mean1850_1900[c(4)] <- c(meanEC_Earth)

#take away year again for second round of calculations
normalized.te <- 
  subset(TE.all, select = -c(1))

#calculate normalized TE
normalized.te <- mapply('/', normalized.te, mean1850_1900)*100

#add back year again
normalized.te <- normalized.te %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.te, "~/time_series_analysis/files/all_models/normalized_TE_time_series_ez_1000.csv")

#melt and add column for model key
normalized.te <- data.table::melt(normalized.te,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure11c <- ggplot(data = normalized.te, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Normalized Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between EZ Depth and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure11c

#save figure
ggsave(filename = "normalized_time_series_ez_1000_TE.png", plot = figure11c, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 11d. ABSOLUTE TRANFER EFFICIENCY TIME SERIES 100m to 1000m ------------

#read in df created for first figure
TE.all <- read_csv("~/time_series_analysis/files/all_models/TE_time_series_100_1000.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(TE.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

#take away year for calculating the row average
mean1850_1900 <- subset(mean1850_1900, select = -c(1))
#row average
#absolute.mean <- rowMeans(mean1850_1900)

#take away year again for second round of calculations
absolute.te <- 
  subset(TE.all, select = -c(1))

#calculate normalized TE
absolute.te <- mapply('-', absolute.te, mean1850_1900)

#add back year again
absolute.te <- absolute.te %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(absolute.te, "~/time_series_analysis/files/all_models/absolute_TE_time_series_100_1000.csv")

#melt and add column for model key
absolute.te <- data.table::melt(absolute.te,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure11d <- ggplot(data = absolute.te, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between 100m and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure11d

#save figure
ggsave(filename = "absolute_time_series_100_1000_TE.png", plot = figure11d, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 11e. ABSOLUTE TRANFER EFFICIENCY TIME SERIES MLDmax to 1000m ------------

#read in df created for first figure
TE.all <- read_csv("~/time_series_analysis/files/all_models/TE_time_series_MLDmax_1000.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(TE.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

#take away year for calculating the row average
mean1850_1900 <- subset(mean1850_1900, select = -c(1))
#row average
#absolute.mean <- rowMeans(mean1850_1900)

#take away year again for second round of calculations
absolute.te <- 
  subset(TE.all, select = -c(1))

#calculate normalized TE
absolute.te <- mapply('-', absolute.te, mean1850_1900)

#add back year again
absolute.te <- absolute.te %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(absolute.te, "~/time_series_analysis/files/all_models/absolute_TE_time_series_MLDmax_1000.csv")

#melt and add column for model key
absolute.te <- data.table::melt(absolute.te,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure11e <- ggplot(data = absolute.te, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between MLDmax and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure11e

#save figure
ggsave(filename = "absolute_time_series_MLDmax_1000_TE.png", plot = figure11e, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 11f. ABSOLUTE TRANFER EFFICIENCY TIME SERIES EZ Depth to 1000m ------------

#read in df created for first figure
TE.all <- read_csv("~/time_series_analysis/files/all_models/TE_time_series_ez_1000.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(TE.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

#take away year for calculating the row average
mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(TE.all,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(5))

#replace NA EC-Earth value
mean1850_1900[c(4)] <- c(meanEC_Earth)

#take away year again for second round of calculations
absolute.te <- 
  subset(TE.all, select = -c(1))

#calculate normalized TE
absolute.te <- mapply('-', absolute.te, mean1850_1900)

#add back year again
absolute.te <- absolute.te %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(absolute.te, "~/time_series_analysis/files/all_models/absolute_TE_time_series_EZ_1000.csv")

#melt and add column for model key
absolute.te <- data.table::melt(absolute.te,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure11f <- ggplot(data = absolute.te, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between EZ Depth and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure11f

#save figure
ggsave(filename = "absolute_time_series_EZ_1000_TE.png", plot = figure11f, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 11g. NORMALIZED TRANFER EFFICIENCY TIME SERIES EZ Depth to 1000m ------------


#read in df created for first figure
TE.all <- read_csv("~/time_series_analysis/files/all_models/TE_time_series_PCD_1000.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(TE.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
normalized.te <- 
  subset(TE.all, select = -c(1))

#calculate normalized TE
normalized.te <- mapply('/', normalized.te, mean1850_1900)*100

#add back year again
normalized.te <- normalized.te %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.te, "~/time_series_analysis/files/all_models/normalized_TE_time_series_PCD_1000.csv")

#melt and add column for model key
normalized.te <- data.table::melt(normalized.te,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure11g <- ggplot(data = normalized.te, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Normalized Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between PCD and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure11g

#save figure
ggsave(filename = "normalized_time_series_PCD_1000_TE.png", plot = figure11g, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 11h. ABSOLUTE TRANFER EFFICIENCY TIME SERIES PCD to 1000m ------------

#read in df created for first figure
TE.all <- read_csv("~/time_series_analysis/files/all_models/TE_time_series_PCD_1000.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(TE.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

#take away year for calculating the row average
mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
absolute.te <- 
  subset(TE.all, select = -c(1))

#calculate normalized TE
absolute.te <- mapply('-', absolute.te, mean1850_1900)

#add back year again
absolute.te <- absolute.te %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(absolute.te, "~/time_series_analysis/files/all_models/absolute_TE_time_series_PCD_1000.csv")

#melt and add column for model key
absolute.te <- data.table::melt(absolute.te,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")

figure11h <- ggplot(data = absolute.te, aes(x = Year, y = TE, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Global Transfer Efficiency (1850-2100)",
       subtitle = "Between PCD and 1000m depth horizons") +
  xlab(NULL) +
  ylab("% Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure11h

#save figure
ggsave(filename = "absolute_time_series_PCD_1000_TE.png", plot = figure11h, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 12. E-RATIO AT 100M TIME SERIES ---------------

epc100 <- read_csv("~/time_series_analysis/files/all_models/time_series_epc100_all.csv")
npp <- read_csv("~/time_series_analysis/files/all_models/time_series_intpp_all.csv")

epc100 <- 
  subset(epc100, select = -c(1))

npp <-
  subset(npp,select = -c(1))

e_ratio_100 <- epc100/npp

e_ratio_100 <- e_ratio_100 %>%
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(e_ratio_100, "~/time_series_analysis/files/all_models/time_series_e_ratio_100.csv")

#melt and add column for model key
e_ratio_100 <- data.table::melt(e_ratio_100,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")


figure12 <- ggplot(data = e_ratio_100, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At 100m depth horizon") +
  xlab(NULL) +
  ylab("E-ratio") +
  scale_y_continuous(n.breaks = 6,limits = c(0.1,0.22)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure12

#save figure
ggsave(filename = "time_series_e_ratio_100.png", plot = figure12, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 13. NORMALIZED E-RATIO AT 100M TIME SERIES ------------

e_ratio_100 <- read_csv("~/time_series_analysis/files/all_models/time_series_e_ratio_100.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(e_ratio_100, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
normalized_e_ratio_100 <- 
  subset(e_ratio_100, select = -c(1))

#calculate normalized TE
normalized_e_ratio_100 <- mapply('/', normalized_e_ratio_100, mean1850_1900)*100

#add back year again
normalized_e_ratio_100 <- normalized_e_ratio_100 %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#end of 21st century average
mean2090_2100 = dplyr::filter(normalized_e_ratio_100, between(Year, 2090, 2100)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)
100 - mean2090_2100

write_csv(normalized_e_ratio_100, "~/time_series_analysis/files/all_models/normalized_time_series_e_ratio_100.csv")


#melt and add column for model key
normalized_e_ratio_100 <- data.table::melt(normalized_e_ratio_100,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")

figure13 <- ggplot(data = normalized_e_ratio_100, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At 100m depth horizon") +
  xlab(NULL) +
  ylab("% Change") +
  scale_y_continuous(n.breaks = 6, limits = c(75,105)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure13

#save figure
ggsave(filename = "normalized_time_series_e_ratio_100.png", plot = figure13, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 14. E-RATIO AT MLDMAX TIME SERIES ---------------

npp <- read_csv("~/time_series_analysis/files/all_models/time_series_intpp_all.csv")
expc <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_MLDmax_all.csv")

npp <-
  subset(npp,select = -c(1))

expc <- 
  subset(expc, select = -c(1))

e_ratio_MLDmax <- expc/npp

#reformat and add back year
e_ratio_MLDmax <- e_ratio_MLDmax %>%
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(e_ratio_MLDmax, "~/time_series_analysis/files/all_models/time_series_e_ratio_MLDmax.csv")

#melt and add column for model key
e_ratio_MLDmax <- data.table::melt(e_ratio_MLDmax,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")

figure14 <- ggplot(data = e_ratio_MLDmax, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At MLDmax depth horizon") +
  xlab(NULL) +
  ylab("E-ratio") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure14

#save figure
ggsave(filename = "time_series_e_ratio_MLDmax.png", plot = figure14, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 15. NORMALIZED E-RATIO AT MLDMAX TIME SERIES ------------

e_ratio_MLDmax <- read_csv("~/time_series_analysis/files/all_models/time_series_e_ratio_MLDmax.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(e_ratio_MLDmax, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
normalized_e_ratio_MLDmax <- 
  subset(e_ratio_MLDmax, select = -c(1))

#calculate normalized TE
normalized_e_ratio_MLDmax <- mapply('/', normalized_e_ratio_MLDmax, mean1850_1900)*100

#add back year again
normalized_e_ratio_MLDmax <- normalized_e_ratio_MLDmax %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(normalized_e_ratio_MLDmax, "~/time_series_analysis/files/all_models/normalized_time_series_e_ratio_MLDmax.csv")

#melt and add column for model key
normalized_e_ratio_MLDmax <- data.table::melt(normalized_e_ratio_MLDmax,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")


figure15 <- ggplot(data = normalized_e_ratio_MLDmax, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At MLDmax depth horizon") +
  xlab(NULL) +
  ylab("% Change") +
  scale_y_continuous(n.breaks = 6, limits = c(75,105)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure15

#save figure
ggsave(filename = "normalized_time_series_e_ratio_MLDmax.png", plot = figure15, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 16. E-RATIO AT 1000m TIME SERIES ---------------

npp <- read_csv("~/time_series_analysis/files/all_models/time_series_intpp_all.csv")
expc_1000 <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_1000_all.csv")

npp <-
  subset(npp,select = -c(1))

expc_1000 <- 
  subset(expc_1000, select = -c(1))

e_ratio_1000 <- expc_1000/npp

#reformat and add back year
e_ratio_1000 <- e_ratio_1000 %>%
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(e_ratio_1000, "~/time_series_analysis/files/all_models/time_series_e_ratio_1000.csv")

#melt and add column for model key
e_ratio_1000 <- data.table::melt(e_ratio_1000,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")

figure16 <- ggplot(data = e_ratio_1000, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At 1000m depth horizon") +
  xlab(NULL) +
  ylab("E-ratio") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure16

#save figure
ggsave(filename = "time_series_e_ratio_1000.png", plot = figure16, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 17. NORMALIZED E-RATIO AT 1000m TIME SERIES ------------

e_ratio_1000 <- read_csv("~/time_series_analysis/files/all_models/time_series_e_ratio_1000.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(e_ratio_1000, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
normalized_e_ratio_1000 <- 
  subset(e_ratio_1000, select = -c(1))

#calculate normalized TE
normalized_e_ratio_1000 <- mapply('/', normalized_e_ratio_1000, mean1850_1900)*100

#add back year again
normalized_e_ratio_1000 <- normalized_e_ratio_1000 %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(normalized_e_ratio_1000, "~/time_series_analysis/files/all_models/normalized_time_series_e_ratio_1000.csv")


#melt and add column for model key
normalized_e_ratio_1000 <- data.table::melt(normalized_e_ratio_1000,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")


figure17 <- ggplot(data = normalized_e_ratio_1000, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At 1000m depth horizon") +
  xlab(NULL) +
  ylab("% Change") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure17

#save figure
ggsave(filename = "normalized_time_series_e_ratio_1000.png", plot = figure17, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 18. POC FLUX TIME SERIES AT EZ DEPTH ------------

setwd("~/time_series_analysis/files/POC_EZ/")
df.sep <- list.files(pattern = "*_time_series_expc_ez.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/time_series_analysis/files/all_models/time_series_expc_ez_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux_expc_ez', variable.name = "Model")


#plot time series at 100m
figure18 <- ggplot(data = df2, aes(x = Year, y = POC_flux_expc_ez, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at Euphotic Zone Depth (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure18

#save figure
ggsave(filename = "time_series_POC_EZ.png", plot = figure18, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)



## 19. NORMALIZED POC FLUX AT EUPHOTIC ZONE DEPTH RELATIVE TO 1850-1900 AVG --------------

#read in df created for first figure
ts.ez.all <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_ez_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(ts.ez.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(ts.ez.all,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(5))

#replace NA EC-Earth value
mean1850_1900[c(4)] <- c(meanEC_Earth)

#need to fix year column
normalized.ez <- 
  subset(ts.ez.all, select = -c(1))

#calculate normalized POC flux
normalized.ez <- mapply('/', normalized.ez, mean1850_1900)*100

normalized.ez <- normalized.ez %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.ez, "~/time_series_analysis/files/all_models/normalized_time_series_expc_ez.csv")


#add column for model key (reformatting data specific to the below plot)
plot.normalized.ez <- data.table::melt(normalized.ez,  id.vars = 'Year', value.name = 'POC_flux_ez', variable.name = "Model")

#plot time series at EZ depth
figure19 <- ggplot(data = plot.normalized.ez, aes(x = Year, y = POC_flux_ez, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Global POC Flux at Euphotic Zone Depth (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Percent Change") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(75,108)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))
#scale_y_continuous(limits = c(60, 112), n.breaks = 6)

figure19

#save figure
ggsave(filename = "normalized_time_series_POC_EZ.png", plot = figure19, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

## 20. GLOBAL AVERAGE DENSITY DIFFERENCE BETWEEN SURFACE AND 200M --------------

setwd("~/time_series_analysis/files/strat_200/")
df.sep <- list.files(pattern = "*_strat_200_time_series.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  #df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/time_series_analysis/files/all_models/time_series_strat_200_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'global_mean_strat_200', variable.name = "Model")

#plot time series at 100m
figure20 <- ggplot(data = df2, aes(x = Year, y = global_mean_strat_200, color = Model)) +
  geom_path() +
  #geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Global Average Stratification (surface-200m)") +
  xlab(NULL) +
  ylab("Density Difference (kg/m3)") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure20

#save figure
ggsave(filename = "time_series_strat_200.png", plot = figure20, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

## 21. GLOBAL AVERAGE DENSITY DIFFERENCE BETWEEN SURFACE AND 1000M --------------

setwd("~/time_series_analysis/files/strat_1000")
df.sep <- list.files(pattern = "*_strat_1000_time_series.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  #df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

write_csv(df, file = "~/time_series_analysis/files/all_models/time_series_strat_1000_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'global_mean_strat_1000', variable.name = "Model")

#plot time series at 100m
figure21 <- ggplot(data = df2, aes(x = Year, y = global_mean_strat_1000, color = Model)) +
  geom_path() +
  #geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Global Average Stratification (surface-1000m)") +
  xlab(NULL) +
  ylab("Density Difference (kg/m3)") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure21

#save figure
ggsave(filename = "time_series_strat_1000.png", plot = figure21, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

## 22. NORMALIZED DENSITY DIFFERENCE BETWEEN SURFACE AND 200M ------------

#read in df created for first figure
ts.strat.200 <- read_csv("~/time_series_analysis/files/all_models/time_series_strat_200_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(ts.strat.200, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#need to fix year column
normalized.strat.200 <- 
  subset(ts.strat.200, select = -c(1))

#calculate normalized POC flux
normalized.strat.200 <- mapply('/', normalized.strat.200, mean1850_1900)*100

normalized.strat.200 <- normalized.strat.200 %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.strat.200, "~/time_series_analysis/files/all_models/normalized_strat_200_time_series.csv")

#add column for model key (reformatting data specific to the below plot)
plot.normalized.strat.200 <- data.table::melt(normalized.strat.200,  id.vars = 'Year', value.name = 'Strat', variable.name = "Model")

#plot normalized time series at 200m
figure22 <- ggplot(data = plot.normalized.strat.200, aes(x = Year, y = Strat, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Upper-Ocean Stratification Between 0-200m (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Percent Change") +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure22

#save figure
ggsave(filename = "normalized_time_series_strat_200.png", plot = figure22, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

## 23. NORMALIZED DENSITY DIFFERENCE BETWEEN SURFACE AND 1000M ------------

#read in df created for first figure
ts.strat.1000 <- read_csv("~/time_series_analysis/files/all_models/time_series_strat_1000_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(ts.strat.1000, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#need to fix year column
normalized.strat.1000 <- 
  subset(ts.strat.1000, select = -c(1))

#calculate normalized POC flux
normalized.strat.1000 <- mapply('/', normalized.strat.1000, mean1850_1900)*100

normalized.strat.1000 <- normalized.strat.1000 %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.strat.1000, "~/time_series_analysis/files/all_models/normalized_strat_1000_time_series.csv")

#add column for model key (reformatting data specific to the below plot)
plot.normalized.strat.1000 <- data.table::melt(normalized.strat.1000,  id.vars = 'Year', value.name = 'Strat', variable.name = "Model")

#plot normalized time series at 200m
figure23 <- ggplot(data = plot.normalized.strat.1000, aes(x = Year, y = Strat, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Upper-Ocean Stratification Between 0-1000m (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Percent Change") +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure23

#save figure
ggsave(filename = "normalized_time_series_strat_1000.png", plot = figure23, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

##24. SEA SURFACE TEMPERATURE TIME SERIES -------------- start back here for changing directories

setwd("~/senior_thesis/plotting_dataframes/time_series/")
df.sep <- list.files("~/senior_thesis/plotting_dataframes/time_series/", pattern = "*_time_series_sst.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/senior_thesis/plotting_dataframes/time_series/time_series_sst_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'sst', variable.name = "Model")

#plot time series at 100m
figure23 <- ggplot(data = df2, aes(x = Year, y = sst, color = Model)) +
  geom_path() +
  #geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Sea Surface Temperature") +
  xlab(NULL) +
  ylab("degrees C") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure23

#save figure
ggsave(filename = "time_series_sst.png", plot = figure23, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)

##24. SEA SURFACE SALINITY TIME SERIES ------------

setwd("~/senior_thesis/plotting_dataframes/time_series/")
df.sep <- list.files("~/senior_thesis/plotting_dataframes/time_series/", pattern = "*_time_series_sss.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/senior_thesis/plotting_dataframes/time_series/time_series_sss_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'sss', variable.name = "Model")

#plot time series at 100m
figure24 <- ggplot(data = df2, aes(x = Year, y = sss, color = Model)) +
  geom_path() +
  #geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Sea Surface Salinity") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(limits = c(33.9, 34.6), n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure24

#save figure
ggsave(filename = "time_series_sss.png", plot = figure24, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)

## 25. 200m TEMPERATURE TIME SERIES -----------

setwd("~/senior_thesis/plotting_dataframes/time_series/")
df.sep <- list.files("~/senior_thesis/plotting_dataframes/time_series/", pattern = "*_time_series_200_temp.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/senior_thesis/plotting_dataframes/time_series/time_series_200_temp_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'sst', variable.name = "Model")

#plot time series
figure25 <- ggplot(data = df2, aes(x = Year, y = sst, color = Model)) +
  geom_path() +
  #geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Ocean Temperature at 200m") +
  xlab(NULL) +
  ylab("degrees C") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure25

#save figure
ggsave(filename = "time_series_200_temp.png", plot = figure25, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)

## 26. 1000m TEMPERATURE TIME SERIES -----------

setwd("~/senior_thesis/plotting_dataframes/time_series/")
df.sep <- list.files("~/senior_thesis/plotting_dataframes/time_series/", pattern = "*_time_series_1000_temp.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/senior_thesis/plotting_dataframes/time_series/time_series_1000_temp_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'sst', variable.name = "Model")

#plot time series
figure25 <- ggplot(data = df2, aes(x = Year, y = sst, color = Model)) +
  geom_path() +
  #geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Ocean Temperature at 1000m") +
  xlab(NULL) +
  ylab("degrees C") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure26

#save figure
ggsave(filename = "time_series_1000_temp.png", plot = figure26, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)

##27. SALINITY AT 200M TIME SERIES ------------

setwd("~/senior_thesis/plotting_dataframes/time_series/")
df.sep <- list.files("~/senior_thesis/plotting_dataframes/time_series/", pattern = "*_time_series_200_sal.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/senior_thesis/plotting_dataframes/time_series/time_series_200_sal_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'sss', variable.name = "Model")

#plot time series at 100m
figure27 <- ggplot(data = df2, aes(x = Year, y = sss, color = Model)) +
  geom_path() +
  #geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Ocean Salinity at 200m") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(limits = c(32.1, 33.5), n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure27

#save figure
ggsave(filename = "time_series_200_sal.png", plot = figure27, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)

##28. SALINITY AT 1000M TIME SERIES ------------

setwd("~/senior_thesis/plotting_dataframes/time_series/")
df.sep <- list.files("~/senior_thesis/plotting_dataframes/time_series/", pattern = "*_time_series_1000_sal.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/senior_thesis/plotting_dataframes/time_series/time_series_1000_sal_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'sss', variable.name = "Model")

#plot time series at 100m
figure28 <- ggplot(data = df2, aes(x = Year, y = sss, color = Model)) +
  geom_path() +
  #geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Ocean Salinity at 1000m") +
  xlab(NULL) +
  ylab(NULL) +
  scale_y_continuous(limits = c(33.9, 34.6), n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure28

#save figure
ggsave(filename = "time_series_1000_sal.png", plot = figure24, path = "~/senior_thesis/figures/time_series/", width = 20, height = 12, units = "cm", dpi = 400)


#29. E-RATIO AT EZ DEPTH TIME SERIES ---------------


npp <- read_csv("~/time_series_analysis/files/all_models/time_series_npp_all.csv")
expc <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_ez_all.csv")

npp <-
  subset(npp,select = -c(1))

expc <- 
  subset(expc, select = -c(1))

e_ratio_EZ <- expc/npp

#reformat and add back year
e_ratio_EZ <- e_ratio_EZ %>%
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(e_ratio_EZ, "~/time_series_analysis/files/all_models/time_series_e_ratio_EZ.csv")

#melt and add column for model key
e_ratio_EZ <- data.table::melt(e_ratio_EZ,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")

figure29 <- ggplot(data = e_ratio_EZ, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At EZ depth horizon") +
  xlab(NULL) +
  ylab("E-ratio") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure29

#save figure
ggsave(filename = "time_series_e_ratio_EZ.png", plot = figure29, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


#30. NORMALIZED E-RATIO AT EZ DEPTH TIME SERIES ----------

e_ratio_EZ <- read_csv("~/time_series_analysis/files/all_models/time_series_e_ratio_EZ.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(e_ratio_EZ, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(e_ratio_EZ,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(5))

#replace NA EC-Earth value
mean1850_1900[c(4)] <- c(meanEC_Earth)


#take away year again for second round of calculations
normalized_e_ratio_EZ <- 
  subset(e_ratio_EZ, select = -c(1))

#calculate normalized TE
normalized_e_ratio_EZ <- mapply('/', normalized_e_ratio_EZ, mean1850_1900)*100

#add back year again
normalized_e_ratio_EZ <- normalized_e_ratio_EZ %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(normalized_e_ratio_EZ, "~/time_series_analysis/files/all_models/normalized_time_series_e_ratio_EZ.csv")

#melt and add column for model key
normalized_e_ratio_EZ <- data.table::melt(normalized_e_ratio_EZ,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")


figure30 <- ggplot(data = normalized_e_ratio_EZ, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At EZ depth horizon") +
  xlab(NULL) +
  ylab("% Change") +
  scale_y_continuous(n.breaks = 6, limits = c(75,105)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure30

#save figure
ggsave(filename = "normalized_time_series_e_ratio_EZ.png", plot = figure30, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 31. TIME SERIES AT PCD -------------

setwd("~/time_series_analysis/files/POC_PCD/")
df.sep <- list.files(pattern = "*_time_series_expc_PCD.csv$")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#join by year
df <- time.series %>% 
  reduce(left_join, by = "Year")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/time_series_analysis/files/all_models/time_series_expc_PCD_all.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux_PCD', variable.name = "Model")


#plot time series at 100m
figure31 <- ggplot(data = df2, aes(x = Year, y = POC_flux_PCD, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Time Series Change in Global POC Flux at PCD (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure31

#save figure
ggsave(filename = "time_series_POC_PCD.png", plot = figure31, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


## 32. NORMALIZED POC FLUX AT PCD RELATIVE TO 1850-1900 AVG --------------


#read in df created for first figure
ts.1000.all <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_PCD_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(ts.1000.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#need to fix year column
normalized.1000 <- 
  subset(ts.1000.all, select = -c(1))

#calculate normalized POC flux
normalized.1000 <- mapply('/', normalized.1000, mean1850_1900)*100

normalized.1000 <- normalized.1000 %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#save df
write_csv(normalized.1000, "~/time_series_analysis/files/all_models/normalized_time_series_expc_PCD.csv")


#add column for model key (reformatting data specific to the below plot)
plot.normalized.PCD <- data.table::melt(normalized.1000,  id.vars = 'Year', value.name = 'POC_flux_PCD', variable.name = "Model")

#plot time series at 1000m
figure32 <- ggplot(data = plot.normalized.PCD, aes(x = Year, y = POC_flux_PCD, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Global POC Flux at PCD (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Percent Change") +
  scale_color_manual(values = color) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(75,108)) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12)) 
#scale_y_continuous(limits = c(80, 120), n.breaks = 6)

figure32

#save figure
ggsave(filename = "normalized_time_series_POC_PCD.png", plot = figure32, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 33. E-RATIO AT PCD TIME SERIES ---------------

npp <- read_csv("~/time_series_analysis/files/all_models/time_series_intpp_all.csv")
expc_1000 <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_PCD_all.csv")

npp <-
  subset(npp,select = -c(1))

expc_1000 <- 
  subset(expc_1000, select = -c(1))

e_ratio_1000 <- expc_1000/npp

#reformat and add back year
e_ratio_1000 <- e_ratio_1000 %>%
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(e_ratio_1000, "~/time_series_analysis/files/all_models/time_series_e_ratio_PCD.csv")

#melt and add column for model key
e_ratio_1000 <- data.table::melt(e_ratio_1000,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")

figure33 <- ggplot(data = e_ratio_1000, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At PCD depth horizon") +
  xlab(NULL) +
  ylab("E-ratio") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure33


#save figure
ggsave(filename = "time_series_e_ratio_PCD.png", plot = figure33, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 34. NORMALIZED E-RATIO AT PCD TIME SERIES ------------

e_ratio_1000 <- read_csv("~/time_series_analysis/files/all_models/time_series_e_ratio_PCD.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(e_ratio_1000, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
normalized_e_ratio_1000 <- 
  subset(e_ratio_1000, select = -c(1))

#calculate normalized TE
normalized_e_ratio_1000 <- mapply('/', normalized_e_ratio_1000, mean1850_1900)*100

#add back year again
normalized_e_ratio_1000 <- normalized_e_ratio_1000 %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(normalized_e_ratio_1000, "~/time_series_analysis/files/all_models/normalized_time_series_e_ratio_PCD.csv")


#melt and add column for model key
normalized_e_ratio_1000 <- data.table::melt(normalized_e_ratio_1000,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")


figure34 <- ggplot(data = normalized_e_ratio_1000, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Percent Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At PCD depth horizon") +
  xlab(NULL) +
  ylab("% Change") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure34

#save figure
ggsave(filename = "normalized_time_series_e_ratio_PCD.png", plot = figure34, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


# 35a. ABSOLUTE E-RATIO AT 100m TIME SERIES ------------

e_ratio <- read_csv("~/time_series_analysis/files/all_models/time_series_e_ratio_100.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(e_ratio, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))


#take away year again for second round of calculations
absolute.erat <- 
  subset(e_ratio, select = -c(1))

#calculate normalized TE
absolute.erat <- mapply('-', absolute.erat, mean1850_1900)

#add back year again
absolute.erat <- absolute.erat %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(absolute.erat, "~/time_series_analysis/files/all_models/absolute_time_series_e_ratio_100.csv")

#melt and add column for model key
absolute.erat.plot <- data.table::melt(absolute.erat,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")


figure35a <- ggplot(data = absolute.erat.plot, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At 100m depth horizon") +
  xlab(NULL) +
  ylab("Change in % Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure35a

#save figure
ggsave(filename = "absolute_time_series_e_ratio_100.png", plot = figure35a, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 35b. ABSOLUTE E-RATIO AT PCD TIME SERIES ------------

e_ratio <- read_csv("~/time_series_analysis/files/all_models/time_series_e_ratio_PCD.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(e_ratio, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
absolute.erat <- 
  subset(e_ratio, select = -c(1))

#calculate normalized TE
absolute.erat <- mapply('-', absolute.erat, mean1850_1900)

#add back year again
absolute.erat <- absolute.erat %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(absolute.erat, "~/time_series_analysis/files/all_models/absolute_time_series_e_ratio_PCD.csv")

#melt and add column for model key
absolute.erat.plot <- data.table::melt(absolute.erat,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")


figure35b <- ggplot(data = absolute.erat.plot, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At the PCD") +
  xlab(NULL) +
  ylab("Change in % Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure35b

#save figure
ggsave(filename = "absolute_time_series_e_ratio_PCD.png", plot = figure35b, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 35c. ABSOLUTE E-RATIO AT EZ DEPTH TIME SERIES ------------

e_ratio <- read_csv("~/time_series_analysis/files/all_models/time_series_e_ratio_EZ.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(e_ratio, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
absolute.erat <- 
  subset(e_ratio, select = -c(1))

#calculate normalized TE
absolute.erat <- mapply('-', absolute.erat, mean1850_1900)

#add back year again
absolute.erat <- absolute.erat %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(absolute.erat, "~/time_series_analysis/files/all_models/absolute_time_series_e_ratio_EZ.csv")

#melt and add column for model key
absolute.erat.plot <- data.table::melt(absolute.erat,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")


figure35c <- ggplot(data = absolute.erat.plot, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At the EZ Depth") +
  xlab(NULL) +
  ylab("Change in % Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure35c

#save figure
ggsave(filename = "absolute_time_series_e_ratio_EZ.png", plot = figure35c, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 35d. ABSOLUTE E-RATIO AT MLDmax TIME SERIES ------------

e_ratio <- read_csv("~/time_series_analysis/files/all_models/time_series_e_ratio_MLDmax.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(e_ratio, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
absolute.erat <- 
  subset(e_ratio, select = -c(1))

#calculate normalized TE
absolute.erat <- mapply('-', absolute.erat, mean1850_1900)

#add back year again
absolute.erat <- absolute.erat %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(absolute.erat, "~/time_series_analysis/files/all_models/absolute_time_series_e_ratio_MLDmax.csv")

#melt and add column for model key
absolute.erat.plot <- data.table::melt(absolute.erat,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")


figure35d <- ggplot(data = absolute.erat.plot, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At the MLDmax") +
  xlab(NULL) +
  ylab("Change in % Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure35d

#save figure
ggsave(filename = "absolute_time_series_e_ratio_MLDmax.png", plot = figure35d, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 35e. ABSOLUTE E-RATIO AT 1000m TIME SERIES ------------

e_ratio <- read_csv("~/time_series_analysis/files/all_models/time_series_e_ratio_1000.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(e_ratio, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
absolute.erat <- 
  subset(e_ratio, select = -c(1))

#calculate normalized TE
absolute.erat <- mapply('-', absolute.erat, mean1850_1900)

#add back year again
absolute.erat <- absolute.erat %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(absolute.erat, "~/time_series_analysis/files/all_models/absolute_time_series_e_ratio_1000.csv")

#melt and add column for model key
absolute.erat.plot <- data.table::melt(absolute.erat,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")


figure35e <- ggplot(data = absolute.erat.plot, aes(x = Year, y = e_ratio, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Globally Integrated E-ratio (1850-2100)",
       subtitle = "At 1000m") +
  xlab(NULL) +
  ylab("Change in % Transferred") +
  scale_y_continuous(n.breaks = 6) +
  scale_x_continuous(expand = c(0,0)) +
  scale_color_manual(values = color) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure35e

#save figure
ggsave(filename = "absolute_time_series_e_ratio_1000.png", plot = figure35e, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

## GFDL-CM4 depth horizon differences ------------

## 1. TIME SERIES AT 100m ---------------

setwd("~/time_series_analysis/files/POC_100/")
df.sep <- list.files(pattern = "*CM4")

#create empty list for storing for loop output
time.series <- list()

for(i in df.sep) {
  
  #read in csv file
  df <- read_csv(i)
  #get rid of random ...1 column
  df <- subset(df, select = -c(...1))
  #store into list
  time.series[[i]] <- df
}

#read in csv file
df <- read_csv("~/time_series_analysis/files/POC_PCD/CM4_time_series_expc_PCD.csv")
#get rid of random ...1 column
df <- subset(df, select = -c(...1))
#store into list
time.series[[5]] <- df

#read in csv file
df <- read_csv("~/time_series_analysis/files/POC_EZ/CM4_time_series_expc_ez.csv")
#get rid of random ...1 column
df <- subset(df, select = -c(...1))
#store into list
time.series[[6]] <- df

#join by year (2015 has a lot of repeats, but that doesn't effect later plotting)
df <- time.series %>% 
  reduce(left_join, by = "Year")

colnames(df) <- c("Year", "112.5 m","62.5 m","87.5 m","100 m" ,"PCD", "EZ Depth")

#save POC flux at 100m time-series data frame for all models
write_csv(df, file = "~/time_series_analysis/files/all_models/CM4_DH_diffs.csv")

#add column for model key (reformatting data specific to the below plot)
df2 <- data.table::melt(df,  id.vars = 'Year', value.name = 'POC_flux_100', variable.name = 'Depth_Horizon')

#plot time series at 100m
figure <- ggplot(data = df2, aes(x = Year, y = POC_flux_100, color = Depth_Horizon)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "GFDL-CM4 POC Flux Depth Horizon Comparison (1850-2100)") +
  xlab(NULL) +
  ylab("POC Flux (Pg C/yr)") +
  scale_y_continuous(n.breaks = 6) +  
  scale_color_manual(values = color3) +
  scale_x_continuous(expand = c(0,0)) +
  theme(plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), 
        legend.key.height = unit(1, 'cm'), 
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

figure

#save figure
ggsave(filename = "time_series_CM4_DH.png", plot = figure, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


