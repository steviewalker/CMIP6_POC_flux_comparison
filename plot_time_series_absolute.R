#' @title Plotting Globally Integrated Time Series Data Absolute Change
#' @author Stevie Walker
#' @date 1/25/22
#' @description plots 4 figures of globally integrated time series at 100m, MLDmax, normalized 100m, and normalized MLDmax
#' @description also saves tidied and combined time series data frames

## POC FLUX ------------

#read in df created for first figure
epc100.all <- read_csv("~/time_series_analysis/files/all_models/time_series_epc100_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(epc100.all, Year <= 1900) %>% 
  summarise_if(is.numeric, mean)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
absolute.epc100 <- 
  subset(epc100.all, select = -c(1))

#calculate normalized TE
absolute.epc100 <- mapply('-', absolute.epc100, mean1850_1900)

absolute.epc100 <- absolute.epc100 %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM)

#save df
write_csv(absolute.epc100, "~/time_series_analysis/files/all_models/absolute_time_series_epc100.csv")

#add column for model key (reformatting data specific to the below plot)
plot.absolute.epc100 <- data.table::melt(absolute.epc100,  id.vars = 'Year', value.name = 'POC_flux_100', variable.name = "Model")

#plot time series at 100m
figure3 <- ggplot(data = plot.absolute.epc100, aes(x = Year, y = POC_flux_100, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Global POC Flux at 100m (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Change in POC Flux") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(-2.8, 0.6), n.breaks = 6) +
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
figure3

#save figure
ggsave(filename = "absolute_time_series_POC_100.png", plot = figure3, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 4. ABSOLUTE POC FLUX AT MLD MAX RELATIVE TO 1850-1900 AVG --------------

#read in df created for first figure
expc.all <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_MLDmax_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(expc.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
absolute.expc <- 
  subset(expc.all, select = -c(1))

#calculate normalized TE
absolute.expc <- mapply('-', absolute.expc, mean1850_1900)

absolute.expc <- absolute.expc %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM)

#save df
write_csv(absolute.expc, "~/time_series_analysis/files/all_models/absolute_time_series_expc_MLDmax.csv")

#add column for model key (reformatting data specific to the below plot)
absolute.expc.MLDmax <- data.table::melt(absolute.expc,  id.vars = 'Year', value.name = 'POC_flux_MLDmax', variable.name = "Model")

#plot time series at MLDmax
figure4 <- ggplot(data = absolute.expc.MLDmax, aes(x = Year, y = POC_flux_MLDmax, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Global POC Flux at MLDmax (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Change in POC Flux") +
  scale_y_continuous(limits = c(-2.8, 0.6), n.breaks = 6) +
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
ggsave(filename = "absolute_time_series_POC_MLDmax.png", plot = figure4, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 5. ABSOLUTE POC FLUX AT PCD RELATIVE TO 1850-1900 AVG --------------

#read in df created for first figure
expc.all <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_PCD_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(expc.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
absolute.expc <- 
  subset(expc.all, select = -c(1))

#calculate normalized TE
absolute.expc <- mapply('-', absolute.expc, mean1850_1900)

absolute.expc <- absolute.expc %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM)

#save df
write_csv(absolute.expc, "~/time_series_analysis/files/all_models/absolute_time_series_expc_PCD.csv")

#add column for model key (reformatting data specific to the below plot)
absolute.expc.PCD <- data.table::melt(absolute.expc,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "Model")

#plot time series at 100m
figure5 <- ggplot(data = absolute.expc.PCD, aes(x = Year, y = POC_flux, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Global POC Flux at PCD (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Change in POC Flux") +
  scale_y_continuous(limits = c(-2.8, 0.6), n.breaks = 6) +
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
ggsave(filename = "absolute_time_series_POC_PCD.png", plot = figure5, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 6. ABSOLUTE POC FLUX AT EZ DEPTH RELATIVE TO 1850-1900 AVG --------------

#read in df created for first figure
expc.all <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_ez_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(expc.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(expc.all,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(5))

#replace NA EC-Earth value
mean1850_1900[c(4)] <- c(meanEC_Earth)

#take away year again for second round of calculations
absolute.expc <- 
  subset(expc.all, select = -c(1))

#calculate normalized TE
absolute.expc <- mapply('-', absolute.expc, mean1850_1900)

absolute.expc <- absolute.expc %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM)

#save df
write_csv(absolute.expc, "~/time_series_analysis/files/all_models/absolute_time_series_expc_EZ.csv")

#add column for model key (reformatting data specific to the below plot)
absolute.expc.ez <- data.table::melt(absolute.expc,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "Model")

#plot time series at EZ Depth
figure6 <- ggplot(data = absolute.expc.ez, aes(x = Year, y = POC_flux, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Global POC Flux at the EZ Depth (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Change in POC Flux") +
  scale_y_continuous(limits = c(-2.8, 0.6), n.breaks = 6) +
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

figure6

#save figure
ggsave(filename = "absolute_time_series_POC_EZ.png", plot = figure6, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

# 7. ABSOLUTE POC FLUX AT 1000M RELATIVE TO 1850-1900 AVG --------------

#read in df created for first figure
expc.all <- read_csv("~/time_series_analysis/files/all_models/time_series_expc_1000_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(expc.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#take away year again for second round of calculations
absolute.expc <- 
  subset(expc.all, select = -c(1))

#calculate normalized TE
absolute.expc <- mapply('-', absolute.expc, mean1850_1900)

absolute.expc <- absolute.expc %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM)

#save df
write_csv(absolute.expc, "~/time_series_analysis/files/all_models/absolute_time_series_expc_1000.csv")

#add column for model key (reformatting data specific to the below plot)
plot.absolute.expc <- data.table::melt(absolute.expc,  id.vars = 'Year', value.name = 'POC_flux', variable.name = "Model")

#plot time series at 1000m
figure7 <- ggplot(data = plot.absolute.expc, aes(x = Year, y = POC_flux, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Change in Global POC Flux at 1000m (1850-2100)",
       subtitle = "Relative to 1850-1900 average") +
  xlab(NULL) +
  ylab("Change in POC Flux") +
  #scale_y_continuous(limits = c(75, 108), n.breaks = 6) +
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

figure7

#save figure
ggsave(filename = "absolute_time_series_POC_1000.png", plot = figure7, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)


## TRANSFER EFFICIENCY ------------

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
meanEC_Earth <- subset(meanEC_Earth, select = c(4))

#replace NA EC-Earth value
mean1850_1900[c(3)] <- c(meanEC_Earth)

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

## E-RATIO -----------

# 35a. ABSOLUTE E-RATIO AT 100m TIME SERIES ------------

e_ratio <- read_csv("~/time_series_analysis/files/all_models/time_series_e_ratio_100.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(e_ratio, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(e_ratio,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(4))

#replace NA EC-Earth value
mean1850_1900[c(3)] <- c(meanEC_Earth)

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

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(e_ratio,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(4))

#replace NA EC-Earth value
mean1850_1900[c(3)] <- c(meanEC_Earth)

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

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(e_ratio,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(4))

#replace NA EC-Earth value
mean1850_1900[c(3)] <- c(meanEC_Earth)

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

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(e_ratio,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(4))

#replace NA EC-Earth value
mean1850_1900[c(3)] <- c(meanEC_Earth)

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

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(e_ratio,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(4))

#replace NA EC-Earth value
mean1850_1900[c(3)] <- c(meanEC_Earth)

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

## NPP -----------

# 9. NORMALIZED NPP TIME SERIES ------------

#read in df created for first figure
ts.npp.all <- read_csv("~/time_series_analysis/files/all_models/time_series_npp_all.csv")

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(ts.npp.all, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)
mean1850_1900 <- subset(mean1850_1900, select = -c(1))

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(ts.npp.all,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(5))

#replace NA EC-Earth value
mean1850_1900[c(4)] <- c(meanEC_Earth)

#take away year again for second round of calculations
absolute.npp <- 
  subset(ts.npp.all, select = -c(1))

#calculate normalized TE
absolute.npp <- mapply('-', absolute.npp, mean1850_1900)

absolute.npp <- absolute.npp %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

#add column for model key
absolute.npp2 <- data.table::melt(absolute.npp,  id.vars = 'Year', value.name = 'NPP', variable.name = "Model")


#save df
write_csv(absolute.npp, "~/time_series_analysis/files/all_models/absolute_time_series_npp.csv")


#plot NPP time series
figure9 <- ggplot(data = absolute.npp2, aes(x = Year, y = NPP, color = Model)) +
  geom_line() +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_bw() +
  labs(title = "Absolute Time Series Change in Global NPP (1850-2100)") +
  xlab(NULL) +
  ylab("Change in NPP") +
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

figure9

#save figure
ggsave(filename = "absolute_time_series_npp.png", plot = figure9, path = "~/time_series_analysis/figures/", width = 20, height = 12, units = "cm", dpi = 400)

