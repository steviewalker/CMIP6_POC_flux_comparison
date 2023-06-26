#' @title Plot regional time series 
#' @author Stevie Walker
#' @date 3/8/23
#' @description plots regional time series data for total flux, per area flux, and normalized flux

#region = "SO_60"
#DH = "POC_MLDmax"

plot_regional_time_series <- function(region,DH) {
  
  #colors for time series
  color = c("violet","brown2" ,"goldenrod2", "aquamarine3", "darkorchid3", "darkorange2", "indianred4", "royalblue2")
  #color = c("violet","goldenrod2", "aquamarine3", "darkorchid3", "darkorange2", "indianred4", "royalblue2")
  
  
  #1. MAKE TOTAL FLUX AND NORMALIZED TOTAL FLUX DATA FRAMES ----------------
  if(DH == "NPP") {
    setwd(paste0("~/regional_time_series_analysis/files/", DH, "/total_npp/"))
  } else {
    setwd(paste0("~/regional_time_series_analysis/files/", DH, "/total_flux/")) 
  }
  if(region == "low_lats" || region == "EQ_Pacific") {
    df.expc <- list.files(pattern = paste0(region,"*"))
    #get rid of low_lats_no_EQ_Pacific values
    df.expc <- df.expc[!str_detect(df.expc,pattern="low_lats_no")]
  } else {
    df.expc <- list.files(pattern = paste0(region,"*"))
  }
  #create empty list for storing for loop output
  time.series2 <- list()
  
  if(DH == "NPP") {
    EC_Earth <- read_csv(df.expc[4])
    df.expc = df.expc[-4]
  } else {}
  
  for(i in df.expc) {
    
    #read in csv file
    df.expc <- read_csv(i)
    #get rid of random ...1 column
    df.expc <- subset(df.expc, select = -c(...1))
    
    #store into list
    time.series2[[i]] <- df.expc
  }
  
  
  #join by year
  df.expc <- time.series2 %>% 
    reduce(left_join, by = "Year")
  
  if(DH == "NPP") {
    df.expc <- left_join(df.expc, EC_Earth, by = "Year")
    df.expc <- df.expc %>%
      relocate("EC-Earth", .before = "GFDL") 
  } else {}
  
  
  #add column for model key
  df2.expc <- data.table::melt(df.expc,  id.vars = 'Year', value.name = 'POC_flux_expc', variable.name = "Model")
  
  #save POC flux time-series data frame for all models
  write_csv(df.expc, file = paste0("~/regional_time_series_analysis/files/all_models/total_flux/", region, "_time_series_", DH,"_all.csv"))
  
  #calculate average for each model from 1850-1900
  mean1850_1900 <- dplyr::filter(df.expc, between(Year, 1850, 1900)) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  mean1850_1900 <- subset(mean1850_1900, select = -c(1))
  
  if(DH == "NPP" || DH == "POC_EZ") {
    
    #EC-Earth is normalized around 2015-2035 because it is missing historical data
    meanEC_Earth <- dplyr::filter(df.expc,between(Year,2015,2035)) %>%
      summarise_if(is.numeric, mean, na.rm = TRUE)
    meanEC_Earth <- subset(meanEC_Earth, select = c(5))
    
    #replace NA EC-Earth value
    mean1850_1900[c(4)] <- c(meanEC_Earth)
    
  } else {}
  
  #need to fix year column
  normalized.expc <- 
    subset(df.expc, select = -c(1))
  
  #calculate normalized POC flux
  normalized.expc <- mapply('/', normalized.expc, mean1850_1900)*100
  
  normalized.expc <- normalized.expc %>% 
    cbind(Year = c(1850:2100)) %>% 
    as_tibble() %>% 
    relocate(Year, .before = CESM) 
  
  #save df
  write_csv(normalized.expc, file = paste0("~/regional_time_series_analysis/files/all_models/total_flux/normalized_", region, "_time_series_", DH,"_all.csv"))
  
  # 2. MAKE PLOT TITLES --------
  
  #Southern Ocean S of 60
  if(region == "SO_60") {
    plot.subtitle = "Southern Ocean (South of 60S)"
    #Southern Ocean S of 50
  } else if(region == "SO_50"){
    plot.subtitle = "Southern Ocean (South of 50S)"
    #low latitudes
  } else if(region == "30_low_lats"){
    plot.subtitle = "Low Latitudes (30S-30N)"
    #Equatorial Pacific
  } else if(region == "EQ_Pacific") {
    plot.subtitle = "Equatorial Pacific (15S to 15N, 160E to 75W)"
    #low latitudes without Equatorial Pacific
  } else if (region == "low_lats_no_EQ_Pacific") {
    plot.subtitle = "Low Latitudes (30S-30N) without Equatorial Pacific"
    #North Atlantic - NOTE: in calc_region_area this is called North_Atlantic_no_Arctic
  } else if (region == "North_Atlantic") {
    plot.subtitle = "North Atlantic (40N to 65N, 70W to 0W)"
  } else {
    #North Atlantic with the Arctic Ocean
    plot.subtitle = "Low Latitudes (15S-15N)"
  }
  
  if(DH == "POC_MLDmax") {
    DH.name = "POC flux at the MLDmax"
  } else if(DH == "POC_1000"){
    DH.name = "POC flux at 1000m"
  } else if(DH == "POC_EZ") {
    DH.name = "POC flux at the EZ depth"
  } else if(DH == "POC_100") {
    DH.name = "POC flux at 100m"
  } else if (DH == "POC_PCD") {
    DH.name = "POC flux at the PCD"
  } else {
    DH.name = "NPP"
  }
  
  # 3. PLOT TOTAL FLUX TIME SERIES -----------
  
  plot.title = paste0("Time Series Change in ", DH.name, " (1850-2100)")
  
  #plot time series at MLDmax
  total_flux_fig <- ggplot(data = df2.expc, aes(x = Year, y = POC_flux_expc, color = Model)) +
    geom_line() +
    geom_smooth(size = 0.5, se = FALSE) +
    theme_bw() +
    labs(title = plot.title,
         subtitle = plot.subtitle) +
    xlab(NULL) +
    ylab("POC Flux (Pg C/yr)") +
    scale_y_continuous(n.breaks = 6) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = color) +
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.title = element_text(size=14), 
          legend.text = element_text(size=12))
  
  total_flux_fig
  
  if(DH == "NPP") {
    total_flux_fig = total_flux_fig +
      ylab("NPP (Pg C/yr)")
  } else {}
  
  #save figure
  ggsave(filename = paste0(region, "_time_series_",DH,".png"), plot = total_flux_fig, path = "~/regional_time_series_analysis/figures/total_flux/", width = 20, height = 12, units = "cm", dpi = 400)
  
  # 4. PLOT NORMALIZED TOTAL FLUX TIME SERIES -----------
  
  #add column for model key (reformatting data specific to the below plot)
  plot.normalized.expc <- data.table::melt(normalized.expc,  id.vars = 'Year', value.name = 'POC_flux_MLDmax', variable.name = "Model")
  
  plot.title = paste0("Normalized Time Series Change in ", DH.name, " (1850-2100)")
  
  #plot time series at 100m
  norm_total_flux_fig <- ggplot(data = plot.normalized.expc, aes(x = Year, y = POC_flux_MLDmax, color = Model)) +
    geom_line() +
    geom_smooth(size = 0.5, se = FALSE) +
    theme_bw() +
    labs(title = plot.title,
         subtitle = plot.subtitle) +
    xlab(NULL) +
    ylab("Percent Change") +
    #scale_y_continuous(limits = c(75, 108), n.breaks = 6) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = color) +
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.title = element_text(size=14), 
          legend.text = element_text(size=12))
  
  norm_total_flux_fig
  
  #save figure
  ggsave(filename = paste0("normalized_",region, "_time_series_",DH,".png"), plot = norm_total_flux_fig, path = "~/regional_time_series_analysis/figures/total_flux/", width = 20, height = 12, units = "cm", dpi = 400)
  
}


#combine individual model files and save
combine_figs <- function(DH) {
  
  if(region == "SO_50") {
    title = "Southern Ocean (South of 50S)"
  } else if(region == "SO_60"){
    title = "Southern Ocean (South of 60S)"
  } else if(region == "30_low_lats"){
    title = "Low Latitudes (30S to 30N)"
  } else if(region == "EQ_Pacific") {
    title = "Equatorial Pacific (15S to 15N, 160E to 75W)"
  } else if (region == "low_lats_no_EQ_Pacific") {
    title = "Low Latitudes without the Equatorial Pacific"
  } else if (region == "North_Atlantic") {
    title = "North Atlantic with the Arctic (North of 40N, 70W to 0W)"
  } else if (region == "North_Atlantic_no_Arctic") {
    title = "North Atlantic without the Arctic (40N to 65N, 70W to 0W)"
  } else {
    title = "Low Latitudes (15S to 15N)"
  }
  
  setwd("~/regional_time_series_analysis/figures/total_flux/")
  
  if(DH == "NPP") {
    A <- readPNG(paste0("normalized_15_low_lats_time_series_",DH,".png"))
    B <- readPNG(paste0("normalized_30_low_lats_time_series_",DH,".png"))
    C <- readPNG(paste0("normalized_low_lats_no_EQ_Pacific_time_series_",DH,".png"))
    D <- readPNG(paste0("normalized_EQ_Pacific_time_series_",DH,".png"))
    E <- readPNG(paste0("normalized_North_Atlantic_time_series_",DH,".png"))
    f <- readPNG(paste0("normalized_SO_50_time_series_",DH,".png"))
    G <- readPNG(paste0("normalized_SO_60_time_series_",DH,".png"))
    
    setwd("~/time_series_analysis/figures/")
    H <- readPNG(paste0("normalized_time_series_npp.png"))
  } else {
    
    A <- readPNG(paste0("normalized_15_low_lats_time_series_POC_",DH,".png"))
    B <- readPNG(paste0("normalized_30_low_lats_time_series_POC_",DH,".png"))
    C <- readPNG(paste0("normalized_low_lats_no_EQ_Pacific_time_series_POC_",DH,".png"))
    D <- readPNG(paste0("normalized_EQ_Pacific_time_series_POC_",DH,".png"))
    E <- readPNG(paste0("normalized_North_Atlantic_time_series_POC_",DH,".png"))
    f <- readPNG(paste0("normalized_SO_50_time_series_POC_",DH,".png"))
    G <- readPNG(paste0("normalized_SO_60_time_series_POC_",DH,".png"))
    
    setwd("~/time_series_analysis/figures/")
    H <- readPNG(paste0("normalized_time_series_POC_",DH,".png"))
  }
  
  grid.combine <- grid.arrange(rasterGrob(H),rasterGrob(A), rasterGrob(B), rasterGrob(C),
                               rasterGrob(D),rasterGrob(E),rasterGrob(f),rasterGrob(G),ncol=4, nrow=2,
                               top= DH)
  
  if(DH == "NPP") {
    ggsave(paste0("~/regional_time_series_analysis/figures/faceted/normalized_",DH,"_regional_time_series.png"), plot = grid.combine, width = 30, height = 12, units = "cm", dpi = 400)
  } else {
    ggsave(paste0("~/regional_time_series_analysis/figures/faceted/normalized_POC_",DH,"_regional_time_series.png"), plot = grid.combine, width = 30, height = 12, units = "cm", dpi = 400)
  }
  
  # absolute flux values --------
  
  setwd("~/regional_time_series_analysis/figures/total_flux/")
  
  if(DH == "NPP") {
    
    A <- readPNG(paste0("15_low_lats_time_series_",DH,".png"))
    B <- readPNG(paste0("30_low_lats_time_series_",DH,".png"))
    C <- readPNG(paste0("low_lats_no_EQ_Pacific_time_series_",DH,".png"))
    D <- readPNG(paste0("EQ_Pacific_time_series_",DH,".png"))
    E <- readPNG(paste0("North_Atlantic_time_series_",DH,".png"))
    f <- readPNG(paste0("SO_50_time_series_",DH,".png"))
    G <- readPNG(paste0("SO_60_time_series_",DH,".png"))
    
    setwd("~/time_series_analysis/figures/")
    H <- readPNG(paste0("time_series_npp.png"))
    
  } else {
    
    A <- readPNG(paste0("15_low_lats_time_series_POC_",DH,".png"))
    B <- readPNG(paste0("30_low_lats_time_series_POC_",DH,".png"))
    C <- readPNG(paste0("low_lats_no_EQ_Pacific_time_series_POC_",DH,".png"))
    D <- readPNG(paste0("EQ_Pacific_time_series_POC_",DH,".png"))
    E <- readPNG(paste0("North_Atlantic_time_series_POC_",DH,".png"))
    f <- readPNG(paste0("SO_50_time_series_POC_",DH,".png"))
    G <- readPNG(paste0("SO_60_time_series_POC_",DH,".png"))
    
    setwd("~/time_series_analysis/figures/")
    H <- readPNG(paste0("time_series_POC_",DH,".png"))
  }
  
  grid.combine <- grid.arrange(rasterGrob(H),rasterGrob(A), rasterGrob(B), rasterGrob(C),
                               rasterGrob(D),rasterGrob(E),rasterGrob(f),rasterGrob(G),ncol=4, nrow=2,
                               top= DH)
  
  if(DH == "NPP") {
    ggsave(paste0("~/regional_time_series_analysis/figures/faceted/",DH,"_regional_time_series.png"), plot = grid.combine, width = 30, height = 12, units = "cm", dpi = 400)
  } else {
    ggsave(paste0("~/regional_time_series_analysis/figures/faceted/POC_",DH,"_regional_time_series.png"), plot = grid.combine, width = 30, height = 12, units = "cm", dpi = 400)
  }  
  
}

combine_interrelations <- function(DH, region) {
  
  setwd("~/regional_time_series_analysis/figures/total_flux/")
  A <- readPNG(paste0("normalized_",region,"_time_series_",DH,".png"))
  C <- readPNG(paste0("normalized_",region,"_time_series_POC_1000.png"))
  D <- readPNG(paste0("normalized_",region,"_time_series_NPP.png"))
  
  setwd("~/regional_time_series_analysis/figures/TE/")
  B <- readPNG(paste0("normalized_",region,"_TE_time_series_",DH,"_POC_1000.png"))
  
  setwd("~/regional_time_series_analysis/figures/e_ratio/")
  E <- readPNG(paste0("normalized_",region,"_e_ratio_time_series_",DH,".png"))
  
  grid.combine <- grid.arrange(rasterGrob(A),rasterGrob(B), rasterGrob(C), rasterGrob(D),
                               rasterGrob(E), ncol=3, nrow=2)
  
  ggsave(paste0("~/regional_time_series_analysis/figures/faceted/interrelations/",region,"_interrelations_regional_time_series_", DH,".png"), plot = grid.combine, width = 40, height = 20, units = "cm", dpi = 400)
  
}

plot_per_area_regional_ts <- function(DH, region) {
  
  # 5. MAKE PER AREA FLUX AND NORMALIZED PER AREA FLUX DATA FRAMES -------------
  
  setwd(paste0("~/regional_time_series_analysis/files/", DH, "/per_area_flux/"))
  
  if(region == "low_lats" || region == "EQ_Pacific") {
    df.expc2 <- list.files(pattern = paste0(region,"*"))
    #get rid of low_lats_no_EQ_Pacific values
    df.expc2 <- df.expc2[!str_detect(df.expc2,pattern="low_lats_no")]
  } else {
    df.expc2 <- list.files(pattern = paste0(region,"*"))
  }
  
  #create empty list for storing for loop output
  time.series2 <- list()
  
  for(i in df.expc2) {
    
    #read in csv file
    df.expc2 <- read_csv(i)
    #get rid of random ...1 column
    df.expc2 <- subset(df.expc2, select = -c(...1))
    #store into list
    time.series2[[i]] <- df.expc2
  }
  
  #join by year (2015 has a lot of repeats, but that doesn't effect later plotting)
  df.expc2 <- time.series2 %>% 
    reduce(left_join, by = "Year")
  
  #convert to g C/m2/yr
  #df.expc = df.expc*1000000000000000
  
  #add column for model key
  df3.expc <- data.table::melt(df.expc2,  id.vars = 'Year', value.name = 'POC_flux_expc', variable.name = "Model")
  
  #save POC flux at MLDmax time-series data frame for all models
  write_csv(df.expc2, file = paste0("~/regional_time_series_analysis/files/all_models/per_area_flux/", region, "_time_series_", DH,"_per_area_all.csv"))
  
  #calculate average for each model from 1850-1900
  mean1850_1900 <- dplyr::filter(df.expc2, between(Year, 1850, 1900)) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  mean1850_1900 <- subset(mean1850_1900, select = -c(1))
  
  #need to fix year column
  normalized.expc2 <- 
    subset(df.expc2, select = -c(1))
  
  #calculate normalized POC flux
  normalized.expc2 <- mapply('/', normalized.expc2, mean1850_1900)*100
  
  normalized.expc2 <- normalized.expc2 %>% 
    cbind(Year = c(1850:2100)) %>% 
    as_tibble() %>% 
    relocate(Year, .before = CESM) 
  
  #save df
  write_csv(normalized.expc2, file = paste0("~/regional_time_series_analysis/files/all_models/per_area_flux/normalized_", region, "_time_series_", DH,"_per_area_all.csv"))
  
  # 6. PLOT PER AREA FLUX TIME SERIES -----------
  
  plot.title = paste0("Per Area Change in ", DH.name, " (1850-2100)")
  
  #plot time series at MLDmax
  area_flux_fig <- ggplot(data = df3.expc, aes(x = Year, y = POC_flux_expc, color = Model)) +
    geom_line() +
    geom_smooth(size = 0.5, se = FALSE) +
    theme_bw() +
    labs(title = plot.title,
         subtitle = plot.subtitle) +
    xlab(NULL) +
    ylab("POC Flux (Pg C/m2/yr)") +
    scale_y_continuous(n.breaks = 6) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = color) +
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.title = element_text(size=14), 
          legend.text = element_text(size=12))
  
  area_flux_fig
  
  #save figure
  ggsave(filename = paste0(region, "_per_area_time_series_",DH,".png"), plot = area_flux_fig, path = "~/regional_time_series_analysis/figures/per_area_flux/", width = 20, height = 12, units = "cm", dpi = 400)
  
  # 4. PLOT NORMALIZED PER AREA FLUX TIME SERIES -----------
  
  #add column for model key (reformatting data specific to the below plot)
  plot.normalized.expc2 <- data.table::melt(normalized.expc2,  id.vars = 'Year', value.name = 'POC_flux_MLDmax', variable.name = "Model")
  
  plot.title = paste0("Normalized Per Area Change in ", DH.name, " (1850-2100)")
  
  #plot normalized per area time series
  norm_area_flux_fig <- ggplot(data = plot.normalized.expc2, aes(x = Year, y = POC_flux_MLDmax, color = Model)) +
    geom_line() +
    geom_smooth(size = 0.5, se = FALSE) +
    theme_bw() +
    labs(title = plot.title,
         subtitle = plot.subtitle) +
    xlab(NULL) +
    ylab("Percent Change") +
    #scale_y_continuous(limits = c(75, 108), n.breaks = 6) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = color) +
    theme(plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.key.size = unit(1, 'cm'), 
          legend.key.height = unit(1, 'cm'), 
          legend.key.width = unit(1, 'cm'), 
          legend.title = element_text(size=14), 
          legend.text = element_text(size=12))
  
  norm_area_flux_fig
  
  #save figure
  ggsave(filename = paste0("normalized_",region, "_per_area_time_series_",DH,".png"), plot = norm_area_flux_fig, path = "~/regional_time_series_analysis/figures/per_area_flux/", width = 20, height = 12, units = "cm", dpi = 400)
  
}
