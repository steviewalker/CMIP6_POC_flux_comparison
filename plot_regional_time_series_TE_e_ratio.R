

plot_regional_ts_TE_e_ratio <- function(region, DH) {
  
  #colors for time series
  color = c("violet","brown2" ,"goldenrod2", "aquamarine3", "darkorchid3", "darkorange2", "indianred4", "royalblue2")
  #color = c("violet","goldenrod2", "aquamarine3", "darkorchid3", "darkorange2", "indianred4", "royalblue2")
  
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
  
  if(DH == "POC_100") {
    DH.name = "100m to 1000m"
  } else if(DH == "POC_MLDmax"){
    DH.name = "MLDmax to 1000m"
  } else if(DH == "POC_PCD"){
    DH.name = "PCD to 1000m"
  } else {
    DH.name = "EZ depth to 1000m"
  }
  
  if(DH == "POC_1000") {
    print("skip TE plots")
  } else {
    
    #Transfer efficiency ---------
    
    title = paste0("Change in Transfer Efficiency - ", DH.name)
    
    TE <- read_csv(paste0("~/regional_time_series_analysis/files/all_models/total_flux/",region, "_TE_time_series_", DH,"_POC_1000.csv"))
    TE <-  data.table::melt(TE,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")
    
    TE_plot <- ggplot(data = TE, aes(x = Year, y = TE, color = Model)) +
      geom_line() +
      geom_smooth(size = 0.5, se = FALSE) +
      theme_bw() +
      labs(title = title,
           subtitle = plot.subtitle) +
      xlab(NULL) +
      ylab("% Transferred") +
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
    
    TE_plot
    
    #save figure
    ggsave(filename = paste0(region, "_TE_time_series_",DH,"_POC_1000.png"), plot = TE_plot, path = "~/regional_time_series_analysis/figures/TE/", width = 20, height = 12, units = "cm", dpi = 400)
    
    #normalized TE
    normalized.TE <- read_csv(paste0("~/regional_time_series_analysis/files/all_models/total_flux/normalized_", region, "_TE_time_series_", DH,"_POC_1000.csv"))
    normalized.TE <- data.table::melt(normalized.TE,  id.vars = 'Year', value.name = 'TE', variable.name = "Model")
    
    normalized.title = paste0("Normalized Change in Transfer Efficiency - ", DH.name)
    
    TE_norm <- ggplot(data = normalized.TE, aes(x = Year, y = TE, color = Model)) +
      geom_line() +
      geom_smooth(size = 0.5, se = FALSE) +
      theme_bw() +
      labs(title = normalized.title,
           subtitle = plot.subtitle) +
      xlab(NULL) +
      ylab("% Transferred") +
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
    
    TE_norm
    
    #save figure
    ggsave(filename = paste0("normalized_",region, "_TE_time_series_",DH,"_POC_1000.png"), plot = TE_norm, path = "~/regional_time_series_analysis/figures/TE/", width = 20, height = 12, units = "cm", dpi = 400)
    
  } 
  
  #e-ratio ----------
  
  if(DH == "POC_100") {
    DH.name2 = "100m"
  } else if(DH == "POC_MLDmax"){
    DH.name2 = "the MLDmax"
  } else if(DH == "POC_PCD"){
    DH.name2 = "the PCD"
  } else if(DH == "POC_EZ") {
    DH.name2 = "the EZ depth"
  } else {
    DH.name2 = "1000m"
  }
  
  title2 = paste0("Change in E-ratio at ", DH.name2)
  
  e_ratio <- read_csv(paste0("~/regional_time_series_analysis/files/all_models/total_flux/",region, "_e_ratio_time_series_", DH,".csv"))
  
  #melt and add column for model key
  e_ratio <- data.table::melt(e_ratio,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")
  
  e_ratio_plot <- ggplot(data = e_ratio, aes(x = Year, y = e_ratio, color = Model)) +
    geom_line() +
    geom_smooth(size = 0.5, se = FALSE) +
    theme_bw() +
    labs(title = title2,
         subtitle = plot.subtitle) +
    xlab(NULL) +
    ylab("E-ratio") +
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
  
  e_ratio_plot
  
  #save figure
  ggsave(filename = paste0(region, "_e_ratio_time_series_",DH,".png"), plot = e_ratio_plot, path = "~/regional_time_series_analysis/figures/e_ratio/", width = 20, height = 12, units = "cm", dpi = 400)
  
  #normalized e-ratio
  normalized_e_ratio <- read_csv(paste0("~/regional_time_series_analysis/files/all_models/total_flux/normalized_", region, "_e_ratio_time_series_", DH,".csv"))
  normalized_e_ratio <- data.table::melt(normalized_e_ratio,  id.vars = 'Year', value.name = 'e_ratio', variable.name = "Model")
  
  normalized.title2 = paste0("Normalized Change in E-ratio at ", DH.name2)
  
  norm_e_ratio_plot <- ggplot(data = normalized_e_ratio, aes(x = Year, y = e_ratio, color = Model)) +
    geom_line() +
    geom_smooth(size = 0.5, se = FALSE) +
    theme_bw() +
    labs(title = title2,
         subtitle = plot.subtitle) +
    xlab(NULL) +
    ylab("E-ratio") +
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
  
  norm_e_ratio_plot
  
  #save figure
  ggsave(filename = paste0("normalized_",region, "_e_ratio_time_series_",DH,".png"), plot = norm_e_ratio_plot, path = "~/regional_time_series_analysis/figures/e_ratio/", width = 20, height = 12, units = "cm", dpi = 400)
  
  
}


combine_regional_TE <- function(DH) {
  
  setwd("~/regional_time_series_analysis/figures/TE/")
  
  if(DH == "POC_1000") { 
    print("skip plotting") 
  } else {
    
    A <- readPNG(paste0("15_low_lats_TE_time_series_",DH,"_POC_1000.png"))
    B <- readPNG(paste0("30_low_lats_TE_time_series_",DH,"_POC_1000.png"))
    C <- readPNG(paste0("low_lats_no_EQ_Pacific_TE_time_series_",DH,"_POC_1000.png"))
    D <- readPNG(paste0("EQ_Pacific_TE_time_series_",DH,"_POC_1000.png"))
    E <- readPNG(paste0("North_Atlantic_TE_time_series_",DH,"_POC_1000.png"))
    f <- readPNG(paste0("SO_50_TE_time_series_",DH,"_POC_1000.png"))
    G <- readPNG(paste0("SO_60_TE_time_series_",DH,"_POC_1000.png"))
    
    setwd("~/time_series_analysis/figures/")
    if(DH == "POC_100") {
      H <- readPNG(paste0("time_series_TE_100_1000.png"))
    } else if (DH == "POC_PCD") {
      H <- readPNG(paste0("time_series_TE_PCD_1000.png"))
    } else if (DH == "POC_EZ") {
      H <- readPNG(paste0("time_series_TE_ez_1000.png"))
    } else {
      H <- readPNG(paste0("time_series_TE_MLDmax_1000.png"))
    }
    
    
    grid.combine <- grid.arrange(rasterGrob(H),rasterGrob(A), rasterGrob(B), rasterGrob(C),
                                 rasterGrob(D),rasterGrob(E),rasterGrob(f),rasterGrob(G),ncol=4, nrow=2,
                                 top= DH)
    
    ggsave(paste0("~/regional_time_series_analysis/figures/faceted/TE_regional_time_series_",DH,"_POC_1000.png"), plot = grid.combine, width = 30, height = 12, units = "cm", dpi = 400)
    
    setwd("~/regional_time_series_analysis/figures/TE/")
    
    #repeat for normalized
    A <- readPNG(paste0("normalized_15_low_lats_TE_time_series_",DH,"_POC_1000.png"))
    B <- readPNG(paste0("normalized_30_low_lats_TE_time_series_",DH,"_POC_1000.png"))
    C <- readPNG(paste0("normalized_low_lats_no_EQ_Pacific_TE_time_series_",DH,"_POC_1000.png"))
    D <- readPNG(paste0("normalized_EQ_Pacific_TE_time_series_",DH,"_POC_1000.png"))
    E <- readPNG(paste0("normalized_North_Atlantic_TE_time_series_",DH,"_POC_1000.png"))
    f <- readPNG(paste0("normalized_SO_50_TE_time_series_",DH,"_POC_1000.png"))
    G <- readPNG(paste0("normalized_SO_60_TE_time_series_",DH,"_POC_1000.png"))
    
    setwd("~/time_series_analysis/figures/")
    if(DH == "POC_100") {
      H <- readPNG(paste0("normalized_time_series_100_1000_TE.png"))
    } else if (DH == "POC_PCD") {
      H <- readPNG(paste0("normalized_time_series_PCD_1000_TE.png"))
    } else if (DH == "POC_EZ") {
      H <- readPNG(paste0("normalized_time_series_ez_1000_TE.png"))
    } else {
      H <- readPNG(paste0("normalized_time_series_MLDmax_1000_TE.png"))
    }
    
    
    grid.combine2 <- grid.arrange(rasterGrob(H),rasterGrob(A), rasterGrob(B), rasterGrob(C),
                                  rasterGrob(D),rasterGrob(E),rasterGrob(f),rasterGrob(G),ncol=4, nrow=2,
                                  top= DH)
    
    ggsave(paste0("~/regional_time_series_analysis/figures/faceted/normalized_TE_regional_time_series_",DH,"_POC_1000.png"), plot = grid.combine2, width = 30, height = 12, units = "cm", dpi = 400)
    
    #free memory space
    gc()
    
  }
  
}


combine_regional_e_ratio <- function(DH) {
  
  setwd("~/regional_time_series_analysis/figures/e_ratio/")
  
  A <- readPNG(paste0("15_low_lats_e_ratio_time_series_",DH,".png"))
  B <- readPNG(paste0("30_low_lats_e_ratio_time_series_",DH,".png"))
  C <- readPNG(paste0("low_lats_no_EQ_Pacific_e_ratio_time_series_",DH,".png"))
  D <- readPNG(paste0("EQ_Pacific_e_ratio_time_series_",DH,".png"))
  E <- readPNG(paste0("North_Atlantic_e_ratio_time_series_",DH,".png"))
  f <- readPNG(paste0("SO_50_e_ratio_time_series_",DH,".png"))
  G <- readPNG(paste0("SO_60_e_ratio_time_series_",DH,".png"))
  
  setwd("~/time_series_analysis/figures/")
  if(DH == "POC_100") {
    H <- readPNG(paste0("time_series_e_ratio_100.png"))
  } else if (DH == "POC_PCD") {
    H <- readPNG(paste0("time_series_e_ratio_PCD.png"))
  } else if (DH == "POC_EZ") {
    H <- readPNG(paste0("time_series_e_ratio_EZ.png"))
  } else if (DH == "POC_MLDmax"){
    H <- readPNG(paste0("time_series_e_ratio_MLDmax.png"))
  } else {
    H <- readPNG(paste0("time_series_e_ratio_1000.png"))
  }
  
  
  grid.combine <- grid.arrange(rasterGrob(H),rasterGrob(A), rasterGrob(B), rasterGrob(C),
                               rasterGrob(D),rasterGrob(E),rasterGrob(f),rasterGrob(G),ncol=4, nrow=2,
                               top= DH)
  
  ggsave(paste0("~/regional_time_series_analysis/figures/faceted/e_ratio_regional_time_series_",DH,".png"), plot = grid.combine, width = 30, height = 12, units = "cm", dpi = 400)
  
  #repeat for normalized 
  
  setwd("~/regional_time_series_analysis/figures/e_ratio/")
  
  A <- readPNG(paste0("normalized_15_low_lats_e_ratio_time_series_",DH,".png"))
  B <- readPNG(paste0("normalized_30_low_lats_e_ratio_time_series_",DH,".png"))
  C <- readPNG(paste0("normalized_low_lats_no_EQ_Pacific_e_ratio_time_series_",DH,".png"))
  D <- readPNG(paste0("normalized_EQ_Pacific_e_ratio_time_series_",DH,".png"))
  E <- readPNG(paste0("normalized_North_Atlantic_e_ratio_time_series_",DH,".png"))
  f <- readPNG(paste0("normalized_SO_50_e_ratio_time_series_",DH,".png"))
  G <- readPNG(paste0("normalized_SO_60_e_ratio_time_series_",DH,".png"))
  
  setwd("~/time_series_analysis/figures/")
  if(DH == "POC_100") {
    H <- readPNG(paste0("normalized_time_series_e_ratio_100.png"))
  } else if (DH == "POC_PCD") {
    H <- readPNG(paste0("normalized_time_series_e_ratio_PCD.png"))
  } else if (DH == "POC_EZ") {
    H <- readPNG(paste0("normalized_time_series_e_ratio_EZ.png"))
  } else if (DH == "POC_MLDmax"){
    H <- readPNG(paste0("normalized_time_series_e_ratio_MLDmax.png"))
  } else {
    H <- readPNG(paste0("normalized_time_series_e_ratio_1000.png"))
  }
  
  grid.combine <- grid.arrange(rasterGrob(H),rasterGrob(A), rasterGrob(B), rasterGrob(C),
                               rasterGrob(D),rasterGrob(E),rasterGrob(f),rasterGrob(G),ncol=4, nrow=2,
                               top= DH)
  
  ggsave(paste0("~/regional_time_series_analysis/figures/faceted/normalized_e_ratio_regional_time_series_",DH,".png"), plot = grid.combine, width = 30, height = 12, units = "cm", dpi = 400)
  
  #free memory space
  gc()
}
