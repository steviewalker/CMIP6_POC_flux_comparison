#' @title Calculate time series of regionally integrated e-ratio and TE change (1850-2100)
#' @author Stevie Walker
#' @date 3/7/22

# TE ---------

regional_ts_TE <- function(region, DH) {

#read in data frames
setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
ts_100 <- read_csv(paste0(region,"_time_series_", DH,"_all.csv"))
ts_1000 <- read_csv(paste0(region,"_time_series_POC_1000_all.csv"))

no_year_100 <- ts_100[2:9]
no_year_1000 <- ts_1000[2:9]

TE <- (no_year_1000/no_year_100)*100
TE$Year = 1850:2100
TE <- TE %>%
  dplyr::relocate(Year, .before = CESM) 

#save df
write_csv(TE, paste0("~/regional_time_series_analysis/files/all_models/total_flux/", region, "_TE_time_series_", DH,"_POC_1000.csv"))

#normalized -------

#calculate average for each model from 1850-1900
mean1850_1900 <- dplyr::filter(TE, between(Year, 1850, 1900)) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

mean1850_1900 <- subset(mean1850_1900, select = -c(1))

if(DH == "POC_EZ") {

#EC-Earth is normalized around 2015-2035 because it is missing historical data
meanEC_Earth <- dplyr::filter(TE,between(Year,2015,2035)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
meanEC_Earth <- subset(meanEC_Earth, select = c(5))

#replace NA EC-Earth value
mean1850_1900[c(4)] <- c(meanEC_Earth)

} else {}

#take away year again for second round of calculations
normalized.te <- 
  subset(TE, select = -c(1))

#calculate normalized TE
normalized.te <- mapply('/', normalized.te, mean1850_1900)*100

#add back year again
normalized.te <- normalized.te %>% 
  cbind(Year = c(1850:2100)) %>% 
  as_tibble() %>% 
  relocate(Year, .before = CESM) 

write_csv(normalized.te, paste0("~/regional_time_series_analysis/files/all_models/total_flux/normalized_", region, "_TE_time_series_", DH,"_POC_1000.csv"))

}

# E-RATIO ---------

regional_ts_e_ratio <- function(region, DH) {
  
  #read in data frames
  setwd("~/regional_time_series_analysis/files/all_models/total_flux/")
  npp <- read_csv(paste0(region,"_time_series_NPP_all.csv"))
  expc <- read_csv(paste0(region,"_time_series_", DH,"_all.csv"))
  
  #subset year
  npp <- subset(npp,select = -c(1))
  expc <- subset(expc, select = -c(1))
  
  #calculate e-ratio
  e_ratio <- expc/npp
  
  #reformat and add back year
  e_ratio <- e_ratio %>%
    cbind(Year = c(1850:2100)) %>% 
    as_tibble() %>% 
    relocate(Year, .before = CESM) 
  
  write_csv(e_ratio, paste0("~/regional_time_series_analysis/files/all_models/total_flux/", region, "_e_ratio_time_series_", DH,".csv"))
  
  #normalized --------
  
  #calculate average for each model from 1850-1900
  mean1850_1900 <- dplyr::filter(e_ratio, between(Year, 1850, 1900)) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  mean1850_1900 <- subset(mean1850_1900, select = -c(1))
  
  #EC-Earth is normalized around 2015-2035 because it is missing historical data
  meanEC_Earth <- dplyr::filter(e_ratio,between(Year,2015,2035)) %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  meanEC_Earth <- subset(meanEC_Earth, select = c(5))
  
  #replace NA EC-Earth value
  mean1850_1900[c(4)] <- c(meanEC_Earth)
  
  #take away year again for second round of calculations
  normalized_e_ratio <- 
    subset(e_ratio, select = -c(1))
  
  #calculate normalized TE
  normalized_e_ratio <- mapply('/', normalized_e_ratio, mean1850_1900)*100
  
  #add back year again
  normalized_e_ratio <- normalized_e_ratio %>% 
    cbind(Year = c(1850:2100)) %>% 
    as_tibble() %>% 
    relocate(Year, .before = CESM) 
  
  write_csv(normalized_e_ratio, paste0("~/regional_time_series_analysis/files/all_models/total_flux/normalized_", region, "_e_ratio_time_series_", DH,".csv"))

}
