#' @title Calculate regionally integrated time series average POC flux
#' @author Stevie Walker
#' @date 4/9/23
#' @description for historical and long-term averages

calc_region_sum <- function(DH) {

setwd("~/regional_time_series_analysis/files/all_models/total_flux/")  
df.expc <- list.files(pattern = paste0(DH,"_all*"))
#get rid of normalized files
df.expc <- df.expc[!str_detect(df.expc,pattern="normalized")]

his.vectors = list()
lt.vectors = list()

regions = c("15_low_lats", "30_low_lats", "EQ_Pacific", "low_lats_no_EQ_Pacific", "North_Atlantic", "SO_50", "SO_60", "Global")

for(i in 1:length(regions)) {
  
  if(i == 8) {
    #calculate global averages (from different directory)
    setwd("~/time_series_analysis/files/all_models/")
    #read in correct csv
    if(DH == "POC_100") {
      df.global <- read_csv("time_series_epc100_all.csv")
    } else if(DH == "POC_MLDmax") {
      df.global <- read_csv("time_series_expc_MLDmax_all.csv")
    } else if(DH == "POC_EZ") {
      df.global <- read_csv("time_series_expc_ez_all.csv")
    } else if(DH == "POC_1000") {
      df.global <- read_csv("time_series_expc_1000_all.csv")
    } else {
      df.global <- read_csv("time_series_expc_PCD_all.csv")
    }
    
    #calculate global averages and add to lists
    his.avg = dplyr::filter(df.global, between(Year, 1850, 1900)) %>% 
      summarise_if(is.numeric, mean, na.rm = TRUE)
    his.avg <- subset(his.avg, select = -c(Year))
    his.avg$Region = regions[i]
    his.avg <- his.avg %>% 
      relocate(Region, .before = CESM)
    his.avg = unlist(his.avg[1,])
    his.vectors[[i]] <- his.avg
    
    lt.avg = dplyr::filter(df.global, between(Year, 2080, 2100)) %>% 
      summarise_if(is.numeric, mean, na.rm = TRUE)
    lt.avg <- subset(lt.avg, select = -c(Year))
    lt.avg$Region = regions[i]
    lt.avg <- lt.avg %>% 
      relocate(Region, .before = CESM)
    lt.avg = unlist(lt.avg[1,])
    lt.vectors[[i]] <- lt.avg
    
  } else {
    
    setwd("~/regional_time_series_analysis/files/all_models/total_flux/")  
    #read in csv file
    df <- read_csv(df.expc[i])
    
    his.avg = dplyr::filter(df, between(Year, 1850, 1900)) %>% 
      summarise_if(is.numeric, mean, na.rm = TRUE)
    his.avg <- subset(his.avg, select = -c(Year))
    his.avg$Region = regions[i]
    his.avg <- his.avg %>% 
      relocate(Region, .before = CESM)
    his.avg = unlist(his.avg[1,])
    
    lt.avg = dplyr::filter(df, between(Year, 2080, 2100)) %>% 
      summarise_if(is.numeric, mean, na.rm = TRUE)
    lt.avg <- subset(lt.avg, select = -c(Year))
    lt.avg$Region = regions[i]
    lt.avg <- lt.avg %>% 
      relocate(Region, .before = CESM)
    lt.avg = unlist(lt.avg[1,])
    
    #store into list
    his.vectors[[i]] <- his.avg
    lt.vectors[[i]] <- lt.avg

  }
  
  }

df.region.his = as.data.frame(do.call(rbind,his.vectors))
df.region.his$Time_Period = rep("historical", 8)

df.region.lt = as.data.frame(do.call(rbind,lt.vectors))
df.region.lt$Time_Period = rep("long-term", 8)

df.region <- rbind(df.region.his,df.region.lt)

#save POC flux time-series data frame for all models
write_csv(df.region, file = paste0("~/regional_time_series_analysis/files/all_models/region_sum_flux/",DH,"_area_sums.csv"))

}

