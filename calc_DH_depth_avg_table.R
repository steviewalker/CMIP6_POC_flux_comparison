#' @title Calculate historical and long-term globally integrated average depth horizon depth
#' @date 3/3/25
#' @description for table S1, EZ depth and MLDmax are calculated from DH depth arrays used in time series calculations, PCD calculated from function calc_PCD_global_depth_avg.R

# for EZ depth, MLDmax - read in future and his time series files, take average


DH_means_lt <- list()
var.names = c("EZ_depth", "EZ_depth_10", "PCD", "MLDmax")
model_fullnames = c("CESM2","GFDL-CM4" ,"CMCC-ESM2", "EC-Earth3-CC", "GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "UKESM1-0-LL")
model_list = c("CESM","CM4" ,"CMCC", "EC-Earth", "GFDL", "IPSL", "MPI", "UKESM")

# EZ depth 1% NPP max -----------

EZ_means_lt = list()
EZ_means_his = list()

for(i in 1:length(model_list)) {
  
  print(model_list[i])
  #read in DH depth file
  setwd("~/time_series_analysis/files/EZ_depth/")
  if(i == 2) {
    #switch for CM4 because 1% is 10% and vice versa
    DH_fut = readRDS(paste0("~/time_series_analysis/files/EZ_depth/ten_percent/",model_list[i],"_ez_depth_10_fut_time_series.Rds"))[,,67:86]
  } else {
    DH_fut = readRDS(paste0("~/time_series_analysis/files/EZ_depth/",model_list[i],"_ez_depth_fut_time_series.Rds"))[,,67:86]
  }
  DH_fut_avg = mean(DH_fut, na.rm = TRUE)
  remove(DH_fut)
  
  lt_mean <- cbind(Model = model_fullnames[i],EZ_depth = DH_fut_avg, time_period = "long-term") %>% 
    as_tibble()
  
  #add if else statement for EC-Earth, UKESM, CM4
  if (i == 4) {
    his_mean = cbind(Model = model_fullnames[i],EZ_depth = NA, time_period = "historical") %>% 
      as_tibble()
  } else if (i == 2) {
    DH_his = readRDS(paste0("~/time_series_analysis/files/EZ_depth/ten_percent/",model_list[i],"_ez_depth_10_his_time_series.Rds"))
    DH_his_avg = mean(DH_his, na.rm = TRUE)
    remove(DH_his)
    his_mean <- cbind(Model = model_fullnames[i],EZ_depth = DH_his_avg, time_period = "historical") %>% 
      as_tibble()
  } else if (i == 8) {
    DH_his = readRDS(paste0("~/time_series_analysis/files/EZ_depth/",model_list[i],"_1_ez_depth_his_time_series.Rds"))
    DH_his_avg = mean(DH_his, na.rm = TRUE)
    remove(DH_his)
    his_mean <- cbind(Model = model_fullnames[i],EZ_depth = DH_his_avg, time_period = "historical") %>% 
      as_tibble()
  } else {
    DH_his = readRDS(paste0("~/time_series_analysis/files/EZ_depth/",model_list[i],"_ez_depth_his_time_series.Rds"))[,,1:50]
    DH_his_avg = mean(DH_his, na.rm = TRUE)
    remove(DH_his)
    his_mean <- cbind(Model = model_fullnames[i],EZ_depth = DH_his_avg, time_period = "historical") %>% 
      as_tibble()
  }
  
  EZ_means_lt[[i]] <- lt_mean
  EZ_means_his[[i]] <- his_mean
  
}

EZ_means_lt_df = Reduce(full_join,EZ_means_lt)
EZ_means_his_df = Reduce(full_join,EZ_means_his)
df = rbind(EZ_means_lt_df,EZ_means_his_df)

# EZ depth 10% NPP max -------------

EZ_10_means_lt = list()
EZ_10_means_his = list()

for(i in 1:length(model_list)) {
  
  print(model_list[i])
  #read in DH depth file
  setwd("~/time_series_analysis/files/EZ_depth/")
  if(i == 2) {
    #switch for CM4 because 1% is 10% and vice versa
    DH_fut = readRDS(paste0("~/time_series_analysis/files/EZ_depth/",model_list[i],"_ez_depth_fut_time_series.Rds"))[,,67:86]
  } else {
    DH_fut = readRDS(paste0("~/time_series_analysis/files/EZ_depth/ten_percent/",model_list[i],"_ez_depth_10_fut_time_series.Rds"))[,,67:86]
  }
  DH_fut_avg = mean(DH_fut, na.rm = TRUE)
  remove(DH_fut)
  
  lt_mean <- cbind(Model = model_fullnames[i],EZ_depth_10 = DH_fut_avg, time_period = "long-term") %>% 
    as_tibble()
  
  #add if else statement for EC-Earth, UKESM, CM4
  if (i == 4) {
    his_mean = cbind(Model = model_fullnames[i],EZ_depth_10 = NA, time_period = "historical") %>% 
      as_tibble()
  } else if (i == 2) {
    DH_his = readRDS(paste0("~/time_series_analysis/files/EZ_depth/",model_list[i],"_ez_depth_his_time_series.Rds"))
    DH_his_avg = mean(DH_his, na.rm = TRUE)
    remove(DH_his)
    his_mean <- cbind(Model = model_fullnames[i],EZ_depth_10 = DH_his_avg, time_period = "historical") %>% 
      as_tibble()
  } else if (i == 8) {
    DH_his = readRDS(paste0("~/time_series_analysis/files/EZ_depth/ten_percent/",model_list[i],"_1_ez_depth_10_his_time_series.Rds"))
    DH_his_avg = mean(DH_his, na.rm = TRUE)
    remove(DH_his)
    his_mean <- cbind(Model = model_fullnames[i],EZ_depth_10 = DH_his_avg, time_period = "historical") %>% 
      as_tibble()
  } else {
    DH_his = readRDS(paste0("~/time_series_analysis/files/EZ_depth/ten_percent/",model_list[i],"_ez_depth_10_his_time_series.Rds"))[,,1:50]
    DH_his_avg = mean(DH_his, na.rm = TRUE)
    remove(DH_his)
    his_mean <- cbind(Model = model_fullnames[i],EZ_depth_10 = DH_his_avg, time_period = "historical") %>% 
      as_tibble()
  }
  
  EZ_10_means_lt[[i]] <- lt_mean
  EZ_10_means_his[[i]] <- his_mean
  
}

EZ_10_means_lt_df = Reduce(full_join,EZ_10_means_lt)
EZ_10_means_his_df = Reduce(full_join,EZ_10_means_his)
df_10 = rbind(EZ_10_means_lt_df,EZ_10_means_his_df)

## MLDmax -----------

MLDmax_means_lt = list()
MLDmax_means_his = list()

for(i in 1:length(model_list)) {
  
  print(model_list[i])
  #read in DH depth file
  setwd("~/time_series_analysis/files/MLDmax/")
  DH_fut = readRDS(paste0("~/time_series_analysis/files/MLDmax/",model_list[i],"_MLD_fut_time_series.Rds"))[,,67:86]
  DH_fut_avg = mean(DH_fut, na.rm = TRUE)
  remove(DH_fut)
  
  lt_mean <- cbind(Model = model_fullnames[i],MLDmax = DH_fut_avg, time_period = "long-term") %>% 
    as_tibble()
  
  #add if else statement for EC-Earth, UKESM, CM4
  DH_his = readRDS(paste0("~/time_series_analysis/files/MLDmax/",model_list[i],"_MLD_his_time_series.Rds"))
  DH_his_avg = mean(DH_his, na.rm = TRUE)
  remove(DH_his)
  his_mean <- cbind(Model = model_fullnames[i],MLDmax = DH_his_avg, time_period = "historical") %>% 
    as_tibble()
  
  MLDmax_means_lt[[i]] <- lt_mean
  MLDmax_means_his[[i]] <- his_mean
  
}

MLDmax_means_lt_df = Reduce(full_join,MLDmax_means_lt)
MLDmax_means_his_df = Reduce(full_join,MLDmax_means_his)
df_MLDmax = rbind(MLDmax_means_lt_df,MLDmax_means_his_df)

DH_table = cbind(df,df_10[2],df_MLDmax[2]) 

DH_table = DH_table %>%
  relocate(time_period, .before = EZ_depth)

setwd("~/time_series_analysis/files/average_change/")
write_csv(DH_table, "global_avg_DH_depth.csv")


## PCD -------------

setwd("~/senior_thesis/")
source("libraries.R")
setwd("~/time_series_analysis/code/")
source("calc_PCD_global_depth_avg.R")

# CESM ---------


calc_PCD_avg(model.name = "CESM",
                 lon.length = 1:320,
                 lat.length = 1:384,
                 DH = "PCD")

# CM4 -------------

calc_PCD_avg(model.name = "CM4",
                 lon.length = 1:360,
                 lat.length = 1:180,
                 DH = "PCD")

# CMCC ------------

calc_PCD_avg(model.name = "CMCC",
                 lon.length = 1:362,
                 lat.length = 1:292,
                 DH = "PCD")

# EC-Earth -------------

calc_PCD_avg(model.name = "EC-Earth",
                 lon.length = 1:362,
                 lat.length = 1:292,
                 DH = "PCD")

# GFDL ----------------

calc_PCD_avg(model.name = "GFDL",
                 lon.length = 1:360,
                 lat.length = 1:180,
                 DH = "PCD")

#IPSL ------------

calc_PCD_avg(model.name = "IPSL",
                 lon.length = 1:362,
                 lat.length = 1:332,
                 DH = "PCD")

#MPI -----------

calc_PCD_avg(model.name = "MPI",
                 lon.length = 1:802,
                 lat.length = 1:404,
                 DH = "PCD")

#UKESM ------

calc_PCD_avg(model.name = "UKESM",
             lon.length = 1:360,
             lat.length = 1:330,
             DH = "PCD")


