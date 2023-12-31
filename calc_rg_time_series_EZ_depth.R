#' @title Calculate yearly euphotic zone depth for historical and future regridded data
#' @author Stevie Walker
#' @date 4/9/23
#' @description finds ez depth for every grid cell in every year from 1850-2100, to be used in regional time series calculation
#' @input npp model files
#' @output 2 large arrays (historical and future) of yearly ez depth, each matrix is one year


rg_time_series_ez_depth <- function(model.name, UKESM.file) {
  
  # Open files ---------
  
  setwd(paste0("~/spatial_analysis/regridded_nc_files/",model.name,"_rg/"))
  nc.pp <- list.files(pattern = "pp")
  
  if(model.name == "EC-Earth") {
    nc_his = NA
  } else if (model.name == "UKESM") {
    nc_his <- nc_open(nc.pp[UKESM.file])
  } else {
    nc_his <- nc_open(nc.pp[1])
  }
  
  if(model.name == "UKESM") {
    nc_fut <- nc_open(nc.pp[UKESM.file])
  } else if (model.name == "EC-Earth") {
    nc_fut = nc_open(nc.pp[1])
  } else {
    nc_fut <- nc_open(nc.pp[2])
  }
  
  if(UKESM.file != 5 && is.na(UKESM.file) == FALSE) {
    print("skip future calculation")
  } else {
    
    # Calculate future euphotic zone depth arrays -----------
    if(model.name == "CMCC" || model.name == "UKESM") {
      #monthly version
      v = seq(from = 1, to = 1021, by = 12)
    } else {
      #yearly version
      v = 1:86
    }
    
    #create list to store for loop ez depth arrays in
    list_ez <- list()
    
    #storage container for one percent npp data
    output <- matrix(nrow = 360, ncol = 180)  
    
    for(k in 1:length(v)) {
      #read in a year of data
      t <- v[k]
      #print which year the for loop is on
      print(paste("future",k,sep = " "))
      
      if(model.name == "UKESM" || model.name == "CMCC") {
        #pulls out monthly array and takes year average
        pp <- ncvar_get(nc_fut, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
        pp <- apply(pp, c(1,2,3),mean,na.rm=FALSE)
      } else {
        #pulls out array for one year, 3D with lat,lon,depth
        pp <- ncvar_get(nc_fut,"pp",start= c(1,1,1,t), count = c(-1,-1,-1,1))*31536000
      }            
      
      #calculates column integrated npp for one year
      for(i in 1:360) {
        for(j in 1:180) {
          
          ret <- list()
          #subset npp for select lat and lon
          ret$npp <- extract(pp, indices = c(i,j), dims = c(1,2))
          #adjust depth variable extraction based on model name
          if(model.name == "CESM") {
            ret$depth <-  ncvar_get(nc_fut, "lev") /100
          } else if (model.name == "IPSL") {
            ret$depth <-  ncvar_get(nc_fut, "olevel")
          } else {
            ret$depth <-  ncvar_get(nc_fut, "lev")
          } 
          
        
          if(model.name == "GFDL" || model.name == "CMCC" || model.name == "CM4") {
            
            #fix negative NPP values
            profile = cbind(ret$npp,ret$depth) %>% as_tibble()
            colnames(profile) = c("npp","depth")
            profile$npp <- ifelse(profile$npp < 0,
                                  0,
                                  profile$npp)
            profile = profile %>% 
              filter(!duplicated(npp))
            
            #finds one or ten percent of max npp (CM4 special exception)
            if(model.name == "CM4") {
              one.percent = max(profile$npp,na.rm = TRUE)*0.1
            } else {
            one.percent = max(profile$npp,na.rm = TRUE)*0.01
            }
            
            #add back to list
            ret$npp = profile$npp
            ret$depth = profile$depth
            ret$one.percent = one.percent
            #true/false test (pulls out second depth value)
            ret$test <- extract(pp[, , 2], indices = c(i,j), dims = c(1,2))
            
          } else {
            #finds one percent of max npp
            ret$one.percent = max(ret$npp,na.rm = TRUE)*0.01
            #finds five percent of max npp
            #ret$one.percent = max(ret$npp,na.rm = TRUE)*0.05
            #true/false test (pulls out second depth value)
            ret$test <- extract(pp[, , 2], indices = c(i,j), dims = c(1,2))
          }
          if (is.na(ret$test) == FALSE) {
            
            #find euphotic zone depth
            ez <- approx(x = ret$npp, y  = ret$depth, xout = ret$one.percent)
            
            if(is.na(ez$y) == FALSE) {
              #store interpolated ez depth into the output matrix
              output[i, j] <- ez$y
              
            } else {
              #true false vector of non-na and na values
              keep <- complete.cases(ret$npp)
              
              #keep rows with values
              new.npp <- ret$npp[keep]
              
              #pull out bottom depth 
              output[i,j] = ret$depth[length(new.npp)]
            }
          } else {
            # land values
            output[i,j] = NA
          }
        }
      }
      #store output into the list
      list_ez[[k]] <- output
    }
    
    
    #converts from list to matrices
    ez_fut <- do.call(cbind, list_ez)
    #combines matrices into a single array
    ez_fut <- array(ez_fut, dim=c(dim(list_ez[[1]]), length(list_ez)))
    
    #save non-averaged arrays for time series
    setwd("~/regional_time_series_analysis/files/rg_EZ_depth/")
    saveRDS(ez_fut, file = paste("rg_",model.name,"_ez_depth_fut_time_series.Rds",sep=""), ascii = TRUE)
    
  }
  
  if(model.name == "EC-Earth" || UKESM.file == 5 && is.na(UKESM.file) == FALSE) {
    print("Done!")
  } else { 
    
    ## Calculate historical average (1850-1900) euphotic zone depth ------------
    
    if(model.name == "CMCC") {
      #monthly version
      v = seq(from = 1, to = 1969, by = 12)
    } else if(model.name == "UKESM" && UKESM.file == 4 && is.na(UKESM.file) == FALSE) {
      #monthly version for 15 year UKESM file
      v = seq(from = 1, to = 169, by = 12)
    } else if(model.name == "UKESM") {
      #monthly version for 50 year UKESM file
      v = seq(from = 1, to = 589, by = 12)
    } else {
      #yearly version
      v = 1:165
    }
    
    #create list to store for loop ez depth arrays in
    list_ez <- list()
    
    #storage container for one percent npp data
    output <- matrix(nrow = 360, ncol = 180)  
    
    for(k in 1:length(v)) {
      
      #read in a year of data
      t <- v[k]
      #print which year the for loop is on
      print(paste("historical",k,sep = " "))
      
      if(model.name == "UKESM" || model.name == "CMCC") {
        #pulls out monthly array and takes year average
        pp <- ncvar_get(nc_his, "pp", start = c(1,1,1,t), count = c(-1,-1,-1,12))*31536000 #convert to mol m-3 yr-1
        pp <- apply(pp, c(1,2,3),mean,na.rm=FALSE)
      } else {
        #pulls out array for one year, 3D with lat,lon,depth
        pp <- ncvar_get(nc_his,"pp",start= c(1,1,1,k), count = c(-1,-1,-1,1))*31536000
      }    
      
      #calculates column integrated npp for one year
      for(i in 1:360) {
        for(j in 1:180) {
          
          ret <- list()
          #subset npp for select lat and lon
          ret$npp <- extract(pp, indices = c(i,j), dims = c(1,2))
          #adjust depth variable extraction based on model name
          if(model.name == "CESM") {
            ret$depth <-  ncvar_get(nc_his, "lev") /100
          } else if (model.name == "IPSL") {
            ret$depth <-  ncvar_get(nc_his, "olevel")
          } else {
            ret$depth <-  ncvar_get(nc_his, "lev")
          } 
          
          if(model.name == "GFDL" || model.name == "CMCC" || model.name == "CM4") {
            
            #fix negative NPP values
            profile = cbind(ret$npp,ret$depth) %>% as_tibble()
            colnames(profile) = c("npp","depth")
            profile$npp <- ifelse(profile$npp < 0,
                                  0,
                                  profile$npp)
            profile = profile %>% 
              filter(!duplicated(npp))
            
            #finds one or ten percent of max npp (CM4 special exception)
            if(model.name == "CM4") {
              one.percent = max(profile$npp,na.rm = TRUE)*0.1
            } else {
              one.percent = max(profile$npp,na.rm = TRUE)*0.01
            }  
            
            #add back to list
            ret$npp = profile$npp
            ret$depth = profile$depth
            ret$one.percent = one.percent
            #true/false test (pulls out second depth value)
            ret$test <- extract(pp[, , 2], indices = c(i,j), dims = c(1,2))
            
          } else {
            
            #finds one percent of max npp
            ret$one.percent = max(ret$npp,na.rm = TRUE)*0.01
            #ret$one.percent = max(ret$npp,na.rm = TRUE)*0.05
            #true/false test (pulls out second depth value)
            ret$test <- extract(pp[, , 2], indices = c(i,j), dims = c(1,2))
          }
          if (is.na(ret$test) == FALSE) {
            
            #include condition for CMCC interpolation error (this error occurs at a couple grid cells that only have negative NPP values)
            tryCatch(
              #find euphotic zone depth
              { ez <- approx(x = ret$npp, y  = ret$depth, xout = ret$one.percent) }, 
              error = function(e){
                message(paste0("An error occurred for location", i,",",j, sep = " "))
                output[i,j] = NA
                print(e)})
            
            if(is.na(ez$y) == FALSE) {
              #store interpolated ez depth into the output matrix
              output[i, j] <- ez$y
              
            } else {
              #true false vector of non-na and na values
              keep <- complete.cases(ret$npp)
              
              #keep rows with values
              new.npp <- ret$npp[keep]
              
              #pull out bottom depth 
              output[i,j] = ret$depth[length(new.npp)]
            }
          } else {
            # land values
            output[i,j] = NA
          }
        }
      }
      #store output into the list
      list_ez[[k]] <- output
      
    }
  }
  
  #converts from list to matrices
  ez_his <- do.call(cbind, list_ez)
  #combines matrices into a single array
  ez_his <- array(ez_his, dim=c(dim(list_ez[[1]]), length(list_ez)))
  
  if(model.name == "UKESM") {
    #save non-averaged arrays for time series
    setwd("~/regional_time_series_analysis/files/rg_EZ_depth/")
    #specify UKESM file
    saveRDS(ez_his, file = paste("rg_",model.name,"_",UKESM.file,"_ez_depth_his_time_series.Rds",sep=""), ascii = TRUE)
  } else {
    #save non-averaged arrays for time series
    setwd("~/regional_time_series_analysis/files/rg_EZ_depth/")
    saveRDS(ez_his, file = paste("rg_",model.name,"_ez_depth_his_time_series.Rds",sep=""), ascii = TRUE)
  }
}

