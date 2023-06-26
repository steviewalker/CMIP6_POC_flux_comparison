#' @title Regridding using CDO
#' @author Stevie Walker & Charlotte Laüfkotter
#' @date 7/15/22
#' @note There is something wrong with the area variable in the ssp585 scenario IPSL files, so the solution we came up with is to remove that variable and then regrid


# Regrid original nc file --------

regrid <- function(model.name, nc.file) {
  
  setwd(paste0("~/senior_thesis/combined_",model.name,"_files/"))
  
  names <- nc.file %>%
    str_split(.,".nc") %>%
    unlist
  
  names_IPSL <- nc.file %>%
    str_split(.,"_") %>%
    unlist
  
  if(names_IPSL[4] == "ssp585") {
    
    #remove the area variable
    system(paste0("ncks -x -v area ", nc.file, " ", "area_rm_",nc.file))
    #regrid
    system(paste0("cdo remapdis,r360x180 area_rm_",nc.file, " ", names[1], "_rg.nc"))
    #move regridded file
    system(paste0("mv ", names[1],"_rg.nc", " ~/spatial_analysis/regridded_nc_files/",model.name,"_rg"))
    
  } else {
    
    #regrid
    system(paste0("cdo remapdis,r360x180 ",nc.file, " ", names[1], "_rg.nc"))
    #move regridded file
    system(paste0("mv ", names[1],"_rg.nc", " ~/spatial_analysis/regridded_nc_files/",model.name,"_rg"))
    
  }  
}
