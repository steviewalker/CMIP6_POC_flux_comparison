# CMIP6_POC_flux_comparison
Multimodel comparison of POC flux changes across multiple export depth horizons in CMIP6 ESMs, code written by Stevie Walker, 2020-2025

This readme file is organized into four sections -  1. ESM file download and prep, 2. time series analysis, 3. spatial maps analysis, 4. regional time series analysis - with brief descriptions of each script. All code is described further and organized in the order you should run it in the .Rmd files. paper_figs.R: plots all figures in the manuscript.

All code was run in a Microsoft Azure Linux server, enabling the analysis of large netcdf model output files. Different virtual machine sizes can be used, but I have found the F series v2 VMs to be the best because they have a performance boost for vector processing workloads and can be sized up to have a high number of CPUs. This is a benefit because code can be run in multiple R sessions. I used the TMUX terminal multiplexer to run code in the background, since some functions take over 2 weeks to run.


Scripts used (in the order you should run them): 


ESM FILE DOWNLOAD AND PREPARATION


1. libraries.R: downloads all packages and loads all libraries needed for analysis
2. get_nc_thredds.R: downloads CMIP6 netcdf files
3. combine_ncfiles.R: combine multipart files into one file from 1850-2014 (historical) or 2015-2100 (SSP5-8.5)
4. grab_metadata.R: extracts metadata from netcdf files


TIME SERIES ANALYSIS


1. get_time.R: extracts time steps for each file
2. calc_time_series_epc100.R: calculates POC flux time series at 100 m for all models except CM4, using epc100 file
3. calc_time_series_MLDmax.R: calculate MLDmax matrices for every year and for every model
4. calc_time_series_expc.R: calculates POC flux time series at MLDmax, PCD, and 1000 m for all models except UKESM, and 100 m for CM4, requires MLDmax arrays from calc_time_series_MLDmax.R
5. calc_time_series_expc_UKESM.R: calculates POC flux time series at MLDmax, PCD, EZ depth, and 1000 m for UKESM, be sure to combine the time series after
6. calc_time_series_ez_depth.R: calculates EZ depth matrices for every year and every model to be used in POC flux EZ depth time series
7. calc_time_series_ez_depth_10percent.R: calculates EZ depth matrices for every year and every model using the 10% NPPmax definition to be used in POC flux EZ depth time series
8. calc_time_series_expc_ez.R: calculates POC flux time series at EZ depth for all models except UKESM
9. calc_time_series_npp.R: calculates column integrated NPP time series in all models except UKESM
10. calc_time_series_npp_UKESM.R: calculates column integrated NPP time series for UKESM
11. plot_time_series.R: calculates TE and e-ratio and saves dataframes, plots absolute and normalized time series for all variables
12. plot_time_series_absolute.R: plots normalized absolute time series for all variables
13. calc_PCD_global_depth_avg.R: source function for step 14
14. calc_DH_avg_table.R: calculates historical and long-term globally integrated average depth horizon depth for EZ depth (1% or 10% NPPmax), PCD, and MLDmax. Uses function in calc_PCD_global_depth_avg.R


SPATIAL MAPS ANALYSIS


1. regrid_nc.R: calls CDO command line suite to regrid models using inverse distance weighted average interpolation (remapdis)
2. calc_multimodel_avg.R: calculates multimodel average of any given variable. use calc_multimodel_avg for all POC flux variables, MLDmax, EZ depth, NPP. Use calc_multimodel_avg_2 for TE and e-ratio. Use calc_multimodel_avg_5 for 5% EZ depth metric. 
3. rg_calc_epc100_avg.R: calculates historical, long-term, and change in POC flux at 100 m
4. rg_plot_POC_100.R: plots historical, long-term, and change in POC flux at 100 m
5. rg_calc_MLD_max.R: calculates historical, long-term, and change in maximum annual mixed layer depth
6. rg_plot_MLD_max.R: plots historical, long-term, and change in maximum annual mixed layer depth. Also plots with a 100 m pivot point
7. rg_calc_EZ_depth.R: calculates historical, long-term, and change in EZ depth at 3 different EZ depth metrics. 
8. rg_plot_EZ_depth.R: plots historical, long-term, and change in EZ depth, also plots difference in EZ depth metric (plot_metric_diff_EZ)
9. rg_calc_PCD.R: calculates historical, long-term, and change in PCD
10. rg_plot_PCD.R: plots historical, long-term, and change in PCD, also plots 100 m pivot point
11. rg_calc_expc_avg.R: calculates historical, long-term, and change in POC flux at EZ depth, PCD, MLDmax, 1000m, and 100 m for CM4
12. rg_plot_expc.R: plots historical, long-term, and change in POC flux at MLDmax, PCD, and 1000 m
13. rg_plot_POC_ez_depth.R: plots historical, long-term, and change in POC flux at EZ depth for different EZ depth metrics, also includes plot EZ metric difference function
14. rg_calc_NPP.R: calculates historical, long-term, and change in water column integrated NPP
15. rg_plot_NPP.R: plots historical, long-term, and change in water column integrated NPP
16. rg_calc_TE_e-ratio: calculates historical, long-term, and change in transfer efficiency and e-ratio for all depth horizons
17. rg_plot_TE.R: plots the historical, long-term, and change in TE for every model and depth horizon
18. calc_model_agreement.R: calculates the percent area and creates a map of where all 8 models agree on the sign of change for any variable
19. DH_differences_assessment.R: creates plots comparing POC flux at EZ depth, MLDmax, or PCD to 100 m


REGIONAL TIME SERIES ANALYSIS


1. calc_rg_time_series_MLDmax.R: calculate MLDmax global matrices (1850-2100) from regridded model output to use in regional time series interpolation
2. calc_rg_time_series_EZ_depth.R: calculate EZ depth global matrices (1850-2100) from regridded model output to use in regional time series interpolation
3. calc_GFDL_region_area.R: calculate regridded region area for all regions
4. calc_regional_time_series_averages.R: calculates historical and long-term average POC flux for each region
5. calc_region_boundaries.R: creates raster of region boundaries for use in overlaying over final figures 2 and 4
6. calc_regional_time_series_epc100.R: calculates regionally integrated time series for POC flux at 100 m
7. calc_regional_time_series_expc.R: calculates regionally integrated time series for POC flux at 100 m (CM4), PCD, MLDmax, and 1000 m
8. calc_regional_time_series_expc_UKESM.R: calculates regionally integrated time series for POC flux PCD, MLDmax, and 1000 m for UKESM, since it has monthly resolved POC flux data. Also includes function to combine UKESM time series files
9. calc_regional_time_series_expc_ez.R: calculates regionally integrated time series for POC flux at EZ depth
10. calc_regional_time_series_expc_ez_UKESM.R: calculates regionally integrated time series for POC flux at EZ depth for UKESM, since it has monthly resolved POC flux data
11. calc_regional_time_series_npp.R: calculates regionally integrated NPP time series
12. calc_regional_e_ratio_TE.R: calculates regionally integrated e-ratio and TE time series
13. plot_regional_time_series.R: plots regional time series of POC fluxes and NPP
14. plot_regional_time_series_TE_e_ratio.R: plots regional time series of TE and e-ratio
