library(targets)
library(tarchetypes)
library(readxl)
options(clustermq.scheduler = "multiprocess")
library(clustermq)
options(tidyverse.quiet = TRUE)
library(tidyverse)

##Load libraries for use in computing targets
tar_option_set(packages = c("fasstr", "EflowStats", "dataRetrieval",
                            "lubridate", "cluster", "factoextra", "NbClust",
                            "sf", "cowplot", "gridGraphics", "stringi",
                            "dendextend", "scico", "tidyverse", "nhdplusTools",
                            "sbtools", "maps", "mapproj"),
               imports = c("fasstr", "EflowStats", "dataRetrieval", 
                           "cluster","factoextra", "NbClust", "dendextend",
                           "tidyverse"))

##Create output file directories
dir.create('1_fetch/out', showWarnings = FALSE)
dir.create('1_fetch/out/stationarity_plots', showWarnings = FALSE)
dir.create('1_fetch/out/stationarity_plots/by_quantiles', showWarnings = FALSE)
dir.create('1_fetch/out/stationarity_plots/by_agg_quantiles', showWarnings = FALSE)
dir.create('1_fetch/out/logs', showWarnings = FALSE)
dir.create('1_fetch/out/sb', showWarnings = FALSE)
dir.create('1_fetch/out/sb/workdir', showWarnings = FALSE)
dir.create('1_fetch/out/sb/dldir', showWarnings = FALSE)
dir.create('3_cluster/out', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/barplots', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/barplots/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/barplots/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/barplots/CONUS', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/diagnostics', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/diagnostics/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/diagnostics/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/maps', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/maps/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/maps/by_agg_quantiles', showWarnings = FALSE)

##Load user defined functions
source("1_fetch/src/get_nwis_data.R")
source("1_fetch/src/get_sb_data.R")
source("1_fetch/src/calc_HIT.R")
source("1_fetch/src/calc_FDC.R")
source("1_fetch/src/moving_window_functions.R")
source("3_cluster/src/seasonal_metric_cluster.R")
source("4_setup_crossval/src/cross_validation_functions.R")

###Define parameters
NWIS_parameter <- '00060'
startDate <- as.Date("1900-10-01") 
endDate <- as.Date("2020-09-30")
##water year or calendar year.
yearType <- "water"
year_start <- 10
##number of complete years we require for a site to be used
complete_years <- 20
##percentile for flood threshold in EflowStats. 0.6 is the default
perc <- 0.6
##statistics to compute within EflowStats
stats_HIT <- c("calc_magAverage", "calc_magLow", "calc_magHigh", 
               "calc_frequencyHigh", "calc_durationHigh", "calc_rateChange"
               )
##EflowStats metrics to use
metrics <- c('ma1', 
             'ml17', 'ml18', 
             'mh20', 
             'fh2', 
             'dh1', 'dh6', 'dh15', 'dh16', 'dh23',
             'ra1', 'ra2', 'ra3', 'ra4'
             )
##metrics to normalize by drainage area
metrics_DA <- c('ma1', 'dh1', 'ra1', 'ra3')
##metric ml17 to normalize by *annual mean/drainage area
metrics_ml17 <- c('ml17')
##metrics to normalize by *median/drainage area
metrics_med_DA <- NULL
#non-exceedance quantiles for additional metrics - daily flows
NE_quants <- c(seq(0.5, 0.95, 0.05), 0.98, 0.99, 0.995)
#Seasons to use in season analysis
# matches water year
season_months <- c(10, 11, 12, seq(1, 9, 1))
season_year_start <- season_months[1]
# suggested by Ken for high flows
season_months_high <- c(12, seq(1, 11, 1))
season_year_start_high <- season_months_high[1]
###moving window parameters
window_length <- 20  ##needs to be <= complete_years
increment <- 1
min_yrs_in_window<- 15  ##minimum number of years of data required within a window
min_windows <- 10  ##Must have this many windows available in order to plot 

#this is the top level Science Base ID of Mike's database
sb_parent_id <- '5669a79ee4b08895842a1d47'

#pre-defined variable list to reduce SB pull
sb_var_list_path <- "1_fetch/in/FHWA-NHDVariableList.xlsx"
sb_var_sheet <- "FY22-FHWA"

#pre-defined reclassification tables for SOHL and NLCD land cover classes
sb_sohl_path <- "1_fetch/in/Legend_FORESCE_Land_Cover.csv"
sb_nlcd_path <- "1_fetch/in/Legend_NLCD_Land_Cover.csv"

###gages2.1 ref site list - not sure how to get this right from sharepoint, so the
##filepath is currently to onedrive.
#gagesii_path <- "C:/Users/jsmith/OneDrive - DOI/Shared Documents - FHWA/General/Data/Gages2.1_RefSiteList.xlsx"
#gagesii_path <- "C:/Users/slevin/OneDrive - DOI/FWA_bridgeScour/Data/Gages2.1_RefSiteList.xlsx"
gagesii_path <- "Gages2.1_RefSiteList.xlsx"

#Drop the following gages from the dataset because they are not representative
#pipeline, ditch, etc.
drop_gages <- c('02084557', '09406300', '09512200', '10143500', '10172200')

##distance to search upstream for nested basins, in km.  note-the nhdplusTools function fails if this 
##value is 10000 or greater.
nav_distance_km<-4500

#set random seed for project
set.seed(12422)


##targets
list(
  #all gagesii (g2) sites 
  tar_target(p1_sites_g2,
             read_xlsx(gagesii_path) %>% 
               mutate(ID = substr(ID, start=2, stop=nchar(ID))) %>%
               #drop 5 sites that are not representative (ditch, pipeline)
               filter(!(ID %in% drop_gages)),
             deployment = 'main'
  ),
  #create a spatial object 
  tar_target(p1_sites_g2_sf,
             st_as_sf(x = p1_sites_g2, coords = c('LON', 'LAT'), 
                      remove = FALSE, dim = 'XY', na.fail = TRUE),
             deployment = 'main'
  ),
  
  #ID numbers for sites to use
  tar_target(
    p1_sites_list,
    {p1_sites_g2$ID
     ## not sure yet how we'll be selecting gages so I'm not putting this in a function yet.
     ##since there is no state attribution in the gagesii list, for East River, I am taking 
     ##AggEco==WestMnts and LON > -117 which cuts off the pacific northwest and cA areas
     
     #p1_sites_g2 %>%
     #  filter(AggEco == "WestMnts") %>%
     #  filter(LON > -117) %>%
     #  filter(LAT > 36) %>%
     #  pull(ID)
     
     #DE - just pulling a bounding box of sites here
     #p1_sites_g2 %>%
     #  filter(LAT < 42) %>%
     #  filter(LON > -76) %>%
     #  pull(ID)
   },
   deployment = 'main'
  ),
  
  ##check to make sure peak and daily flow are actually available for all sites
  tar_target(p1_has_data,
             has_data_check(p1_sites_list, NWIS_parameter, endDate),
             deployment = 'main'
  ),
  
  ##fetch daily streamflow
  #this is deployed on main to avoid overloading the NWIS server with download requests
  tar_target(p1_daily_flow_csv, 
             get_nwis_daily_data(p1_has_data, outdir="./1_fetch/out", 
                                 NWIS_parameter, startDate, endDate),
             map(p1_has_data),
             deployment = 'main',
             format = "file"
  ),
  ##generate log file to track changes to dataRetrieval daily flow request
  tar_target(p1_daily_flow_log, 
             get_daily_flow_log(files_in = p1_daily_flow_csv, 
                                file_out = "./1_fetch/out/logs/daily_flow_log.csv"),
             deployment = 'main',
             format = "file"
  ),
  
  ##prescreen data to remove provisional data and handle odd column names
  tar_target(p1_prescreen_daily_data, 
             prescreen_daily_data(p1_daily_flow_csv, prov_rm = TRUE),
             map(p1_daily_flow_csv),
             deployment = 'worker'
  ),
  
  ##compute the number of complete years based on when the year starts
  #These are being run on main because parallel processing is taking too long.
  #Likely because the mapped branches build quickly and the prescreen data are large
  tar_target(p1_screen_daily_flow,
             screen_daily_data(p1_has_data, p1_prescreen_daily_data, year_start),
             map(p1_has_data),
             deployment = 'main'
  ),
  ##For seasonal analysis
  tar_target(p1_screen_daily_flow_season,
             screen_daily_data(p1_has_data, p1_prescreen_daily_data, season_year_start),
             map(p1_has_data),
             deployment = 'main'
  ),
  tar_target(p1_screen_daily_flow_season_high,
             screen_daily_data(p1_has_data, p1_prescreen_daily_data, season_year_start_high),
             map(p1_has_data),
             deployment = 'main'
  ),
  
  ##select sites with enough complete years
  tar_target(p1_screened_site_list,
             filter_complete_years(p1_screen_daily_flow, complete_years),
             deployment = 'main'
  ),
  ##seasonal
  tar_target(p1_screened_site_list_season,
             filter_complete_years(p1_screen_daily_flow_season, complete_years),
             deployment = 'main'
  ),
  tar_target(p1_screened_site_list_season_high,
             filter_complete_years(p1_screen_daily_flow_season_high, complete_years),
             deployment = 'main'
  ),
  
  ##clean and format daily data so it can be used in EflowStats 
  tar_target(p1_clean_daily_flow,
             clean_daily_data(p1_screened_site_list, p1_prescreen_daily_data, 
                              p1_screen_daily_flow, yearType, year_start),
             map(p1_screened_site_list),
             deployment = 'main'
  ),
  ##seasonal
  tar_target(p1_clean_daily_flow_season,
             clean_daily_data(p1_screened_site_list_season, p1_prescreen_daily_data, 
                              p1_screen_daily_flow_season, yearType, season_year_start),
             map(p1_screened_site_list_season),
             deployment = 'main'
  ),
  tar_target(p1_clean_daily_flow_season_high,
             clean_daily_data(p1_screened_site_list_season_high, p1_prescreen_daily_data, 
                              p1_screen_daily_flow_season_high, yearType, 
                              season_year_start_high),
             map(p1_screened_site_list_season_high),
             deployment = 'main'
  ),
  
  #get drainage area from NWIS
  #this is deployed on main to avoid overloading the NWIS server with download requests
  tar_target(p1_drainage_area,
             get_NWIS_drainArea(p1_screened_site_list),
             map(p1_screened_site_list),
             deployment = 'main'
  ),
  ##generate log file to track changes to dataRetrieval drainage area request
  tar_target(p1_drainage_area_log, 
             get_drainage_area_log(file_in = p1_drainage_area, 
                                   file_out = "./1_fetch/out/logs/drainage_area_log.csv"),
             deployment = 'main',
             format = "file"
  ),
  
  ##get and save as file peak flow from NWIS for eflowstats
  #this is deployed on main to avoid overloading the NWIS server with download requests
  tar_target(p1_peak_flow_csv,
             get_nwis_peak_data(p1_screened_site_list, outdir="./1_fetch/out",
                                startDate, endDate),
             map(p1_screened_site_list),
             deployment = 'main',
             format="file"
  ),
  ##generate log file to track changes to dataRetrieval peak flow request
  tar_target(p1_peak_flow_log, 
             get_peak_flow_log(files_in = p1_peak_flow_csv, 
                               file_out = "./1_fetch/out/logs/peak_flow_log.csv"),
             deployment = 'main',
             format = "file"
  ),
  
  ##generate table of data to download from sciencebase
  tar_target(p1_sb_table_full_csv,
             make_dl_table(sb_parent_id = sb_parent_id, 
                           outdir = "./1_fetch/out/sb"),
             deployment = 'main',
             format = "file"
             ),
  
  #read in sciencebase variable list excel sheet
  tar_target(p1_sb_var_list,
             read_xlsx(path = sb_var_list_path, sheet = sb_var_sheet),
             deployment = 'main'
  ),
  
  ##reduce list to watershed attributes of interest
  tar_target(p1_sb_table_reduced_csv, 
             reduce_sb_table(sb_table_full = p1_sb_table_full_csv, 
                             sb_var_list = p1_sb_var_list,
                             outdir = "./1_fetch/out/sb"), 
             deployment = 'main', 
             format = "file"
             ),
  
  ##generate table of landscape data for gagesii list  
  tar_target(p1_sb_data_g2_csv,
             download_children(sites = p1_sites_g2, 
                               sb_table_reduced = p1_sb_table_reduced_csv,
                               dldir = "./1_fetch/out/sb/dldir", 
                               workdir = "./1_fetch/out/sb/workdir",
                               outdir = "./1_fetch/out/sb",
                               out_file_name = "sb_data_g2.csv"),
             deployment = 'main',
             format = "file"
             ),
  
  #read in SOHL land use reclassification table
  tar_target(p1_sb_sohl_table,
             read_csv(sb_sohl_path, show_col_types = FALSE),
             deployment = 'main'
             ),
  
  #read in NLCD land use reclassification table
  tar_target(p1_sb_nlcd_table,
             read_csv(sb_nlcd_path, show_col_types = FALSE),
             deployment = 'main'
             ),
  
  #combine, reclassify, and calculate average land covers for each gage
  tar_target(p1_avg_land_cover_g2, 
             {file_ind <- grep(p1_sb_data_g2_csv, pattern = "FAILS", invert = TRUE)
             calc_avg_land_cover(sb_data = p1_sb_data_g2_csv[file_ind])
             }, 
             deployment = 'main',
             ),
  
  ##convert monthly weather data by year to average monthly weather
  tar_target(p1_monthly_weather_g2, 
             {file_ind <- grep(p1_sb_data_g2_csv, pattern = "FAILS", invert = TRUE)
               calc_avg_monthly_weather(sb_data = p1_sb_data_g2_csv[file_ind])
             }, 
             deployment = 'main', 
             ),

  ##convert annual wildfire data to long-term average wildfire
  tar_target(p1_avg_wildfire_g2, 
             {file_ind <- grep(p1_sb_data_g2_csv, pattern = "FAILS", invert = TRUE)
             calc_avg_wildfire(sb_data = p1_sb_data_g2_csv[file_ind])
             }, 
             deployment = 'main', 
  ),
  
  ##get flood threshold from NWIS for eflowstats
  #this is deployed on main to avoid overloading the NWIS server with download requests
  tar_target(p1_flood_threshold,
             get_floodThreshold(p1_screened_site_list, p1_clean_daily_flow,
                                p1_peak_flow_csv, perc, yearType),
             map(p1_screened_site_list),
             deployment = 'main'
  ),
  
  ##compute all HIT metrics for screened sites list
  tar_target(p1_HIT_metrics,
             calc_HITmetrics(site_num = p1_screened_site_list, 
                             clean_daily_flow = p1_clean_daily_flow, 
                             yearType = yearType,
                             drainArea_tab = p1_drainage_area,
                             floodThreshold_tab = p1_flood_threshold,
                             stat_vec = stats_HIT,
                             save_metrics = metrics,
                             norm_DA = metrics_DA,
                             norm_med_DA = metrics_med_DA,
                             norm_ml17 = metrics_ml17,
                             out_format = 'pivot'),
             map(p1_screened_site_list),
             deployment = 'worker'
  ),
  
  ##compute additional FDC-based metrics for screened sites list
  tar_target(p1_FDC_metrics,
             calc_FDCmetrics(site_num = p1_screened_site_list, 
                             clean_daily_flow = p1_clean_daily_flow, 
                             yearType = yearType,
                             drainArea_tab = p1_drainage_area,
                             NE_probs = NE_quants,
                             seasonal = FALSE,
                             year_start = year_start,
                             out_format = 'pivot'),
             map(p1_screened_site_list),
             deployment = 'worker'
  ),
  
  ##compute seasonal FDC-based metrics using water year seasons
  tar_target(p1_FDC_metrics_season,
             calc_FDCmetrics(site_num = p1_screened_site_list_season, 
                             clean_daily_flow = p1_clean_daily_flow_season, 
                             yearType = yearType,
                             drainArea_tab = NULL,
                             NE_probs = NE_quants,
                             seasonal = TRUE,
                             season_months = season_months,
                             stat_type = 'POR',
                             year_start = season_year_start,
                             out_format = 'pivot'),
             map(p1_screened_site_list_season),
             deployment = 'worker'
  ),
  
  ##compute seasonal FDC-based metrics using high flow seasons
  tar_target(p1_FDC_metrics_season_high,
             calc_FDCmetrics(site_num = p1_screened_site_list_season_high, 
                             clean_daily_flow = p1_clean_daily_flow_season_high, 
                             yearType = yearType,
                             drainArea_tab = NULL,
                             NE_probs = NE_quants,
                             seasonal = TRUE,
                             season_months = season_months_high,
                             stat_type = 'POR',
                             year_start = season_year_start_high,
                             out_format = 'pivot'),
             map(p1_screened_site_list_season_high),
             deployment = 'worker'
  ),
  
  ##Cluster analysis to make model regions from FDC metrics
  tar_target(p3_metric_names,
             {colnames(p1_FDC_metrics)[-c(1,grep(colnames(p1_FDC_metrics), pattern = 'mhfdc'))]
               },
             deployment = 'main'
  ),
  tar_target(p3_metric_names_quants,
             {as.character(NE_quants)},
             deployment = 'main'
  ),
  tar_target(p3_metric_names_quants_agg,
             {c(str_c(as.character(NE_quants)[1:5], collapse = ','), 
                str_c(as.character(NE_quants)[6:10], collapse = ','))
               },
             deployment = 'main'
  ),
  
  #barplot for all metrics, averaged over all gages
  tar_target(p3_seasonal_barplot_COUNS_png,
             plot_seasonal_barplot(metric_mat = p1_FDC_metrics_season,
                                   metric = p3_metric_names,
                                   season_months = season_months,
                                   by_cluster = FALSE,
                                   dir_out = '3_cluster/out/seasonal_plots/barplots/CONUS/'),
             map(p3_metric_names),
             deployment = 'worker',
             format = 'file'),

  #Compute clusters
  tar_target(p3_FDC_clusters,
             seasonal_metric_cluster(metric_mat = p1_FDC_metrics_season,
                                     metric = p3_metric_names,
                                     dist_method = 'euclidean'),
             map(p3_metric_names),
             deployment = 'worker'),
  tar_target(p3_FDC_clusters_quants,
             seasonal_metric_cluster(metric_mat = p1_FDC_metrics_season,
                                     metric = p3_metric_names_quants,
                                     dist_method = 'euclidean'),
             map(p3_metric_names_quants),
             deployment = 'worker'),
  tar_target(p3_FDC_clusters_quants_agg,
             seasonal_metric_cluster(metric_mat = p1_FDC_metrics_season,
                                     metric = p3_metric_names_quants_agg,
                                     dist_method = 'euclidean',
                                     quantile_agg = TRUE),
             map(p3_metric_names_quants_agg),
             deployment = 'worker'),
  
  #Select only the best clustering method
  tar_target(p3_FDC_best_cluster_method,
             select_cluster_method(clusts = p3_FDC_clusters),
             deployment = 'main'),
  tar_target(p3_FDC_best_cluster_method_quants,
             select_cluster_method(clusts = p3_FDC_clusters_quants),
             deployment = 'main'),
  tar_target(p3_FDC_best_cluster_method_quants_agg,
             select_cluster_method(clusts = p3_FDC_clusters_quants_agg, 
                                   quantile_agg = TRUE),
             deployment = 'main'),
  
  #Compute cluster diagnostics
  tar_target(p3_FDC_cluster_diagnostics,
             compute_cluster_diagnostics(clusts = p3_FDC_clusters,
                                         metric_mat = p1_FDC_metrics_season,
                                         kmin = 2, kmax = 20,
                                         alpha = 0.05, boot = 50,
                                         index = 'all', 
                                         dist_method = 'euclidean',
                                         clust_method = 'ward.D2'),
             map(p3_FDC_clusters),
             deployment = 'worker'),
  tar_target(p3_FDC_cluster_diagnostics_quants,
             compute_cluster_diagnostics(clusts = p3_FDC_clusters_quants,
                                         metric_mat = p1_FDC_metrics_season,
                                         kmin = 2, kmax = 20,
                                         alpha = 0.05, boot = 50,
                                         index = 'all', 
                                         dist_method = 'euclidean',
                                         clust_method = 'ward.D2'),
             map(p3_FDC_clusters_quants),
             deployment = 'worker'),
  tar_target(p3_FDC_cluster_diagnostics_quants_agg,
             compute_cluster_diagnostics(clusts = p3_FDC_clusters_quants_agg,
                                         metric_mat = p1_FDC_metrics_season,
                                         kmin = 2, kmax = 20,
                                         alpha = 0.05, boot = 50,
                                         index = 'all', 
                                         dist_method = 'euclidean',
                                         clust_method = 'ward.D2',
                                         quantile_agg = TRUE),
             map(p3_FDC_clusters_quants_agg),
             deployment = 'worker'),
  
  #Plot diagnostics for clusters
  tar_target(p3_FDC_cluster_diagnostics_png,
             plot_cluster_diagnostics(clusts = p3_FDC_clusters,
                                      metric_mat = p1_FDC_metrics_season,
                                      nbclust_metrics = p3_FDC_cluster_diagnostics,
                                      dist_method = 'euclidean',
                                      clust_method = 'ward.D2',
                                      dir_out = '3_cluster/out/seasonal_plots/diagnostics/'),
             map(p3_FDC_clusters, p3_FDC_cluster_diagnostics),
             deployment = 'worker',
             format = 'file'),
  tar_target(p3_FDC_cluster_diagnostics_quants_png,
             plot_cluster_diagnostics(clusts = p3_FDC_clusters_quants,
                                      metric_mat = p1_FDC_metrics_season,
                                      nbclust_metrics = p3_FDC_cluster_diagnostics_quants,
                                      dist_method = 'euclidean',
                                      clust_method = 'ward.D2',
                                      dir_out = '3_cluster/out/seasonal_plots/diagnostics/by_quantiles'),
             map(p3_FDC_clusters_quants, p3_FDC_cluster_diagnostics_quants),
             deployment = 'worker',
             format = 'file'),
  tar_target(p3_FDC_cluster_diagnostics_quants_agg_png,
             plot_cluster_diagnostics(clusts = p3_FDC_clusters_quants_agg,
                                      metric_mat = p1_FDC_metrics_season,
                                      nbclust_metrics = p3_FDC_cluster_diagnostics_quants_agg,
                                      dist_method = 'euclidean',
                                      clust_method = 'ward.D2',
                                      dir_out = '3_cluster/out/seasonal_plots/diagnostics/by_agg_quantiles',
                                      quantile_agg = TRUE),
             map(p3_FDC_clusters_quants_agg, p3_FDC_cluster_diagnostics_quants_agg),
             deployment = 'worker',
             format = 'file'),
  
  #Assign cluster numbers to gages
  tar_target(p3_gages_clusters,
             add_cluster_to_gages(gages = p1_sites_g2,
                                  clusts = p3_FDC_clusters,
                                  screened_sites = p1_screened_site_list_season,
                                  best_clust = p3_FDC_best_cluster_method,
                                  min_clusts = 3, max_clusts = 15, by_clusts = 4),
             deployment = 'main'),
  tar_target(p3_gages_clusters_quants,
             add_cluster_to_gages(gages = p1_sites_g2,
                                  clusts = p3_FDC_clusters_quants,
                                  screened_sites = p1_screened_site_list_season,
                                  best_clust = p3_FDC_best_cluster_method_quants,
                                  min_clusts = 3, max_clusts = 15, by_clusts = 4),
             deployment = 'main'),
  tar_target(p3_gages_clusters_quants_agg,
             add_cluster_to_gages(gages = p1_sites_g2,
                                  clusts = p3_FDC_clusters_quants_agg,
                                  screened_sites = p1_screened_site_list_season,
                                  best_clust = p3_FDC_best_cluster_method_quants_agg,
                                  min_clusts = 3, max_clusts = 15, by_clusts = 4,
                                  quantile_agg = TRUE),
             deployment = 'main'),
  tar_target(p3_gages_clusters_quants_agg_selected,
             add_cluster_to_gages(gages = p1_sites_g2,
                                  clusts = p3_FDC_clusters_quants_agg,
                                  screened_sites = p1_screened_site_list_season,
                                  best_clust = p3_FDC_best_cluster_method_quants_agg,
                                  min_clusts = 4, max_clusts = 6, by_clusts = 1,
                                  quantile_agg = TRUE),
             deployment = 'main'),
  
  #Assign cluster column names to a target for later branch iteration
  tar_target(p3_cluster_cols,
             colnames(p3_gages_clusters)[-1],
             deployment = 'main'),
  tar_target(p3_cluster_cols_quants,
             colnames(p3_gages_clusters_quants)[-1],
             deployment = 'main'),
  tar_target(p3_cluster_cols_quants_agg,
             colnames(p3_gages_clusters_quants_agg)[-1],
             deployment = 'main'),
  tar_target(p3_cluster_cols_quants_agg_selected,
             colnames(p3_gages_clusters_quants_agg_selected)[-1],
             deployment = 'main'),
  
  #Plot maps of gages with clusters
  tar_target(p3_cluster_map_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/'),
             deployment = 'main',
             format = 'file'),
  tar_target(p3_cluster_map_quants_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters_quants,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/by_quantiles'),
             deployment = 'main',
             format = 'file'),
  tar_target(p3_cluster_map_quants_agg_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters_quants_agg,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/by_agg_quantiles',
                              facet=FALSE),
             deployment = 'main',
             format = 'file'),
  tar_target(p3_cluster_map_quants_agg_facet_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters_quants_agg,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/by_agg_quantiles',
                              facet = TRUE),
             deployment = 'main',
             format = 'file'),
  tar_target(p3_cluster_map_quants_agg_selected_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters_quants_agg_selected,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/by_agg_quantiles',
                              facet=FALSE),
             deployment = 'main',
             format = 'file'),
  tar_target(p3_cluster_map_quants_agg_facet_selected_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters_quants_agg_selected,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/by_agg_quantiles',
                              facet = TRUE),
             deployment = 'main',
             format = 'file'),
  
  #barplot for all metrics, averaged over cluster gages
  tar_target(p3_seasonal_barplot_clusters_png,
             plot_seasonal_barplot(metric_mat = p1_FDC_metrics_season,
                                   metric = p3_metric_names,
                                   season_months = season_months,
                                   by_cluster = TRUE,
                                   panel_plot = TRUE,
                                   cluster_table = p3_gages_clusters,
                                   dir_out = '3_cluster/out/seasonal_plots/barplots/'),
             map(p3_metric_names),
             deployment = 'worker',
             format = 'file'),
  tar_target(p3_seasonal_barplot_clusters_quants_png,
             plot_seasonal_barplot(metric_mat = p1_FDC_metrics_season,
                                   metric = p3_metric_names_quants,
                                   season_months = season_months,
                                   by_cluster = TRUE,
                                   panel_plot = TRUE,
                                   cluster_table = p3_gages_clusters_quants,
                                   dir_out = '3_cluster/out/seasonal_plots/barplots/by_quantiles',
                                   by_quantile = TRUE),
             map(p3_metric_names_quants),
             deployment = 'worker',
             format = 'file'),
  tar_target(p3_seasonal_barplot_clusters_quants_agg_png,
             plot_seasonal_barplot(metric_mat = p1_FDC_metrics_season,
                                   metric = p3_metric_names_quants_agg,
                                   season_months = season_months,
                                   by_cluster = TRUE,
                                   panel_plot = TRUE,
                                   cluster_table = p3_gages_clusters_quants_agg,
                                   dir_out = '3_cluster/out/seasonal_plots/barplots/by_agg_quantiles',
                                   quantile_agg = TRUE,
                                   by_quantile = TRUE),
             map(p3_metric_names_quants_agg),
             deployment = 'worker',
             format = 'file'),
  tar_target(p3_seasonal_barplot_clusters_quants_agg_selected_png,
             plot_seasonal_barplot(metric_mat = p1_FDC_metrics_season,
                                   metric = p3_metric_names_quants_agg,
                                   season_months = season_months,
                                   by_cluster = TRUE,
                                   panel_plot = TRUE,
                                   cluster_table = p3_gages_clusters_quants_agg_selected,
                                   dir_out = '3_cluster/out/seasonal_plots/barplots/by_agg_quantiles',
                                   quantile_agg = TRUE,
                                   by_quantile = TRUE),
             map(p3_metric_names_quants_agg),
             deployment = 'worker',
             format = 'file'),

  
  ########moving window nonstationarity stuff
   ##table with all the FDC metrics computed on a moving window. The parameter min_yrs_in_window
  ##screens out any moving windows for which there are too few years to be reliable. Can be an issue 
  ##when there are large gaps in the data record because a 20 year window might only have a few years of 
  ##actual data.The yr_ct column indicates how many complete years were in the window just to keep track of it.

   tar_target(p1_moving_window_metrics,
              calc_moving_window_metrics(site_num = p1_screened_site_list,
                                         window_length = window_length,
                                         increment = increment,
                                         min_yrs_in_window = min_yrs_in_window,  
                                         clean_daily_flow = p1_clean_daily_flow,
                                         yearType = yearType,
                                         drainArea_tab = p1_drainage_area,
                                         NE_probs = NE_quants,
                                         digits = 3, seasonal = FALSE,
                                         year_start = year_start),
             map(p1_screened_site_list),
             deployment = 'worker'
  ),
  
  ##screen out any sites that don't have enough moving windows to plot (min_windows)
  ##using 10 for a default
  tar_target(p1_screened_plot_sites,
             screen_plot_sites(moving_window_metrics = p1_moving_window_metrics,
                               min_windows = min_windows),
             deployment = 'main'
  ),
  
  tar_target(p1_moving_window_plots_png,
             make_plots_by_site(site = p1_screened_plot_sites,
                                moving_window_metrics = p1_moving_window_metrics,
                                window_length = window_length,
                                outdir = "1_fetch/out/stationarity_plots"),
             map(p1_screened_plot_sites),
             deployment = 'worker',
             format = "file"
  ),
  
  #CONUS average
  tar_target(p1_moving_window_summary_plots_png,
             plot_trend_summary(moving_window_metrics = p1_moving_window_metrics,
                                screened_plot_sites = p1_screened_plot_sites,
                                by_cluster = FALSE,
                                outdir = "1_fetch/out/stationarity_plots"),
             deployment = 'worker',
             format = "file"
  ),
  
  #Cluster region average
  tar_target(p1_moving_window_summary_plots_cluster_png,
             plot_trend_summary(moving_window_metrics = p1_moving_window_metrics,
                                screened_plot_sites = p1_screened_plot_sites,
                                by_cluster = TRUE,
                                cluster_table = p3_gages_clusters,
                                cluster_column = p3_cluster_cols,
                                outdir = "1_fetch/out/stationarity_plots"),
             map(p3_cluster_cols),
             deployment = 'worker',
             format = "file"
  ),
  #This will not work because it doesn't know to split by quantile
  # tar_target(p1_moving_window_summary_plots_cluster_quants_png,
  #            plot_trend_summary(moving_window_metrics = p1_moving_window_metrics,
  #                               screened_plot_sites = p1_screened_plot_sites,
  #                               by_cluster = TRUE,
  #                               cluster_table = p3_gages_clusters_quants,
  #                               cluster_column = p3_cluster_cols_quants,
  #                               outdir = "1_fetch/out/stationarity_plots/by_quantiles"),
  #            map(p3_cluster_cols_quants),
  #            deployment = 'worker',
  #            format = "file"
  # ),
  
  #matrix of nested gages - proportion of overlapping area if column name gage is upstream of the row name gage, 0 otherwise
  tar_target(p4_nested_gages,
             get_nested_gages(gagesii = p1_sites_g2,
                              nav_distance_km = nav_distance_km),
             deployment = 'worker'
            )  
) #end list
