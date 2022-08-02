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
                            "sbtools", "maps", "mapproj", "ranger", "Boruta",
                            "tidymodels", "doParallel", "vip", "gstat"),
               imports = c("fasstr", "EflowStats", "dataRetrieval", 
                           "cluster","factoextra", "NbClust", "dendextend",
                           "tidyverse", "ranger", "Boruta", "tidymodels"))

##Create output file directories
dir.create('1_fetch/out', showWarnings = FALSE)
dir.create('1_fetch/out/stationarity_plots', showWarnings = FALSE)
dir.create('1_fetch/out/stationarity_plots/by_quantiles', showWarnings = FALSE)
dir.create('1_fetch/out/stationarity_plots/by_agg_quantiles', showWarnings = FALSE)
dir.create('1_fetch/out/logs', showWarnings = FALSE)
dir.create('1_fetch/out/sb', showWarnings = FALSE)
dir.create('1_fetch/out/sb/workdir', showWarnings = FALSE)
dir.create('1_fetch/out/sb/dldir', showWarnings = FALSE)
dir.create('1_fetch/out/sb/data', showWarnings = FALSE)
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
dir.create('5_EDA/out', showWarnings = FALSE)
dir.create('5_EDA/out/metrics_plots', showWarnings = FALSE)
dir.create('5_EDA/out/feature_plots', showWarnings = FALSE)
dir.create('6_predict/out', showWarnings = FALSE)
dir.create('6_predict/out/Boruta', showWarnings = FALSE)
dir.create('6_predict/out/vip', showWarnings = FALSE)
dir.create('6_predict/out/hypopt', showWarnings = FALSE)
dir.create('6_predict/out/split_boxplots', showWarnings = FALSE)
dir.create('6_predict/out/pred_obs', showWarnings = FALSE)


##Load user defined functions
source("1_fetch/src/get_nwis_data.R")
source("1_fetch/src/get_sb_data.R")
source("1_fetch/src/calc_HIT.R")
source("1_fetch/src/calc_FDC.R")
source("1_fetch/src/moving_window_functions.R")
source("3_cluster/src/seasonal_metric_cluster.R")
source("4_setup_crossval/src/cross_validation_functions.R")
source("5_EDA/src/EDA_metric_plots.R")
source("5_EDA/src/EDA_feature_plots.R")
source("5_EDA/src/select_features.R")
source("6_predict/src/train_models.R")
source("6_predict/src/plot_diagnostics.R")

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
###percentage of drainage area overlap between nested basins above which they will be grouped together
nested_threshold<-.5

# list of science base identifiers containing feature variables of interest
sb_var_ids_path <- "1_fetch/in/sb_var_ids.csv"

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
nav_distance_km <- 4500

#Random Forest Parameters
#maximum number of runs for Boruta feature screening algorithm
Boruta_runs <- 300
#number of trees
Boruta_trees <- 500
#number of cores
Boruta_cores <- 35
#Cross validation folds
cv_folds <- 5


##targets
list(
  #file target for gagesii (g2) sites 
  tar_target(p1_sites_g2_xlsx,
             gagesii_path,
             deployment = 'main',
             format = "file"
  ),
  #all gagesii (g2) sites 
  tar_target(p1_sites_g2,
             read_xlsx(p1_sites_g2_xlsx) %>% 
               mutate(ID = substr(ID, start=2, stop=nchar(ID))) %>%
               #drop 5 sites that are not representative (ditch, pipeline)
               filter(!(ID %in% drop_gages)),
             deployment = 'main'
  ),
  #create a spatial object 
  tar_target(p1_sites_g2_sf,
             st_as_sf(x = p1_sites_g2, coords = c('LON', 'LAT'), 
                      crs = st_crs(4326),remove = FALSE, dim = 'XY', na.fail = TRUE),
             deployment = 'main'
  ),
  
  #ID numbers for sites to use
  tar_target(
    p1_sites_list,
    p1_sites_g2$ID,
   deployment = 'main'
  ),
  
  ##check to make sure peak and daily flow are actually available for all sites
  tar_target(p1_has_data,
             has_data_check(p1_sites_list, NWIS_parameter, endDate),
             deployment = 'main'
  ),
  
  ##fetch daily streamflow
  #this is deployed on main to avoid overloading the NWIS server with download requests
  #note that the download occasionally randomly hangs even with the timeout.
  #you can stop and restart the pipeline when this happens.
  tar_target(p1_daily_flow_csv, 
             get_nwis_daily_data(p1_has_data, outdir="./1_fetch/out", 
                                 NWIS_parameter, startDate, endDate, 
                                 timeout = 60),
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
  # tar_target(p1_screen_daily_flow_season_high,
  #            screen_daily_data(p1_has_data, p1_prescreen_daily_data, season_year_start_high),
  #            map(p1_has_data),
  #            deployment = 'main'
  # ),
  
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
  # tar_target(p1_screened_site_list_season_high,
  #            filter_complete_years(p1_screen_daily_flow_season_high, complete_years),
  #            deployment = 'main'
  # ),
  
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
  # tar_target(p1_clean_daily_flow_season_high,
  #            clean_daily_data(p1_screened_site_list_season_high, p1_prescreen_daily_data, 
  #                             p1_screen_daily_flow_season_high, yearType, 
  #                             season_year_start_high),
  #            map(p1_screened_site_list_season_high),
  #            deployment = 'main'
  # ),
  
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
                                startDate, endDate, timeout = 60),
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
  
  #file target for sciencebase variable list csv
  tar_target(p1_sb_var_ids_csv,
             sb_var_ids_path,
             deployment = 'main',
             format = "file"
  ),
  #read in sciencebase variable list csv
  tar_target(p1_sb_var_ids,
             read_csv(file = p1_sb_var_ids_csv, show_col_types = FALSE),
             deployment = 'main'
  ),
  
  ##generate tables of feature variables from gagesii list  
  tar_target(p1_sb_data_g2_csv,
             get_sb_data(sites = p1_sites_g2, 
                         sb_var_ids = p1_sb_var_ids,
                         dldir = "./1_fetch/out/sb/dldir", 
                         workdir = "./1_fetch/out/sb/workdir",
                         outdir = "./1_fetch/out/sb/data",
                         out_file_name = "sb_data_g2_"),
             map(p1_sb_var_ids),
             iteration = "list",
             deployment = 'main',
             format = "file"
  ),
  
  ##generate log file to track updates to sb variables
  tar_target(p1_sb_data_g2_log,
             get_sb_data_log(sb_var_ids = p1_sb_var_ids,
                             file_out = "1_fetch/out/logs/sb_update_log.csv"),
             deployment = "main",
             format = "file"
  ),
  
  ##merge and select feature variables from gagesii list
  tar_target(p1_feature_vars_g2, 
             prep_feature_vars(sb_var_data = p1_sb_data_g2_csv, 
                               sites = p1_sites_g2, 
                               retain_vars = c("ID", "LAT", "LON",
                                 "npdes", "fwwd", "strg", "devl", "cndp")), 
             deployment = "main"
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
  
  ##combined metrics tables
  tar_target(p2_all_metrics,
             inner_join(p1_FDC_metrics,p1_HIT_metrics)
  ),
  #Metrics ml17 and 18 have NAs for some gages, so we will not predict them.
  #also dropping 0.98, 0.99, and 0.995 metrics
  tar_target(p2_all_metrics_predict,
             p2_all_metrics %>% 
               select(-ml17, -ml18, -contains('0.98'),
                      -contains('0.99'), -contains('0.995'))
  ),
  ##list of all the metrics names - for dynamic branching
  tar_target(p2_all_metrics_names,
             colnames(p2_all_metrics)[-1]
  ),
  tar_target(p2_all_metrics_names_predict,
             colnames(p2_all_metrics_predict)[-1]
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
  # tar_target(p1_FDC_metrics_season_high,
  #            calc_FDCmetrics(site_num = p1_screened_site_list_season_high, 
  #                            clean_daily_flow = p1_clean_daily_flow_season_high, 
  #                            yearType = yearType,
  #                            drainArea_tab = NULL,
  #                            NE_probs = NE_quants,
  #                            seasonal = TRUE,
  #                            season_months = season_months_high,
  #                            stat_type = 'POR',
  #                            year_start = season_year_start_high,
  #                            out_format = 'pivot'),
  #            map(p1_screened_site_list_season_high),
  #            deployment = 'worker'
  # ),
  
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
             format = 'file'
  ),

  #Compute clusters
  tar_target(p3_FDC_clusters,
             seasonal_metric_cluster(metric_mat = p1_FDC_metrics_season,
                                     metric = p3_metric_names,
                                     dist_method = 'euclidean'),
             map(p3_metric_names),
             deployment = 'worker'
  ),
  tar_target(p3_FDC_clusters_quants,
             seasonal_metric_cluster(metric_mat = p1_FDC_metrics_season,
                                     metric = p3_metric_names_quants,
                                     dist_method = 'euclidean'),
             map(p3_metric_names_quants),
             deployment = 'worker'
  ),
  tar_target(p3_FDC_clusters_quants_agg,
             seasonal_metric_cluster(metric_mat = p1_FDC_metrics_season,
                                     metric = p3_metric_names_quants_agg,
                                     dist_method = 'euclidean',
                                     quantile_agg = TRUE),
             map(p3_metric_names_quants_agg),
             deployment = 'worker'
  ),
  
  #Select only the best clustering method
  tar_target(p3_FDC_best_cluster_method,
             select_cluster_method(clusts = p3_FDC_clusters),
             deployment = 'main'
  ),
  tar_target(p3_FDC_best_cluster_method_quants,
             select_cluster_method(clusts = p3_FDC_clusters_quants),
             deployment = 'main'
  ),
  tar_target(p3_FDC_best_cluster_method_quants_agg,
             select_cluster_method(clusts = p3_FDC_clusters_quants_agg, 
                                   quantile_agg = TRUE),
             deployment = 'main'
  ),
  
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
             deployment = 'worker'
  ),
  tar_target(p3_FDC_cluster_diagnostics_quants,
             compute_cluster_diagnostics(clusts = p3_FDC_clusters_quants,
                                         metric_mat = p1_FDC_metrics_season,
                                         kmin = 2, kmax = 20,
                                         alpha = 0.05, boot = 50,
                                         index = 'all', 
                                         dist_method = 'euclidean',
                                         clust_method = 'ward.D2'),
             map(p3_FDC_clusters_quants),
             deployment = 'worker'
  ),
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
             deployment = 'worker'
  ),
  
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
             format = 'file'
  ),
  tar_target(p3_FDC_cluster_diagnostics_quants_png,
             plot_cluster_diagnostics(clusts = p3_FDC_clusters_quants,
                                      metric_mat = p1_FDC_metrics_season,
                                      nbclust_metrics = p3_FDC_cluster_diagnostics_quants,
                                      dist_method = 'euclidean',
                                      clust_method = 'ward.D2',
                                      dir_out = '3_cluster/out/seasonal_plots/diagnostics/by_quantiles'),
             map(p3_FDC_clusters_quants, p3_FDC_cluster_diagnostics_quants),
             deployment = 'worker',
             format = 'file'
  ),
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
             format = 'file'
  ),
  
  #Assign cluster numbers to gages
  tar_target(p3_gages_clusters,
             add_cluster_to_gages(gages = p1_sites_g2,
                                  clusts = p3_FDC_clusters,
                                  screened_sites = p1_screened_site_list_season,
                                  best_clust = p3_FDC_best_cluster_method,
                                  min_clusts = 3, max_clusts = 15, by_clusts = 4),
             deployment = 'main'
  ),
  tar_target(p3_gages_clusters_quants,
             add_cluster_to_gages(gages = p1_sites_g2,
                                  clusts = p3_FDC_clusters_quants,
                                  screened_sites = p1_screened_site_list_season,
                                  best_clust = p3_FDC_best_cluster_method_quants,
                                  min_clusts = 3, max_clusts = 15, by_clusts = 4),
             deployment = 'main'
  ),
  tar_target(p3_gages_clusters_quants_agg,
             add_cluster_to_gages(gages = p1_sites_g2,
                                  clusts = p3_FDC_clusters_quants_agg,
                                  screened_sites = p1_screened_site_list_season,
                                  best_clust = p3_FDC_best_cluster_method_quants_agg,
                                  min_clusts = 3, max_clusts = 15, by_clusts = 4,
                                  quantile_agg = TRUE),
             deployment = 'main'
  ),
  tar_target(p3_gages_clusters_quants_agg_selected,
             add_cluster_to_gages(gages = p1_sites_g2,
                                  clusts = p3_FDC_clusters_quants_agg,
                                  screened_sites = p1_screened_site_list_season,
                                  best_clust = p3_FDC_best_cluster_method_quants_agg,
                                  min_clusts = 4, max_clusts = 6, by_clusts = 1,
                                  quantile_agg = TRUE),
             deployment = 'main'
  ),
  
  #Assign cluster column names to a target for later branch iteration
  tar_target(p3_cluster_cols,
             colnames(p3_gages_clusters)[-1],
             deployment = 'main'
  ),
  tar_target(p3_cluster_cols_quants,
             colnames(p3_gages_clusters_quants)[-1],
             deployment = 'main'
  ),
  tar_target(p3_cluster_cols_quants_agg,
             colnames(p3_gages_clusters_quants_agg)[-1],
             deployment = 'main'
  ),
  tar_target(p3_cluster_cols_quants_agg_selected,
             colnames(p3_gages_clusters_quants_agg_selected)[-1],
             deployment = 'main'
  ),
  
  #Plot maps of gages with clusters
  tar_target(p3_cluster_map_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p3_cluster_map_quants_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters_quants,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/by_quantiles'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p3_cluster_map_quants_agg_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters_quants_agg,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/by_agg_quantiles',
                              facet=FALSE),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p3_cluster_map_quants_agg_facet_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters_quants_agg,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/by_agg_quantiles',
                              facet = TRUE),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p3_cluster_map_quants_agg_selected_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters_quants_agg_selected,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/by_agg_quantiles',
                              facet=FALSE),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p3_cluster_map_quants_agg_facet_selected_png,
             plot_cluster_map(gages = p1_sites_g2_sf,
                              cluster_table = p3_gages_clusters_quants_agg_selected,
                              screened_sites = p1_screened_site_list_season,
                              dir_out = '3_cluster/out/seasonal_plots/maps/by_agg_quantiles',
                              facet = TRUE),
             deployment = 'main',
             format = 'file'
  ),
  
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
             format = 'file'
  ),
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
             format = 'file'
  ),
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
             format = 'file'
  ),
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
             format = 'file'
  ),

  
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
  
  #matrix of nested gages - proportion of overlapping area. Column name gage is downstream of the row name gage.
  tar_target(p4_nested_gages,
             get_nested_gages(gagesii = p1_sites_g2,
                              nav_distance_km = nav_distance_km,
                              screened_site_list = p1_screened_site_list),
             deployment = 'worker'
  ),
  
  tar_target(p4_nested_groups,
              add_nested_group_id(p4_nested_gages, p1_drainage_area, nested_threshold)),
  ###EDA plots
  ##maps and violin plots of all metrics by cluster.  k is the number of clusters to use in 
  ##the cluster table
  tar_target(p5_EDA_plots_metrics,
             make_EDA_metric_plots(metric = p2_all_metrics_names,
                                   k = 5,
                                  cluster_table = p3_gages_clusters_quants_agg_selected,
                                  metrics_table = p2_all_metrics,
                                  gages = p1_sites_g2_sf,
                                  out_dir = "5_EDA/out/metrics_plots"
                                   ),
             map(p2_all_metrics_names),
             format="file"
  ),
  
  #Maps and violin plots of feature variables
  tar_target(p5_EDA_plots_features, 
             make_EDA_feature_plots(feature_vars = p1_feature_vars_g2, 
                                    out_dir = "5_EDA/out/feature_plots"), 
             format = "file"
  ),
  
  #Down select features from full database
  tar_target(p5_screen_attr_g2,
             refine_features(nhdv2_attr = p1_feature_vars_g2, 
                             drop_columns = c('NO10AVE', 'NO200AVE', 'NO4AVE',
                                              'LAT', 'LON',
                                              # using ACC because CAT is highly correlated
                                              'CAT_PHYSIO',
                                              #CAT soils have NAs. Using TOT instead
                                              "CAT_HGA", "CAT_HGAC", "CAT_HGAD", "CAT_HGB", 
                                              "CAT_HGBC", "CAT_HGBD", "CAT_HGC", "CAT_HGCD",
                                              "CAT_HGD", "CAT_HGVAR", 
                                              #Duplicate with RF7100
                                              "RFACT",
                                              #Keeping shallow and deep soil info. Dropping middle 2.
                                              "SRL35AG", "SRL45AG",
                                              #Min elevation nearly identical for ACC and CAT
                                              "CAT_ELEV_MIN",
                                              #Canal ditch cndp better than ACC_CANALDITCH (no 0s)
                                              "TOT_CANALDITCH", "ACC_CANALDITCH",
                                              #storage available everywhere with NID and NORM STORAGE
                                              "strg",
                                              #CAT storage almost same for NID and NORM
                                              "CAT_NORM_STORAGE",
                                              #ACC and TOT correlations strange:
                                              'TOT_STRM_DENS', 
                                              #CAT hydrologic attributes very correlated with ACC
                                              'CAT_CWD', 'CAT_BFI', 'CAT_RF7100', 'CAT_SATOF', 
                                              'CAT_RH', 'CAT_WDANN', 'CAT_ET', 'CAT_PET', 'CAT_MINP6190', 
                                              'CAT_MAXP6190', 'CAT_FSTFZ6190', 'CAT_LSTFZ6190', 
                                              #5 odd TOT waterbody variables - using ACC instead
                                              'TOT_PLAYA', 'TOT_ICEMASS', 'TOT_LAKEPOND', 
                                              'TOT_RESERVOIR', 'TOT_SWAMPMARSH')),
             deployment = 'main'
  ),
  # remove TOT variables that are highly correlated with other variables (> 0.9)
  tar_target(p5_attr_g2,
             drop_high_corr_ACCTOT(features = p5_screen_attr_g2, 
                                   threshold_corr = 0.9,
                                   drop_var = 'TOT'),
             deployment = 'main'
  ),
  
  #########
  #Predict
  #Note - may be best to select features again using the local region databases
  #values can have different correlations in a region
  # Rain dominated region
  #Boruta screening
  # Not using the target map argument for now so that we get only the vhfdc1_q0.9 metric
  tar_target(p6_Boruta_rain,
             screen_Boruta(features = p5_attr_g2,
                           cluster_table = p3_gages_clusters_quants_agg_selected %>%
                             select(ID, contains('_k5')) %>%
                             rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                    high = '0.75,0.8,0.85,0.9,0.95_k5'),
                           metrics_table = p2_all_metrics_predict,
                           metric_name = 'vhfdc1_q0.9',
                           train_region = 'rain',
                           ncores = Boruta_cores, 
                           brf_runs = Boruta_runs, 
                           ntrees = Boruta_trees,
                           train_prop = 0.8,
                           nested_groups = p4_nested_groups
             ),
             #map(p2_all_metrics_names_predict),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_rain,
             train_models_grid(brf_output = p6_Boruta_rain,
                               ncores = Boruta_cores,
                               v_folds = cv_folds, 
                               nested_groups = p4_nested_groups
             ),
             #map(p6_Boruta_rain),
             deployment = 'worker'
  ),
  #Test rain-dominated flood model in full rain-dominated region
  tar_target(p6_test_RF_rain_rain,
             predict_test_data(model_wf = p6_train_RF_rain$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  #Test rain-dominated flood model in snow-dominated region
  tar_target(p6_test_RF_rain_snow,
            predict_test_data(model_wf = p6_train_RF_rain$workflow,
                              features = p5_attr_g2,
                              cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                select(ID, contains('_k5')) %>%
                                rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                       high = '0.75,0.8,0.85,0.9,0.95_k5'),
                              metrics_table = p2_all_metrics_predict,
                              metric_name = 'vhfdc1_q0.9',
                              test_region = 'snow'),
            deployment = 'main'
  ),
  # Test on rain test region
  tar_target(p6_test_RF_rain_test_rain,
             predict_test_data_from_data(model_wf = p6_train_RF_rain$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'rain',
                                         test_gages = p6_Boruta_rain$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Test on snow test region
  tar_target(p6_test_RF_rain_test_snow,
             predict_test_data_from_data(model_wf = p6_train_RF_rain$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'snow',
                                         test_gages = p6_Boruta_snow$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Snow dominated region
  # Boruta screening
  tar_target(p6_Boruta_snow,
             screen_Boruta(features = p5_attr_g2,
                          cluster_table = p3_gages_clusters_quants_agg_selected %>%
                            select(ID, contains('_k5')) %>%
                            rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                   high = '0.75,0.8,0.85,0.9,0.95_k5'),
                          metrics_table = p2_all_metrics_predict,
                          metric_name = 'vhfdc1_q0.9',
                          train_region = 'snow',
                          ncores = Boruta_cores,
                          brf_runs = Boruta_runs,
                          ntrees = Boruta_trees,
                          train_prop = 0.8, 
                          nested_groups = p4_nested_groups
            ),
            #map(p2_all_metrics_names_predict),
            deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_snow,
            train_models_grid(brf_output = p6_Boruta_snow,
                              ncores = Boruta_cores,
                              v_folds = cv_folds,
                              nested_groups = p4_nested_groups
            ),
            #map(p6_Boruta_snow),
            deployment = 'worker'
  ),
  # Test on rain
  tar_target(p6_test_RF_snow_rain,
             predict_test_data(model_wf = p6_train_RF_snow$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on full snow region
  tar_target(p6_test_RF_snow_snow,
             predict_test_data(model_wf = p6_train_RF_snow$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Test on rain test region
  tar_target(p6_test_RF_snow_test_rain,
             predict_test_data_from_data(model_wf = p6_train_RF_snow$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'rain',
                                         test_gages = p6_Boruta_rain$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Test on snow test region
  tar_target(p6_test_RF_snow_test_snow,
             predict_test_data_from_data(model_wf = p6_train_RF_snow$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'snow',
                                         test_gages = p6_Boruta_snow$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # All data in both regions
  # Boruta screening
  tar_target(p6_Boruta_rain_snow,
             screen_Boruta(features = p5_attr_g2,
                          cluster_table = p3_gages_clusters_quants_agg_selected %>%
                            select(ID, contains('_k5')) %>%
                            rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                   high = '0.75,0.8,0.85,0.9,0.95_k5'),
                          metrics_table = p2_all_metrics_predict,
                          metric_name = 'vhfdc1_q0.9',
                          train_region = c('rain', 'snow'),
                          ncores = Boruta_cores,
                          brf_runs = Boruta_runs,
                          ntrees = Boruta_trees,
                          train_prop = 0.8,
                          nested_groups = p4_nested_groups
            ),
            #map(p2_all_metrics_names_predict),
            deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_rain_snow,
            train_models_grid(brf_output = p6_Boruta_rain_snow,
                              ncores = Boruta_cores,
                              v_folds = cv_folds,
                              nested_groups = p4_nested_groups
            ),
            #map(p6_Boruta_rain_snow),
            deployment = 'worker'
  ),
  # Test on rain
  tar_target(p6_test_RF_rain_snow_rain,
             predict_test_data(model_wf = p6_train_RF_rain_snow$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on snow
  tar_target(p6_test_RF_rain_snow_snow,
             predict_test_data(model_wf = p6_train_RF_rain_snow$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Exact same testing data as rain and snow regions above
  # Boruta screening
  tar_target(p6_Boruta_rain_snow_exact,
             screen_Boruta_exact(features = p5_attr_g2,
                           cluster_table = p3_gages_clusters_quants_agg_selected %>%
                             select(ID, contains('_k5')) %>%
                             rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                    high = '0.75,0.8,0.85,0.9,0.95_k5'),
                           metrics_table = p2_all_metrics_predict,
                           metric_name = 'vhfdc1_q0.9',
                           train_region = c('rain', 'snow'),
                           ncores = Boruta_cores,
                           brf_runs = Boruta_runs,
                           ntrees = Boruta_trees,
                           train_prop = 0.8,
                           exact_test_data = c(p6_Boruta_rain$input_data$testing$GAGES_ID,
                                               p6_Boruta_snow$input_data$testing$GAGES_ID),
                           nested_groups = p4_nested_groups
             ),
             #map(p2_all_metrics_names_predict),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_rain_snow_exact,
             train_models_grid(brf_output = p6_Boruta_rain_snow_exact,
                               ncores = Boruta_cores,
                               v_folds = cv_folds,
                               nested_groups = p4_nested_groups),
             #map(p6_Boruta_rain_snow_exact),
             deployment = 'worker'
  ),
  # Test on full rain region
  tar_target(p6_test_RF_rain_snow_exact_rain,
             predict_test_data(model_wf = p6_train_RF_rain_snow_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on full snow region
  tar_target(p6_test_RF_rain_snow_exact_snow,
             predict_test_data(model_wf = p6_train_RF_rain_snow_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Test on rain test region
  tar_target(p6_test_RF_rain_snow_exact_test_rain,
             predict_test_data_from_data(model_wf = p6_train_RF_rain_snow_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain',
                               test_gages = p6_Boruta_rain$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Test on snow test region
  tar_target(p6_test_RF_rain_snow_exact_test_snow,
             predict_test_data_from_data(model_wf = p6_train_RF_rain_snow_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow',
                               test_gages = p6_Boruta_snow$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # All data CONUS OOB error
  # Boruta screening
  tar_target(p6_Boruta_CONUS_g2,
             screen_Boruta(features = p5_attr_g2,
                          cluster_table = p3_gages_clusters_quants_agg_selected %>%
                            select(ID, contains('_k5')) %>%
                            rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                   high = '0.75,0.8,0.85,0.9,0.95_k5'),
                          metrics_table = p2_all_metrics_predict,
                          metric_name = 'vhfdc1_q0.9',
                          train_region = 'all',
                          ncores = Boruta_cores,
                          brf_runs = Boruta_runs,
                          ntrees = Boruta_trees,
                          train_prop = 0.8,
                          nested_groups = p4_nested_groups
            ),
            #map(p2_all_metrics_names_predict),
            deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_CONUS_g2,
            train_models_grid(brf_output = p6_Boruta_CONUS_g2,
                              ncores = Boruta_cores,
                              v_folds = cv_folds,
                              nested_groups = p4_nested_groups
            ),
            #map(p6_Boruta_CONUS_g2),
            deployment = 'worker'
  ),
  # Test on rain
  tar_target(p6_test_RF_CONUS_g2_rain,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on snow
  tar_target(p6_test_RF_CONUS_g2_snow,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Exact same testing data as rain and snow regions above
  # Boruta screening
  tar_target(p6_Boruta_CONUS_g2_exact,
             screen_Boruta_exact(features = p5_attr_g2,
                                 cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                   select(ID, contains('_k5')) %>%
                                   rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                          high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                 metrics_table = p2_all_metrics_predict,
                                 metric_name = 'vhfdc1_q0.9',
                                 train_region = c('all'),
                                 ncores = Boruta_cores,
                                 brf_runs = Boruta_runs,
                                 ntrees = Boruta_trees,
                                 train_prop = 0.8,
                                 exact_test_data = c(p6_Boruta_rain$input_data$testing$GAGES_ID,
                                                     p6_Boruta_snow$input_data$testing$GAGES_ID),
                                 nested_groups = p4_nested_groups
             ),
             #map(p2_all_metrics_names_predict),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_CONUS_g2_exact,
             train_models_grid(brf_output = p6_Boruta_CONUS_g2_exact,
                               ncores = Boruta_cores,
                               v_folds = cv_folds,
                               nested_groups = p4_nested_groups),
             #map(p6_Boruta_CONUS_g2_exact),
             deployment = 'worker'
  ),
  # Test on full rain region
  tar_target(p6_test_RF_CONUS_g2_exact_rain,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on full snow region
  tar_target(p6_test_RF_CONUS_g2_exact_snow,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Test on rain test region
  tar_target(p6_test_RF_CONUS_g2_exact_test_rain,
             predict_test_data_from_data(model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain',
                               test_gages = p6_Boruta_rain$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Test on snow test region
  tar_target(p6_test_RF_CONUS_g2_exact_test_snow,
             predict_test_data_from_data(model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow',
                               test_gages = p6_Boruta_snow$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # With cluster regions as input feature
  # Boruta screening
  tar_target(p6_Boruta_CONUS_g2_exact_clust,
             screen_Boruta_exact(features = left_join(p5_attr_g2, p3_gages_clusters_quants_agg_selected %>% 
                                                        select(ID, '0.75,0.8,0.85,0.9,0.95_k5') %>%
                                                        rename(clusters = '0.75,0.8,0.85,0.9,0.95_k5'), 
                                                      by = c('GAGES_ID' = 'ID')) %>% 
                                   na.omit(),
                                 cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                   select(ID, contains('_k5')) %>%
                                   rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                          high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                 metrics_table = p2_all_metrics_predict,
                                 metric_name = 'vhfdc1_q0.9',
                                 train_region = c('all'),
                                 ncores = Boruta_cores,
                                 brf_runs = Boruta_runs,
                                 ntrees = Boruta_trees,
                                 train_prop = 0.8,
                                 exact_test_data = c(p6_Boruta_rain$input_data$testing$GAGES_ID,
                                                     p6_Boruta_snow$input_data$testing$GAGES_ID),
                                 nested_groups = p4_nested_groups
             ),
             #map(p2_all_metrics_names_predict),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_CONUS_g2_exact_clust,
             train_models_grid(brf_output = p6_Boruta_CONUS_g2_exact_clust,
                               ncores = Boruta_cores,
                               v_folds = cv_folds,
                               nested_groups = p4_nested_groups),
             #map(p6_Boruta_CONUS_g2_exact_clust),
             deployment = 'worker'
  ),
  # Test on full rain region
  tar_target(p6_test_RF_CONUS_g2_exact_clust_rain,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                               features = left_join(p5_attr_g2, p3_gages_clusters_quants_agg_selected %>% 
                                                      select(ID, '0.75,0.8,0.85,0.9,0.95_k5') %>%
                                                      rename(clusters = '0.75,0.8,0.85,0.9,0.95_k5'), 
                                                    by = c('GAGES_ID' = 'ID')) %>% 
                                 na.omit(),
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on full snow region
  tar_target(p6_test_RF_CONUS_g2_exact_clust_snow,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                               features = left_join(p5_attr_g2, p3_gages_clusters_quants_agg_selected %>% 
                                                      select(ID, '0.75,0.8,0.85,0.9,0.95_k5') %>%
                                                      rename(clusters = '0.75,0.8,0.85,0.9,0.95_k5'), 
                                                    by = c('GAGES_ID' = 'ID')) %>% 
                                 na.omit(),
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Test on rain test region
  tar_target(p6_test_RF_CONUS_g2_exact_clust_test_rain,
             predict_test_data_from_data(model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                                         features = left_join(p5_attr_g2, p3_gages_clusters_quants_agg_selected %>% 
                                                                select(ID, '0.75,0.8,0.85,0.9,0.95_k5') %>%
                                                                rename(clusters = '0.75,0.8,0.85,0.9,0.95_k5'), 
                                                              by = c('GAGES_ID' = 'ID')) %>% 
                                           na.omit(),
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'rain',
                                         test_gages = p6_Boruta_rain$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Test on snow test region
  tar_target(p6_test_RF_CONUS_g2_exact_clust_test_snow,
             predict_test_data_from_data(model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                                         features = left_join(p5_attr_g2, p3_gages_clusters_quants_agg_selected %>% 
                                                                select(ID, '0.75,0.8,0.85,0.9,0.95_k5') %>%
                                                                rename(clusters = '0.75,0.8,0.85,0.9,0.95_k5'), 
                                                              by = c('GAGES_ID' = 'ID')) %>% 
                                           na.omit(),
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'snow',
                                         test_gages = p6_Boruta_snow$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  
  
  # Visualize Model Diagnostics:
  
  # Boruta screening
  tar_target(p6_Boruta_rain_png,
             plot_Boruta(p6_Boruta_rain$brf_All,
                         metric = p6_Boruta_rain$metric,
                         region = 'rain',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_snow_png,
             plot_Boruta(p6_Boruta_snow$brf_All,
                         metric = p6_Boruta_snow$metric,
                         region = 'snow',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_rain_snow_png,
             plot_Boruta(p6_Boruta_rain_snow$brf_All,
                         metric = p6_Boruta_rain_snow$metric,
                         region = 'rain_snow',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_rain_snow_exact_png,
             plot_Boruta(p6_Boruta_rain_snow_exact$brf_All,
                         metric = p6_Boruta_rain_snow_exact$metric,
                         region = 'rain_snow_exact',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_CONUS_g2_png,
             plot_Boruta(p6_Boruta_CONUS_g2$brf_All,
                         metric = p6_Boruta_CONUS_g2$metric,
                         region = 'CONUS_g2',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_CONUS_g2_exact_png,
             plot_Boruta(p6_Boruta_CONUS_g2_exact$brf_All,
                         metric = p6_Boruta_CONUS_g2_exact$metric,
                         region = 'CONUS_g2_exact',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_CONUS_g2_exact_clust_png,
             plot_Boruta(p6_Boruta_CONUS_g2_exact_clust$brf_All,
                         metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                         region = 'CONUS_g2_exact_clust',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF variable importance plot 
  #Should add error bars over X random seeds
  tar_target(p6_vip_rain_png,
             plot_vip(RF_model = p6_train_RF_rain$best_fit,
                      metric = p6_Boruta_rain$metric,
                      region = 'rain',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_snow_png,
             plot_vip(RF_model = p6_train_RF_snow$best_fit,
                      metric = p6_Boruta_snow$metric,
                      region = 'snow',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_rain_snow_png,
             plot_vip(RF_model = p6_train_RF_rain_snow$best_fit,
                      metric = p6_Boruta_rain_snow$metric,
                      region = 'rain_snow',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_rain_snow_exact_png,
             plot_vip(RF_model = p6_train_RF_rain_snow_exact$best_fit,
                      metric = p6_Boruta_rain_snow_exact$metric,
                      region = 'rain_snow_exact',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_CONUS_g2_png,
             plot_vip(RF_model = p6_train_RF_CONUS_g2$best_fit,
                      metric = p6_Boruta_CONUS_g2$metric,
                      region = 'CONUS_g2',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_CONUS_g2_exact_png,
             plot_vip(RF_model = p6_train_RF_CONUS_g2_exact$best_fit,
                      metric = p6_Boruta_CONUS_g2_exact$metric,
                      region = 'CONUS_g2_exact',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_CONUS_g2_exact_clust_png,
             plot_vip(RF_model = p6_train_RF_CONUS_g2_exact_clust$best_fit,
                      metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                      region = 'CONUS_g2_exact_clust',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF hyperparameter optimization
  tar_target(p6_hypopt_rain_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_rain$grid_params,
                      metric = p6_Boruta_rain$metric,
                      region = 'rain',
                      out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_snow_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_snow$grid_params,
                      metric = p6_Boruta_snow$metric,
                      region = 'snow',
                      out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_rain_snow_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_rain_snow$grid_params,
                      metric = p6_Boruta_rain_snow$metric,
                      region = 'rain_snow',
                      out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_rain_snow_exact_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_rain_snow_exact$grid_params,
                                            metric = p6_Boruta_rain_snow_exact$metric,
                                            region = 'rain_snow_exact',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_CONUS_g2_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_CONUS_g2$grid_params,
                      metric = p6_Boruta_CONUS_g2$metric,
                      region = 'CONUS_g2',
                      out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_CONUS_g2_exact_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_CONUS_g2_exact$grid_params,
                                            metric = p6_Boruta_CONUS_g2_exact$metric,
                                            region = 'CONUS_g2_exact',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_CONUS_g2_exact_clust_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_CONUS_g2_exact_clust$grid_params,
                                            metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                                            region = 'CONUS_g2_exact_clust',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  
  # Train test split boxplots for metric
  tar_target(p6_split_boxplot_rain_png,
             plot_metric_boxplot(data_split = p6_Boruta_rain$input_data,
                                 metric = p6_Boruta_rain$metric,
                                 region = 'rain',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_snow_png,
             plot_metric_boxplot(data_split = p6_Boruta_snow$input_data,
                                 metric = p6_Boruta_snow$metric,
                                 region = 'snow',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_rain_snow_png,
             plot_metric_boxplot(data_split = p6_Boruta_rain_snow$input_data,
                                 metric = p6_Boruta_rain_snow$metric,
                                 region = 'rain_snow',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_rain_snow_exact_png,
             plot_metric_boxplot(data_split = p6_Boruta_rain_snow_exact$input_data,
                                 metric = p6_Boruta_rain_snow_exact$metric,
                                 region = 'rain_snow_exact',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_CONUS_g2_png,
             plot_metric_boxplot(data_split = p6_Boruta_CONUS_g2$input_data,
                                 metric = p6_Boruta_CONUS_g2$metric,
                                 region = 'CONUS_g2',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_CONUS_g2_exact_png,
             plot_metric_boxplot(data_split = p6_Boruta_CONUS_g2_exact$input_data,
                                 metric = p6_Boruta_CONUS_g2_exact$metric,
                                 region = 'CONUS_g2_exact',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_CONUS_g2_exact_clust_png,
             plot_metric_boxplot(data_split = p6_Boruta_CONUS_g2_exact_clust$input_data,
                                 metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                                 region = 'CONUS_g2_exact_clust',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF prediction vs. observed
  tar_target(p6_pred_obs_rain_png,
             plot_pred_obs(df_pred_obs = p6_test_RF_rain_rain$pred,
                           metric = p6_Boruta_rain$metric,
                           region = 'rain',
                           out_dir = '6_predict/out/pred_obs'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_snow_png,
             plot_pred_obs(df_pred_obs = p6_test_RF_snow_snow$pred,
                           metric = p6_Boruta_snow$metric,
                           region = 'snow',
                           out_dir = '6_predict/out/pred_obs'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_rain_snow_png,
             plot_pred_obs(df_pred_obs = NULL,
                           metric = p6_Boruta_rain_snow$metric,
                           region = 'rain_snow',
                           out_dir = '6_predict/out/pred_obs', 
                           from_predict = TRUE, 
                           model_wf = p6_train_RF_rain_snow$workflow,
                           pred_data = p6_Boruta_rain_snow$input_data$split$data),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_rain_snow_exact_png,
             plot_pred_obs(df_pred_obs = NULL,
                           metric = p6_Boruta_rain_snow_exact$metric,
                           region = 'rain_snow_exact',
                           out_dir = '6_predict/out/pred_obs', 
                           from_predict = TRUE, 
                           model_wf = p6_train_RF_rain_snow_exact$workflow,
                           pred_data = p6_Boruta_rain_snow_exact$input_data$split$data),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_CONUS_g2_png,
             plot_pred_obs(df_pred_obs = NULL,
                           metric = p6_Boruta_CONUS_g2$metric,
                           region = 'CONUS_g2',
                           out_dir = '6_predict/out/pred_obs', 
                           from_predict = TRUE, 
                           model_wf = p6_train_RF_CONUS_g2$workflow,
                           pred_data = p6_Boruta_CONUS_g2$input_data$split$data),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_CONUS_g2_exact_png,
             plot_pred_obs(df_pred_obs = NULL,
                           metric = p6_Boruta_CONUS_g2_exact$metric,
                           region = 'CONUS_g2_exact',
                           out_dir = '6_predict/out/pred_obs', 
                           from_predict = TRUE, 
                           model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                           pred_data = p6_Boruta_CONUS_g2_exact$input_data$split$data),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_CONUS_g2_exact_clust_png,
             plot_pred_obs(df_pred_obs = NULL,
                           metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                           region = 'CONUS_g2_exact_clust',
                           out_dir = '6_predict/out/pred_obs', 
                           from_predict = TRUE, 
                           model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                           pred_data = p6_Boruta_CONUS_g2_exact_clust$input_data$split$data),
             deployment = 'main',
             format = 'file'
  ),
   
  # RF residual vs. y 
  #Should be for the mean over X random seeds
  #May want to do some oversampling in extreme tails
  # plot(x = input_data %>% 
  #        filter(region == 'rain') %>% 
  #        pull({{metric_name}}), y = abs(rf$predictions - input_data %>% 
  #                                         filter(region == 'rain') %>% 
  #                                         pull({{metric_name}})))
  
  # Spatial residuals
  #Should be for the mean over X random seeds
  
  
  # Model RMSE comparison boxplots / barplots
  tar_target(p6_compare_RMSE_RF_png,
             barplot_compare_RF(rain_mod = p6_train_RF_rain, 
                                snow_mod = p6_train_RF_snow, 
                                rain_snow_mod = p6_train_RF_rain_snow, 
                                CONUS_mod = p6_train_RF_CONUS_g2,
                                test_rain_rain = p6_test_RF_rain_rain, 
                                test_rain_snow = p6_test_RF_rain_snow,
                                test_snow_rain = p6_test_RF_snow_rain, 
                                test_snow_snow = p6_test_RF_snow_snow, 
                                test_rain_snow_rain = p6_test_RF_rain_snow_rain, 
                                test_rain_snow_snow = p6_test_RF_rain_snow_snow, 
                                test_CONUS_rain = p6_test_RF_CONUS_g2_rain, 
                                test_CONUS_snow = p6_test_RF_CONUS_g2_snow,
                                flow_metric = 'vhfdc1_q0.9', 
                                perf_metric = 'rmse', 
                                out_dir = '6_predict/out'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_compare_RMSE_RF_exact_full_png,
             barplot_compare_RF(rain_mod = p6_train_RF_rain, 
                                snow_mod = p6_train_RF_snow, 
                                rain_snow_mod = p6_train_RF_rain_snow_exact, 
                                CONUS_mod = p6_train_RF_CONUS_g2_exact,
                                test_rain_rain = p6_test_RF_rain_rain, 
                                test_rain_snow = p6_test_RF_rain_snow,
                                test_snow_rain = p6_test_RF_snow_rain, 
                                test_snow_snow = p6_test_RF_snow_snow, 
                                test_rain_snow_rain = p6_test_RF_rain_snow_exact_rain, 
                                test_rain_snow_snow = p6_test_RF_rain_snow_exact_snow, 
                                test_CONUS_rain = p6_test_RF_CONUS_g2_exact_rain, 
                                test_CONUS_snow = p6_test_RF_CONUS_g2_exact_snow,
                                flow_metric = 'vhfdc1_q0.9_exact_full', 
                                perf_metric = 'rmse', 
                                out_dir = '6_predict/out'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_compare_RMSE_RF_exact_test_png,
             barplot_compare_RF(rain_mod = p6_train_RF_rain, 
                                snow_mod = p6_train_RF_snow, 
                                rain_snow_mod = p6_train_RF_rain_snow_exact, 
                                CONUS_mod = p6_train_RF_CONUS_g2_exact,
                                test_rain_rain = p6_test_RF_rain_test_rain, 
                                test_rain_snow = p6_test_RF_rain_test_snow,
                                test_snow_rain = p6_test_RF_snow_test_rain, 
                                test_snow_snow = p6_test_RF_snow_test_snow, 
                                test_rain_snow_rain = p6_test_RF_rain_snow_exact_test_rain, 
                                test_rain_snow_snow = p6_test_RF_rain_snow_exact_test_snow, 
                                test_CONUS_rain = p6_test_RF_CONUS_g2_exact_test_rain, 
                                test_CONUS_snow = p6_test_RF_CONUS_g2_exact_test_snow,
                                flow_metric = 'vhfdc1_q0.9_exact_test', 
                                perf_metric = 'rmse', 
                                out_dir = '6_predict/out'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_compare_RMSE_RF_exact_clust_test_png,
             barplot_compare_RF(rain_mod = p6_train_RF_rain, 
                                snow_mod = p6_train_RF_snow, 
                                rain_snow_mod = p6_train_RF_rain_snow_exact, 
                                CONUS_mod = p6_train_RF_CONUS_g2_exact_clust,
                                test_rain_rain = p6_test_RF_rain_test_rain, 
                                test_rain_snow = p6_test_RF_rain_test_snow,
                                test_snow_rain = p6_test_RF_snow_test_rain, 
                                test_snow_snow = p6_test_RF_snow_test_snow, 
                                test_rain_snow_rain = p6_test_RF_rain_snow_exact_test_rain, 
                                test_rain_snow_snow = p6_test_RF_rain_snow_exact_test_snow, 
                                test_CONUS_rain = p6_test_RF_CONUS_g2_exact_clust_test_rain, 
                                test_CONUS_snow = p6_test_RF_CONUS_g2_exact_clust_test_snow,
                                flow_metric = 'vhfdc1_q0.9_exact_clust_test', 
                                perf_metric = 'rmse', 
                                out_dir = '6_predict/out'),
             deployment = 'main',
             format = 'file'
  ),
  
  
  
  #####residual maps
  tar_target(p6_residual_map_RF_rain,
             make_residual_map(df_pred_obs = p6_test_RF_rain_rain$pred, 
                               sites_g2_sf = p1_sites_g2_sf, 
                               metric = p6_test_RF_rain_rain$metric, 
                               pred_gage_ids = p6_test_RF_rain_rain$pred_gage_id, 
                               region = "rain", 
                               out_dir= "6_predict/out/pred_obs/")),
  
  tar_target(p6_residual_map_RF_snow,
             make_residual_map(df_pred_obs = p6_test_RF_snow_snow$pred, 
                               sites_g2_sf = p1_sites_g2_sf, 
                               metric = p6_test_RF_snow_snow$metric, 
                               pred_gage_ids = p6_test_RF_snow_snow$pred_gage_id, 
                               region = "snow", 
                               out_dir= "6_predict/out/pred_obs/")),
  
  tar_target(p6_residual_map_RF_rain_snow,
             make_residual_map(df_pred_obs = NULL ,
                               sites_g2_sf = p1_sites_g2_sf,
                               metric = p6_Boruta_rain_snow$metric,
                               pred_gage_ids = p6_Boruta_rain_snow$input_data$split$data$GAGES_ID,
                               region = "rain_snow",
                               out_dir = "6_predict/out/pred_obs/",
                               from_predict = TRUE,
                               model_wf = p6_train_RF_rain_snow$workflow,
                               pred_data = p6_Boruta_rain_snow$input_data$split$data)),
  tar_target(p6_residual_map_RF_rain_snow_exact,
             make_residual_map(df_pred_obs = NULL ,
                               sites_g2_sf = p1_sites_g2_sf,
                               metric = p6_Boruta_rain_snow_exact$metric,
                               pred_gage_ids = p6_Boruta_rain_snow_exact$input_data$split$data$GAGES_ID,
                               region = "rain_snow_exact",
                               out_dir = "6_predict/out/pred_obs/",
                               from_predict = TRUE,
                               model_wf = p6_train_RF_rain_snow_exact$workflow,
                               pred_data = p6_Boruta_rain_snow_exact$input_data$split$data)),
  
  tar_target(p6_residual_map_RF_CONUS_g2,
             make_residual_map(df_pred_obs = NULL ,
                               sites_g2_sf = p1_sites_g2_sf,
                               metric = p6_Boruta_CONUS_g2$metric,
                               pred_gage_ids = p6_Boruta_CONUS_g2$input_data$split$data$GAGES_ID,
                               region = "CONUS_g2",
                               out_dir = "6_predict/out/pred_obs/",
                               from_predict = TRUE,
                               model_wf = p6_train_RF_CONUS_g2$workflow,
                               pred_data = p6_Boruta_CONUS_g2$input_data$split$data)),
  
  tar_target(p6_residual_map_RF_CONUS_g2_exact,
             make_residual_map(df_pred_obs = NULL ,
                               sites_g2_sf = p1_sites_g2_sf,
                               metric = p6_Boruta_CONUS_g2_exact$metric,
                               pred_gage_ids = p6_Boruta_CONUS_g2_exact$input_data$split$data$GAGES_ID,
                               region = "CONUS_g2_exact",
                               out_dir = "6_predict/out/pred_obs/",
                               from_predict = TRUE,
                               model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                               pred_data = p6_Boruta_CONUS_g2_exact$input_data$split$data)),
  
  tar_target(p6_residual_map_RF_CONUS_g2_exact_clust,
             make_residual_map(df_pred_obs = NULL ,
                               sites_g2_sf = p1_sites_g2_sf,
                               metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                               pred_gage_ids = p6_Boruta_CONUS_g2_exact_clust$input_data$split$data$GAGES_ID,
                               region = "CONUS_g2_exact_clust",
                               out_dir = "6_predict/out/pred_obs/",
                               from_predict = TRUE,
                               model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                               pred_data = p6_Boruta_CONUS_g2_exact_clust$input_data$split$data))
  
  
) #end list
