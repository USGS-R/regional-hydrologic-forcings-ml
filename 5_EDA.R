source("5_EDA/src/EDA_metric_plots.R")
source("5_EDA/src/EDA_feature_plots.R")
source("5_EDA/src/select_features.R")
source("5_EDA/src/plot_gages_reaches.R")

p5_targets_list <- list(
  
  ###EDA plots
  # Feature variables
  tar_target(p5_EDA_plots_feature_vars,
             make_EDA_feature_plots(feature_vars = p1_feature_vars_g2,
                                    out_dir = "5_EDA/out/feature_plots"),
             format = "file"
  ),
  ##maps and violin plots of all metrics by cluster.  k is the number of clusters to use in 
  ##the cluster table
  tar_target(p5_EDA_plots_metrics,
             make_EDA_metric_plots(metric = p2_all_metrics_names,
                                   k = 5, 
                                   cluster_table = p3_gages_clusters_quants_agg_selected,
                                   high_q_grep = '0.9', 
                                   low_q_grep = '0.5', 
                                   high_q_start = 0.75, 
                                   metrics_table = p2_all_metrics,
                                   gages = p1_feature_vars_g2_sf,
                                   out_dir = "5_EDA/out/metrics_plots"),
             map(p2_all_metrics_names),
             format="file"
  ),
  #Low flow
  tar_target(p5_EDA_plots_metrics_low_novhfdc3,
             make_EDA_metric_plots(metric = p2_all_metrics_names_low,
                                   k = 5,
                                   cluster_table = p3_gages_clusters_quants_agg_low_novhfdc3,
                                   high_q_grep = '0.4',
                                   low_q_grep = '0.1',
                                   high_q_start = 0.25,
                                   metrics_table = p2_FDC_metrics_low,
                                   gages = p1_feature_vars_g2_sf,
                                   out_dir = "5_EDA/out/metrics_plots_LowFlow"
             ),
             map(p2_all_metrics_names_low),
             format="file"
  ),
  #Clusters based on raw metric values
  tar_target(p5_EDA_plots_metrics_raw_metrics,
             make_EDA_metric_plots(metric = p2_FDC_metrics_names,
                                   k = 5, 
                                   cluster_table = p3_gages_clusters_quants_agg_raw_metrics,
                                   high_q_grep = '0.9', 
                                   low_q_grep = '0.5', 
                                   high_q_start = 0.75, 
                                   metrics_table = p2_FDC_metrics,
                                   gages = p1_feature_vars_g2_sf,
                                   out_dir = "5_EDA/out/metrics_plots_RawClusts"),
             map(p2_FDC_metrics_names),
             format="file"
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
                                              #Canal ditch cndp better than CANALDITCH (no 0s),
                                              # but not available everywhere.
                                              "cndp",
                                              #storage available everywhere with NID and NORM STORAGE
                                              "strg",
                                              #development available everywhere from SOHL
                                              "devl",
                                              #these GAGESII data are not available everywhere
                                              "fwwd", "npdes",
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
                                              'TOT_RESERVOIR', 'TOT_SWAMPMARSH',
                                              #Using ACC PHYSIO instead
                                              'TOT_PHYSIO')),
             deployment = 'main'
  ),
  # remove TOT variables that are highly correlated with other variables (> 0.9)
  tar_target(p5_attr_g2,
             drop_high_corr_ACCTOT(features = p5_screen_attr_g2, 
                                   threshold_corr = 0.9,
                                   cor_method = 'spearman',
                                   drop_var = 'TOT',
                                   categorical_cols = 'PHYSIO'),
             deployment = 'main'
  ),
  
  #map of gages used
  tar_target(p5_g2_map_png,
             make_gages_map(gages = p1_feature_vars_g2_sf, 
                            out_dir = '5_EDA/out'),
             deployment = 'main'
  )
)