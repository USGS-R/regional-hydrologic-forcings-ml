make_EDA_metric_plots <- function(metric, k, cluster_table, high_q_grep, low_q_grep,
                                  high_q_start, metrics_table, gages, out_dir){
  #' @description function to make maps and violin plots for all flow metrics by cluster
  #' 
  #' @param metric the name of the flow metric
  #' @param k the number of clusters to search for column names within the 
  #' cluster_table using grep. "k"k must be a column name in cluster_table
  #' @param cluster_table table of gages (rows) with columns of the clusters
  #' @param high_q_grep character quantile within the high quantile names to grep 
  #' from the cluster_table names
  #' @param low_q_grep character quantile within the low quantile names to grep 
  #' from the cluster_table names
  #' @param high_q_start numeric value of the start of the high quantiles
  #' @param metrics_table table of flow metrics (columns) for gages (rows)
  #' @param gages table of gages (rows) with columns for "ID", "LAT", and "LON"
  #' @param out_dir directory to save files
  #'  
  #' @return filepaths to plots

  
  ##get the high and low quantile cluster columns for the specified number of clusters
  ##from the cluster_table.  This will throw an error if cluster table does not 
  ##have the specified number of clusters.
  high_quant_cluster_col <- intersect(grep(paste0("k", k), names(cluster_table)),
                                    grep(high_q_grep, names(cluster_table)))
  low_quant_cluster_col <- intersect(grep(paste0("k", k), names(cluster_table)),
                                   grep(low_q_grep, names(cluster_table)))
  
  
  ##find the appropriate cluster column for the current metric
  if (metric %in% c('ma', 'ml17', 'ml18')){
    cluster_col <- low_quant_cluster_col
  } else if (!grepl("_q", metric)){
    ##other metrics from HIT
    cluster_col <- high_quant_cluster_col
  }else{
    ##get quantile from FDC metric name
    metric_quantile <- as.numeric(str_split(metric, pattern="_q")[[1]][2])
    cluster_col <- ifelse(metric_quantile < high_q_start, low_quant_cluster_col, 
                          high_quant_cluster_col)
    cluster_name <- ifelse(metric_quantile < high_q_start, 'Low', 'High')
  } ##end if
 
   ##renaming the cluster column to make referencing this column easier later on
  cluster_table <- cluster_table %>% 
     rename(cluster = all_of(cluster_col))  
  
  ##get the column index of the metric
  metric_col <- which(names(metrics_table) == metric)
  ##renaming the first column to match the cluster and gages ID field
  names(metrics_table)[1] <- "ID"
  
  ##create data frame for plots
  df_plot <- 
    inner_join(gages[ , c("ID", "LAT", "LON")], cluster_table[ , c("ID", "cluster")]) %>%
    left_join(., metrics_table[c(1, metric_col)])


  ##loop through clusters and make a map and violin plot for each cluster
  states <- map_data("state")
  fileout_all <- vector()
  for (cl in 1:k){
    
    df <- df_plot %>% 
      filter(cluster == cl)
    
    p_violin <- ggplot(df, aes(x = 1, y = .data[[metric]])) +
      geom_violin(draw_quantiles = c(0.5)) +
      geom_jitter(height = 0,color = "steelblue", alpha = 0.5, width = 0.2) +
      xlab(paste(cluster_name, "Cluster" , cl)) +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
    
    p_map <- ggplot(states, aes(x = long, y = lat, group = group)) +
      geom_polygon(fill = "white", colour = "gray") +
      geom_sf(data = df, inherit.aes = FALSE, 
              aes(color = .data[[metric]]), 
              size = 0.5) +
      scale_color_scico(palette = 'batlow') +
      theme(legend.position = "bottom",
            legend.key.size = unit(.5, 'cm')) +
      xlab('Longitude') + 
      ylab('Latitude') +
      ggtitle(paste(cluster_name, "Cluster", cl))

     fname <- paste0(metric, "_k", k, "_", cluster_name, "Cluster", cl, ".png")
     fileout <- file.path(out_dir, fname)  
     save_plot(filename = fileout, 
               plot = plot_grid(p_map, p_violin, scale = c(1, .6)))
    
     fileout_all <- append(fileout_all, fileout)
  }
  
  return(fileout_all)
}#end function
  
