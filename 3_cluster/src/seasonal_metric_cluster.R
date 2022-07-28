seasonal_metric_cluster <- function(metric_mat, metric, 
                                    dist_method = 'euclidean',
                                    quantile_agg = FALSE
                                    ){
  #' @description computes clusters for seasonal metrics 
  #' written to be branched over the metric
  #' 
  #' @param metric_mat p1_FDC_metrics_season. rows are gauges, columns are seasonal metrics
  #' @param metric the name of the metric without the _s season at the end
  #' @param dist_method the distance computation for dist()
  #' @param quantile_agg logical for whether or not quantiles are aggregated
  #'  
  #' @return hclust clusters object
  
  #Select all of the seasonal columns for this metric
  if(quantile_agg){
    #get all of the quantiles into a vector
    metric <- str_split(string = metric, pattern = ',', simplify = TRUE)
    #get the column indices from metric_mat with these metric patterns
    col_inds <- get_column_inds(metric, metric_mat)
    metric_mat <- metric_mat[, c(1,col_inds)]
  }else{
    metric_mat <- metric_mat[, c(1,grep(x = colnames(metric_mat), 
                                        pattern = paste0(metric,'_'),
                                        fixed = TRUE))]
  }
  
  #Scaling of metrics should not be necessary because the metrics are on [0,1]
  
  #Compute the distance matrix between all sites
  dists <- dist(metric_mat[,-1], method = dist_method)
  
  #Compute clusters using different methods
  clust_methods_hclust <- c( "average", "single", "complete", "ward.D2")
  names(clust_methods_hclust) <- clust_methods_hclust
  
  #compute clusters using hclust function instead of agnes becuase it's faster
  clusts <- purrr::map(.x = clust_methods_hclust,
                      .f = hclust, 
                      d = dists, members = NULL)
  #add metric name
  #add agglomeration coefficient
  for (i in 1:length(clusts)){
    clusts[[i]]$ac = round(coef.hclust(clusts[[i]]), 3)
    clusts[[i]]$metric = metric
  }
  
  return(clusts)
}

select_cluster_method <- function(clusts, quantile_agg = FALSE){
  #' @description Function to extract the best cluster method for each metric
  #' 
  #' @param clusts list output from seasonal_metric_cluster
  #' @param quantile_agg logical for whether or not quantiles are aggregated
  #'  
  #' @return table reporting the best cluster method for each metric
  
  #data.frame of the metric, method, and ac value
  df <- matrix(nrow = length(clusts), ncol = 3, data = '')
  for (i in 1:nrow(df)){
    if(quantile_agg){
      #convert the metric to a character string
      clusts[[i]]$metric <- str_c(clusts[[i]]$metric, collapse = ',')
    }
    df[i,] <- as.character(clusts[[i]][c('metric', 'method', 'ac')])
  }
  df <- as.data.frame(df)
  colnames(df) <- c('metric', 'method', 'ac')
  df$ac <- as.numeric(df$ac)
  
  #df of the clustering method with the max ac value for each metric
  df_max <- df[1:length(unique(df$metric)),]
  df_max[,] <- NA
  for (i in 1:nrow(df_max)){
    a <- df[df$metric == unique(df$metric)[i],]
    df_max[i,] <- a[a$ac == max(a$ac),]
  }
  
  return(df_max)
}

compute_cluster_diagnostics <- function(clusts, metric_mat,
                                        kmin, kmax, alpha, boot = 50,
                                        index = 'all',
                                        dist_method = 'euclidean',
                                        clust_method = 'ward.D2',
                                        quantile_agg = FALSE
                                        ){
  #' @description Function to compute cluster diagnostic metrics
  #' 
  #' @param clusts list output from seasonal_metric_cluster
  #' @param metric_mat p1_FDC_metrics_season. rows are gauges, columns are seasonal metrics
  #' @param kmin,kmax min and max number of clusters to use
  #' @param alpha significance level
  #' @param boot number of bootstrap replicates
  #' @param index the NbClust index to compute. 'all' computes all except those 
  #' with long compute times.
  #' @param dist_method the distance computation for dist()
  #' @param clust_method the cluster method to use. 
  #' Character of one of the named list elements of clusts.
  #' @param quantile_agg logical for whether or not quantiles are aggregated
  #'  
  #' @return list containing the flow metric name, the NBclust metric values,
  #' and the gap statistic value
  
  clusts <- clusts[[clust_method]]
  
  #Select all of the seasonal columns for this metric
  if(quantile_agg){
    #get the column indices from metric_mat with these metric patterns
    col_inds <- get_column_inds(clusts$metric, metric_mat)
    metric_mat <- metric_mat[, col_inds]
  }else{
    metric_mat <- metric_mat[, grep(x = colnames(metric_mat), 
                                    pattern = paste0(clusts$metric,'_'),
                                    fixed = TRUE)]
  }
  
  #Compute NbClust cluster diagnostics
  nbclust_metrics <- NbClust::NbClust(data = metric_mat, diss = NULL, 
                                      distance = dist_method, 
                                      min.nc = kmin, max.nc = kmax, 
                                      method = clust_method, index = index, 
                                      alphaBeale = alpha)
  
  #Compute gap statistic
  gap_stat <- cluster::clusGap(as.matrix(metric_mat), FUNcluster = hcut,
                               K.max = kmax, B = boot, d.power = 2,
                               hc_func = 'hclust', hc_method = clust_method,
                               hc_metric = dist_method, verbose = FALSE)
  
  return(list(flow_metric = clusts$metric, 
              #dropping the suggested best cluster partition to save space
              nbclust_metrics = nbclust_metrics[-4], 
              gap_stat = gap_stat))
}

plot_cluster_diagnostics <- function(clusts, metric_mat, nbclust_metrics,
                                     dist_method = 'euclidean',
                                     clust_method = 'ward.D2',
                                     dir_out,
                                     quantile_agg = FALSE){
  #' @description Function to make a cluster diagnostic panel plot
  #' 
  #' @param clusts list output from seasonal_metric_cluster
  #' @param metric_mat p1_FDC_metrics_season. rows are gauges, columns are seasonal metrics
  #' @param nbclust_metrics output from compute_cluster_diagnostics
  #' @param dist_method the distance computation for dist()
  #' @param clust_method the cluster method to use. 
  #' Character of one of the named list elements of clusts.
  #' @param dir_out directory to save plot png files
  #' @param quantile_agg logical for whether or not quantiles are aggregated
  #'  
  #' @return filepaths to the plots
  
  clusts <- list(clusts[[clust_method]])
  
  fileout <- vector('character', length = length(clusts))
  
  for(cl in 1:length(clusts)){
    #Select all of the seasonal columns for this metric
    if(quantile_agg){
      #get the column indices from metric_mat with these metric patterns
      col_inds <- get_column_inds(clusts[[cl]]$metric, metric_mat)
      metric_mat <- metric_mat[, c(1,col_inds)]
      #change metric to a concatenated string for plot names
      clusts[[cl]]$metric <- str_c(clusts[[cl]]$metric, collapse = '-')
    }else{
      metric_mat <- metric_mat[, c(1,grep(x = colnames(metric_mat), 
                                          pattern = paste0(clusts[[cl]]$metric,'_'),
                                          fixed = TRUE))]
    }
    
    fileout[cl] <- file.path(dir_out, 
                             paste0(clusts[[cl]]$metric, '_', 
                                    clust_method, '_diagnostics.png'))
    
    #dendrogram
    p1 <- ggplot(dendextend::as.ggdend(as.dendrogram(clusts[[cl]]))) +
      labs(title = paste0("Dendrogram of ", clusts[[cl]]$metric, " with\n", 
                          clust_method, ' Clustering. AC = ', clusts[[cl]]$ac))
    
    #WSS
    p2 <- fviz_nbclust(x = as.matrix(metric_mat[,-1]), FUNcluster = hcut, method = 'wss', 
                       k.max = 20, hc_func = 'hclust', hc_method = clust_method, 
                       hc_metric = dist_method) +
      labs(title = paste0('WSS for Metric: ', clusts[[cl]]$metric,
                          ',\nCluster Method: ', clust_method))
    
    #histogram of optimal number of clusters
    p3 <- ggplot(data = as.data.frame(t(nbclust_metrics$nbclust_metrics$Best.nc)), 
                 aes(Number_clusters)) + 
      geom_histogram(bins = 20, binwidth = 0.5) +
      labs(title = paste0('Suggested Optimal Number of Clusters from 26 Metrics\nMetric: ', 
                          clusts[[cl]]$metric, ', Cluster Method: ', clust_method)) +
      xlab("Suggested Optimal Number of Clusters") +
      ylab("Count")
    
    #gap statistic
    p4 <- fviz_gap_stat(nbclust_metrics$gap_stat, maxSE = list(method = 'globalmax')) +
      labs(title = paste0('Gap Statistic for Metric: ', clusts[[cl]]$metric,
                          ',\nCluster Method: ', clust_method))
    
    save_plot(filename = fileout[cl], base_height = 8, base_width = 8, 
              plot = plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2))
  }
  
  return(fileout)
}

add_cluster_to_gages <- function(gages, screened_sites, clusts, best_clust,
                                 min_clusts, max_clusts, by_clusts, 
                                 quantile_agg = FALSE){
  #' @description Function to add the cluster numbers to gages
  #' 
  #' @param gages dataframe of the gages (rows)
  #' @param screened_sites the sites used in the cluster analysis
  #' @param clusts cluster analysis results from seasonal_metric_cluster
  #' @param best_clust the best cluster analysis method
  #' @param min_clusts minimum number of clusters to add to gages as columns
  #' @param max_clusts maximum number of clusters to add to gages as columns
  #' @param by_clusts increment of clusters to add to gages as columns
  #' @param quantile_agg logical for whether or not quantiles are aggregated
  #'  
  #' @return gages with columns for the cluster analysis methods
  
  #Select the gages that have clusters computed
  gages_clusts <- gages[gages$ID %in% screened_sites, "ID"]
  
  #add columns with cluster numbers
  clust_nums <- seq(min_clusts, max_clusts, by_clusts)
  for(i in 1:length(clusts)){
    if(quantile_agg){
      #change metric to a concatenated string for comparison
      clusts[[i]]$metric <- str_c(clusts[[i]]$metric, collapse = ',')
    }
    #find only the best cluster methods
    if(clusts[[i]]$method == best_clust$method[best_clust$metric == clusts[[i]]$metric]){
      #get clusters from the best cluster method
      for (k in clust_nums){
        gages_clusts <- cbind(gages_clusts, cutree(clusts[[i]], k = k))
      }
      colnames(gages_clusts)[(ncol(gages_clusts) - length(clust_nums) 
                              + 1):ncol(gages_clusts)] <- paste0(clusts[[i]]$metric, 
                                                                 '_k', clust_nums)
      
    }
  }
  
  return(gages_clusts)
}

plot_seasonal_barplot <- function(metric_mat, metric, 
                                  season_months,
                                  by_cluster = FALSE,
                                  cluster_table = NULL,
                                  panel_plot = NULL,
                                  dir_out,
                                  quantile_agg = FALSE,
                                  by_quantile = FALSE){
  #' @description Function to plot the average seasonal distribution for all sites,
  #' or plot the average seasonal distribution for sites in the cluster
  #' 
  #' @param metric_mat p1_FDC_metrics_season. rows are gauges, columns are seasonal metrics
  #' @param metric character of the metric to plot
  #' @param season_months numeric vector of 12 months in water year order
  #' @param by_cluster logical, makes a plot with panels for each cluster
  #' @param cluster_table output of add_cluster_to_gages
  #' @param panel_plot makes a panel plot instead of individual plots
  #' @param dir_out directory to save plot png files
  #' @param quantile_agg logical for whether or not quantiles in metric are 
  #' a vector to be aggregated
  #' @param by_quantile logical, does metric contain quantiles? if TRUE, 
  #' plots will be made for each streamflow metric instead of averaging over all 
  #' streamflow metrics
  #'  
  #' @return filepaths to the plots
  
  if(by_cluster & is.null(cluster_table)){
    stop('cluster_table must be supplied to plot by clusters.')
  }
  
  #Select all of the column names used for this metric
  if(quantile_agg){
    #get all of the quantiles into a vector
    metric_vec <- str_split(string = metric, pattern = ',', simplify = TRUE)
    #get the column indices from metric_mat with these metric patterns
    col_inds <- get_column_inds(metric_vec, metric_mat)
    metric_mat <- metric_mat[, c(1,col_inds)]
  }else{
    metric_mat <- metric_mat[, c(1,grep(x = colnames(metric_mat), 
                                        pattern = paste0(metric,'_'),
                                        fixed = TRUE))]
  }
  
  #get the month labels
  month_chars <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
  season_months <- stringi::stri_join(month_chars[season_months][c(1,4,7,10)],
                                      month_chars[season_months][c(2,5,8,11)],
                                      month_chars[season_months][c(3,6,9,12)])
  
  if (by_cluster){
    #Select all of the column names used for this metric
    cluster_table <- cluster_table[, c(1,grep(x = colnames(cluster_table), 
                                              pattern = paste0(metric,'_'),
                                              fixed = TRUE))]
    
    #Get the total number of clusters in all of the columns. 
    #There will be 2 elements after splitting
    k <- unlist(strsplit(colnames(cluster_table[,-1]), 
                         split = '_k'))[seq(2,ncol(cluster_table[,-1])*2,2)] %>%
      as.numeric()
    
    #get all directories to create based on the number of clusters
    dir_out <- file.path(dir_out, paste0('cluster', k))
    
    #Determine the number of files to be created
    if(panel_plot){
      if(by_quantile){
        #one panel per streamflow metric within each analysis
        metric_names <- unique(apply(str_split(string = colnames(metric_mat)[-1], 
                                               pattern = '_', simplify = T), 
                                     MARGIN = 1, FUN = first))
        fileout <- vector('character', length = length(k)*length(metric_names))
      }else{
        #one panel plot per analysis
        fileout <- vector('character', length = length(k))
      }
    }else{
      #one plot per cluster
      fileout <- vector('character', length = sum(k))
    }
    
    #loop over the analyses to make plots
    for (i in 1:length(k)){
      dir.create(dir_out[i], showWarnings = FALSE)
      if(panel_plot){
        #create matrix of colmeans as rows to plot with facet_wrap
        metric_mat_c <- get_colmeans_panel_plt(metric_names, metric_mat, by_quantile, 
                               quantile_agg, cluster_table, ki = k[i], i,
                               season_months)
        
        #make panel plots
        if(by_quantile){
          #need to loop over metric names to create plots
          for (j in 1:length(metric_names)){
            fileout[j+(i-1)*length(metric_names)] <- file.path(dir_out[i], paste0('SeasonalBarplot_', 
                                                       colnames(cluster_table)[i+1], '_Metric_',
                                                       metric_names[j], '.png'))
            
            plt <- ggplot(metric_mat_c[metric_mat_c$metric == metric_names[j], ]) + 
              ylim(0,1) +
              xlab('Season Months') + 
              ylab('Seasonal Fraction') +
              ggtitle(paste0('Cluster Metric: ', metric, ' Flow Metric: ', metric_names[j])) +
              geom_col(aes(season, data)) + 
              scale_x_discrete(limits=season_months) + 
              geom_errorbar(aes(x = season, 
                                ymin = ymin, 
                                ymax = ymax),
                            width = 0.4) +
              facet_wrap(~reorder(cluster, label_order))
            ggsave(filename = fileout[j+(i-1)*length(metric_names)], plot = plt, device = 'png')
          }
        }else{
          fileout[i] <- file.path(dir_out[i], paste0('SeasonalBarplot_', 
                                                     colnames(cluster_table)[i+1], '.png'))
          
          plt <- ggplot(metric_mat_c) + 
            ylim(0,1) +
            xlab('Season Months') + 
            ylab('Seasonal Fraction') +
            ggtitle(paste0('Metric ', metric)) +
            geom_col(aes(season, data)) + 
            scale_x_discrete(limits=season_months) + 
            geom_errorbar(aes(x = season, 
                              ymin = ymin, 
                              ymax = ymax),
                          width = 0.4) +
            facet_wrap(~reorder(cluster, label_order))
          ggsave(filename = fileout[i], plot = plt, device = 'png')
        }
      }else{
        for (cl in 1:k[i]){
          #metric matrix for gages in cluster
          metric_mat_c <- filter(metric_mat, 
                                 site_num %in% cluster_table$ID[cluster_table[,i+1] == cl]) %>%
            select(-site_num)
          
          #file index
          ind_f <- ifelse(test = i > 1, cl + cumsum(k)[i-1], cl)
          
          fileout[ind_f] <- file.path(dir_out[i], 
                                   paste0('SeasonalBarplot_', colnames(cluster_table)[i+1], 
                                   '_c', cl, '.png'))
          
          png(filename = fileout[ind_f], width = 5, height = 5, units = 'in', res = 200)
          barplot(height = colMeans(metric_mat_c), width = 1, 
                  names.arg = season_months, 
                  xlim = c(0,4), ylim = c(0,1),
                  space = 0, main = paste0('Metric ', metric, ', k = ', k[i],
                                           ',\nCluster ', cl, ', ', nrow(metric_mat_c), ' sites'), 
                  xlab = 'Season Months', ylab = 'Seasonal Fraction')
          #add error bars as 5th - 95th percentiles
          arrows(x0 = c(0.5,1.5,2.5,3.5), 
                 y0 = as.numeric(apply(X = metric_mat_c, MARGIN = 2, FUN = quantile, 
                                       probs = 0.05)),
                 x1 = c(0.5,1.5,2.5,3.5), 
                 y1 = as.numeric(apply(X = metric_mat_c, MARGIN = 2, FUN = quantile, 
                                       probs = 0.95)),
                 angle = 90, length = 0.1, code = 3)
          dev.off()
        }
      }
    }
  }else{
    #plots not made by cluster
    fileout <- file.path(dir_out, paste0('SeasonalBarplot_', metric, '.png'))
    png(filename = fileout, width = 5, height = 5, units = 'in', res = 200)
    barplot(height = colMeans(metric_mat[,-1]), width = 1, 
            names.arg = season_months, 
            xlim = c(0,4), ylim = c(0,1),
            space = 0, main = paste0('Metric ', metric, ', ', nrow(metric_mat), ' sites'),
            xlab = 'Season Months', ylab = 'Seasonal Fraction')
    #add error bars as 5th - 95th percentiles
    arrows(x0 = c(0.5,1.5,2.5,3.5), 
           y0 = as.numeric(apply(X = metric_mat[,-1], MARGIN = 2, FUN = quantile, 
                                 probs = 0.05)),
           x1 = c(0.5,1.5,2.5,3.5), 
           y1 = as.numeric(apply(X = metric_mat[,-1], MARGIN = 2, FUN = quantile, 
                                 probs = 0.95)),
           angle = 90, length = 0.1, code = 3)
    dev.off()
  }
  
  return(fileout)
}


#Not currently used 
#Function to plot the cuts in trees from kmin to kmax clusters
#clusts is the output from hclust
plot_cuttree <- function(clusts, kmin, kmax, seq_by, dir_out){
  for (k in seq(kmin, kmax, seq_by)){
    fileout <- file.path(dir_out, paste0(clusts$metric, '_', clusts$method, '_', k,'.png'))
    png(fileout, res = 300, units = 'in', width = 7, height = 5)
    plot(clusts, cex = 0.6, hang = -1, 
         main = paste0("Dendrogram of ", clusts$metric, " with ", clusts$method,
                       ' Clustering,\nCut at ', k, ' Clusters'))
    rect.hclust(clusts, k = k, border = rainbow(45, alpha = 1))
    dev.off()
  }
  
  return(fileout)
}


#can add bounding boxes for clusters as argument
plot_cluster_map <- function(gages, cluster_table, screened_sites, dir_out,
                             facet = FALSE){
  #' @description Function to make a map of the resulting clusters
  #' 
  #' @param gages dataframe of the gages (rows)
  #' @param cluster_table output of add_cluster_to_gages
  #' @param screened_sites the sites used in the cluster analysis
  #' @param dir_out directory to save plot png files
  #' @param facet logical for whether or not to make a facet wrap over the clusters
  #'  
  #' @return filepaths to the plots
  
  ncol_gages <- ncol(gages)
  
  #get only sites with metrics computed
  gages <- gages[which(gages$ID %in% screened_sites),]
  
  #Add the cluster_table to gages by ID join
  gages <- cbind(gages, cluster_table)
  
  #U.S. States
  states <- map_data("state")
  
  fileout <- vector('character', length = ncol(cluster_table)-1)
  
  for(i in 1:(ncol(cluster_table)-1)){
    fileout[i] <- ifelse(facet,
                         file.path(dir_out, paste0(colnames(cluster_table)[i+1], '_facet_map.png')),
                         file.path(dir_out, paste0(colnames(cluster_table)[i+1], '_map.png')))
    
    #number of clusters from the column name
    k <- as.numeric(str_split(string = str_split(string = colnames(cluster_table)[i+1], 
                                      pattern = '_')[[1]] %>% last(), 
                   pattern = 'k')[[1]] %>% last())
    
    #png(filename = fileout[i], width = 8, height = 5, units = 'in', res = 200)
    #plot gage locations, colored by their cluster
    p1 <- ggplot(states, aes(x=long, y=lat, group=group)) +
      geom_polygon(fill="white", colour="gray") +
      geom_sf(data = gages, inherit.aes = FALSE, 
              aes(color = factor(.data[[colnames(gages)[ncol_gages+i]]])), 
              size = 0.75) + 
      ggtitle(paste0('Quantiles ', 
                     str_split(colnames(cluster_table)[i+1], pattern = '_', 
                               simplify = T)[1])) +
      xlab('Longitude') + 
      ylab('Latitude') +
      labs(color='Cluster') +
      scale_color_scico_d(palette = 'batlow')
      if(facet){
        p1 <- p1 + facet_wrap(~.data[[colnames(gages)[ncol_gages+i]]]) +
          theme(legend.position = 'none')
      }
    
    ggsave(filename = fileout[i],
           plot = p1)
  }
  
  return(fileout)
}


get_column_inds <- function(metric, metric_mat){
  #' @description gets the column indices from metric_mat with these metric patterns
  #' 
  #' @param metric character string to search for in the column names of metric_mat
  #' @param metric_mat p1_FDC_metrics_season. rows are gauges, columns are seasonal metrics
  #'  
  #' @return vector of column indices
  
  col_inds <- vector('numeric', length = 0L)
  for (m in 1:length(metric)){
    col_inds <- c(col_inds, grep(x = colnames(metric_mat), 
                                 pattern = paste0(metric[m],'_'),
                                 fixed = TRUE))
  }
  return(col_inds)
}

get_colmeans_panel_plt <- function(metric_names, metric_mat, by_quantile, 
                                   quantile_agg, cluster_table, ki, i,
                                   season_months){
  #' @description computes column means for use in the panel plot
  #' 
  #' @param metric_names the names of the metrics for which to compute column means
  #' @param metric_mat p1_FDC_metrics_season. rows are gauges, columns are seasonal metrics
  #' @param by_quantile 
  #' @param quantile_agg logical for whether or not quantiles are aggregated
  #' @param cluster_table output of add_cluster_to_gages
  #' @param ki number of clusters
  #' @param i cluster_table column index to use
  #' @param season_months numeric vector of 12 months in water year order
  #'  
  #' @return dataframe with columns for the colmeans, season, cluster, ymin, ymax, 
  #' label_order, and metric name.
  
  #Determine the dimensions of the data frame based on what kind of plot is being made
  if(by_quantile){
    #panel plots are made for each streamflow metric name in each analysis, ki
    metric_mat_c <- as.data.frame(matrix(nrow = ki*4*length(metric_names), 
                                         ncol = 7))
  }else{
    #panel plot is specific to the analysis, ki
    metric_mat_c <- as.data.frame(matrix(nrow = ki*4, ncol = 6))
  }
  
  #track the number of sites in each cluster within analysis ki
  num_sites <- vector('numeric', length = ki)
  
  #loop over clusters to get data for that cluster
  for (cl in 1:ki){
    #full matrix of all sites within that cluster 
    full_mat <- filter(metric_mat, 
                       site_num %in% cluster_table$ID[cluster_table[,i+1] == cl]) %>%
      select(-site_num)
    
    num_sites[cl] <- nrow(full_mat)
    
    if(by_quantile){
      #by quantile, so metric contains quantile names instead of streamflow metric names
      if(quantile_agg){
        #make new columns for each metric name_season
        metric_names_full_mat <- unlist(lapply(paste0(metric_names, '_s'), 
                                               FUN = paste0, seq(1,4,1)))
        
        #Get dimensions of the matrix with those new columns
        full_mat_names <- matrix(nrow = nrow(full_mat)*ncol(full_mat)/length(metric_names)/4, 
                                 ncol = length(metric_names_full_mat))
        
        #determine the streamflow metric represented in each column 
        cols_first <- apply(str_split(colnames(full_mat), '_', simplify = TRUE), 1, first)
        #determine the season represented in each column
        cols_last <- apply(str_split(colnames(full_mat), '_', simplify = TRUE), 1, last)
        
        #fill in full_mat_names with data from full_mat
        for(s in 1:length(metric_names_full_mat)){
          first_s <- str_split(metric_names_full_mat[s], '_', simplify = TRUE) %>% first()
          last_s <- str_split(metric_names_full_mat[s], '_', simplify = TRUE) %>% last()
          full_mat_names[,s] <- stack(full_mat[, (cols_first == first_s) & 
                                                 (cols_last == last_s)])$value
        }
        full_mat_names <- as.data.frame(full_mat_names)
        colnames(full_mat_names) <- metric_names_full_mat
        
        #Use the new matrix to compute mean and error bars for each season and each streamflow metric
        metric_mat_c[(1+(cl-1)*4*length(metric_names)):(4*cl*length(metric_names)), ] <- 
          data.frame(data = full_mat_names %>% colMeans(),
                     season = rep(season_months, length(metric_names)), 
                     cluster = paste0('Cluster ', cl, ', ', num_sites[cl], ' sites'),
                     ymin = as.numeric(apply(full_mat_names, MARGIN = 2,
                                             FUN = quantile, probs = 0.05)), 
                     ymax = as.numeric(apply(full_mat_names, MARGIN = 2, 
                                             FUN = quantile, probs = 0.95)),
                     #used so that the panel plot orders clusters from 1:n
                     label_order = cl,
                     #used to get only the streamflow metric name
                     metric = apply(str_split(metric_names_full_mat, '_', 
                                              simplify = TRUE), 1, first))
      }else{
        #get streamflow metric names for each column in full_mat
        metric_names_full_mat <- apply(str_split(colnames(full_mat), '_', simplify = TRUE), 
                                       1, first)
        #Use full_mat to compute mean and error bars for each season and each streamflow metric
        metric_mat_c[(1+(cl-1)*4*length(metric_names)):(4*cl*length(metric_names)), ] <- 
          data.frame(data = full_mat %>% colMeans(),
                     season = rep(season_months, length(metric_names)), 
                     cluster = paste0('Cluster ', cl, ', ', num_sites[cl], ' sites'),
                     ymin = as.numeric(apply(full_mat, MARGIN = 2, 
                                             FUN = quantile, probs = 0.05)),
                     ymax = as.numeric(apply(full_mat, MARGIN = 2, 
                                             FUN = quantile, probs = 0.95)),
                     label_order = cl,
                     metric = metric_names_full_mat)
      }
    }else{
      #not by quantile, so metric contains streamflow metric names instead of quantiles
      metric_mat_c[(1+(cl-1)*4):(4*cl), ] <- 
        data.frame(data = full_mat %>% colMeans(),
                   season = season_months, 
                   cluster = paste0('Cluster ', cl, ', ', num_sites[cl], ' sites'), 
                   ymin = as.numeric(apply(full_mat, MARGIN = 2,
                                           FUN = quantile, probs = 0.05)), 
                   ymax = as.numeric(apply(full_mat, MARGIN = 2, 
                                           FUN = quantile, probs = 0.95)),
                   label_order = cl)
    }
  }
  if(by_quantile){
    colnames(metric_mat_c) <- c('data', 'season', 'cluster', 'ymin', 'ymax', 
                                'label_order', 'metric')
  }else{
    colnames(metric_mat_c) <- c('data', 'season', 'cluster', 'ymin', 'ymax', 
                                'label_order')
  }
  
  return(metric_mat_c)
}

