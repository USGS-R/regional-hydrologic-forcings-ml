make_EDA_feature_plots <- function(feature_vars, out_dir) {
  #' 
  #' @description Function to plot feature variable distributions.
  #' It plots maps of feature ranges across study area.
  #' It plots violin distributions across study area.
  #'
  #' @param feature_vars feature variables for all gages2.1 locations (p1_feature_vars_g2)
  #' @param out_dir output directory for storing maps and plots
  #' 
  #' @return maps and plots to output directory
  
  for (i in 5:ncol(feature_vars)) {
    
    data_to_plot <- select(feature_vars, 1:4, all_of(i))
    col_name <- names(data_to_plot)[5]
    
    if (str_detect(col_name, "/")) {
      col_name <- str_replace(col_name, "/", "_")
      data_to_plot <- 
        rename_with(data_to_plot, ~ str_replace(col_name, "/", "_"), .cols = 5)
    }
    
    violin <- ggplot(data_to_plot, aes(x = "", y = .data[[col_name]])) +
      geom_violin(draw_quantiles = c(0.5)) +
      geom_jitter(height = 0, color = "steelblue", alpha = 0.5, width = 0.2) +
      labs(x = "", y = "") + 
      theme_bw() +
      theme(plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"), 
            axis.ticks.x = element_blank())
    
    data_to_map <- data_to_plot %>%
      st_as_sf(coords = c("LON", "LAT"), crs = 4326)
    map <- ggplot(data_to_map) +
      geom_sf(aes(color = .data[[col_name]]), size = 0.3) + 
      scale_color_viridis_c(option="plasma") + 
      theme_bw() + 
      theme(plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"),
            axis.text = element_text(size = 6),
            legend.title = element_text(size = 10, vjust = 0.75), 
            legend.position = "top")
    
    combo <- plot_grid(violin, map, rel_widths = c(0.25, 1), rel_heights = c(0.8, 1)) %>%
      suppressWarnings()
    fileout <- file.path(out_dir, paste0(col_name, ".png"))
    save_plot(filename = fileout, combo)
  }
  return(out_dir)
}