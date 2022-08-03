
get_sb_data <- function(sites, sb_var_ids, dldir, workdir, outdir, out_file_name) {
  
  #'@description recursively downloads and saves feature variable values from ScienceBase
  #'
  #'@param sites data frame with a COMID column including all reaches of interest
  #'@param sb_var_ids data frame with ScienceBase identifier column
  #'@param dldir filepath for downloads
  #'@param workdir filepath for unzipping, joining, etc.
  #'@param outdir filepath for final data downloads
  #'@param out_file_name filename for final data downloads
  #'
  #'@return series of .csv files with ScienceBase feature variable values
  #'joined to a list of COMIDs of interest
  
  data_at_sites <- tibble(COMID = sites$COMID)
  itemfails <- "1st"
  item <- sb_var_ids[[1]]
  name <- sb_var_ids[[2]]
  message(paste('starting SB ID', item, name))
  item_file_download(item, dest_dir = dldir, overwrite_file = TRUE)
  files <- list.files(path = dldir, pattern = NULL, full.names = TRUE)
  
  for (file in files)
  {
    last3 <- str_sub(file, -3, -1)
    last3 <- tolower(last3)
    if (last3 == "xml")
    {
      file.remove(file)
    }
    if (last3 == "zip")
    {
      unzip(file, exdir=workdir)
      file.remove(file)
    }
    if (last3 == "txt")
    {
      file.copy(file, workdir)
      file.remove(file)
    }
    filestomerge <- c(list.files(workdir, ".txt", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), 
                      list.files(workdir, ".csv", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), 
                      list.files(workdir, ".TXT", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), 
                      list.files(workdir, ".CSV", recursive = TRUE, full.names = TRUE, include.dirs = TRUE))
    for (filem in filestomerge)
    {
      print(filem)
      success = "yes"
      tryCatch(tempfile <- read_delim(filem, delim = ",", show_col_types = FALSE), 
               error = function(e) {success <<- "no"})
      names(tempfile) <- toupper(names(tempfile))
      if (!"COMID" %in% colnames(tempfile))
      {
        success <- "no"
      }
      if (success == "no")
      {
        itemfails <- c(itemfails, filem)
        print("data table error")
      }
      if (success == "yes")
      {
        print("We're merging")
        try(tempfile <- subset(tempfile, select =-c(NODATA)), silent = TRUE)
        try(tempfile <- subset(tempfile, select =-c(CAT_NODATA)), silent = TRUE)
        try(tempfile <- subset(tempfile, select =-c(ACC_NODATA)), silent = TRUE)
        try(tempfile <- subset(tempfile, select =-c(TOT_NODATA)), silent = TRUE)
        data_at_sites <- merge(data_at_sites, tempfile, by = "COMID", all.x = TRUE)
      }
      file.remove(filem)
    }
    
    f <- list.files(workdir)
    unlink(file.path(workdir,f), recursive=TRUE)
  }
  
  if (length(itemfails) > 0)
  {
    failist <- as.data.frame(itemfails[-1])
    colnames(failist) <- c('filename')
    rownames(failist) <- NULL
  }
  
  itemfails <- as.data.frame(itemfails)
  
  filepath1 <- paste0(outdir, "/", out_file_name, item, ".csv")
  write_csv(data_at_sites, filepath1)
  filepath2 <- paste0(outdir, "/", out_file_name, item, "_FAILS.csv")
  write_csv(itemfails, filepath2)
  return_df = c(filepath1, filepath2)
  return(return_df)
}

get_sb_data_log <- function(sb_var_ids, file_out) {
  
  #'@description generates a log file to track changes to ScienceBase data
  #'
  #'@param sb_var_ids data frame with ScienceBase identifier column
  #'@param file_out filepath for log file
  #'
  #'@return log file documenting dates of changes to ScienceBase feature variable values
  
  sb_log <- tibble(sb_id = character(), 
                   last_update = character())
  for (i in 1:nrow(sb_var_ids)) {
    sb_id <- as.character(sb_var_ids[i,1])
    last_update <- item_get_fields(sb_id, "provenance")$lastUpdated
    log_item <- tibble(sb_id, last_update)
    sb_log <- bind_rows(sb_log, log_item)
  }
  
  write_csv(sb_log, file_out)
  return(file_out)
  
}

prep_feature_vars <- function(sb_var_data, sites_all, sites_screened, retain_vars) {
  
  #'@description joins all data downloaded from ScienceBase with sites of interest
  #'
  #'@param sb_var_data target generated from the 'get_sb_data()' function mapped over the 
  #''p1_sb_var_ids' targets; the 'p1_sb_var_ids' target reads in a .csv of ScienceBase
  #'identifiers and names, and the 'get_sb_data()' function generates a list of .csv file
  #'locations containing the feature variable data from each ScienceBase identifier
  #'joined with the COMIDs contained in the 'sites' parameter
  #'@param sites_all data frame with a COMID column including all reaches of interest
  #'@param sites_screened list of sites that passed screening functions
  #'@param retain_vars character strings of additional column headers to keep in the 
  #'sites data frame
  #'
  #'@return data frame with COMID column appended by all feature variables of interest; 
  #'time-varying features converted to long-term averages where applicable
  
  data <- sites_all %>%
    select(COMID, all_of(retain_vars)) %>%
    filter(ID %in% sites_screened) %>%
    mutate(across(where(is.character)  & !starts_with('ID'), as.numeric))
  
  if("ID" %in% retain_vars) {
    data <- rename(data, GAGES_ID = ID)
  }
  
  #join all sciencebase data to single data frame with comids
  for (i in 1:length(sb_var_data)) {
    filepath <- sb_var_data[[i]][1]
    data_temp <- read_csv(filepath, show_col_types = FALSE) %>%
      group_by(COMID) %>%
      summarise_all(mean)
    data <- left_join(data, data_temp, by = "COMID")
  }
  
  #handles any duplicated COMIDs
  if(!("ID" %in% retain_vars)) {
    data <- data %>%
      group_by(COMID) %>%
      summarise_all(mean) 
  }
  
  #reclassify land use data for consistency across time periods
  land_cover <- data %>%
    select(COMID, 
           contains("SOHL40"), contains("SOHL50"), contains("SOHL60"),
           contains("SOHL70"), contains("SOHL80"), contains("SOHL90"), 
           contains("SOHL00")) %>%
    group_by(COMID) %>%
    summarise_all(mean) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           year = if_else(str_sub(name, 9, 10) == "00", 2000, 
                          as.numeric(paste0("19", str_sub(name, 9, 10)))), 
           class = as.numeric(str_sub(name, 12))) %>%
    mutate(new_class = case_when(class == 1 ~ "WATER", class == 2 ~ "DEVELOPED", 
                                 class == 3 ~ "FOREST", class == 4 ~ "FOREST", 
                                 class == 5 ~ "FOREST", class == 6 ~ "MINING", 
                                 class == 7 ~ "BARREN", class == 8 ~ "FOREST", 
                                 class == 9 ~ "FOREST", class == 10 ~ "FOREST", 
                                 class == 11 ~ "GRASSLAND", class == 12 ~ "SHRUBLAND", 
                                 class == 13 ~ "CROPLAND", class == 14 ~ "HAY/PASTURE", 
                                 class == 15 ~ "WETLAND", class == 16 ~ "WETLAND", 
                                 class == 17 ~ "ICE/SNOW")) %>%
    group_by(COMID, unit, year, new_class) %>%
    summarise(value = sum(value)) %>%
    mutate(lc_sum = sum(value)) %>%
    ungroup()
  
  #identify comids with substantial area outside of CONUS and remove
  comid_out_conus <- land_cover %>%
    filter(lc_sum < 90 | lc_sum > 101)
  comid_out_conus <- unique(comid_out_conus$COMID)
  data <- data %>%
    subset(!(COMID %in% comid_out_conus))
  
  #convert decadal land use to long-term average land use
  land_cover <- land_cover %>%
    subset(!(COMID %in% comid_out_conus)) %>%
    group_by(COMID, unit, year, new_class) %>%
    mutate(lc_adj = (value / lc_sum) * 100) %>%
    ungroup() %>%
    select(COMID, unit, new_class, lc_adj) %>%
    group_by(COMID, unit, new_class) %>%
    summarise(value = mean(lc_adj), .groups = "drop") %>%
    mutate(label = paste0(unit, "_SOHL_", new_class)) %>%
    select(COMID, label, value) %>%
    pivot_wider(names_from = "label", values_from = "value")
    
  #convert decadal dam information to long-term average dam information
  dams <- data %>%
    select(COMID, contains("NDAMS"), contains("STORAGE"), contains("MAJOR")) %>%
    group_by(COMID) %>%
    summarise_all(mean) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           feature = str_sub(name, 5, -5),
           year = str_sub(name, -4)) %>%
    filter(year <= 2010) %>%
    group_by(COMID, unit, feature, year) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    group_by(COMID, unit, feature) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    mutate(label = paste0(unit, "_", feature)) %>%
    select(COMID, label, value) %>%
    pivot_wider(names_from = label, values_from = value)
 
  #convert annual monthly weather data to long-term average monthly weather data
  weather <- data %>%
    select(COMID, contains("_PPT_"), contains("_TAV_")) %>%
    group_by(COMID) %>%
    summarise_all(mean) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           type = str_sub(name, 5, 7),
           month_name = str_sub(name, 9, 11)) %>%
    group_by(COMID, unit, type, month_name) %>%
    summarise(avg_monthly = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(month_num = case_when(month_name == "JAN" ~ 1, month_name == "FEB" ~ 2, 
                                 month_name == "MAR" ~ 3, month_name == "APR" ~ 4, 
                                 month_name == "MAY" ~ 5, month_name == "JUN" ~ 6, 
                                 month_name == "JUL" ~ 7, month_name == "AUG" ~ 8, 
                                 month_name == "SEP" ~ 9, month_name == "OCT" ~ 10, 
                                 month_name == "NOV" ~ 11, month_name == "DEC" ~ 12),
           label = paste0(unit, "_", type, "_", month_name)) %>%
    arrange(COMID, type, unit, month_num) %>%
    select(COMID, label, avg_monthly) %>%
    pivot_wider(names_from = "label", values_from = "avg_monthly")

  
  #convert annual wildfire data to long-term average wildfire data
  wildfire <- data %>%
    select(COMID, contains("_WILDFIRE_")) %>%
    group_by(COMID) %>%
    summarise_all(mean) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           year = str_sub(name, 14, 17)) %>%
    group_by(COMID, unit) %>%
    summarise(avg_annual = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(label = paste0(unit, "_AVG_WILDFIRE")) %>%
    arrange(COMID, label) %>%
    select(COMID, label, avg_annual) %>%
    pivot_wider(names_from = "label", values_from = "avg_annual")
  
  #determine dominant physiographic region
  phys_region <- data %>%
    select(COMID, contains("_PHYSIO_")) %>%
    select(-contains("AREA")) %>%
    group_by(COMID) %>%
    summarise_all(mean) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           region = str_sub(name, 12))
  
  dom_phys_reg <- tibble()
  for (i in unique(phys_region$COMID)) {
    phys_reg_comid <- filter(phys_region, COMID == i)
    
    dom_phys_reg_comid <- tibble()
    for (j in unique(phys_reg_comid$unit)) {
      phys_reg_comid_unit_ties <- filter(phys_reg_comid, unit == j) %>%
        arrange(desc(value))
      
      #if tie for dominant phyiographic region, default to CAT value
      if (phys_reg_comid_unit_ties[[1,3]] > phys_reg_comid_unit_ties[[2,3]]) {
        phys_reg_comid_unit <- phys_reg_comid_unit_ties[1, ]
      } else {
        phys_reg_comid_unit_tied <- phys_reg_comid %>%
          filter(unit == "CAT")
        phys_reg_comid_unit <- 
          phys_reg_comid_unit_tied[which.max(phys_reg_comid_unit_tied$value), ]
        phys_reg_comid_unit <- mutate(phys_reg_comid_unit, unit == j)
      }
      dom_phys_reg_comid <- bind_rows(dom_phys_reg_comid, phys_reg_comid_unit)
    }
    dom_phys_reg <- bind_rows(dom_phys_reg, dom_phys_reg_comid)
  }
  
  dom_phys_reg$region <- as.numeric(dom_phys_reg$region)
  phys_region <- dom_phys_reg %>%
    mutate(label = paste0(unit, "_PHYSIO")) %>%
    arrange(COMID, label) %>%
    select(COMID, label, region) %>%
    pivot_wider(names_from = "label", values_from = "region")
  
  #remove unwanted feature variables and re-join long-term average datasets
  data <- data %>%
    select(-ends_with(".y"), -ID, -starts_with("..."), -contains("_S1"), 
           -contains("_PHYSIO_"), -contains("SOHL"), -contains("NDAMS"), 
           -contains("STORAGE"), -contains("MAJOR"), -contains("_TAV_"), 
           -contains("_PPT_"), -contains("WILDFIRE")) %>%
    left_join(phys_region, by = "COMID") %>%
    left_join(land_cover, by = "COMID") %>%
    left_join(dams, by = "COMID") %>%
    left_join(weather, by = "COMID") %>%
    left_join(wildfire, by = "COMID")
  
  #rename duplicated headers (if any)
  rename_dup_headers <- list()
  for (i in 1:ncol(data)) {
    header <- names(data)[i]
    if (str_sub(header, -2) == ".x") {
      new_header <- str_sub(header, 1, -3)
    } else {
      new_header <- header
    }
    rename_dup_headers[[i]] <- new_header
  }
  rename_dup_headers <- as.character(rename_dup_headers)
  names(data) <- rename_dup_headers
  
  return(data)
}
