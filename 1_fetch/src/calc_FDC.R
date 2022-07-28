calc_FDCmetrics <- function(site_num, clean_daily_flow, yearType, 
                            drainArea_tab, NE_probs, digits = 3,
                            seasonal = FALSE, season_months = NULL,
                            stat_type = 'POR', year_start, out_format = 'EflowStats',
                            threshold_type = 'high'){
  #' @description function to compute FDC-based flow metrics
  #' 
  #' @param site_num character string containing the gage number. Must match a 
  #' row in the column "site_no" of clean_daily flow
  #' @param clean_daily_flow table of flows for each gage
  #' @param yearType either 'water' for water year (month 10), or a numeric month
  #' @param drainArea_tab table of drainage areas
  #' @param NE_probs numeric vector of non-exceedance probabilities
  #' @param digits number of decimal places to use in round
  #' @param seasonal logical indicating if the metrics should be computed by season or not
  #' @param season_months numeric vector with 12 elements specifying which
  #' months should be used for each season. season 1 is first 3 elements, 
  #' season 2 is the next 3, ...
  #' @param stat_type 'ATS' for annual timeseries or 'POR' for period of record
  #' @param year_start numeric month at which the year should begin
  #' @param out_format format for the output table of metrics. The default matches
  #' the EflowStats format. Use 'pivot' for a simpler table.
  #' @param threshold_type one of either 'high' or 'low' indicating if the threshold
  #' should consider all flows higher or lower than the threshold.
  #' 
  #' @return table of FDC metrics for each gage
  
  message(paste('starting site', site_num))
  data <- clean_daily_flow %>%
    filter(site_no == site_num)
  
  #Validate data
  if (yearType == 'water' & year_start == 10){
    #use the same function as EflowStats
    data <- validate_data(data, yearType)
  }else{
    #Use user-defined validation based on EflowStats function
    data <- validate_data_yr_start(data, year_start)
  }
  
  #add column indicating groups of continuous years
  data <- make_data_groups(data)
  
  #Add columns needed to compute seasonal information
  if(seasonal){
    data <- add_season_cols(data, season_months)
  }
  
  if(!seasonal){
    drain_area <- drainArea_tab %>%
      filter(site_no == site_num) %>%
      pull(drainArea)
  }
  
  #Get the non-exceedance flow values (mh15 style metrics)
  NE_flows <- quantile(data$discharge, probs = NE_probs, type = 6, names = F)
  
  #Compute metrics for each NE_flows threshold
  #dh, fh, and vh are for duration, frequency, and volume of high flows. 
  #fdc is for flow-duration curve. 
  #1 and 2 are two different volume metrics (volume and max flow)
  if(seasonal){
    dhfdc_s <- fhfdc_s <- vhfdc1_s <- vhfdc2_s <- vector('numeric', 
                                                         length=length(NE_flows)*4)
    if(threshold_type == 'low'){
      #volume greater than threshold
      vhfdc3_s <- vector('numeric', length=length(NE_flows)*4)
    }
  }else{
    dhfdc <- fhfdc <- vhfdc1 <- vhfdc2 <- vector('numeric', length=length(NE_flows))
    if(threshold_type == 'low'){
      #volume greater than threshold
      vhfdc3 <- vector('numeric', length=length(NE_flows))
    }
  }
  for (i in 1:length(NE_flows)){
    if(seasonal){
      #get indices to store the 4 seasonal fractions for this NE_flows value
      inds_flow_i <- (1+(i-1)*4):(4+(i-1)*4)
      
      #prepare data for duration and volume calculations
      data_processed <- prep_seasonal_data(data, NE_flows[i], 
                                           type = threshold_type, digits = digits)
      #to compute volume above threshold for low flow metrics
      if(threshold_type == 'low'){
        data_processed_high <- prep_seasonal_data(data, NE_flows[i], 
                                             type = 'high', digits = digits)
      }
      
      #calculate metrics
      dhfdc_s[inds_flow_i] <- calc_dhfdc_metrics(data_processed, NE_flows[i],
                                                             stat_type, seasonal)
      fhfdc_s[inds_flow_i] <- calc_fhfdc_metrics(data, NE_flows[i],
                                                             stat_type, seasonal)
      vhfdc <- calc_vhfdc_metrics(data_processed, NE_flows[i], stat_type, seasonal,
                                  type2 = threshold_type)
      vhfdc1_s[inds_flow_i] <- vhfdc[[1]]
      vhfdc2_s[inds_flow_i] <- vhfdc[[2]]
      #compute volume above threshold for low flow metrics
      if(threshold_type == 'low'){
        vhfdc <- calc_vhfdc_metrics(data_processed_high, NE_flows[i], stat_type, seasonal,
                                    type2 = 'high')
        vhfdc3_s[inds_flow_i] <- vhfdc[[1]]
      }
    }else{
      #prepare data for duration and volume calculations
      data_processed <- prep_data(data, NE_flows[i], 
                                  type = threshold_type, digits = digits)
      #to compute volume above threshold for low flow metrics
      if(threshold_type == 'low'){
        data_processed_high <- prep_data(data, NE_flows[i], 
                                         type = 'high', digits = digits)
      }
      
      #calculate metrics
      dhfdc[i] <- calc_dhfdc_metrics(data_processed, NE_flows[i], stat_type, seasonal)
      fhfdc[i] <- calc_fhfdc_metrics(data, NE_flows[i], stat_type, seasonal)
      vhfdc <- calc_vhfdc_metrics(data_processed, NE_flows[i], stat_type, seasonal,
                                  type2 = threshold_type)
      vhfdc1[i] <- vhfdc[[1]][1]
      vhfdc2[i] <- vhfdc[[2]][1]
      #compute volume above threshold for low flow metrics
      if(threshold_type == 'low'){
        vhfdc <- calc_vhfdc_metrics(data_processed_high, NE_flows[i], stat_type, seasonal,
                                    type2 = 'high')
        vhfdc3[i] <- vhfdc[[1]][1]
      }
    }
  }
  rm(i)
  
  #Compile metrics for output data
  #non-seasonal FDC magnitudes and volumes are divided by drainage area
  #all values rounded
  if(seasonal){
    dhfdc_s <- round(dhfdc_s, digits)
    fhfdc_s <- round(fhfdc_s, digits)
    vhfdc1_s <- round(vhfdc1_s, digits)
    vhfdc2_s <- round(vhfdc2_s, digits)
    
    season_names <- c(unlist(lapply(X = paste0('dhfdc_q', NE_probs, '_s'), 
                                    FUN = paste0, seq(1,4,1))),
                      unlist(lapply(X = paste0('fhfdc_q', NE_probs, '_s'), 
                                    FUN = paste0, seq(1,4,1))),
                      unlist(lapply(X = paste0('vhfdc1_q', NE_probs, '_s'), 
                                    FUN = paste0, seq(1,4,1))),
                      unlist(lapply(X = paste0('vhfdc2_q', NE_probs, '_s'), 
                                    FUN = paste0, seq(1,4,1))))
    
    if (threshold_type == 'low'){
      vhfdc3_s <- round(vhfdc3_s, digits)
      season_names <- c(season_names,
                        unlist(lapply(X = paste0('vhfdc3_q', NE_probs, '_s'), 
                                      FUN = paste0, seq(1,4,1))))
    }
  }else{
    mhfdc <- round(NE_flows/drain_area, digits)
    dhfdc <- round(dhfdc, digits)
    fhfdc <- round(fhfdc, digits)
    vhfdc1 <- round(vhfdc1/drain_area, digits)
    vhfdc2 <- round(vhfdc2/drain_area, digits)
    annual_names <- c(paste0('mhfdc_q', NE_probs),
                      paste0('dhfdc_q', NE_probs),
                      paste0('fhfdc_q', NE_probs),
                      paste0('vhfdc1_q', NE_probs),
                      paste0('vhfdc2_q', NE_probs))
    
    if (threshold_type == 'low'){
      vhfdc3 <- round(vhfdc3/drain_area, digits)
      annual_names <- c(annual_names,
                        paste0('vhfdc3_q', NE_probs))
    }
  }
  
  #Make a data.frame of values to match the format of the EflowStats metrics
  if(seasonal){
    if (threshold_type == 'low'){
      statistic <- c(dhfdc_s, fhfdc_s, vhfdc1_s, vhfdc2_s, vhfdc3_s)
    }else{
      statistic <- c(dhfdc_s, fhfdc_s, vhfdc1_s, vhfdc2_s)
    }
    out_data <- data.frame(indice = season_names,
                           statistic = statistic,
                           site_num = site_num)
  }else{
    if (threshold_type == 'low'){
      statistic <- c(mhfdc, dhfdc, fhfdc, vhfdc1, vhfdc2, vhfdc3)
    }else{
      statistic <- c(mhfdc, dhfdc, fhfdc, vhfdc1, vhfdc2)
    }
    out_data <- data.frame(indice = annual_names,
                           statistic = statistic,
                           site_num = site_num)
  }
  
  if (out_format == 'pivot'){
    out_data <- pivot_wider(out_data, names_from = 'indice', values_from = 'statistic')
  }
  
  return(out_data)
}

make_data_groups <- function(data){
  #' @description The EflowStats::find_events function assumes the timeseries is continuous. 
  #' Years do not have to be continuous, so that is handled here by making
  #' groups of continuous years. The function adds a groups column to data.
  #' 
  #' @param data tbl containing the column "year_val" 
  #' 
  #' @return data with a groups column added.
  
  yr_srt <- sort(unique(data$year_val))
  yr_diff <- diff(yr_srt)
  if (any(yr_diff > 1)){
    #There are gaps in years.
    #Make numbers to be assigned to groups
    grp_nums <- seq(1,length(which(yr_diff > 1))+1,1)
    data$groups <- NA
    for (g in 1:length(grp_nums)){
      if (g == 1){
        grp_yrs <- yr_srt[yr_srt < yr_srt[which(yr_diff > 1)+1][g]]
      }else if (g == length(grp_nums)){
        grp_yrs <- yr_srt[yr_srt >= yr_srt[which(yr_diff > 1)+1][g-1]]
      }else{
        grp_yrs <- yr_srt[which((yr_srt < yr_srt[which(yr_diff > 1)+1][g]) & (yr_srt >= yr_srt[which(yr_diff > 1)+1][g-1]))]
      }
      data$groups[data$year_val %in% grp_yrs] <- grp_nums[g]
    }
    rm(g, grp_nums, grp_yrs)
  }else{
    #No gaps. Use one group
    data$groups <- 1
  }
  
  return(data)
}

add_season_cols <- function(data, season_months){
  #' @description adds columns for each month and season to data
  #' 
  #' @param data tbl containing a column for "date" 
  #' @param season_months numeric vector with 12 elements specifying which
  #' months should be used for each season. season 1 is first 3 elements, 
  #' season 2 is the next 3, ...
  #' 
  #' @return data with season columns added
  
  if(is.null(season_months)){
    stop('season_months must be specified for seasonal FDC-based metrics')
  }
  #Add column for month and season
  data <- data %>% mutate(month_val = lubridate::month(date),
                          season = dplyr::case_when(month_val %in% season_months[1:3] ~ 1,
                                                    month_val %in% season_months[4:6] ~ 2,
                                                    month_val %in% season_months[7:9] ~ 3,
                                                    month_val %in% season_months[10:12] ~ 4,
                                                    TRUE ~ 0))
  
  if(any(data$season == 0)){
    stop(paste('some dates or months are misspecified for site', site_num))
  }
  if(!all(c(1,2,3,4) %in% unique(data$season))){
    stop(paste('some seasons are not present for site', site_num))
  }
  
  return(data)
}

prep_seasonal_data <- function(data, NE_flow, type = 'high', digits = 3){
  #' @description prepares the data by finding events that meet the NE_flow threshold,
  #' and adds a column of event numbers to data.
  #' 
  #' @param data tbl containing columns for "groups" and "discharge"
  #' @param NE_flow flow threshold to use. A small buffer will be added because
  #' the EflowStats function does not grab the exact value of the threshold
  #' @param type threshold type to use. Can be "low" for flows less than the threshold
  #' or "high" for flows greater than the threshold.
  #' 
  #' @return data with a column added for the event number
  
  #Find events within each group of continuous years
  data$event <- dplyr::do(dplyr::group_by(data, groups), 
                          {find_events(.$discharge, 
                                       threshold = ifelse(type == 'high', NE_flow, NE_flow + 10^-digits), 
                                       type = type)})$event
  
  #Trim events of unknown duration/volume
  data <- trim_events(data)
  
  #renumber the NA events and flows as 0. This helps for next renumbering step
  data[is.na(data$event), c('discharge', 'event')] <- 0
  
  #renumber the events to be monotonic. Gaps in event number are okay.
  data <- make_monotonic_events(data)
  
  return(data)
}

prep_data <- function(data, NE_flow, type = 'high', digits = 3){
  #' @description prepares the data by finding events that meet the NE_flow threshold,
  #' and adds a column of event numbers to data.
  #' 
  #' @param data tbl containing columns for "groups" and "discharge"
  #' @param NE_flow flow threshold to use. A small buffer will be added because
  #' the EflowStats function does not grab the exact value of the threshold
  #' @param type threshold type to use. Can be "low" for flows less than the threshold
  #' or "high" for flows greater than the threshold.
  #' 
  #' @return data with a column added for the event number
  
  #Find events within each group of continuous years
  data <- dplyr::do(dplyr::group_by(data, groups), 
                    {find_events(.$discharge, 
                                 threshold = ifelse(type == 'high', NE_flow, NE_flow + 10^-digits), 
                                 type = type)})
  
  #Trim events of unknown duration/volume
  data <- trim_events(data)
  
  #renumber the NA events and flows as 0. This helps for next renumbering step
  data[is.na(data$event), c('flow', 'event')] <- 0
  
  #renumber the events to be monotonic. Gaps in event number are okay.
  data <- make_monotonic_events(data)
  
  #drop event 0
  data <- data[-which(data$event == 0),]
  
  return(data)
}

trim_events <- function(data){
  #' @description removes events if they are at the beginning or end of the record
  #' 
  #' @param data tbl containing columns for "groups" and "event
  #' 
  #' @return data with events removed if they are at the beginning or end of the record
  
  #Trim events of unknown duration/volume
  for (g in 1:max(data$groups)){
    grp_evnts <- data$event[data$groups == g]
    if (!is.na(grp_evnts[1])){
      data$event[data$groups == g][which((grp_evnts == grp_evnts[1]) == TRUE)] <- NA
    }
    if (!is.na(grp_evnts[length(grp_evnts)])){
      data$event[data$groups == g][which((grp_evnts == grp_evnts[length(grp_evnts)]) == TRUE)] <- NA
    }
  }
  
  return(data)
}

make_monotonic_events <- function(data){
  #' @description places events in ascending order
  #' 
  #' @param data tbl containing columns for "groups" and "event
  #' 
  #' @return data with events in ascending order
  
  if (max(data$groups) > 1){
    for (g in 2:max(data$groups)){
      #get indices of events in group g (event 0 is not a real event)
      inds_event_g <- which((data$groups == g) & (data$event > 0))
      data$event[inds_event_g] <- data$event[inds_event_g] + max(
        data$event[data$groups < g])
    }
  }
  
  return(data)
}

get_year_frac <- function(seasonal_var, metric_colname, 
                          check_event_colname, na_method = FALSE){
  #' @description function adds a year fraction for each season to the attribute table.
  #' 
  #' @param seasonal_var tbl with columns for "year_val" and the metric_colname
  #' @param metric_colname is the column with the metric
  #' @param check_event_colname is the column to check for no events
  #' @param na_method (TRUE/FALSE) should the NA method be used instead of sum
  #' to check for years with no events?
  #' 
  #' @return seasonal_var with the year fraction column added
  
  seasonal_var$year_frac <- 0
  
  for(j in 1:length(unique(seasonal_var$year_val))){
    #get indices of the 4 seasons in this year
    inds_seasons <- c((1+(4*(j-1))):(4*j))
    
    #There can be years with no events, so need to check for those first
    #is.na or sum check can be used to see if a year has events, depending on the metric.
    if (na_method){
      check_invalid_year <- all(is.na(seasonal_var[inds_seasons, check_event_colname]))
    }else{
      check_invalid_year <- (sum(seasonal_var[inds_seasons, check_event_colname]) == 0)
    }
    if(check_invalid_year){
      seasonal_var$year_frac[inds_seasons] <- NA
    }else{
      if (na_method){
        #Set the NAs to 0 so the fractions are 0 for those seasons
        seasonal_var[inds_seasons, metric_colname][is.na(seasonal_var[inds_seasons, metric_colname])] <- 0
      }
      seasonal_var$year_frac[inds_seasons] <- seasonal_var[inds_seasons, metric_colname]/sum(seasonal_var[inds_seasons, metric_colname])
    }
  }

  return(seasonal_var)
}

seasonal_mean <- function(seasonal_var){
  #' @description computes the mean for each season over all years with data
  #' 
  #' @param seasonal_var tbl with columns for "year_frac" and "season"
  #' 
  #' @return numeric vector with 4 elements that contain the seasonal mean.
  
  metric <- c(mean(seasonal_var$year_frac[seasonal_var$season == 1], na.rm = TRUE),
              mean(seasonal_var$year_frac[seasonal_var$season == 2], na.rm = TRUE),
              mean(seasonal_var$year_frac[seasonal_var$season == 3], na.rm = TRUE),
              mean(seasonal_var$year_frac[seasonal_var$season == 4], na.rm = TRUE))
  
  return(metric)
}

get_seasonal_frac <- function(seasonal_metric, na_method = TRUE){
  #' @description computes the fraction per season for the provided seasonal metric values
  #' 
  #' @param seasonal_metric vector containing 4 seasonal metric values
  #' @param na_method (TRUE/FALSE) should the NA method be used instead of sum
  #' to check for years with no events?
  #' 
  #' @return 4 element vector whose elements sum to 1
  
  #vector to store seasonal fractions of a metric
  fracs <- vector('numeric', length = 4)
  
  if(na_method){
    check_invalid <- all(is.na(seasonal_metric))
  }else{
    check_invalid <- (sum(seasonal_metric) == 0)
  }
  
  if (check_invalid){
    #No events in any season
    fracs[1:4] <- NA
  }else{
    if(na_method){
      #Change the NAs to 0s and compute the fractions per season
      seasonal_metric[is.na(seasonal_metric)] <- 0
    }
    fracs[1:4] <- seasonal_metric/sum(seasonal_metric)
  }
  
  return(fracs)
}

assign_mnx_event <- function(seasonal_mnxQ, type){
  #' @description When events overlap in season, this function assigns the min (max) 
  #' to the season with the smaller (larger) value for that event.
  #' 
  #' @param seasonal_mnxQ dataframe with columns for event and mnxQ (min or max flow)
  #' @param type indicates if the second volume metric should be computed as 
  #' "high" for computing max flow or "low" for computing min flow for the event
  #' 
  #' @return seasonal_mnxQ with reassigned events
  
  for(d in 1:length(unique(seasonal_mnxQ$event))){
    #get indices of event d
    inds_event_d <- which(seasonal_mnxQ$event == unique(seasonal_mnxQ$event)[d])
    if(length(inds_event_d) > 1){
      #Find index of min or max flow
      if (type == 'high'){
        ind_mnx <- which(seasonal_mnxQ$mnxQ[inds_event_d] == max(seasonal_mnxQ$mnxQ[inds_event_d]))
      }else{
        ind_mnx <- which(seasonal_mnxQ$mnxQ[inds_event_d] == min(seasonal_mnxQ$mnxQ[inds_event_d]))
      }
      if(length(ind_mnx) > 1){
        #Assign to the first season because that's when the event started
        #Other seasons get converted to NA (these are deleted later)
        seasonal_mnxQ[inds_event_d,][-1, 'mnxQ'] <- NA
      }else{
        #Assign to season in which the min or max occurred
        #Other season gets converted to NA (these are deleted later)
        seasonal_mnxQ[inds_event_d,][-ind_mnx, 'mnxQ'] <- NA
      }
      rm(ind_mnx)
    }
  }
  
  return(seasonal_mnxQ)
}

calc_dhfdc_metrics <- function(data, NE_flow, stat_type = 'POR', seasonal = FALSE){
  #' @description Get the duration of events above each of the NE_flows (dh17 style metrics)
  #' Seasonal durations are relative to the total time in that season, not the 
  #' total duration of the event. On average, that should reflect the event
  #' duration in that season.
  #' 
  #' @param data output of prep_data or prep_seasonal data
  #' @param NE_flow numeric flow value
  #' @param stat_type 'ATS' for annual timeseries or 'POR' for period of record
  #' @param seasonal logical indicating if the metrics should be computed by season or not
  #' 
  #' @return high flow duration metrics for NE_flow.
  
  if(seasonal){
    #vector to store the fractions for each season
    dhfdc <- vector('numeric', length = 4)
    
    event_durations <- dplyr::summarize(dplyr::group_by(data, season, event), 
                                        duration = length(event),
                                        .groups = 'drop_last')
    
    #Set event 0 to NA duration. This ensures that all seasons in all years 
    #have data, even if they do not have events.
    event_durations$duration[event_durations$event == 0] <- NA
    
    #Compute seasonal fractions using ATS or POR approach:
    #Computes annual fraction per season, then averages over all years
    if (stat_type == 'ATS'){
      #Compute duration for each event and add to dataframe
      seasonal_durations <- dplyr::mutate(dplyr::group_by(data, season, event), 
                                          duration = length(event))
      
      #get to a table of one entry per event in each season
      seasonal_durations <- dplyr::summarize(dplyr::group_by(seasonal_durations,
                                                             year_val, season, 
                                                             event), 
                                             avg = mean(duration, na.rm=TRUE), .groups = 'keep')
      
      seasonal_durations$avg[seasonal_durations$event == 0] <- NA
      
      #average for each season in each year
      seasonal_durations <- dplyr::summarize(dplyr::group_by(seasonal_durations, 
                                                             year_val, season), 
                                             avg = mean(avg, na.rm=TRUE), .groups = 'keep')
      
      seasonal_durations$avg[is.na(seasonal_durations$avg)] <- 0
      
      seasonal_durations <- get_year_frac(seasonal_durations, 'avg', 'avg', FALSE)
      
      dhfdc[1:4] <- seasonal_mean(seasonal_durations)
    }else{
      seasonal_durations <- dplyr::summarize(dplyr::group_by(event_durations, season), 
                                             avg = mean(duration, na.rm=TRUE)) %>%
        arrange(season) %>%
        pull(avg)
      
      dhfdc[1:4] <- get_seasonal_frac(seasonal_durations, TRUE)
    }
  }else{
    if (nrow(data) > 0){
      event_durations <- dplyr::summarize(dplyr::group_by(data, event), 
                                          duration = length(event))
      dhfdc <- mean(event_durations$duration)
    }else{
      dhfdc <- NA
    }
  }
  
  return(dhfdc)
}


calc_fhfdc_metrics <- function(data, NE_flow, stat_type = 'POR', seasonal = FALSE){
  #' @description Get the number of events above each of the NE_flows (fh1 style metrics)
  #' 
  #' @param data output of prep_data or prep_seasonal data
  #' @param NE_flow numeric flow value
  #' @param stat_type 'ATS' for annual timeseries or 'POR' for period of record
  #' @param seasonal logical indicating if the metrics should be computed by season or not
  #' 
  #' @return high flow frequency metrics for NE_flow.
  
  if(seasonal){
    #vector to store the fractions for each season
    fhfdc <- vector('numeric', length = 4)
    
    seasonal_counts <- dplyr::do(dplyr::group_by(data, year_val, season), 
                                 {find_events(.$discharge, 
                                              threshold = NE_flow, type = "high")
                                 }) %>%
      dplyr::arrange(year_val, season)
    
    #changing NAs to 0s so that seasons with no events are counted as 0 
    #instead of NA
    seasonal_counts$event[is.na(seasonal_counts$event)] <- 0
    
    #This function double counts events when they overlap in season.
    seasonal_counts <- dplyr::summarize(dplyr::group_by(seasonal_counts, year_val, season), 
                                        numEvents = max(event), .groups = 'keep')
    
    #Compute seasonal fractions using ATS or POR approach:
    #Computes annual fraction per season, then averages over all years
    if (stat_type == 'ATS'){
      seasonal_counts <- get_year_frac(seasonal_counts, 'numEvents', 'numEvents', FALSE)
      
      fhfdc[1:4] <- seasonal_mean(seasonal_counts)
    }else{
      #Computes seasonal average duration over all years, then fraction per season.
      seasonal_counts <- dplyr::summarize(group_by(seasonal_counts, season),
                                         avg=mean(numEvents, na.rm=TRUE)) %>%
        arrange(season) %>%
        pull(avg)
      
      fhfdc[1:4] <- get_seasonal_frac(seasonal_counts, FALSE)
    }
  }else{
    yearly_counts <- dplyr::do(dplyr::group_by(data, year_val), 
                               {find_events(.$discharge, 
                                            threshold = NE_flow, type = "high")
                               })
    #changing NAs to 0s so that years with no events are counted as 0s 
    #instead of being omitted
    yearly_counts$event[is.na(yearly_counts$event)] <- 0
    yearly_counts <- dplyr::summarize(dplyr::group_by(yearly_counts, year_val), 
                                      numEvents = max(event))
    fhfdc <- mean(yearly_counts$numEvents)
  }
  
  return(fhfdc)
}


calc_vhfdc_metrics <- function(data, NE_flow, stat_type = 'POR', seasonal = FALSE,
                               type2 = 'high'){
  #' @description Get the flow volume and average of the max flow for events above each of 
  #' the NE_flows (mh21 style metrics for total flow instead of flow above threshold, 
  #' and mh24 style metrics for max flow)
  #' 
  #' @param data output of prep_data or prep_seasonal data
  #' @param NE_flow numeric flow value
  #' @param stat_type 'ATS' for annual timeseries or 'POR' for period of record
  #' @param seasonal logical indicating if the metrics should be computed by season or not
  #' @param type2 indicates if the second volume metric should be computed as 
  #' "high" for computing max flow or "low" for computing min flow for the event
  #' 
  #' @return flow volume and peak flow metrics for NE_flow.
  
  if(seasonal){
    #vector to store the fractions for each season
    #1 is for volume, 2 is for max or min flow
    vhfdc1 <- vhfdc2 <- vector('numeric', length = 4)
    
    #This function splits event volume over seasons when they overlap in season.
    seasonal_volumes <- dplyr::summarize(dplyr::group_by(data, year_val, season), 
                                         numEvents = length(unique(event[event != 0])), 
                                         totalFlow = sum(discharge),
                                         .groups = 'keep') %>%
      mutate(flow_event = totalFlow/numEvents)
    
    #Get the min or max flow for each event
    seasonal_mnxQ <- dplyr::summarize(dplyr::group_by(data, year_val,
                                                      season, event),
                                      .groups = 'drop_last', 
                                      mnxQ = ifelse(type2 == 'high', 
                                                    max(discharge),
                                                    min(discharge))) %>%
      dplyr::arrange(year_val, season)
    
    #Assign the min (max) to the season with the smaller (larger) value for that event.
    seasonal_mnxQ <- assign_mnx_event(seasonal_mnxQ, type = type2)
    
    #Seasons with no events have flows reported. Want those to not be included 
    #in the calculation of average seasonal maximums
    seasonal_mnxQ[seasonal_mnxQ$event == 0, c('event', 'mnxQ')] <- NA
    
    #Compute seasonal fractions using ATS or POR approach:
    #Computes annual fraction per season, then averages over all years
    if (stat_type == 'ATS'){
      seasonal_volumes <- get_year_frac(seasonal_volumes, 'flow_event', 'numEvents', FALSE)
      
      vhfdc1[1:4] <- seasonal_mean(seasonal_volumes)
      
      #Compute the average of the min or max flow in each season
      seasonal_mnxQ <- dplyr::summarize(dplyr::group_by(seasonal_mnxQ, year_val, season),
                                        season_mnx = suppressWarnings(
                                          ifelse(type2 == 'high', 
                                                 max(mnxQ, na.rm = TRUE),
                                                 min(mnxQ, na.rm = TRUE))),
                                        .groups = 'keep')
      #Some seasons do not have events. Set to NA
      seasonal_mnxQ[seasonal_mnxQ$season_mnx == -Inf, "season_mnx"] <- NA
      
      seasonal_mnxQ <- get_year_frac(seasonal_mnxQ, 'season_mnx', 'season_mnx', TRUE)
      
      vhfdc2[1:4] <- seasonal_mean(seasonal_mnxQ)
    }else{
      #Computes seasonal average over all years, then fraction per season.
      if (max(seasonal_volumes$totalFlow) == 0){
        #Handle case when all flows are 0 to avoid a divide 0/0
        seasonal_volumes <- dplyr::summarize(group_by(seasonal_volumes, season),
                                             avg = sum(numEvents)) %>%
          arrange(season) %>%
          pull(avg)
        
        vhfdc1[1:4] <- get_seasonal_frac(seasonal_volumes, TRUE)
        
        #Compute the indicator of the flow events in each season
        seasonal_mnxQ <- volume_indicator(vhfdc1)
        
      }else{
        seasonal_volumes <- dplyr::summarize(group_by(seasonal_volumes, season),
                                             avg = mean(flow_event, na.rm=TRUE)) %>%
          arrange(season) %>%
          pull(avg)
        
        vhfdc1[1:4] <- get_seasonal_frac(seasonal_volumes, TRUE)
        
        #Compute the average of the maximum flow in each season
        seasonal_mnxQ <- dplyr::summarize(group_by(seasonal_mnxQ, season),
                                          avg = mean(mnxQ, na.rm=TRUE)) %>%
          arrange(season) %>%
          pull(avg)
      }
      
      vhfdc2[1:4] <- get_seasonal_frac(seasonal_mnxQ, TRUE)
    }
  }else{
    if (nrow(data) > 0){
      #volume
      num_events <- length(unique(data$event))
      total_flow <- sum(data$flow)
      vhfdc1 <- total_flow/num_events
      
      #min or max flow 
      event_mnx <- dplyr::group_by(data[c("flow", "event")], event)
      event_mnx <- dplyr::summarize(event_mnx, mnxQ = ifelse(type2 == 'high', 
                                                             max(flow), 
                                                             min(flow)))
      vhfdc2 <- mean(event_mnx$mnxQ)
    }else{
      #no events
      vhfdc1 <- NA
      vhfdc2 <- NA
    }
  }
  
  return(list(vhfdc1, vhfdc2))
}

volume_indicator <- function(seasonal_volume){
  #' @description binary indicator of if the seasonal volume is greater than 0.
  #' 
  #' @param seasonal_volume numeric vector
  #' 
  #' @return binary vector of same length as seasonal_volume
  
  ind = vector('numeric', length(seasonal_volume))
  for(i in 1:length(seasonal_volume)){
    ind[i] = ifelse(seasonal_volume[i] == 0, 0, 1)
  }
  
  return(ind)
}

calc_season_average <- function(seasonal_var, metric_colname){
  #' @description computes the average of the metric for each season
  #' 
  #' @param seasonal_var dataframe with columns for the season, and metric_colname
  #' @param metric_colname character column name
  #' 
  #' @return 
  
  seasonal_var <- dplyr::summarize(dplyr::group_by(seasonal_var, season), 
                                         avg = mean(metric_colname, na.rm=TRUE)) %>%
    arrange(season) %>%
    pull(avg)
  
  return(seasonal_var)
}
