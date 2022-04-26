has_data_check <- function(site_nums, parameterCd, endDate){
  ##check to see if all sites actually have daily flow, peak flow data, and a drainage area.  There are
  ##gages in gagesii that do not have one or the other, so screen these out before we try
  ##to download the data
  dv_screen <- whatNWISdata(siteNumber = site_nums, parameterCd = parameterCd, service = "dv",
                          convertType = FALSE) %>%
    #Check specifically that the stat_cd for mean flow is available
    filter(stat_cd == '00003') %>%
    #Drop sites with start dates after 2020-09-30
    filter(as.Date(begin_date) < endDate)
  pk_screen <- whatNWISdata(siteNumber = site_nums, service = "pk", convertType = FALSE)
  sites_with_data <- intersect(dv_screen$site_no, pk_screen$site_no)
  
  #remove sites that do not have a drainage area in NWIS
  rm_sites <- which(is.na(readNWISsite(siteNumbers = sites_with_data)$drain_area_va))
  sites_with_data <- sites_with_data[-rm_sites] 

  return(sites_with_data)
}

filter_complete_years <- function(screen_daily_flow, complete_years){
  
  complete_yr_count <- screen_daily_flow %>% 
    filter(!is.na(complete_yrs)) %>%  
    group_by(site_no) %>%
    count()
  message("complete yr ok")
  
  keep_sites <- complete_yr_count %>%
    filter(n >= complete_years) %>%
    pull(site_no)
  return(keep_sites)
}

get_nwis_daily_data <- function(site_num, outdir, parameterCd, 
                                startDate, endDate, timeout = 60){
  #'@description downloads daily NWIS data and renames columns. 
  #'Download is retried 3 times if it takes longer than timeout time. An error 
  #'message is printed if it does not succeed.
  #'
  #'@param site_num character string ID number for the NWIS site to check
  #'@param outdir file path to save the downloaded data
  #'@param parameterCd NWIS parameter code
  #'@param startDate earliest date to download
  #'@param endDate latest date to download
  #'@param timeout timeout time in seconds
  #'
  #'@return filepath to csv of downloaded data
  
  message(paste('starting site', site_num))
  
  #use tryCatch to handle odd download hangs
  #set counter for number of retries - we'll stop after 3 retries
  counter <- 0
  while (counter < 3){
    ##read daily NWIS data, and save to csv file.
    data_out <- catch_download_timeout_daily(timeout, site_num, parameterCd, 
                                       startDate, endDate)
    if(is.null(data_out)){
      counter <- counter + 1
      if (counter == 3){
        stop(paste('Daily data download for site', site_num, 
                   'timed out after', timeout, 
                   'seconds. Increase time or investigate this site.'))
      }
    }else{
      #successful download. Break loop by setting counter to 3.
      counter <- 3
    }
  }
  ##renaming column names for discharge and discharge_cd
  names(data_out)[grep(pattern="X_.*\\cd", x=names(data_out))] <- "discharge_cd"
  names(data_out)[grep(pattern="X_",x=names(data_out))] <- "discharge"
  filepath <- file.path(outdir, paste0(site_num, "_dv.csv"))
  write_csv(data_out, file=filepath)
  return(filepath)
}

catch_download_timeout_daily <- function(time, site_num, parameterCd, 
                                         startDate, endDate){
  #'@description downloads daily NWIS data.
  #'
  #'@param time timeout time in seconds
  #'@param site_num character string ID number for the NWIS site to check
  #'@param parameterCd NWIS parameter code
  #'@param startDate earliest date to download
  #'@param endDate latest date to download
  #'
  #'@return table of downloaded data if successful. NA if download resulted in an error
  
  tryCatch(
    expr = {result <- R.utils::withTimeout(expr = readNWISdv(site_num, parameterCd, 
                                                      startDate, endDate), 
                                    timeout = time, 
                                    onTimeout = 'error')
    message('Download successful.')
    return(result)
    },
    error = function(e){
      message('Download timed out. Retrying download.')
      return(NULL)
    }
  )
}

get_daily_flow_log <- function(files_in, file_out) {
  message(paste('generating log for dataRetrieval daily flow request'))
  daily_flow_list <- purrr::map(files_in, prescreen_daily_data, prov_rm = FALSE)
  daily_flow_df <- bind_rows(daily_flow_list)
  daily_flow_log <- daily_flow_df %>%
    group_by(site_no) %>%
    summarize(total_days = n(),
              approved_days = sum(discharge_cd == "A"),
              start_date = min(Date), 
              end_date = max(Date), 
              mean_all_flow = mean(discharge, na.rm = TRUE), 
              sd_all_flow = sd(discharge, na.rm = TRUE), 
              mean_approved_flow = mean(discharge[discharge_cd == "A"], na.rm = TRUE), 
              sd_approved_flow = sd(discharge[discharge_cd == "A"], na.rm = TRUE))
  write_csv(daily_flow_log, file_out)
  return(file_out)
}

get_nwis_peak_data <- function(site_num, outdir, startDate, endDate, 
                               timeout = 60){
  #'@description downloads peak NWIS data and renames columns. 
  #'Download is retried 3 times if it takes longer than timeout time. An error 
  #'message is printed if it does not succeed.
  #'
  #'@param site_num character string ID number for the NWIS site to check
  #'@param outdir file path to save the downloaded data
  #'@param startDate earliest date to download
  #'@param endDate latest date to download
  #'@param timeout timeout time in seconds
  #'
  #'@return filepath to csv of downloaded data
  
  message(paste('starting site', site_num))
  
  #use tryCatch to handle odd download hangs
  #set counter for number of retries - we'll stop after 3 retries
  counter <- 0
  while (counter < 3){
    ##read NWIS peak data, and save to csv file.
    data_out <- catch_download_timeout_peak(timeout, site_num, startDate, endDate)
    
    if(is.null(data_out)){
      counter <- counter + 1
      if (counter == 3){
        stop(paste('Daily data download for site', site_num, 
                   'timed out after', timeout, 
                   'seconds. Increase time or investigate this site.'))
      }
    }else{
      #successful download. Break loop by setting counter to 3.
      counter <- 3
    }
  }
  
  ##Removing any NA dates or peak_va values - dates are returned as NA if day of occurrence is not known
  ## and peak_va is sometimes returned as NA if the gage height is known but not the discharge.
  data_out <- data_out[which(!is.na(data_out$peak_dt)), ]
  data_out <- data_out[which(!is.na(data_out$peak_va)), ]
  filepath <- file.path(outdir, paste0(site_num, "_pk.csv"))
  write_csv(data_out, file=filepath)
  return(filepath)
}

catch_download_timeout_peak <- function(time, site_num, 
                                         startDate, endDate){
  #'@description downloads NWIS peak flow data.
  #'
  #'@param time timeout time in seconds
  #'@param site_num character string ID number for the NWIS site to check
  #'@param startDate earliest date to download
  #'@param endDate latest date to download
  #'
  #'@return table of downloaded data if successful. NA if download resulted in an error
  
  tryCatch(
    expr = {result <- R.utils::withTimeout(expr = readNWISpeak(siteNumbers = site_num, 
                                                               startDate = startDate, 
                                                               endDate = endDate), 
                                           timeout = time, 
                                           onTimeout = 'error')
    return(result)
    },
    error = function(e){
      return(NULL)
    }
  )
}

get_peak_flow_log <- function(files_in, file_out) {
  message(paste('generating log for dataRetrieval peak flow request'))
  peak_flow_list <- suppressWarnings(
    purrr::map(files_in, read_csv, 
        col_types = cols(agency_cd = col_skip(), 
                         site_no = col_character(), 
                         peak_dt = col_date(format = "%Y-%m-%d"), 
                         peak_tm = col_skip(), 
                         peak_va = col_double(), 
                         peak_cd = col_skip(), 
                         gage_ht = col_skip(), 
                         gage_ht_cd = col_skip(), 
                         year_last_pk = col_skip(), 
                         ag_dt = col_skip(), 
                         ag_tm = col_skip(), 
                         ag_gage_ht = col_skip(), 
                         ag_gage_ht_cd = col_skip(), 
                         peak_dateTime = col_skip(), 
                         ag_dateTime = col_skip())))
  peak_flow_df <- bind_rows(peak_flow_list)
  peak_flow_log <- peak_flow_df %>%
    group_by(site_no) %>%
    summarize(num_peaks = n(), 
              first_date = min(peak_dt), 
              last_date = max(peak_dt), 
              max_peak = max(peak_va, na.rm = TRUE), 
              mean_peak = mean(peak_va, na.rm = TRUE), 
              sd_peak = sd(peak_va, na.rm = TRUE))
  write_csv(peak_flow_log, file_out)
  return(file_out)
}

prescreen_daily_data <- function(filename, prov_rm = TRUE){
  #loads data from file and removes provisional and estimated data if prov_rm = TRUE
  message('loading ', filename)
  d0 <- read_csv(filename,
                 name_repair = "minimal", 
                 col_types = cols(agency_cd = col_character(), 
                                  site_no = col_character(), Date = col_date(format = "%Y-%m-%d"), 
                                  discharge = col_number(), discharge_cd = col_character(), 
                                  dateTime = col_character(), tz_cd = col_character()))
  ##Handle sites with unexpected number of columns ( != 5 )
  if (ncol(d0) == 7) {
    #site has 2 columns named discharge and 2 named discharge_cd
    d1 <- d0 %>% select(1:5) %>% na.omit()
    d2 <- d0 %>% select(1:3, 6:7) %>% na.omit()
    
    data <- rbind(d1, d2)
    data <- data[order(data$Date),]
    
    #take only the unique records
    data <- unique(data)
    #check that each date has only one record
    # some dates have multiple and it seems like that's because of rounding to the tenths place
    # Keeping the first dataset
    data <- data[which(duplicated(data$Date) == FALSE),]
    
  }else if (ncol(d0) == 5) {
    data <- d0 %>% na.omit()
  }else if (ncol(d0) > 7) {
    stop(paste('A new site has > 2 discharge columns. Special handling is needed for file', filename))
  }else {
    data <- tibble(agency_cd = character(), site_no = character(), Date = Date(), 
                   discharge = numeric(), discharge_cd = character())
  }
  
  if (prov_rm == TRUE){
    ###remove any provisional data - this will be counted as 'no data'
    prov_data <- grep('P|e', data$discharge_cd)
    if(length(prov_data) > 0){data <- data[-prov_data, ]}
  }
  
  return(data)
}

screen_daily_data <- function(site, prescreen_data, year_start = 'water'){
  #screens data for complete years and outputs complete years by site
  if(year_start == 'water'){
    year_start <- 10
  }else if(!is.numeric(year_start)){
    stop('year_start must be numeric or "water"')
  }
  
  data <- filter(prescreen_data, site_no == site)
  
  if (nrow(data) > 0) {
    missing_data <- screen_flow_data(data.frame(site_no = data$site_no, 
                                                Date = data$Date,
                                                Value = data$discharge),
                                     water_year_start = year_start)
    complete_yrs <- missing_data %>%
      filter(n_missing_Q == 0) %>%
      select(Year)
    
    if(nrow(complete_yrs) > 0){
      data_out <- data.frame(site_no = unique(data$site_no),
                             complete_yrs = complete_yrs$Year)
    }else{
      data_out <- data.frame(site_no = unique(data$site_no),
                             complete_yrs=NA)
    } 
  }else {
    data_out <- data.frame(site_no = character(), 
                           complete_yrs = numeric())
  }
  
  return(data_out)
}

clean_daily_data <- function(site, prescreen_data, screen_daily_flow, yearType, year_start){
  ##remove NAs from data and remove incomplete years of data
  ##EflowStats requires the cleaned data to have dates in the first column and 
  ##discharge in the second column.
  message(paste('starting site', site))
  data <- prescreen_data %>%
    filter(site_no == site)
  
  #add a column for the water year based on year_start
  data$waterYear <- calc_water_year(data$Date, year_start)
  ##remove all data from years with data gaps
  keep_years <- screen_daily_flow %>%
    filter(site_no == site) %>%
    pull(complete_yrs)
  #noting that waterYear could = calendar year when year_start = 1
  data_sc <- data[which(data$waterYear %in% keep_years), ]
  
  df <- data.frame(date = as.Date(data_sc$Date), discharge = data_sc$discharge)
  ###run EflowStats validation to produce clean, ready to process data
  if (yearType == 'water' & year_start == 10){
    #use the EflowStats validation function
    clean_data <- validate_data(df, yearType)
  }else{
    #Use user-defined validation based on EflowStats function
    clean_data <- validate_data_yr_start(df, year_start)
  }
  clean_data$site_no <- unique(data_sc$site_no)
  return(clean_data)
}#end clean_daily_data function

validate_data_yr_start <- function(x, year_start){
  #based on EflowStats::validate_data()
  x <- x[, 1:2]
  col1_class <- class(x[, 1])
  col2_class <- class(x[, 2])
  if (col1_class != "Date" && col2_class != "numeric") {
    warning("First column of x must contain a vector of class date.\nSecond column of x must contain a vector of class numeric.")
    return(FALSE)
  }
  else if (col1_class != "Date") {
    warning("First column of x must contain a vector of class date.")
    return(FALSE)
  }
  else if (col2_class != "numeric" & col2_class != "integer") {
    warning("Second column of x must contain a vector of class numeric.")
    return(FALSE)
  }
  if (!(year_start %in% seq(1,12,1))) {
    warning("year_start must be numeric on 1:12")
    return(FALSE)
  }
  if (anyNA(x)) {
    warning("dataframe x cannot contain NA values")
    return(FALSE)
  }
  names(x) <- c("date", "discharge")
  x <- dplyr::arrange(x, date)
  if (year_start != 1) {
    x$year_val <- calc_water_year(x$date, year_start)
    x$day <- calc_water_day(x$date, year_start)
  }
  else {
    x$year_val <- lubridate::year(x$date)
    x$day <- lubridate::yday(x$date)
  }
  x$leapYear <- lubridate::leap_year(x$year_val)
  fullYearCheck <- dplyr::summarize(dplyr::group_by(x, year_val), 
                                    completeYear = if (!any(leapYear)) {
                                      ifelse(length(day) == 365, T, F)
                                    }
                                    else if (any(leapYear)) {
                                      ifelse(length(day) == 366, T, F)
                                    })
  x$leapYear <- NULL
  if (any(fullYearCheck$completeYear == F)) {
    incYears <- paste(fullYearCheck$year_val[fullYearCheck$completeYear == 
                                               F], collapse = ",")
    warning(paste0("Every year as defined by the year_start argument must be complete, \n                    the following years have missing data: ", 
                   incYears))
    return(FALSE)
  }
  return(x)
}

get_NWIS_drainArea <- function(site_num){
  message(paste('starting site', site_num))
  data_out <- data.frame(site_no = as.character(site_num),
                         drainArea = readNWISsite(siteNumbers = site_num)$drain_area_va)
  return(data_out)
}

get_drainage_area_log <- function(file_in, file_out) {
  message(paste('generating log for dataRetrieval drainage area request'))
  write_csv(file_in, file_out)
  return(file_out)
}

get_floodThreshold <- function(site_num, p1_clean_daily_flow, p1_peak_flow, 
                               perc, yearType){
  message(paste('starting site', site_num))
  df_dv <- p1_clean_daily_flow %>%
    filter(site_no == site_num) %>%
    select(date, discharge)
  
  filepath <- p1_peak_flow[grep(site_num, p1_peak_flow)]
  peaks <- read_csv(filepath, col_types = cols()) 
  peaks$site_no <- as.character(peaks$site_no)
  df_pk <- data.frame(date=as.Date(peaks$peak_dt),peak=peaks$peak_va)
  floodThreshold <- get_peakThreshold(df_dv, df_pk, perc=perc, yearType=yearType)
  if(is.na(floodThreshold)){
    floodThreshold <- quantile(x = df_dv$discharge, probs = 0.66)
  }
  df_out <- data.frame(site_no=site_num, floodThreshold)
  return(df_out)
}

calc_water_year <- function(dates, year_start){
  if(!(year_start %in% seq(1,12,1))){
    stop('year_start must be in 1:12')
  }
  
  if (year_start == 1){
    water_year <- year(dates)
  }else{
    water_year <- ifelse(month(dates) >= year_start, year(dates)+1, year(dates))
  }
  return(water_year)
} #end calc_water_year

calc_water_day <- function(dates, year_start){
  #based on code in EflowStats::get_waterYear()
  if(!(year_start %in% seq(1,12,1))){
    stop('year_start must be in 1:12')
  }
  
  if(year_start == 1){
    year_day <- lubridate::yday(dates)
  }else{
    #get the calendar day of a non-leap year on which the year starts
    day_start <- lubridate::yday(paste0('1990-', year_start, '-01'))
    #get calendar day of all dates
    year_day <- lubridate::yday(dates)
    #1 if this year was a leap year
    yrs_leap <- lubridate::leap_year(dates)
    #1 if last year was a leap year
    yrs_leap_last <- lubridate::leap_year(dates - lubridate::years(1))
    #get indices that are after the water year
    if (year_start == 2){
      #no leap year correction
      after_waterday <- year_day >= (day_start)
      year_day[after_waterday] <- year_day[after_waterday] - ((day_start - 1))
      year_day[!after_waterday] <- year_day[!after_waterday] + 
        (365 - day_start + 1 + yrs_leap_last[!after_waterday])
    }else{
      after_waterday <- year_day >= (day_start + yrs_leap)
      year_day[after_waterday] <- year_day[after_waterday] - ((day_start - 1) +
                                                                yrs_leap[after_waterday])
      year_day[!after_waterday] <- year_day[!after_waterday] + (365 - day_start + 1)
    }
  }
  return(year_day)
}

#[Jared] we could use this function to reformat the p1_HIT_metrics. Example:
# tar_load(p1_HIT_metrics)
# pivot_wider(data = p1_HIT_metrics, names_from = 'indice', values_from = 'statistic')
combine_flow_metrics <- function(...) {
  flow_metrics_all_sites <- bind_rows(...) %>%
    pivot_wider(names_from = indice, values_from = statistic)
  return(flow_metrics_all_sites)
}
