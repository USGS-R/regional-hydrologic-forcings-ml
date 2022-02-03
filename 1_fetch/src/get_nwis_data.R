has_data_check <- function(site_nums, parameterCd){
  ##check to see if all sites actually have daily flow and peak flow data.  There are
  ##gages in gagesii that do not have one or the other, so screen these out before we try
  ##to download the data
  dv_screen <- whatNWISdata(siteNumber=site_nums, parameterCd=parameterCd, service="dv",
                            convertType = FALSE)
  pk_screen <- whatNWISdata(siteNumber=site_nums, service="pk", convertType = FALSE)
  sites_with_peak_screen <- as_tibble(intersect(dv_screen$site_no, pk_screen$site_no))
  message(paste("there are", length(sites_with_peak_screen$value), 
                "sites in GagesII with daily flow and peak flow data"))
  sites_without_peak_screen <- as_tibble(unique(dv_screen$site_no))
  message(paste("there are", length(sites_without_peak_screen$value), 
                "sites in GagesII with daily flow and no peak flow data"))
  extra_sites <- setdiff(sites_without_peak_screen, sites_with_peak_screen)
  extra_sites_info <- whatNWISdata(siteNumber = as.character(extra_sites$value), 
                                   parameterCd = parameterCd, 
                                   service = "dv", 
                                   convertType = FALSE)
  write_csv(extra_sites_info, "./1_fetch/out/logs/extra_sites_without_peaks.csv")
  return("./1_fetch/out/logs/extra_sites_without_peaks.csv")
}

# filter_complete_years <- function(screen_daily_flow, complete_years){
#   
#   complete_yr_count <- screen_daily_flow %>% 
#     filter(!is.na(complete_yrs)) %>%  
#     group_by(site_no) %>%
#     count()
#   message("complete yr ok")
#   
#   keep_sites <- complete_yr_count %>%
#     filter(n >= complete_years) %>%
#     pull(site_no)
#   return(keep_sites)
# }
# 
# get_nwis_daily_data <- function(site_num, outdir, parameterCd, startDate, endDate){
#   message(paste('starting site', site_num))
#   ##read daily NWIS data, and save to csv file.
#   data_out <- readNWISdv(site_num, parameterCd, startDate, endDate)
#   ##renaming column names for discharge and discharge_cd
#   names(data_out)[grep(pattern="X_.*\\cd", x=names(data_out))] <- "discharge_cd"
#   names(data_out)[grep(pattern="X_",x=names(data_out))] <- "discharge"
#   filepath <- file.path(outdir, paste0(site_num, "_dv.csv"))
#   write_csv(data_out, file=filepath)
#   return(filepath)
# }
# 
# get_daily_flow_log <- function(files_in, file_out) {
#   message(paste('generating log for dataRetrieval daily flow request'))
#   daily_flow_list <- map(files_in, read_csv, 
#               col_types = cols(agency_cd = col_skip(), 
#                                site_no = col_character(), 
#                                Date = col_date(format = "%Y-%m-%d"), 
#                                discharge = col_double(), 
#                                discharge_cd = col_character()))
#   daily_flow_df <- bind_rows(daily_flow_list)
#   daily_flow_log <- daily_flow_df %>%
#     group_by(site_no) %>%
#     summarize(total_days = n(),
#               approved_days = sum(discharge_cd == "A"),
#               start_date = min(Date), 
#               end_date = max(Date), 
#               mean_all_flow = mean(discharge, na.rm = TRUE), 
#               sd_all_flow = sd(discharge, na.rm = TRUE), 
#               mean_approved_flow = mean(discharge[discharge_cd == "A"], na.rm = TRUE), 
#               sd_approved_flow = sd(discharge[discharge_cd == "A"], na.rm = TRUE))
#   write_csv(daily_flow_log, file_out)
#   return(file_out)
# }
# 
# get_nwis_peak_data <- function(site_num, outdir, startDate, endDate){
#   message(paste('starting site', site_num))
#   ##read NWIS peak data, and save to csv file.
#   data_out <- readNWISpeak(site_num, startDate, endDate)
#   
#   ##Removing any NA dates or peak_va values - dates are returned as NA if day of occurrence is not known
#   ## and peak_va is sometimes returned as NA if the gage height is known but not the discharge.
#   data_out <- data_out[which(!is.na(data_out$peak_dt)), ]
#   data_out <- data_out[which(!is.na(data_out$peak_va)), ]
#   filepath <- file.path(outdir, paste0(site_num, "_pk.csv"))
#   write_csv(data_out, file=filepath)
#   return(filepath)
# }
# 
# get_peak_flow_log <- function(files_in, file_out) {
#   message(paste('generating log for dataRetrieval peak flow request'))
#   peak_flow_list <- suppressWarnings(
#     map(files_in, read_csv, 
#         col_types = cols(agency_cd = col_skip(), 
#                          site_no = col_character(), 
#                          peak_dt = col_date(format = "%Y-%m-%d"), 
#                          peak_tm = col_skip(), 
#                          peak_va = col_double(), 
#                          peak_cd = col_skip(), 
#                          gage_ht = col_skip(), 
#                          gage_ht_cd = col_skip(), 
#                          year_last_pk = col_skip(), 
#                          ag_dt = col_skip(), 
#                          ag_tm = col_skip(), 
#                          ag_gage_ht = col_skip(), 
#                          ag_gage_ht_cd = col_skip(), 
#                          peak_dateTime = col_skip(), 
#                          ag_dateTime = col_skip())))
#   peak_flow_df <- bind_rows(peak_flow_list)
#   peak_flow_log <- peak_flow_df %>%
#     group_by(site_no) %>%
#     summarize(num_peaks = n(), 
#               first_date = min(peak_dt), 
#               last_date = max(peak_dt), 
#               max_peak = max(peak_va, na.rm = TRUE), 
#               mean_peak = mean(peak_va, na.rm = TRUE), 
#               sd_peak = sd(peak_va, na.rm = TRUE))
#   write_csv(peak_flow_log, file_out)
#   return(file_out)
# }
# 
# screen_daily_data <- function(filename, year_start){
#   ##screen for years with missing data
#   data <- read_csv(filename,
#                    col_types=cols(agency_cd=col_character(),
#                                   site_no=col_character(), Date=col_date(format="%Y-%m-%d"),
#                                   discharge=col_double(), discharge_cd=col_character()))
#  
#   ###prior to screening, remove any provisional data - this will be counted as 'no data'
#   prov_data <- grep('P|e', data$discharge_cd)
#   if(length(prov_data) > 0){data <- data[-prov_data, ]}
#   
#   if(year_start == 'water'){
#     year_start <- 10
#   }else if(is.character(year_start)){
#     stop('year_start must be numeric or "water"')
#   }
#   
#   missing_data <- screen_flow_data(data.frame(site_no=data$site_no, 
#                                               Date=data$Date,
#                                               Value=data$discharge),
#                                    water_year_start=year_start)
#   complete_yrs <- missing_data %>%
#     filter(n_missing_Q == 0) %>%
#     select(Year)
# 
#   if(nrow(complete_yrs) > 0){
#     data_out <- data.frame(site_no=unique(data$site_no),
#                            complete_yrs=complete_yrs$Year)
#   }else{
#     data_out <- data.frame(site_no=unique(data$site_no),
#                            complete_yrs=NA)
#   }
#   
#   return(data_out)
# }
# 
# clean_daily_data <- function(site, filenames, screen_daily_flow, yearType, year_start){
#   ##remove NAs from data and remove incomplete years of data
#   ##EflowStats requires the cleaned data to have dates in the first column and 
#   ##discharge in the second column.
#   message(paste('starting site', site))
#   filepath <- filenames[grep(site, filenames)]
#   data <- read_csv(filepath,
#                    col_types=cols(agency_cd=col_character(),
#                                   site_no=col_character(),
#                                   Date=col_date(format="%Y-%m-%d"),
#                                   discharge=col_double(),
#                                   discharge_cd=col_character()))
#   
#   #add a column for the water year based on year_start
#   data$waterYear <- calc_water_year(data$Date, year_start)
#   ##remove all data from years with data gaps
#   keep_years <- screen_daily_flow %>%
#     filter(site_no == site) %>%
#     pull(complete_yrs)
#   #noting that waterYear could = calendar year when year_start = 1
#   data_sc <- data[which(data$waterYear %in% keep_years), ]
#   
#   df <- data.frame(date=as.Date(data_sc$Date), discharge=data_sc$discharge)
#   ###run EflowStats validation to produce clean, ready to process data
#   if (yearType == 'water' & year_start == 10){
#     #use the EflowStats validation function
#     clean_data <- validate_data(df, yearType)
#   }else{
#     #Use user-defined validation based on EflowStats function
#     clean_data <- validate_data_yr_start(df, year_start)
#   }
#   clean_data$site_no <- unique(data_sc$site_no)
#   return(clean_data)
# }#end clean_daily_data function
# 
# validate_data_yr_start <- function(x, year_start){
#   #based on EflowStats::validate_data()
#   x <- x[, 1:2]
#   col1_class <- class(x[, 1])
#   col2_class <- class(x[, 2])
#   if (col1_class != "Date" && col2_class != "numeric") {
#     warning("First column of x must contain a vector of class date.\nSecond column of x must contain a vector of class numeric.")
#     return(FALSE)
#   }
#   else if (col1_class != "Date") {
#     warning("First column of x must contain a vector of class date.")
#     return(FALSE)
#   }
#   else if (col2_class != "numeric" & col2_class != "integer") {
#     warning("Second column of x must contain a vector of class numeric.")
#     return(FALSE)
#   }
#   if (!(year_start %in% seq(1,12,1))) {
#     warning("year_start must be numeric on 1:12")
#     return(FALSE)
#   }
#   if (anyNA(x)) {
#     warning("dataframe x cannot contain NA values")
#     return(FALSE)
#   }
#   names(x) <- c("date", "discharge")
#   x <- dplyr::arrange(x, date)
#   if (year_start != 1) {
#     x$year_val <- calc_water_year(x$date, year_start)
#     x$day <- calc_water_day(x$date, year_start)
#   }
#   else {
#     x$year_val <- lubridate::year(x$date)
#     x$day <- lubridate::yday(x$date)
#   }
#   x$leapYear <- lubridate::leap_year(x$year_val)
#   fullYearCheck <- dplyr::summarize(dplyr::group_by(x, year_val), 
#                                     completeYear = if (!any(leapYear)) {
#                                       ifelse(length(day) == 365, T, F)
#                                     }
#                                     else if (any(leapYear)) {
#                                       ifelse(length(day) == 366, T, F)
#                                     })
#   x$leapYear <- NULL
#   if (any(fullYearCheck$completeYear == F)) {
#     incYears <- paste(fullYearCheck$year_val[fullYearCheck$completeYear == 
#                                                F], collapse = ",")
#     warning(paste0("Every year as defined by the year_start argument must be complete, \n                    the following years have missing data: ", 
#                    incYears))
#     return(FALSE)
#   }
#   return(x)
# }
# 
# get_NWIS_drainArea <- function(site_num){
#   message(paste('starting site', site_num))
#   data_out <- data.frame(site_no=as.character(site_num),
#                          drainArea=readNWISsite(siteNumbers=site_num)$drain_area_va)
#   return(data_out)
# }
# 
# get_drainage_area_log <- function(file_in, file_out) {
#   message(paste('generating log for dataRetrieval drainage area request'))
#   write_csv(file_in, file_out)
#   return(file_out)
# }
# 
# get_floodThreshold <- function(site_num, p1_clean_daily_flow, p1_peak_flow, 
#                                perc, yearType){
#   message(paste('starting site', site_num))
#   df_dv <- p1_clean_daily_flow %>%
#     filter(site_no == site_num) %>%
#     select(date, discharge)
#   
#   filepath <- p1_peak_flow[grep(site_num, p1_peak_flow)]
#   peaks <- read_csv(filepath, col_types = cols()) 
#   peaks$site_no <- as.character(peaks$site_no)
#   df_pk <- data.frame(date=as.Date(peaks$peak_dt),peak=peaks$peak_va)
#   floodThreshold <- get_peakThreshold(df_dv, df_pk, perc=perc, yearType=yearType)
#   df_out <- data.frame(site_no=site_num, floodThreshold)
#   return(df_out)
# }
# 
# calc_water_year <- function(dates, year_start){
#   if(!(year_start %in% seq(1,12,1))){
#     stop('year_start must be in 1:12')
#   }
#   
#   if (year_start == 1){
#     water_year <- year(dates)
#   }else{
#     water_year <- ifelse(month(dates) >= year_start, year(dates)+1, year(dates))
#   }
#   return(water_year)
# } #end calc_water_year
# 
# calc_water_day <- function(dates, year_start){
#   #based on code in EflowStats::get_waterYear()
#   if(!(year_start %in% seq(1,12,1))){
#     stop('year_start must be in 1:12')
#   }
#   
#   if(year_start == 1){
#     year_day <- lubridate::yday(dates)
#   }else{
#     #get the calendar day of a non-leap year on which the year starts
#     day_start <- lubridate::yday(paste0('1990-', year_start, '-01'))
#     #get calendar day of all dates
#     year_day <- lubridate::yday(dates)
#     #1 if this year was a leap year
#     yrs_leap <- lubridate::leap_year(dates)
#     #1 if last year was a leap year
#     yrs_leap_last <- lubridate::leap_year(dates - lubridate::years(1))
#     #get indices that are after the water year
#     if (year_start == 2){
#       #no leap year correction
#       after_waterday <- year_day >= (day_start)
#       year_day[after_waterday] <- year_day[after_waterday] - ((day_start - 1))
#       year_day[!after_waterday] <- year_day[!after_waterday] + 
#         (365 - day_start + 1 + yrs_leap_last[!after_waterday])
#     }else{
#       after_waterday <- year_day >= (day_start + yrs_leap)
#       year_day[after_waterday] <- year_day[after_waterday] - ((day_start - 1) +
#                                                                 yrs_leap[after_waterday])
#       year_day[!after_waterday] <- year_day[!after_waterday] + (365 - day_start + 1)
#     }
#   }
#   return(year_day)
# }
# 
# #[Jared] we could use this function to reformat the p1_HIT_metrics. Example:
# # tar_load(p1_HIT_metrics)
# # pivot_wider(data = p1_HIT_metrics, names_from = 'indice', values_from = 'statistic')
# combine_flow_metrics <- function(...) {
#   flow_metrics_all_sites <- bind_rows(...) %>%
#     pivot_wider(names_from = indice, values_from = statistic)
#   return(flow_metrics_all_sites)
# }
