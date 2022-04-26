get_babies <- function(idin) {
  listall <- item_list_children(idin, limit = 999)
  listall2 <- lapply(listall, `[`, c('id'))
  ids <- unlist(listall2)
  return(ids)
}

make_dl_table <- function(sb_parent_id, outdir) {
  
  #start with doing the top level id so we have something to work with.
  idlist <- get_babies(sb_parent_id)
  
  #these are our 2 working lists for our loop
  #the = "blank" thing is maybe a little lazy, but i take care of it later.
  parlist <- idlist
  dllist <- "blank"
  
  #getting ready for our loop
  end <- FALSE
  count <- 1
  
  #the main stuff
  #the first if just ends the loop when you go past the end of the parlist
  #otherwise, we test the id in parlist. We use item_get_fields to check if the id has children.
  #if yes, we get the ids of those children with our get_babies function
  #and we add them to the end of parlist so we can check those ids later in our loop
  #if the id has no children, that means its an id with download links.
  #and we add them to our dllist.
  while (end == FALSE)
  {
    if (is.na(parlist[count]) == TRUE)
    {
      end <- TRUE
      print("we are done")
    }
    if (is.na(parlist[count]) == FALSE)
    {
      idN <- parlist[count]
      print(paste("trying", idN))
      test <- item_get_fields(idN,'hasChildren')
      if (test == TRUE)
      {
        templist <- get_babies(idN)
        parlist <- c(parlist, templist)
        print("adding to parlist")
      }
      if (test == FALSE)
      {
        print("adding one to dllist")
        dllist <- c(dllist, idN)
      }
      count <- count + 1
    }
  }
  
  #this gets rid of our first "blank" entry
  dllist <- dllist[-1]
  
  #resetting count for the next loop
  count <- 1
  
  #this loop just gets a parentid, title, and last modified date for each download id to add back to our dllist
  #just uses item_get_fields
  #i made this loop in a way that did not need my lazy "blank" first entry
  for (x in dllist)
  {
    parentid <- item_get_fields(x, 'parentId')
    parenttitle <- item_get_fields(parentid, 'title')
    dllisttitle <- item_get_fields(x, 'title')
    dllistdate <- item_get_fields(x, 'provenance')
    if (count == 1)
    {
      tabledl <- c(x, parenttitle, dllisttitle, dllistdate$lastUpdated)
    }
    if (count == 2)
    {
      tableadd <- c(x, parenttitle, dllisttitle, dllistdate$lastUpdated)
      tabledl <- rbind(tabledl, tableadd)
    }
    count <- 2
  }
  
  #renaming some columns and getting rid of that ugly first column
  colnames(tabledl) <- c('id', 'parent_title', 'title', 'lastUpdated')
  rownames(tabledl) <- NULL
  tabledl <- as.data.frame(tabledl)
  
  #if you would like to export the table as csv
  filepath <- file.path(outdir, paste0("sb_table_full.csv"))
  write_csv(tabledl, filepath)
  return(filepath)
}

reduce_sb_table <- function(sb_table_full, sb_var_list, outdir) {
  sb_table_full <- read_csv(sb_table_full, col_types = 'cccT')
  
  #extract sciencebase ids and add to data frame
  sbid <- vector('character')
  for (i in 1:nrow(sb_var_list)){
    sbid[i] <- strsplit(x = sb_var_list$Science.Base.Link[i], split = 'item/')[[1]] %>% 
      last()
  }
  rm(i)
  
  sb_var_list$sbid <- sbid
  
  #remove duplicates
  sbid <- unique(sbid)
  
  #retrieve sciencebase ids not in list
  missing <- sbid[!(sbid %in% sb_table_full$id)]
  missing_vars <- sb_var_list[sb_var_list$sbid %in% missing,]
  
  if(nrow(missing_vars) > 0){
    for (i in 1:nrow(missing_vars)) {
      message(paste("Variable not in sb_table_full:", missing_vars[[i,2]]))
    }
  }
  
  
  #additional IDs to get missing values
  #if there are missing values, edit this function with the IDs you want to add
  #or, edit the sb_var_list with the IDs you want.
  retain_ids <- c("5734acafe4b0dae0d5de622d", 
                  "5730f062e4b0dae0d5db1fbe", 
                  "573362bce4b0dae0d5dd6193", 
                  "574ddc8ce4b07e28b66901b3",
                  "574f3e86e4b0ee97d51abf31",
                  "574f238fe4b0ee97d51a8916")
  
  retain_sb_table <- sb_table_full[sb_table_full$id %in% c(sbid, retain_ids),]
  
  #save list of sb ids
  filepath <- file.path(outdir, paste0("sb_table_reduced.csv"))
  write_csv(retain_sb_table, filepath)
  return(filepath)
}

download_children <-function(sites, sb_table_reduced, dldir, workdir, outdir, out_file_name) {
  #making a copy to work with
  data_at_sites <- sites
  itemfails <- "1st"
  sb_table_reduced <- read_csv(sb_table_reduced, 
                               col_types = cols(id = col_character(), 
                                                parent_title = col_character(), 
                                                title = col_character(), 
                                                lastUpdated = col_character()))
  
  for (row in 1:nrow(sb_table_reduced))
  {
    item <- as.character(sb_table_reduced[row, 'id'])
    message(paste('Row', row, 'SB ID', item))
    item_file_download(item, dest_dir = dldir, overwrite_file = TRUE)
    files <- list.files(path = dldir, pattern=NULL, full.names = TRUE)
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
    }
    filestomerge <- c(list.files(workdir, ".txt", recursive=TRUE, full.names=TRUE, include.dirs=TRUE),list.files(workdir, ".csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE),list.files(workdir, ".TXT", recursive=TRUE, full.names=TRUE, include.dirs=TRUE),list.files(workdir, ".CSV", recursive=TRUE, full.names=TRUE, include.dirs=TRUE))
    for (filem in filestomerge)
    {
      print(filem)
      success = "yes"
      tryCatch(tempfile <- read_delim(filem, delim = ",", show_col_types = FALSE), error = function(e) {success <<- "no"})
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
  
  filepath1 <- file.path(outdir, out_file_name)
  write_csv(data_at_sites, filepath1)
  filepath2 <- file.path(outdir, paste0("FAILS_", out_file_name))
  write_csv(itemfails, filepath2)
  return_df = c(filepath1, filepath2)
  return(return_df)
}

calc_avg_land_cover <- function(sb_data) {
  #'@description Combines decadal modeled historic land-use land-cover data from
  #'SOHL datasets spanning from 1940 - 2000. Calculates long-term average.
  #'
  #'@param sb_data data frame of gages and landscape attributes from ScienceBase
  #'
  #'@return target to be joined in final landscape attribute table

  sb_data <- read_csv(sb_data, show_col_types = FALSE) %>%
    suppressMessages()
  sohl_lc <- sb_data %>%
    select(COMID, 
           contains("SOHL40"), contains("SOHL50"), contains("SOHL60"),
           contains("SOHL70"), contains("SOHL80"), contains("SOHL90"), 
           contains("SOHL00")) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           year = if_else(str_sub(name, 9, 10) == "00", 2000, 
                          as.numeric(paste0("19", str_sub(name, 9, 10)))), 
           class = as.numeric(str_sub(name, 12))) %>%
    group_by(COMID, unit, year, class) %>%
    summarise(value = mean(value)) %>%
    ungroup()

  qa_lc_sum_year <- sohl_lc %>%
    group_by(COMID, unit, year) %>%
    summarise(lc_sum = sum(value)) %>%
    ungroup() %>%
    filter(lc_sum < 99 | lc_sum > 101)
  qa_lc_sum_year_comid <- unique(qa_lc_sum_year$COMID)
}

calc_avg_monthly_weather <- function(sb_data) {
  sb_data <- read_csv(sb_data, show_col_types = FALSE) %>%
    suppressMessages()
  weather <- sb_data %>%
    select(COMID, contains("_PPT_"), contains("_TAV_")) %>%
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
  return(weather)
}

calc_avg_wildfire <- function(sb_data) {
  sb_data <- read_csv(sb_data, show_col_types = FALSE) %>%
    suppressMessages()
  wildfire <- sb_data %>%
    select(COMID, contains("_WILDFIRE_")) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           year = str_sub(name, 14, 17)) %>%
    group_by(COMID, unit) %>%
    summarise(avg_annual = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(label = paste0(unit, "_AVG_WILDFIRE")) %>%
    arrange(COMID, label) %>%
    select(COMID, label, avg_annual) %>%
    pivot_wider(names_from = "label", values_from = "avg_annual")
  return(wildfire)
}