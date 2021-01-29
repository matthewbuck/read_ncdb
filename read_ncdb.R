require('tidyverse')

read_ncdb <- function(datafile) {
  #datafile should be a string
  #should be the raw text from ncdb
  input <- readr::read_lines('NCDB_PUF_Labels_2017.sas')
  fields <- input[14:166]
  var_names <- stringr::str_extract(fields,'\\w+')
  var_positions <- stringr::str_extract(fields,'\\d+-\\d+')
  var_width <- purrr::map_dbl(var_positions, 
                            columnWidth)

  df <- read_fwf(datafile, 
                 col_positions = fwf_widths(var_width, col_names = var_names),
                 col_types = cols(
                   .default = col_double(),
                   PUF_CASE_ID = col_character(),
                   PUF_FACILITY_ID = col_character(),
                   PRIMARY_SITE = col_character(),
                   TNM_CLIN_T = col_character(),
                   TNM_CLIN_N = col_character(),
                   TNM_CLIN_M = col_character(),
                   TNM_CLIN_STAGE_GROUP = col_character(),
                   TNM_PATH_T = col_character(),
                   TNM_PATH_N = col_character(),
                   TNM_PATH_M = col_character(),
                   TNM_PATH_STAGE_GROUP = col_character()
                 ))
    
  return(df)
}

columnWidth <- function(x) {
  start <- as.double(stringr::str_extract(x, '\\d+'))
  end <- as.double(stringr::str_extract(x, '-\\d+'))*-1
  width <- end - start + 1
  
  return(width)
}
  


