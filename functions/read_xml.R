# load xml data and transform it to dataframe
# adapted from https://gist.github.com/ryanpraski/ba9baee2583cfb1af88ca4ec62311a3d
load_xml <- function(path = NULL){
  
  suppressPackageStartupMessages(library(dplyr))
  
  #load apple health export.xml file
  xml <- XML::xmlParse(path)
  
  #transform xml file to data frame - select the Record rows from the xml file
  xml_df <- XML:::xmlAttrsToDataFrame(xml["//Record"])
  
  # format variables correctly
  xml_df_clean <- xml_df %>% 
    dplyr::select(type, sourceName, unit, startDate, endDate, value) %>% 
    # remove prefix from type variable
    dplyr::mutate(type = stringr::str_remove(type, "HKQuantityTypeIdentifier|HKCategoryTypeIdentifier|HKDataType")) %>%
    # formats
    dplyr::mutate(dplyr::across(tidyselect::ends_with("Date"), function(x) lubridate::ymd_hms(x))) %>% 
    dplyr::mutate(dplyr::across(c(type, sourceName, unit), as.factor)) %>% 
    # making value numeric will introduce NA's for AppleStandHour, AudioExposureEvent and SleepAnalysis
    dplyr::mutate(value = as.numeric(value)) %>% 
    # select and rename some variables
    dplyr::select(
      activity = type,
      source = sourceName,
      unit,
      start_date = startDate,
      end_date = endDate,
      value
      ) %>% 
    # create some more derived features
    dplyr::mutate(
      date = as.Date(start_date),
      year = lubridate::year(start_date),
      month = lubridate::month(start_date),
      day = lubridate::wday(start_date, label = T),
      activity_length_min = round(as.numeric((end_date - start_date)/60), digits = 2)
      )
  
  #message("Making value numeric will introduce NA's for AppleStandHour, AudioExposureEvent and SleepAnalysis")
  
  return(xml_df_clean)
  
}
