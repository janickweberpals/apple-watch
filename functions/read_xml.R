# load xml data and transform it to dataframe
# adapted from https://gist.github.com/ryanpraski/ba9baee2583cfb1af88ca4ec62311a3d
load_xml <- function(){
  
  suppressPackageStartupMessages(library(dplyr))
  
  #load apple health export.xml file
  xml <- XML::xmlParse(here::here("data/apple_health_export", "export.xml"))
  
  #transform xml file to data frame - select the Record rows from the xml file
  xml_df <- XML:::xmlAttrsToDataFrame(xml["//Record"])
  
  # format variables correctly
  xml_df_clean <- xml_df %>% 
    dplyr::select(type, sourceName, unit, startDate, endDate, value) %>% 
    # make value variable numeric
    dplyr::mutate(value = as.numeric(as.character(value))) %>% 
    # make endDate in a date time variable POSIXct using lubridate with eastern time zone
    dplyr::mutate_at(dplyr::vars(tidyr::ends_with("Date")), function(x) lubridate::ymd_hms(x, tz="Europe/Zurich")) %>% 
    # remove prefix from type variable
    dplyr::mutate(type2 = stringr::str_remove(type, "HKQuantityTypeIdentifier|HKCategoryTypeIdentifier|HKDataType")) %>% 
    # remove some variables
    dplyr::select(activity = type2,
                  source = sourceName,
                  unit,
                  startDate,
                  endDate,
                  value) %>% 
    # create some more derived features
    dplyr::mutate(
      date = as.Date(startDate),
      year = lubridate::year(startDate),
      month = lubridate::month(startDate),
      day = lubridate::wday(startDate, label = T),
      activityLengthMin = round(as.numeric((endDate - startDate)/60), digits = 2)
    )
  
  
  return(xml_df_clean)
  
}
