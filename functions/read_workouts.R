# load xml data and transform it to dataframe
# adapted from https://gist.github.com/ryanpraski/ba9baee2583cfb1af88ca4ec62311a3d
load_workouts <- function(){
  
  suppressPackageStartupMessages(library(tidyverse))
  
  # load static file that will be appended
  workouts <- readr::read_csv(here::here("data", "workouts_master.csv"), show_col_types = FALSE)
  
  # load new measurements
  tmp <- readr::read_csv(here::here("data", "allWorkouts.csv"), show_col_types = FALSE)
  
  # append and save
  workouts_appended <- dplyr::bind_rows(workouts, tmp) %>% 
    dplyr::distinct()
  
  readr::write_csv(workouts_appended, here::here("data", "workouts_master.csv"))
  
  # format variables correctly
  workouts_clean <- workouts_appended %>% 
    dplyr::rename_all(function(x) tolower(stringr::str_replace_all(x, " ", "_"))) %>% 
    # create some more derived features
    dplyr::mutate(
      date = as.Date(start),
      year = lubridate::year(start),
      month = lubridate::month(start),
      day = lubridate::wday(start, label = T),
      activityLengthMin = round(as.numeric(end - start), digits = 2)
    ) %>% 
    dplyr::rename(
      activity = type,
      startDate = start,
      endDate = end
    )
  
  
  return(workouts_clean)
  
}
