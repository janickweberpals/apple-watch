# load xml data and transform it to dataframe
# adapted from https://gist.github.com/ryanpraski/ba9baee2583cfb1af88ca4ec62311a3d
load_workouts <- function(){
  
  # load static file that will be appended
  workouts <- readr::read_csv(here::here("data", "workouts_master.csv"), show_col_types = FALSE)
  
  # load new measurements
s
  
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
      activity_length_min = round(as.numeric(end - start), digits = 2)
    ) %>% 
    dplyr::rename(
      activity = type,
      start_date = start,
      end_date = end
    )
  
  
  return(workouts_clean)
  
}
