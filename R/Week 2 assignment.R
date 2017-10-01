#' @title Read Data from Fatality Analysis Reporting System
#'
#' @description Read data from a csv file. The function also uses the tbl_df wrapper from dplyr to make sure 
#' the system won't print a lot of data to the screen
#'
#' @param filename the path to the data
#' @return the wrapped dataset
#' @examples
#' data <- fars_read("accident_2013.csv.bz2")
#' @note The data has to be in csv format 
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' @title Make a file name 
#'
#' @description Make a file name with speicified accident year
#'
#' @param year your desired accident year
#' @return a character string of the name of the file
#' @examples
#' make_filename(2013)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' @title Read the year 
#'
#' @description Read user specified year for the FARS data
#'
#' @param year your desired accident year
#' @return a tibble with month and year
#' @examples
#' fars_read_years(2013)
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' @title Summarize the FARS data with specified accident year 
#'
#' @description Summarize number of accidents across each month for the specified year
#'
#' @param year accident year
#' @return a tibble showing number of accident by month
#' @examples
#' fars_summarize_years(2013)
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' @title Mapping the accident for any given state
#'
#' @description Mapping the location of the accidents (based on latitude and longitude) with user speicifed state number
#' and accident year
#' 
#' @param state.num state code
#' @param year accident year
#' 
#' @return a map showing the location of the accidents
#' @note the function will require "maps" package
#' @examples
#' fars_map_state(10,2013)

#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}


