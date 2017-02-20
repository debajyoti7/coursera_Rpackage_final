#' Read fars data
#'
#' Check if input file exists in current working directory and if found, read data from it.
#' If file isn't found, throws error message and stops executing the function.
#'
#' @param filename Name of input file (String). This parameter isn't initialisied by default.
#'
#' @import readr
#' @import dplyr
#'
#' @return This function returns a dplyr dataframe
#'
#'@export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Generates a filename
#'
#' Take _year_ value as input, converts it into an integer, and generates a character vector containing a formatted combination
#' of text and provided variable values.
#'
#' @param year Year (YYYY). This parameter isn't initialisied by default. Handles integers, floats as well as strings.
#'
#' @return This function returns a character vector to serve as a filename for an archived csv file.
#'
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename("2013")
#' make_filename("2013.076")
#' }
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read fars data based on input year
#'
#' Generates string name from given input _year_,
#' check if a matching input file exists in current working directory and if found, reads the data.
#' If matching file is not found in the current working directory, it throws an __invalid year__ error.
#'
#' @param years Year (YYYY). This parameter isn't initialisied by default. Handles integers, floats as well as strings.
#'
#' @return This function returns a subset of fars data containing only the year and month values in long format
#' @export
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years("2013")
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat, year = ~year) %>%
        dplyr::select_("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Summarize fars data by Year
#'
#'Try to read fars data in current working directory and if successful, summarize data by count of instances per month.
#'
#' @param years Year (YYYY). This parameter isn't initialisied by default. Handles integers, floats as well as strings.
#'
#' @return This functions returns a summarized dataframe giving count of instances per month of the year in a wide format. It returns one row for each month and one column per year in the data.
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years("2013")
#' }
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_("year", "MONTH") %>%
    dplyr::summarize_(n = ~n()) %>%
    tidyr::spread_("year", "n")
}


#' Plot accident locations on map
#'
#' Read fars data from file in current working directory and plot accident location for given state during the given year,
#' within a fixed maximum threshold for longitude (900) and latitude (90). Throws error if invalid values are provided for _year_ and _state_.
#'
#' @param state.num State Id. This parameter isn't initialisied by default. Handles integers, floats as well as strings.
#' @param year Year (YYYY). This parameter isn't initialisied by default. Handles integers, floats as well as strings.
#'
#' @import dplyr
#' @import maps
#' @import graphics
#'
#' @return map plot with rectangular markers for accident locations. Returns message if no accidents are available to be plotted.
#' @export
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2013)
#' }
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == state.num)
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
