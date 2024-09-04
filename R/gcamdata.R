#' left_join_error_no_match
#'
#' A restrictive version of \code{\link{left_join}}.
#'
#' @param d Data frame (typically from pipeline)
#' @param ... Rest of call to \code{\link{left_join}}
#' @param ignore_columns Optional column name(s) to ignore, character vector
#' @return Joined data.
#' @details Restrictive version of dplyr::left_join meant for replacing `match` calls.
# Ensures that number of rows of data doesn't change, and everything has matched data.
#' @importFrom dplyr left_join
#' @importFrom assertthat assert_that
#' @export
left_join_error_no_match <- function(d, ..., ignore_columns = NULL) {
  assertthat::assert_that(tibble::is_tibble(d))
  dnames <- names(d)
  drows <- nrow(d)
  d <- dplyr::left_join(d, ...)
  if(nrow(d) != drows) {
    stop("left_join_no_match: number of rows in data changed")
  }
  names_to_check <- dplyr::setdiff(names(d), dnames) %>% dplyr::setdiff(ignore_columns)
  if(any(is.na(d[names_to_check]))) {
    stop("left_join_no_match: NA values in new data columns")
  }
  d
}

#' repeat_add_columns
#'
#' Repeat a data frame for each entry in a second, binding the columns together.
#'
#' @param x Data frame (tibble) to repeat
#' @param y A copy of \code{x} is created for each row of this tibble
#' @return A repeated \code{x} with columns from \code{y} added.
#' @details This corresponds to \code{repeat_and_add_vector} in the old data system.
#' @importFrom assertthat assert_that
#' @importFrom dplyr full_join mutate
#' @author BBL
#' @export
#' @examples
#' x <- tibble::tibble(x = 1:3)
#' y <- tibble::tibble(y = c(4, 5), z = c(6, 7))
#' repeat_add_columns(x, y)
repeat_add_columns <- function(x, y) {
  UNIQUE_JOIN_FIELD <- NULL           # silence package checks.
  assertthat::assert_that(tibble::is_tibble(x))
  assertthat::assert_that(tibble::is_tibble(y))

  x %>%
    dplyr::mutate(UNIQUE_JOIN_FIELD = 1) %>%
    dplyr::full_join(dplyr::mutate(y, UNIQUE_JOIN_FIELD = 1), by = "UNIQUE_JOIN_FIELD") %>%
    dplyr::select(-UNIQUE_JOIN_FIELD)
}


#' read.GCAM.csv
#'
#' read raw csv files from Assumptions/GCAM folder
#'
#' @param basename name of the file to be read in, without .csv
#' @param na.strings set to ""
#'
#' @return A data frame from raw csv file saved in the Assumptions/GCAM folder
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' A23.globaltech_retirement <- read.GCAM.csv("A23.globaltech_retirement")

read.GCAM.csv <- function(basename, na.strings="") {
  pathname <- file.path('Assumptions/GCAM', paste0(basename, ".csv"))
  return( read.csv(pathname, na.strings=na.strings, stringsAsFactors=F, comment.char = "#") )
}

