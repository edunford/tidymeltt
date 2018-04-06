#' guess_date
#'
#' Guesses the most likely date format from a specified date string and convert
#' it to a `Date` class.
#'
#' @param date_string character string containing date information
#' @param default default date ordering; see `lubridate` documentation regarding
#'   formatting conventions.
#'
#' @return `Date` vector.
#' @export
#'
#' @examples
#'
#' guess_date(c("2001-03-04","2002-30-04","2012-01-01"))
#'
#' # If pattern is inconsistent, only the most consistent will be returned
#' guess_date(c("Jan. 1, 2017","Feb. 13, 2018","2005-06-07"))
#'
#'
guess_date = function(date_string,default = 'mdy'){
  UseMethod("guess_date")
}


guess_date.character = function(date_string, default = 'mdy'){

  # Guess Date Format
  date_possibilities <- get_date_regex('mdy')

  guess = function(date_string) Reduce('pattern_replace', c(date_string, rep(date_possibilities, 3)))

  best_guess = date_string %>%
    purrr::map(guess) %>%
    tibble::tibble(options=.) %>%
    tidyr::unnest(.) %>%
    dplyr::group_by(options) %>%
    dplyr::count(.) %>%
    dplyr::ungroup(.) %>%
    dplyr::filter(n==max(n)) %>%
    .$options

  # Convert date_string to `Date` vector class
  as.Date(date_string,best_guess)
}


guess_date.Date = function(date_string, default = 'mdy'){
  date_string # If `Date` class, return input
}







# Internal functions ------------------------------------------------------
#(using ramnathv/intellidate initial `intellidate` code)




#' Constructs a regex union based on a vector of values
#'
#' @keywords internal
#'
join_regex <- function(...){
  regex <- paste(..., collapse = "|")
  paste("\\b(", regex, ")\\b", sep = "")
}


#' Vectorized version of the switch statement
vec_switch <- function(...) {
  mapply(switch, ..., USE.NAMES = FALSE)
}


#' Returns strftime tokens already matched in a given string
#'
tokens_matched <- function(string){
  tokens <- regmatches(string, gregexpr("%[a-zA-Z]", string))[[1]]
  matched <- lapply(tokens, function(token){
    switch(token, `%B` = "%m", `%b` = "%m", `%Y` = "%y", `%d`= c("%d", "%e"),
           `%H` = c("%H", "%I"), `%p` = "%H", token)
  })
  return(matched %>%  unlist %>%unique)
}


#' Replaces a regex pattern with a strftime token
#'
pattern_replace <- function(string, regex){
  string  <- unlist(string)
  pat     <- join_regex(regex[[1]])
  tokens <- lapply(string, function(x) {
    setdiff(regex[[2]], tokens_matched(x))
  })
  # replace null tokens with pattern
  tokens[sapply(tokens, length) == 0] <- pat
  string <- mapply(stringr::str_replace, string, pat, tokens)
  return(unique(string))
}


#' Establish basic component elements to guess on.
#'
get_date_regex <- function(default = 'mdy'){
  day.name <- stringr::str_c(c('Mon', 'Tues', 'Wednes', 'Thurs', 'Fri', 'Satur', 'Sun'),'day')
  day.abbr <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
  DATE_REGEXES <- list(
    M_NAME    = list(month.name, "%B"), M_ABBR    = list(month.abb , "%b"),
    D_NAME    = list(day.name  , "%A"), D_ABBR    = list(day.abbr  , "%a"),
    Y_4_DIG   = list("[0-9]{4}", "%Y"), Y_2_DIG   = list(c("00", 32:99), "%y"),
    DY_2_DIG  = list(13:31, c("%d", "%y")),
    ALL_2_DIG = list(sprintf("%02d", 1:12), stringr::str_c("%", strsplit(default, "")[[1]])),
    ALL_1_DIG = list(pat = 1:9, token = c("%e", "%m"))
  )
  return(DATE_REGEXES)
}
