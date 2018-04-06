#' bundle
#'
#' Bundle together input data into a nested tibble object for processing in the tidymeltt.
#'
#' @param ... One or more unquoted expressions (specifically, `data.frame` and `tibble` objects) separated by commas.
#'
#' @return A nested tibble object
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' dat1 = tibble(date=c(1,2,3,4,5))
#' dat2 = tibble(date=c(1,2,3,4))
#' bundle(dat1,dat2)
#'
#'
bundle <- function(...){
  UseMethod("bundle")
}

#' @export
bundle.data.frame <- function(...){
  varnames = deparse(substitute(c(...))) %>% gsub("c+\\(|\\)","",.) %>% strsplit(split=", ") %>% .[[1]]
  ind_nest = function(x){ x %>% dplyr::mutate(source_index = 1:nrow(.)) %>% tidyr::nest(.)}
  obj <- list(...) %>%
    purrr::map_df(.,ind_nest) %>%
    dplyr::transmute(source=varnames,data)

  # Assign class
  obj <- as_meltt(obj)

  return(obj)
}







# define meltt_tbl class --------------------------------------------------

#' as_meltt
#'
#' Coerce a data.frame or tibble object to be class meltt_tbl
#'
#' @param .data A bundled tbl_df.
#' @param ...	Other arguments passed on to methods. Not currently used.
#'
#' @return bundled data with additional "meltt_neighbor" column class feature
#' @export
#'
#' @examples
as_meltt = function(.data,...){
  UseMethod("as_meltt")
}

#' @examples
as_meltt.tbl_df = function(.data,...){
  class(.data) <- c(class(.data),"meltt_tbl")
  .data
}

#' @examples
as_meltt.tbl = function(.data,...){
  class(.data) <- c(class(.data),"meltt_tbl")
  .data
}


#' is_meltt
#'
#' Test if a data.set or tibble object is of class meltt_tbl.
#'
#' @param x object being evaluated
#'
#' @return logical scaler if evaluation is true.
#'
#' @export
#'
#' @examples
is_meltt = function(x){
  UseMethod("is_meltt")
}

#' @export
is_meltt.tbl_df = function(x){
  ifelse("meltt_tbl" %in% class(x),T,F)
}




