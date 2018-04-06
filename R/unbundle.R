#' unbundle
#'
#' Unbundle (flatten) a bundled meltt_df.
#'
#'@param .data A bundled meltt_tbl containing a `neighbor` feature.
#'@param ... column requested columns. Default all columns are returned.
#'
#'@return returns tbl_df of all data.
#' @importFrom magrittr "%>%"
#'@export
#'
#' @examples
#'
#' set.seed(123)
#' N = 100
#' dates = seq(from = as.Date("2007-01-01"),to=as.Date("2007-01-01"),by = "day")
#' lons = runif(N,0,.1)
#' lats = runif(N,0,.1)
#' d1 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
#' d2 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
#' d3 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
#' #d3$date[1:10] = NA
#'
#' bundle(d1,d2,d3) %>%
#' define_space(.) %>%
#' define_time(d1="date",d2="date",d3="date") %>%
#' define_close(.,space=2,time=1) -> xx
#'
#'
#' # Unbundle and look at cluster communities
#' xx %>% unbundle(neighbors) %>%
#' group_by(neighbor_id) %>% count %>%
#' arrange(desc(n))
#'
#'
unbundle <- function(.data,...){
  UseMethod("unbundle")
}


#'@export
unbundle.meltt_tbl <- function(.data,...){
  varnames = deparse(substitute(c(...))) %>% gsub("c+\\(|\\)","",.) %>% strsplit(split=", ") %>% .[[1]]
  if(length(varnames)==0){varnames <- colnames(.data)}
  .data %>%
    scrub_meltt_class %>%
    dplyr::select(source,varnames) %>%
    tidyr::unnest(.)
}

#'@export
unbundle.tbl <- function(.data,...){
  varnames = deparse(substitute(c(...))) %>% gsub("c+\\(|\\)","",.) %>% strsplit(split=", ") %>% .[[1]]
  if(length(varnames)==0){varnames <- colnames(.data)}
  .data %>%
    scrub_meltt_class %>%
    dplyr::select(source,varnames) %>%
    tidyr::unnest(.)
}



#' scrub_meltt_class
#'
#' drops any meltt_ class features from object. Necessary for unnesting.
#'
#' @param .data meltt_tbl
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scrub_meltt_class = function(.data){
  .data %>% map_df(.,function(x){
    if(any(str_detect(class(x),"meltt"))){
      class(x) = "list"
    }
    x
  })
}
