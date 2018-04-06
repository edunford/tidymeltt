#' define_time
#'
#' Set the temporal variable. `define_time()` and `define_space()` are used to
#' establish the parameters that set the spatio-temporal window.
#'
#' Only individual dates are specified, no episodal windows (i.e. start-end
#' dates). To deal with episodal events, see `episodal_boot()`
#'
#' @param .data A bundled meltt.tbl_df.
#' @param ...	pass the name of each primary date column and the bundled dataset it points to.
#'
#' @return returns bundle with defined date features.
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#'
#' set.seed(123)
#' N = 10
#' dates = seq(from = as.Date("2007-01-01"),to=as.Date("2007-01-01"),by = "day")
#' lons = runif(N,0,.1)
#' lats = runif(N,0,.1)
#' d1 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
#' d2 = data.frame(startdate = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
#' d3 = data.frame(date_start = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
#'
#' bundle(d1,d2,d3) %>% define_time(d1="date",d2="startdate",d3="date_start")
#'
define_time = function(.data,...){
  UseMethod("define_time")
}

#' @export
define_time.meltt_tbl = function(.data,...){


  # Check that inputs match data.
  dat_cols = list(...)
  if(length(dat_cols)==0){
    stop("\nNo date columns specified. Please specify which date variable should be used to define time.")
  }else{
    if(length(dat_cols) > nrow(.data)){
      stop("\nToo many date columns specified. There are only ",nrow(.data),
           " bundled datasets. The number of specified dates should correspond with that number.\nYou have ",
           length(dat_cols)," ==> that's ", length(dat_cols) - nrow(.data)," too many!\n")
    }
    if(length(dat_cols) < nrow(.data)){
      stop("\nNot enough date columns specified. There are ",nrow(.data),
           " bundled datasets. The number of specified dates should correspond with that number.\nYou have ",
           length(dat_cols)," ==> ", nrow(.data) - length(dat_cols)," more required.\n")
    }
  }

  obj = dat_cols %>%
    dplyr::tibble(source=names(.),date_name=.) %>%
    tidyr::unnest(.) %>%
    dplyr::right_join(.data,by="source") %>%

    # Check that input and source names match
    {if(any(is.na(.$date_name))){
      stop("\nAll inputs don't point to a unique bundled source. Date variable name must point to a unique dataset in the bundle.")
      }else{.} }  %>%

    dplyr::group_by(source) %>%
    dplyr::mutate(temporal_feature=purrr::map(data,~dplyr::select(.x,date_name)),
                  temporal_feature=purrr::map(temporal_feature,function(x) guess_date(x[,1])) ) %>%
    dplyr::select(-date_name) %>%
    dplyr::ungroup(.)

  # Define Class
  class(obj) <- c(class(obj),"meltt_tbl")

  # Return object
  return(obj)


}


