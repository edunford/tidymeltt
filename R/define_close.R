#' define_close
#'
#' Locates all events from one bundled dataset that fall near (i.e. within a
#' the same space/time window) an event from another bundled dataset.
#'
#' @param .data A bundled meltt_tbl containing a `spatial` and `temporal`
#'   features.
#' @param space integer that defines the spatial window in terms of kilometers
#'   (km). Default  is 1 km.
#' @param time integer that defines the temporal window in terms of days.
#'   Default is 1 day.
#'
#' @return bundled meltt_tbl with a `neighbor` feature. `neighbors` index
#'   proximate events across bundled datasets
#' @export
#' @importFrom magrittr "%>%"
#' @useDynLib tidymeltt
#'
#' @examples
#'
#' # Generate Fake Spatio-Temporal Data
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
define_close = function(.data,space,time){
  UseMethod("define_close")
}


#' @export
define_close.meltt_tbl = function(.data,space=1,time=1){

  if(!class_scan(.data,"sf")){ stop("\n\nNo spatial feature detected! Use define_space() to define a spatial feature.\n") }
  if(!class_scan(.data,"Date")){ stop("\n\nNo temporal feature detected! Use define_time() to define a temporal feature.\n") }


  # If data is above a certain N threshold, show progress bar.
  N = .data$data %>% lapply(.,function(x) nrow(x)) %>% unlist %>% sum(.)
  if(N>=5e3){verbose=T}else{verbose=F}

  # Draw out relevant elements from each bundle.
  layout = .data %>%
    dplyr::transmute(source = forcats::fct_inorder(source),
                     source_index = purrr::map(data,~dplyr::select(.x,source_index)),
                     date=temporal_feature,
                     purrr::map(spatial_feature,function(x) sf::st_coordinates(x) %>% tibble::as.tibble(.) %>% dplyr::select(latitude=X,longitude=Y) ),
                     purrr::map(spatial_feature,~dplyr::select(.x,missing_coord_info))
    ) %>%
    tidyr::unnest(.) %>%
    dplyr::mutate(missing = ifelse(is.na(date),1,missing_coord_info)) %>%
    dplyr::select(source,date,latitude,longitude,source_index,missing)

  # Locate proximate events
  ind = layout %>%
    dplyr::filter(missing!=1) %>%
    dplyr::transmute(source=forcats::fct_inorder(source),date,latitude,longitude) %>%
    data.matrix() %>%
    proximity(dat=.,t = time,s=space,verbose=verbose) %>% .[-1,]

  # Allocate cohorts to entries
  cohorts = ind %>%
    tibble::as.tibble() %>%
    tidyr::gather(key, index,-V3) %>%
    dplyr::select(index,neighbor_id=V3) %>%
    dplyr::distinct(.) %>%
    dplyr::group_by(index) %>%
    tidyr::nest() %>%
    dplyr::rename(neighbor_id=data)
  layout$neighbor_id = 0
  layout[cohorts$index,]$neighbor_id =  cohorts$neighbor_id


  # re-bundle and stamp class
  obj = layout %>%
    dplyr::select(source,source_index,neighbor_id) %>%
    dplyr::group_by(source) %>%
    tidyr::nest(.) %>%
    dplyr::transmute(source=as.character(source),
                     neighbors=data)
  obj[,2] <- as_neighbor(obj[,2]) # give neighbor feature special class to ease downstream identification
  obj <- dplyr::right_join(.data,obj,by="source")

  # Define Class
  obj <- as_meltt(obj)

  return(obj)
}






# Class features  ---------------------------------------------------------



#' class_scan
#'
#' Scan through and report back all classes of the bundled data
#'
#' @param .data A bundled meltt_tbl.
#' @param locate_class detect it a specific class exists. Default is NULL.
#'
#' @return If locate_class is NULL, returns a list of all classes in each
#'   bundled column; else a logical value is return if the class exists.
#' @export
#'
#' @examples
class_scan = function(.data,locate_class=NULL){
  classes = .data %>% purrr::map(function(x) unique(unlist(purrr::map(x,class))))

  if(!is.null(locate_class)){
    any(classes %>% purrr::map_lgl(.,function(x) locate_class %in% x))
  }

}


#' as_neighbor
#'
#' @param .data A bundled meltt.tbl_df.
#' @param ...	Other arguments passed on to methods. Not currently used.
#'
#' @return bundled data with additional "meltt_neighbor" column class feature
#' @export
#'
#' @examples
as_neighbor = function(.data,...){
  UseMethod("as_neighbor")
}

#' @export
as_neighbor.tbl_df = function(.data,...){
  for(i in seq_along(.data)){class(.data[[i]]) <- c(class(.data[[i]]),"meltt_neighbor")}
  .data
}


#' is_neighbor
#'
#' @description
#'
#' Deterimine if columns in bundled data is a neighbor feature.
#'
#' @param .data A bundled meltt_tbl.
#'
#' @return logical scalar value.
#' @export
#'
#' @examples
#'
#' is_neighbor(.data)
#'
is_neighbor = function(.data){
  UseMethod("is_neighbor")
}

#' @export
is_neighbor.tbl_df = function(.data){
  sapply(.data,function(x) any("meltt_neighbor" %in% class(x)) )
}




