#' define_space
#'
#' Define the spatial variable as an sf object. `define_time()` and `define_space()` are used to
#' establish the parameters that set the spatio-temporal window using `proximate()`.
#'
#' @param .data A bundled meltt.tbl_df.
#' @param ...	Other arguments passed on to methods. Not currently used.
#'
#' @return returns bundle with defined spatial features.
#' @importFrom magrittr "%>%"
#' @export
#'
#'
#' @examples
#' set.seed(123)
#' N = 10
#' dates = seq(from = as.Date("2007-01-01"),to=as.Date("2007-01-01"),by = "day")
#' lons = runif(N,0,.1)
#' lats = runif(N,0,.1)
#' d1 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
#' d2 = data.frame(date = sample(dates,size = N,replace = T),Latitude = sample(lats,N,T),Longitude = sample(lons,N,T))
#' d3 = data.frame(date = sample(dates,size = N,replace = T),lat = sample(lats,N,T),lon = sample(lons,N,T))
#'
#' bundle(d1,d2,d3) %>% define_space
#'
define_space = function(.data,...){
  UseMethod("define_space")
}

#' @export
define_space.meltt_tbl = function(.data,...){

  # Throw error if no longitude-latitude coordinates cannot be located for any bundled dataset.
  det = .data %>% dplyr::mutate(is_not_lon = purrr::map_lgl(data, function (x) !any(grepl("longitude",colnames(x),ignore.case=T)) ),
                                is_not_lat = purrr::map_lgl(data, function (x) !any(grepl("latitude",colnames(x),ignore.case=T)) ))
  if( any(det$is_not_lon | det$is_not_lat) ){
    det %>%
      dplyr::filter(is_not_lon | is_not_lat) %>%
      dplyr::mutate(missing = ifelse(is_not_lon,"a dedicated 'Longitude' column could not be detected.",NA),
             missing = ifelse(is_not_lat,"a dedicated 'Latitude' column could not be detected.",missing),
             missing = ifelse(is_not_lat & is_not_lon,"dedicated 'Longitude' & 'Latitude' columns could not be detected.",missing),
             message = paste0("For source `", source,"` ",missing) ) %>%
      dplyr::select(message) %>% paste0(.,collapse="\n") %>%
      stop("\n\n",.,"\n\nRename columns to follow standard naming conventions for coordinate systems (e.g. 'longitude'/'latitude').")
  }


  # Generate Spatial feature
  obj = .data %>% dplyr::group_by(source) %>%
    dplyr::mutate(

      # Locate Longitude and Latitude
      spatial_feature=purrr::map(data, ~dplyr::select(.x,dplyr::contains("longitude",ignore.case = T),dplyr::contains("latitude",ignore.case = T))),

      # throw error if there is more than one named coordinate column (e.g. latitude and old_latitude)
      spatial_feature=purrr::map2(spatial_feature,source, function(x,y){
        if(ncol(x)>2){
          stop("\n\nMore than one Latitude/Longitude column picked up in source ",y,". Rename column names so that there is only one set of coordinate columns")
        }else{
          x
        }
      }),

      # rename column names
      spatial_feature=purrr::map(spatial_feature,
                                 function(x) {colnames(x) = c("longitude","latitude");x}
      ),

      # Scan through and clean issue columns
      spatial_feature=purrr::map(spatial_feature,
                                 function(x) {x %>% dplyr::mutate(
                                   longitude = as.numeric(longitude),
                                   latitude = as.numeric(latitude),
                                   missing_coord_info = ifelse(is.na(longitude)|is.na(latitude),1,0),
                                   latitude = ifelse(missing_coord_info == 1,0,latitude),
                                   longitude = ifelse(missing_coord_info == 1,0,longitude)
                                 ) }
      ),

      # Generate geometry
      spatial_feature = purrr::map(spatial_feature,
                                   ~sf::st_as_sf(.x,coords=c("longitude","latitude"), crs = 4326, agr = "constant")
      )) %>%
    dplyr::select(source,data,spatial_feature) %>%
    dplyr::ungroup(.)


  # Define Class
  class(obj) <- c(class(obj),"meltt_tbl")

  # Return object
  return(obj)
}







#' missing_space
#'
#' Report the number and proportion of missing geo-coordinate entries by
#' bundeled datasets. Missingness is usually due to an entry being marked as
#' `NA` or defined as the wrong class.
#'
#' @param .data A bundled meltt.tbl_df.
#' @param ... Other arguments passed on to methods. Not currently used.
#'
#' @return prints report of the number/proportion of missing coordinate entries.
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
#' d2 = data.frame(date = sample(dates,size = N,replace = T),Latitude = sample(lats,N,T),Longitude = sample(lons,N,T))
#' d3 = data.frame(date = sample(dates,size = N,replace = T),LATITUDE = sample(lats,N,T),LONGITUDE = sample(lons,N,T))
#'
#' d3$LATITUDE[1:3] = NA
#'
#' bundle(d1,d2,d3) %>% define_space %>% missing_space
#'
missing_space <- function(.data,...){
  UseMethod("missing_space")
}

missing_space.meltt_tbl <- function(.data,...){

  if( !"spatial_feature" %in% colnames(.data)  ){
    stop("\nNo spatial feature detected. Use define_space() to set one up.\n")
  }

  # Report the proportion of missing data locations by dataset.
  .data %>%
    dplyr::transmute(source,
                     `No. Missing Coord Entries` = purrr::map(spatial_feature, function(x) sum(x$missing_coord_info) ),
                     `Prop. Missing Coord Entries` = purrr::map(spatial_feature, function(x) paste0(round( (sum(x$missing_coord_info)/nrow(x)) ,3)*100,"%") )) %>%
    tidyr::unnest(.) %>%
    dplyr::ungroup(.) %>%
    knitr::kable(.,format="pandoc")
}


