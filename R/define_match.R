#'define_match
#'
#'disambiguates between unique and duplicate entries using the input taxonomies
#'(i.e. assumptions about which events should be considered matches).
#'
#'@param .data A bundled meltt_tbl containing a `neighbor` feature.
#'@param ... Other arguments passed on to methods. Not currently used. (Later
#'  version will have the specific adjustments  arguments in the original meltt
#'  program)
#'
#'@return returns bundle meltt_dcf with match feature.
#'@importFrom magrittr "%>%"
#'@export
#'
#' @examples
#'
#'
#'# Simulate event data
#' set.seed(123)
#' D = simulate_event_data(N=50,
#'                         known.events = 5,
#'                         known.episodes = 0,
#'                         known.episodes.events = 0,
#'                         n.datasets = 4,
#'                         time.distort = 1,
#'                         space.distort = 1,
#'                         episode.window = 10,
#'                         min_date = as.Date("2001-01-01"),
#'                         max_date =as.Date("2001-02-01"),
#'                         lat.bounds = c(-1,1),
#'                         lon.bounds =c(-1,1),
#'                         taxonomies = data.frame(k=c(rep(1,4),rep(2,3),rep(3,4)),
#'                                                 depth = c(10,5,3,2, 13,6,3, 15,10,5,2)))
#' var_1 = D$user.taxonomies$var_1 %>% rename(source=data.source)
#' var_2 = D$user.taxonomies$var_2 %>% rename(source=data.source)
#' var_3 = D$user.taxonomies$var_3 %>% rename(source=data.source)
#' D1 = D$user.data %>% filter(dataset=="D1")
#' D2 = D$user.data %>% filter(dataset=="D2")
#' D3 = D$user.data %>% filter(dataset=="D3")
#' D4 = D$user.data %>% filter(dataset=="D4")
#'
#'
#' bundle(D1,D2,D3,D4) %>%
#' define_space(.) %>%
#' define_time(D1="date",D2="date",D3="date",D4="date") %>%
#' define_close(.,space=2,time=1) %>%
#' define_taxonomy(var_1,var_2,var_3) -> .data
#'
#'
define_match = function(.data,...){
  UseMethod("define_match")
}


define_match.meltt_tbl = function(.data,...){


  # Clean taxonomies: drop var and index


  # Gen. data matrix for algorithm

  sub = .data[ ,colnames(.data) == "source" | is_neighbor(.data) | is_taxonomy(.data)] %>%
    unbundle(.)

  map(function(x) is_tibble(x))

  .data %>% map(data,function(x) x )

  .data %>% unbundle(.)

  s = .data[,"source"]
  n = .data[,is_neighbor(.data)] %>% as_meltt(.) %>%  unbundle(.)
  t = .data[,is_taxonomy(.data)] %>% unnest

  sub = .data[ , is_neighbor(.data) | is_taxonomy(.data)]
  .data %>%
    dplyr::select(source) %>%
    dplyr::bind_cols(.,sub)



    dplyr::mutate_all(funs(as.list)) %>%
    dplyr::mutate(source=as.character(source)) %>%
    tidyr::unnest(.,.id='source')






  sub %>%  unnest(.)

  col_names <- colnames(sub)[-1]
  sub %>%
    unnest() %>% {
      select(.,colnames(.)[  !str_detect(colnames(.),"source_index+\\d")]) %>%
        select(.,colnames(.)[ !str_detect(colnames(.),col_names) ])
    }

  .data %>% map(function(x) is.meltt_assumption(x))
  sub



    select(-data,-spatial_feature,-temporal_feature) %>%
    unnest()


}









# ----



