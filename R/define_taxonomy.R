#' define_taxonomy
#'
#'@description
#'
#'
#' Define the secondary information to be used to disambiguate between
#' neighboring entries.
#'
#'
#' Taxonomies:
#'
#'
#'  * Generalize mutually occurring variables across bundled datasets.
#'
#'  * Provide schemas that offer guidance on how mutually occurring variables map
#'onto one another.
#'
#'  * Provide flexibility in how mutually occurring variables map by creating
#' levels that go from granular to broad.
#'
#'
#' Formatting Requirement: Taxonomy data.frames _must_ be formatted as follows:
#'
#' 1. the first column must correspond with the name of the bundled data and be
#'entitled source.
#'
#' 2. the second column must correspond with the target taxonomy variable. This
#'name should be consistent across bundled datasets.
#'
#' Ex: source  variable  level_granular  ...  level_broad
#'
#'
#'@param .data A bundled meltt_tbl.
#'@param ... pass taxonomies as data.frame or tibble objects. First column
#'  should correspond with the source (as the bundled data is named) and second
#'  variable should correspond with names variable.
#'
#' @return returns bundle with input taxonomy features.
#' @importFrom magrittr "%>%"
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
#' define_taxonomy(var_1,var_2,var_3) -> .data
#'
#'
define_taxonomy <- function(.data,...){
  UseMethod("define_taxonomy")
}


#' @export
define_taxonomy.meltt_tbl <- function(.data,...){

  # collect and bundle input taxonomies
  input_assumptions = rlang::exprs(...)

  if(length(input_assumptions)==0){error("No ")}

  .tax <- input_assumptions %>%

    # Evaluate and curate
    purrr::map(.,function(x){
      tmp <- eval(x) %>%
        dplyr::mutate_if(is.factor,as.character)
      colnames(tmp)[2] <- as.character(x)

      # map assumption onto input data
      tmp2 <- .data %>%
        dplyr::select(source,data) %>%
        tidyr::unnest(.) %>%
        dplyr::left_join(.,tmp,by=colnames(tmp)[1:2]) %>%
        dplyr::select(source,source_index,colnames(tmp)[-1]) %>%
        dplyr::group_by(source) %>%
        tidyr::nest(.)
      colnames(tmp2)[2] <- as.character(x) # rename nest as input variable
      tmp2
    })

  # map assumptions onto bundled data
  obj <- .data
  for(i in 1:length(.tax)){
    .tax[[i]][2] <- as_taxonomy(.tax[[i]][2]) # give assumptions special class to ease identification
    obj <- left_join(obj,.tax[[i]],by="source")
  }


  # Define Class
  class(obj) <- c(class(obj),"meltt_tbl")

  # Return object
  return(obj)
}







# meltt_taxonomy class --------------------------------------------------


#' as_taxonomy
#'
#' @param .data A bundled meltt.tbl_df.
#' @param ...	Other arguments passed on to methods. Not currently used.
#'
#' @return bundled data with additional "meltt_assumption" column class feature
#' @export
#'
#' @examples
as_taxonomy = function(.data,...){
  UseMethod("as_taxonomy")
}

#' @export
as_taxonomy.meltt_tbl = function(.data,...){
  for(i in seq_along(.data)){class(.data[[i]]) <- c(class(.data[[i]]),"meltt_assumption")}
  .data
}


#' @export
as_taxonomy.tbl_df = function(.data,...){
  for(i in seq_along(.data)){class(.data[[i]]) <- c(class(.data[[i]]),"meltt_assumption")}
  .data
}





#' is_taxonomy
#'
#' @description
#'
#' Deterimine if columns in bundled data are input assumptions.
#'
#' @param .data A bundled meltt_tbl.
#'
#' @return logical scalar value.
#' @export
#'
#' @examples
#'
#' is_taxonomy(.data)
#'
is_taxonomy = function(.data){
  UseMethod("is_taxonomy")
}

#' @export
is_taxonomy.meltt_tbl = function(.data){
  sapply(.data,function(x) any("meltt_assumption" %in% class(x)) )
}


