
.data = bundle(dat1,dat2)


def_space = function(.data,by){

  # Standardizes naming conventions of variables

  # e.g by = c( dat1=c("lon","lat"),dat2=c("latitude","longitude") ))

}


def_time = function(.data,by){

  # Standardizes time conventions

  # e.g by = c( dat1="date",dat2="startdate"))

}




proximate = function(.data,s,t){

  # Calculate Spatio-temporal proximate entries

  # class "meltt.proximate"
}


# taxonomies = function(.data,...){
#
#   # Maps taxonomies onto data
#
#   # Receives class "meltt.proximate"
#
#   # class "meltt.taxonomy"
# }



disambiguate = function(.data,...){

  # Once you have prox entries, you want to know which are "true" and which are not.

  # ... is the input taxonomies that one is disambituating with.

}


co_occur = function(.data,s.range,t.range){

  # Knox analysis with spatio-temporal co-occurrence.

  # Use all the data if N is low, or sample if N is high

}





list(dat1,dat2) %>% map_df(function(x) nest(x)) %>%
  mutate(eval(quote(dat1)))

names(c(dat1,dat2))

tibble(source = !!dat1,data=)


eval(quote(2 + 2))




