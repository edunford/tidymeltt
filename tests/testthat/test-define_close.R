context("test-define_close.R")

test_that("Check that proximate function behaves properly.", {

  # Generate Fake Spatio-Temporal Data
  set.seed(123)
  N = 10
  dates = seq(from = as.Date("2007-01-01"),to=as.Date("2007-01-01"),by = "day")
  lons = runif(N,0,.1)
  lats = runif(N,0,.1)
  d1 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
  d2 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
  d3 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))


  test_obj = bundle(d1,d2,d3) %>%
    define_space(.) %>%
    define_time(d1="date",d2="date",d3="date") %>%
    define_close(.,space=2,time=1)

  # Ensure object export as meltt tibble class "meltt_tbl"
  expect_s3_class(test_obj,"meltt_tbl")


  test_obj %>% unbundle(data,neighbors) %>%
    filter(neighbor_id==2)


})
