context("test-define_time.R")

test_that("Test behavior for guess_date().", {

  # Test retrieval of different date configurations
  dates = c("Feb. 13, 1988","Mar. 3, 2014","Apr. 2, 2014","4-4-2018")
  exp = as.Date(c("1988-02-13","2014-03-03","2014-04-02",NA))
  expect_equal(guess_date(dates),exp)


  dates = c("4-4-18","1-1-00","1-30-99","6-22-97","4-18-01")
  exp = as.Date(c("2018-04-04","2000-01-01","1999-01-30","1997-06-22","2001-04-18"))
  expect_equal(guess_date(dates),exp)


  dates = c("February 3 2017","August 8 2014","September 24 2000")
  exp = as.Date(c("2017-02-03","2014-08-08","2000-09-24"))
  expect_equal(guess_date(dates),exp)


  dates = c("2/3/2000","2/1/1998","6/3/2007")
  exp = as.Date(c("2000-03-02","1998-02-01","2007-03-06"))
  expect_equal(guess_date(dates),exp)


  # If input is class Date, should return the vector
  exp = as.Date(c("2018-04-04","2000-01-01","1999-01-30","1997-06-22","2001-04-18"))
  expect_equal(guess_date(exp),exp)

  # Test that error is return if numeric is entered
  expect_error(guess_date(c(1,44567,16789)))


})



# Generate Fake Data
set.seed(123)
N = 10
dates = seq(from = as.Date("2007-01-01"),to=as.Date("2007-01-01"),by = "day")
lons = runif(N,0,.1)
lats = runif(N,0,.1)
d1 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
d2 = data.frame(startdate = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
d3 = data.frame(date_start = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))

bundle(d1,d2,d3) %>% define_time(d1="date",d2="startdate",d3="date_start")

test_that("Test behavior for define_time().", {

  # Expect class (general element)
  expect_s3_class(bundle(d1,d2,d3) %>% define_time(d1="date",d2="startdate",d3="date_start"),"meltt_tbl")

  # Test that function produces date elements
  bundle(d1,d2,d3) %>% define_time(d1="date",d2="startdate",d3="date_start") %>%
    dplyr::mutate(dateclass = purrr::map(temporal_feature,function(x) class(x)=="Date" )) %>%
    .$dateclass %>% unlist %>% all(.) %>% expect_true(.)

  # Test specification errors.
  expect_error(bundle(d1,d2,d3) %>% define_time)
  expect_error(bundle(d1,d2,d3) %>% define_time(d1="date",d2="startdate"))
  expect_error(bundle(d1,d2,d3) %>% define_time(d1="date",d2="startdate",d3="date_start",d4="date"))
  expect_error(bundle(d1,d2,d3) %>% define_time(d1="date",d2="startdate"))
  expect_error(bundle(d1,d2,d3) %>% define_time(d1="date",d2="startdate",DD3="date_start"))
  expect_error(bundle(d1,d2,d3) %>% define_time(d1="date",d2="startdate",d2="date_start"))
  expect_error(bundle(d1,d2,d3) %>% define_time(d1="enddate",d2="startdate",d3="date_start"))

  # Test input errors.
  d3$date_start = format.Date(d3$date_start,"%B %d %Y") # Different kind of Entry
  bundle(d1,d2,d3) %>% define_time(d1="date",d2="startdate",d3="date_start") %>%
    .$temporal_feature %>% .[[3]] %>% class(.) %>%
    expect_equal(.,"Date")

})


