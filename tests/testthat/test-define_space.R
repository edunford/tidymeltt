context("test-define_space.R")



# Class and Property Conditions -------------------------------------------

# Generate Fake Test Data
set.seed(123)
N = 10
dates = seq(from = as.Date("2007-01-01"),to=as.Date("2007-01-01"),by = "day")
lons = runif(N,0,.1)
lats = runif(N,0,.1)
d1 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
d2 = data.frame(date = sample(dates,size = N,replace = T),Latitude = sample(lats,N,T),Longitude = sample(lons,N,T))
d3 = data.frame(date = sample(dates,size = N,replace = T),LATITUDE = sample(lats,N,T),LONGITUDE = sample(lons,N,T))
D = bundle(d1,d2,d3) %>% define_space(.)


test_that("Test class and properties for define_space().", {

  # spatial_feature columns generate
  expect_equal( "spatial_feature"  %in%  colnames(D), T )

  # spatial_feature is a nested list
  expect_equal( class(D$spatial_feature) == "list", T )

  # spatial_feature is of class `sf`
  expect_equal( all(lapply(D$spatial_feature,class) %>% sapply(.,function(x) any(x %in% "sf" ))) ,T)

  # spatial_feature are sfc_POINT classes
  expect_true(all(lapply(D$spatial_feature,function(x) class(x$geometry)) %>% sapply(.,function(x) any(x %in% "sfc_POINT" ))))

})


# Missing Data Conditions -------------------------------------------------

# Generate Fake Test Data
set.seed(123)
N = 10
dates = seq(from = as.Date("2007-01-01"),to=as.Date("2007-01-01"),by = "day")
lons = runif(N,0,.1)
lats = runif(N,0,.1)
d1 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
d2 = data.frame(date = sample(dates,size = N,replace = T),Latitude = sample(lats,N,T),Longitude = sample(lons,N,T))
d3 = data.frame(date = sample(dates,size = N,replace = T),LATITUDE = sample(lats,N,T),LONGITUDE = sample(lons,N,T))
d1$latitude[1:3]  = NA
d2$Longitude[3:6]  = NA
D = bundle(d1,d2,d3) %>% define_space(.)

test_that("Test handling of missing coordinate data for define_space().", {

  # Test that missing values are correctly detected
  D$spatial_feature %>% sapply(function(x) sum(x$missing_coord_info)) %>%
  expect_equal(.,c(3,4,0))

  # Test the missing points are converted to 0, 0 generic point locations
  D$spatial_feature %>% sapply(function(x) sum(as.character(x$geometry) %in% "c(0, 0)") ) %>%
    expect_equal(.,c(3,4,0))

  # Test that the right conversions are going through when mapping the spatial points to SF geometries (test just one instance)
  locs = sf::st_as_sf(d1[4:10,c("longitude","latitude")],coords=c("longitude","latitude"), crs = 4326, agr = "constant")
  expect_equal(as.character(D$spatial_feature[[1]]$geometry[4:10]),as.character(locs$geometry))
})


# test missing_space() ------------------------------------------------------
test_that("Test that missing_space() renders correct report.", {

  # Test that the right style report is rendered
  txt = D %>% missing_space
  c( gsub(" ","",txt[3])  == "d1330%",
     gsub(" ","",txt[4]) == "d2440%",
     gsub(" ","",txt[5]) == "d300%") %>%
    all(.) %>%
    expect_true(.)
})



# Naming Convention Conditions -------------------------------------------------

# Generate Fake Test Data
set.seed(123)
N = 10
dates = seq(from = as.Date("2007-01-01"),to=as.Date("2007-01-01"),by = "day")
lons = runif(N,0,.1)
lats = runif(N,0,.1)
d1 = data.frame(date = sample(dates,size = N,replace = T),latitude = sample(lats,N,T),longitude = sample(lons,N,T))
d2 = data.frame(date = sample(dates,size = N,replace = T),Latitude = sample(lats,N,T),Longitude = sample(lons,N,T))
D = bundle(d1,d2,d3)

test_that("Test handling of similarly or mis-named coordinate features.", {

  # Mis-named column
  d3 = data.frame(date = sample(dates,size = N,replace = T),LAT = sample(lats,N,T),LONG = sample(lons,N,T))
  expect_error(bundle(d1,d2,d3) %>% define_space(.))

  # Multiple Similarly named columns
  d3 = data.frame(date = sample(dates,size = N,replace = T),LATITUDE = sample(lats,N,T),LONGITUDE = sample(lons,N,T),old_latitude=rnorm(N))
  expect_error(bundle(d1,d2,d3) %>% define_space(.))

})
