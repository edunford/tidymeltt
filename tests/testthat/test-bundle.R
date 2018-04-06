context("test-bundle.R")

test_that("Test that bundle only responds to data.frame and tibble objects.", {

  # Ensure bundle breaks on numeric vector
  expect_error(bundle(x=1:3,y=1:4))

  # Ensure bundle breaks on string vectors
  expect_error(bundle(x=as.character(1:3),y=as.character(1:4)))

  # Ensure bundle breaks on lists
  expect_error(bundle(x=as.list(1:3),y=as.list(1:4)))

  expect_error(bundle(x=matrix(1:3),y=matrix(1:4)))

  # Ensure data.frame class
  expect_true(is.data.frame(bundle(x=data.frame(x=1:4),y=data.frame(y=1:4))))

  # ensure is tibble
  expect_true(tibble::is.tibble(bundle(x=data.frame(x=1:4),y=data.frame(y=1:4))))

  # Ensure that tibble inputs work
  expect_true(tibble::is.tibble(bundle(x=tibble::tibble(x=1:4),y=tibble::tibble(y=1:4))))

  # Ensure is meltt tibble class "meltt.tbl_df"
  expect_s3_class(bundle(x=tibble::tibble(x=1:4),y=tibble::tibble(y=1:4)),"meltt_tbl")

})


