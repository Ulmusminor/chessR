## get_each_player_chessdotcom tests (tbd)


## get_raw_chessdotcom tests

test_that("get_raw_chessdotcom() works", {
  testthat::skip_on_cran()
  expect_type(chessdotcom_hikaru_recent, "list")
  expect_true(nrow(chessdotcom_hikaru_recent) != 0)
})
